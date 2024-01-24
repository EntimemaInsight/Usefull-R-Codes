# Load libraries
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(zoo)
library(writexl)

### Disable scientific notation 
options(scipen=999)

# Load Flag file with main credit information 
FlagCreditBeginDates = readxl::read_xlsx("./Input Data/Основни параметри кредити ФЛАГ 31.10.2021.xlsx", sheet = 1)

FlagCreditBeginDatesFormatted = FlagCreditBeginDates %>%
  tidyr::separate(CreditNumberAndDate,
                  into = c("CreditNumber","CreditBeginDate"),
                  sep = "/",
                  convert = FALSE) %>%
  mutate(CreditNumber = as.integer(CreditNumber),
         CreditBeginDate = dmy(CreditBeginDate),
         CreditBeginYear = year(CreditBeginDate),
         CreditClosed = ifelse(RepaidSum == CreditAmountWithdrawn,1,0)
         , Maturity_Grouped = case_when(CreditPeriodInMonths <= 9 ~ "1) До 9 месеца"
                                        , CreditPeriodInMonths <= 12 ~ "2) 10-12 месеца"
                                        , CreditPeriodInMonths <= 48 ~ "3) 1-4 години"
                                        , CreditPeriodInMonths > 48 ~ "4) Над 4 години"
                                        )
         , InterestrateGrouped = case_when(CurrentInterestRate <= 2 ~ "1) До 2%"
                                           , CurrentInterestRate <= 3 ~ "2) 2.01% до 3%"
                                           , CurrentInterestRate <= 5 ~ "3) 3.01% до 5%"
                                           , CurrentInterestRate > 5 ~ "4) Над 5%"
         )
         )

write_xlsx(FlagCreditBeginDatesFormatted, "./Output Data/010_FlagCreditBeginDatesFormatted.xlsx") # this is needed for graphs in PBI 


### Import data about Payments
FLAGPmtsHistory = readxl::read_excel("./Input Data/История на плащанията по заеми до 31.10.2021.xlsx",sheet = 1) 

# Extract Credit End Date
FLAGCreditEndDates = FLAGPmtsHistory %>%
  filter(!is.na(CreditEndDate)) %>%
  mutate(CreditNumber = str_replace(CreditNumber,"\\.",""))%>%
  mutate(CreditNumber = as.numeric(CreditNumber),
         DateSumAssimilation = as.Date(DateSumAssimilation),
         CreditEndDate = as.Date(CreditEndDate))

### Add the Credit End Dates to the initial table with the begin dates
FullCreditBeginEndDates = FlagCreditBeginDatesFormatted %>%
  left_join(FLAGCreditEndDates 
            %>% select(CreditNumber, DateSumAssimilation, CreditEndDate), 
            by = "CreditNumber") %>%
  mutate(CreditPeriodInMonths = elapsed_months(CreditEndDate,CreditBeginDate),
         CreditPeriodInMonthsGrouped = cut(CreditPeriodInMonths, breaks = c(0,9,12,18,24,48,200))) %>%
  #mutate(CreditNumber = case_when(CreditNumber %in% c(2221,2222,2223) ~ 2221,
  #                                CreditNumber %in% c(2081,2082,2083) ~ 2081,
  #                                CreditNumber %in% c(2001,2002) ~ 2001,
  #                                CreditNumber %in% c(1931,1932) ~ 1931,
  #                                CreditNumber %in% c(1891,1892,1893) ~ 1891,
  #                                CreditNumber %in% c(1881,1882) ~ 1881,
  #                                CreditNumber %in% c(1661,1662,1663) ~ 1661,
  #                                CreditNumber %in% c(2171,2172,1663) ~ 2171,
  #                                TRUE ~ CreditNumber)
  #       ) %>%
  group_by(CreditNumber)%>%
  mutate(CreditAmount = sum(CreditAmount),
         CreditAmountAfterAnex = sum(CreditAmountAfterAnex),
         CurrentInterestRate = mean(CurrentInterestRate),
         CreditAmountWithdrawn = sum(CreditAmountWithdrawn),
         AmountLeftForWithdrawals = sum(AmountLeftForWithdrawals),
         RepaidSum = sum(RepaidSum),
         LeftForRepayment = sum(LeftForRepayment)) %>%
  distinct(CreditNumber, .keep_all = T)

### Load Data - Portfolio Data - with a lot of fixed info about the credits
# However, there may still be some wrongly entered info about some credits
# This is the portoflio that was prepared during the first phase of the project in 2018 
PortfolioData = readRDS("./Input Data/PortfoliosFullInfoWithKids.rds") %>%
  mutate(PrincipalPaidForThePeriod = ifelse(CreditNumberFull == 433 & 
                                              ExportMonth == as.Date("2015-04-01"),
                                            1187262.28, 
                                            PrincipalPaidForThePeriod),
         CurrentPrincipal = ifelse(CreditNumberFull == 138 & ExportMonth == as.Date("2011-06-01"), 221483.0, CurrentPrincipal)
  ) %>%
  select(CreditNumberFull, everything())


# Load function for months calculation
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}


### Load portfolios files

FilesLocation2 = list.files("//hind.smartitbg.int\\FileServer\\Data Analyses\\Analysis\\9. Personal folders\\Velko Kamenov\\IFRS 9 Provisioning - FLAG - 2021\\Data2\\Data Sent 2021"
                            , full.names = T
                            )

test = as.Date(ceiling_date(ymd(paste0(gsub("_",".",str_sub(FilesLocation2[1],-12,-6)),".01")), unit = "months"))-1


Read_PF = function(x){
  
  df = openxlsx::read.xlsx(x, sheet = 1)
  
  df = df[-1,]
  
  df = df %>%
    mutate_if(is.numeric,as.character)
  
  date = as.Date(ceiling_date(ymd(paste0(gsub("_",".",str_sub(x,-12,-6)),".01")), unit = "months"))-1
  
  df$ExportDate = date
  
  return(df)
  
}

# Read all the files with the function
NewPortfolios <- lapply(FilesLocation2,Read_PF)

# Convert all the files to data frame
NewPortfoliosDF = do.call(bind_rows,lapply(NewPortfolios,data.frame))

NewPortfoliosDF_Wrangled = NewPortfoliosDF %>%
  select(-X21,-IS_FC7_ACCOUNT, -AMOUNT_DUE,-AMOUNT_SETTLED,-POG_PLAN_RED_LIHVA
         , -`Просрочена.лихва.по.погасителен.план`, -`Главница.по.погасителен.план`
         ) %>%
  mutate(FinalMaturityDate = as.Date(as.numeric(`Дата.на.крайно.погасяване.дд.мм.гг`), origin = "1899-12-30")
         #, 
         ) %>%
  select(-`Дата.на.крайно.погасяване.дд.мм.гг`) %>%
  plyr::rename(c("X..на.договор.за.кредит" = "CreditNumber"
                 , "Размер.на.кредита.в.левова.равностойност" = "CreditSum"
                 , "Лихвено.равнище.към.края.на.отчетния.период"="InterestLevel"
                 , "Текуща.главница..BGN"="CurrentPrincipal"
                 , "Неустойка.за.просрочена.лихва.за.отчетния.период...начислена..BGN" = "NeustoikaProsrochenaLihvaNachislena"
                 , "Неустойка.за.просрочена.лихва.за.отчетния.период...платена..BGN"="NeustoikaProsrochenaLihvaPlatena"
                 , "Начислени.лихви.при.реструктурирани.кредити"="InterestRestructuredAccrued"
                 , "Платени.лихви.при.реструктурирани.кредити"="InterestRestructuredRepaid"
                 , "Наказателна.лихва.за.просрочена.главница.за.отчетния.период...платена..BGN" = "PenaltyInterestRepaid"
                 , "Наказателна.лихва.за.просрочена.главница.за.отчетния.период...начислена..BGN" = "PenaltyInterestAccrued"
                 , "Просрочена.редовна.лихва.към.края.на.отчетния.период..BGN" = "DefaultInterest"
                 , "Просрочена.главница.към.края.на.отчетния.период..BGN" = "DefaultPrincipal"
                 , "Платена.главница.за.отчетния.период..BGN" = "PrincipalPaidForThePeriod"
                 , "Редовна.лихва.за.отчетния.период...платена..BGN" = "InterestPaidForThePeriod"
                 , "Редовна.лихва.за.отчетния.период...начислена..BGN" = "InterestAccruedForThePeriod"
                 #, 
                 )
               ) %>%
  mutate(CreditNumber = str_trim(CreditNumber)
         , CreditNumber = case_when(CreditNumber == "386 Подлимит 2" ~ "386"
                                    , CreditNumber == "ДБК 970" ~ "970"
                                    , CreditNumber == "ДБК 609" ~ "609"
                                    , CreditNumber == "400/2" ~ "400"
                                    , CreditNumber == "386.2" ~ "386"
                                    , CreditNumber == "400.2" ~ "402"
                                    , CreditNumber == "1064." ~ "1064"
                                    , TRUE ~ CreditNumber
                                    )
         ) %>%
  group_by(CreditNumber) %>%
  mutate(CreditBeginDate = min(ExportDate)
         , MaxMaturityDate = max(FinalMaturityDate)
         )%>%
  ungroup() %>%
  mutate(InDelay = ifelse(DefaultPrincipal > 0 | DefaultInterest > 0, 1, 0)
         , NumberOfPeriodsDays = difftime(ExportDate, CreditBeginDate, units = "days")
         , NumberOfPeriodsDays = as.numeric(NumberOfPeriodsDays)
         , NumberOfPeriodsMonths = elapsed_months(ExportDate, CreditBeginDate)
         ) %>%
  left_join(FullCreditBeginEndDates %>%
              mutate(CreditNumber = as.character(CreditNumber)) %>%
              select(CreditNumber, CreditType, CreditAmountWithdrawn, RepaidSum, DateSumAssimilation),
            by = "CreditNumber") %>%
  filter(!is.na(DateSumAssimilation)) %>%
  group_by(CreditNumber) %>%
  mutate(MaxMaturityDate = max(FinalMaturityDate)) %>%
  ungroup() %>%
  mutate(ExportMonth = floor_date(ExportDate, unit = "months")
         , CreditSum = as.numeric(CreditSum)
         , InterestLevel = as.numeric(InterestLevel)
         , CurrentPrincipal = as.numeric(CurrentPrincipal)
         , PrincipalPaidForThePeriod = as.numeric(PrincipalPaidForThePeriod)
         , InterestAccruedForThePeriod = as.numeric(InterestAccruedForThePeriod)
         , InterestPaidForThePeriod = as.numeric(InterestPaidForThePeriod)
         , NeustoikaProsrochenaLihvaNachislena = as.numeric(NeustoikaProsrochenaLihvaNachislena)
         , InterestRestructuredAccrued = as.numeric(InterestRestructuredAccrued)
         , NeustoikaProsrochenaLihvaPlatena = as.numeric(NeustoikaProsrochenaLihvaPlatena)
         , InterestRestructuredRepaid = as.numeric(InterestRestructuredRepaid)
         , PenaltyInterestAccrued = as.numeric(PenaltyInterestAccrued)
         , PenaltyInterestRepaid = as.numeric(PenaltyInterestRepaid)
         , DefaultPrincipal = as.numeric(DefaultPrincipal)
         , DefaultInterest = as.numeric(DefaultInterest)
         ) %>%
  select(CreditNumber, CreditSum, InterestLevel, CurrentPrincipal, ExportDate, FinalMaturityDate, PrincipalPaidForThePeriod
         , InterestAccruedForThePeriod, InterestPaidForThePeriod, NeustoikaProsrochenaLihvaNachislena
         , NeustoikaProsrochenaLihvaPlatena, InterestRestructuredAccrued, InterestRestructuredRepaid
         , PenaltyInterestAccrued, PenaltyInterestRepaid, DefaultPrincipal, DefaultInterest
         , ExportMonth, CreditBeginDate, InDelay, CreditType, CreditAmountWithdrawn, RepaidSum, DateSumAssimilation
         , NumberOfPeriodsDays, NumberOfPeriodsMonths, MaxMaturityDate
         )

saveRDS(NewPortfoliosDF_Wrangled, "./Output Data/010_NewPortfoliosDF_Wrangled.rds")

Full_Portfolio_Old_Plus_New = PortfolioData %>%
  plyr::rename(c("CreditNumberFull" = "CreditNumber")) %>%
  mutate(CreditNumber = as.character(CreditNumber)) %>%
  bind_rows(NewPortfoliosDF_Wrangled)

saveRDS(Full_Portfolio_Old_Plus_New, "./Output Data/010_Full_Portfolio_Old_Plus_New.rds")
  
write_xlsx(Full_Portfolio_Old_Plus_New, "./Output Data/010_Full_Portfolio_Old_Plus_New.xlsx")















