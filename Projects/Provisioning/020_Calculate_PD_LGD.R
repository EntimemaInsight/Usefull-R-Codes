# Load libraries
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(zoo)

# Load function for months calculation
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

### Disable scientific notation 
options(scipen=999)

PortfolioData = readRDS("./Output Data/010_Full_Portfolio_Old_Plus_New.rds") 

# Calculate discounted CFs for all credits - needed later for the LGD calculation
PortfolioDataWithPVs = PortfolioData %>%
  arrange(CreditNumber, ExportDate) %>%
  mutate(TotalPmtPerPeriod = PrincipalPaidForThePeriod + 
           InterestPaidForThePeriod + 
           PenaltyInterestRepaid + 
           NeustoikaProsrochenaLihvaPlatena +
           InterestRestructuredRepaid
  )%>%
  mutate(PV_Each_CFs_Days = TotalPmtPerPeriod/(((1 + InterestLevel/100/365))^NumberOfPeriodsDays),
         PV_Each_CFs_Months = TotalPmtPerPeriod/(((1 + InterestLevel/100/12))^NumberOfPeriodsMonths),
         DefaultPrincipalAndInterest = DefaultInterest + DefaultPrincipal,
         DefaultPrincipalAndInterestFromCurrentPrincipal = DefaultPrincipalAndInterest/CurrentPrincipal*100,
         DefaultPrincipalAndInterestFromCurrentPrincipal = ifelse(CurrentPrincipal == 0, 0, DefaultPrincipalAndInterestFromCurrentPrincipal),
         DefaultOver10PercentOfPrincipal = ifelse(DefaultPrincipalAndInterestFromCurrentPrincipal>10,1,0),
         DefaultAnyAmount = ifelse(DefaultPrincipalAndInterest>0,1,0))


CreditsStaged = PortfolioDataWithPVs %>%
  #filter(MaxMaturityDate <= as.Date("2018-09-01")) %>%
  #filter(CreditBeginDate <= as.Date("2015-12-31")) %>%
  # Label credits at each point in time - Stage 1, Stage 2 or Stage 3
  # For this purpose use a cumsum function which is reset every time a credit repays all its defaults
  group_by(CreditNumber) %>%
  mutate(ConseqDefaults = ave(DefaultAnyAmount, 
                              cumsum(DefaultAnyAmount == 0), 
                              FUN = cumsum)
  ) %>%
  mutate(Stage = case_when(ConseqDefaults %in% c(0,1) ~ "Stage1",
                           ConseqDefaults == 2 ~ "Stage2",
                           TRUE ~ "Stage3"),
         MOB = elapsed_months(ExportDate, CreditBeginDate),
         Default = ifelse(Stage == "Stage3",1,0)
  ) %>%
  ungroup() %>%
  group_by(CreditNumber) %>%
  mutate(HasEverDefaulted = max(Default)) %>%
  ungroup() 


### Get all default dates
AllDefaultDates = CreditsStaged %>%
  filter(Default == 1) %>%
  select(CreditNumber, ExportMonth, Default)


# Create helper dates table with next 12 months after every default date
DefaultsHelperDatesTable = expand.grid(FirstObsPoint = seq(min(AllDefaultDates$ExportMonth), max(AllDefaultDates$ExportMonth), by="months"),
                                       NextObsPoint = seq(min(AllDefaultDates$ExportMonth), as.Date("2022-08-01"), by="months")) %>%
  mutate(MOB = elapsed_months(NextObsPoint,FirstObsPoint)) %>%
  filter(MOB > 0 & MOB <=12) %>%
  select(-MOB)


# Join the helper dates table to the data frame with all default dates for the clients
AllDefaultDatesMarked = AllDefaultDates %>%
  left_join(DefaultsHelperDatesTable, by = c("ExportMonth"="FirstObsPoint")) %>%
  group_by(CreditNumber, NextObsPoint) %>%
  summarise(Default_Next_12_Fixed = max(Default)) %>%
  ungroup() %>%
  plyr::rename(c("NextObsPoint"="ExportMonth"))


# Join the fixed 12 month default window for the defaulted credits 
CreditsStaged = CreditsStaged %>%
  left_join(AllDefaultDatesMarked, by = c("CreditNumber" = "CreditNumber",
                                          "ExportMonth" = "ExportMonth")) %>%
  mutate(DefaultFixed = ifelse(is.na(Default_Next_12_Fixed),0,Default_Next_12_Fixed)) %>%
  mutate(StageFixed = case_when(DefaultFixed == 1 ~ "Stage3",
                                TRUE ~ Stage))


### Get stage 1 credits 
Stage1Credits = CreditsStaged %>%
  filter(Stage == "Stage1")

# Filter only stage 2 credits
Stage2Credits = CreditsStaged %>%
  filter(Stage == "Stage2")

# Filter only Stage 3 credits 
Stage3Credits = CreditsStaged %>%
  filter(StageFixed == "Stage3") 

# Create helper dates table - in order to see how many of the credits in stage 1 have defaulted in a period of 12 months after being at stage 1
HelperTable = expand.grid(FirstObsPoint = seq(min(Stage1Credits$ExportMonth), max(Stage1Credits$ExportMonth), by="months"),
                          NextObsPoint = seq(min(Stage1Credits$ExportMonth), max(Stage1Credits$ExportMonth), by="months")) %>%
  mutate(MOB = elapsed_months(NextObsPoint,FirstObsPoint)) %>%
  filter(MOB > 0 & MOB <=12) %>%
  select(-MOB)


# Join default credits to credits in stage 1 to calculate PD for the 12-months after a credit enters Stage 1
Stage1PDFull = Stage1Credits %>%
  left_join(HelperTable, by = c("ExportMonth" = "FirstObsPoint")) %>%
  select(CreditNumber, ExportMonth, NextObsPoint) %>%
  # Join the default credits at all points in time during a 12-month period after a credit enters stage 1
  # Skip all the rest since we consider only 12-month PDs here
  left_join(Stage3Credits %>%
              select(CreditNumber, ExportMonth, DefaultFixed), by = c("CreditNumber"="CreditNumber",
                                                                          "NextObsPoint"="ExportMonth")) %>%
  mutate(MOB = elapsed_months(NextObsPoint,ExportMonth)) %>%
  filter(!is.na(MOB)) %>%
  mutate(DefaultFixed = ifelse(is.na(DefaultFixed),0,DefaultFixed)) %>%
  # Get summary info on a rolling 12-month basis for all credits in respect to defaults 
  group_by(CreditNumber,ExportMonth) %>%
  summarise(Default_Next_12_Months = max(DefaultFixed)) %>%
  ungroup() %>%
  # Calculate the number of defaulted credits in a 12-month period after a group of credits has been in Stage1
  group_by(ExportMonth) %>%
  summarise(TotalCredits = n(),
            Defaulted = sum(Default_Next_12_Months)) %>%
  ungroup() %>%
  mutate(PD = Defaulted/TotalCredits*100) 

#writexl::write_xlsx(Stage1PDFull,"./Output Data/Stage1PDFullNew.xlsx")

# Calculate Stage2 PD
Stage2PD = Stage2Credits %>%
  select(CreditNumber, ExportMonth) %>%
  left_join(Stage3Credits %>%
              group_by(CreditNumber)%>%
              summarise(DefaultFixed = max(DefaultFixed)) %>%
              ungroup(), 
            by = "CreditNumber") %>%
  mutate(DefaultFixed = ifelse(is.na(DefaultFixed),0,DefaultFixed)) %>%
  group_by(ExportMonth) %>%
  summarise(TotalCredits = n(),
            Defaults = sum(DefaultFixed)) %>%
  ungroup() %>%
  mutate(PD = Defaults/TotalCredits*100)

Stage2HelperDatesTable = expand.grid(ExportMonth = seq(min(Stage2PD$ExportMonth), max(Stage2PD$ExportMonth), by="months"),
                                     ExportMonth2 = seq(min(Stage2PD$ExportMonth), max(Stage2PD$ExportMonth), by="months")) %>%
  mutate(MOB = elapsed_months(ExportMonth2,ExportMonth)) %>%
  filter(MOB == 0) %>%
  select(-MOB)

Stage2PD = Stage2HelperDatesTable %>%
  left_join(Stage2PD, by = "ExportMonth") %>%
  select(-ExportMonth2) %>%
  mutate(TotalCredits = ifelse(is.na(TotalCredits),0,TotalCredits),
         Defaults = ifelse(is.na(Defaults),0,Defaults),
         PD = ifelse(is.na(PD),0,PD)) %>%
  filter(TotalCredits > 0)


#writexl::write_xlsx(Stage2PD,"./Output Data/Stage2PDFullNew.xlsx")

### --- Calculate LGD --- ### 

# Params 
HigherRisk = 25  #2.5
AdministrativeCosts = 1.5 #2
HigherRate = 4.7 + 2.624722222 # The level of inflation plus the Risk free-rate for Bulgaria 

FlagCreditBeginDates = readxl::read_xlsx("./Input Data/Основни параметри кредити ФЛАГ 31.10.2021.xlsx", sheet = 1)

FlagCreditBeginDatesFormatted = FlagCreditBeginDates %>%
  tidyr::separate(CreditNumberAndDate,
                  into = c("CreditNumber","CreditBeginDate"),
                  sep = "/",
                  convert = FALSE) %>%
  mutate(CreditBeginDate = dmy(CreditBeginDate),
         CreditBeginYear = year(CreditBeginDate),
         CreditClosed = ifelse(RepaidSum == CreditAmountWithdrawn,1,0))

LGD = PortfolioDataWithPVs %>%
  filter(CreditNumber %in% Stage3Credits$CreditNumber) %>%
  filter(MaxMaturityDate <= as.Date("2021-10-01")) %>%
  group_by(CreditNumber) %>%
  mutate(AverageInterestFullPeriod = mean(InterestLevel, na.rm = T)) %>%
  ungroup() %>%
  mutate(FullNewDiscountRate = AverageInterestFullPeriod + HigherRisk + AdministrativeCosts + HigherRate) %>%
  mutate(PV_Each_CFs_Months = TotalPmtPerPeriod/(((1 + FullNewDiscountRate/100/12))^NumberOfPeriodsMonths)) %>%
  group_by(CreditNumber) %>%
  summarise(TotalRepaidDiscounted = sum(PV_Each_CFs_Months, na.rm = T),
            PrincipalDue = max(CurrentPrincipal, na.rm = T)) %>%
  ungroup() %>%
  mutate(TotalRepaidDiscounted = ifelse(TotalRepaidDiscounted>PrincipalDue,PrincipalDue,TotalRepaidDiscounted),
         Loss = round(TotalRepaidDiscounted - PrincipalDue, digits = 0)) %>%
  left_join(FlagCreditBeginDatesFormatted %>%
              select(CreditNumber, CreditBeginDate),
            by = c("CreditNumber"="CreditNumber")) %>%
  mutate(CreditNumber = as.numeric(CreditNumber)) %>%
  arrange(CreditNumber) %>%
  select(CreditNumber, CreditBeginDate, TotalRepaidDiscounted, PrincipalDue, Loss)

writexl::write_xlsx(LGD,"./Output Data/LGD.xlsx")
