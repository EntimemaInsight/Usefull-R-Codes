rm(list = ls())
options(scipen = 999)
##### Start Block: Load Libraries #####

number_of_cores <- 12
Reporting_Date <- as.Date("2020-01-01")
##### End Block: Input Parameters #####

##### Start Block: Load Input data #####
source(".\\Functions\\F_Elapsed_Months.R")
##### End Block: Load Input data #####
#### 01 Connect to the database ####
connection_easy_bg <- DBI::dbConnect(odbc::odbc()
                                     , driver= "SQL Server"
                                     , server = 'hidalgo'
                                     , database = "BISmart")
##### Start Block: Get all new loans #####
Query <- paste0("select 
                a.CodeContract,
                a.CreditSK,
                a.CreditAccountSK,
                a.ClientSK,
                c.EGN,
                a.StatusSK,
                b.ProductName,
                a.ScoringType,
                a.Scoring,
                a.ScoringLimit, 
                a.Sum, 
                a.Interest,
                a.CreditPayment,
                a.PeriodSK,
                a.ScoringModelDecision, 
                a.CreditBeginDate,
                a.CompleteDate,
                a.DateRefused,
                a.DateDenied,
                a.DateApproved,
                a.RefinanceType,
                a.PreviousCreditsCount
                FROM [BIsmart].[dwh].[DimCredit] a
                LEFT JOIN [dwh].[DimProduct] b ON a.ProductSK = b.ProductSK 
                LEFT JOIN [dwh].[DimClient] c ON a.ClientSK = c.ClientSK 
                WHERE a.latest = 1;")

All_Loans <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))

New_Loans <- All_Loans %>%
  filter(is.na(DateRefused), is.na(DateDenied), !is.na(DateApproved),
         DateApproved >= as.Date("2019-06-01"),
         DateApproved <= as.Date("2019-12-31"))

Scorings_per_month <- All_Loans %>%
  mutate(DateApproved = floor_date(DateApproved, unit = "month")) %>%
  filter(DateApproved > as.Date("2019-01-01"),
         Scoring == -1) %>%
  group_by(DateApproved, ScoringType) %>%
  summarize(Count = n()) %>%
  ungroup()


Query <- paste0("select  
max(DateSk) as LastDateSK,
max(Date) as LastDate,
min(DateSk) as FirstDateSK,
min(Date) as FirstDate
from dwh.dimDate
group by Year,Month
order by LastDate
;")

Dates <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query)) %>%
  mutate(LastDate = as.Date(LastDate),
         FirstDate = as.Date(FirstDate)) %>%
  filter(FirstDate >= as.Date("2017-12-01"),
         FirstDate < as.Date("2020-02-01"))



downloand_credit_progress <- function(arg1)
{
  YYYYMM <- substr(arg1, 1, 6)
  tableName <- paste("dwh.FactCreditBalances", YYYYMM, sep = "")
  
  Query <- paste0("
  select
  fcb.CreditAccountSK,
  fcb.CurrentDelay,
  fcb.MaxDelay,
  fcb.PenaltyPaid + fcb.PrincipalPaid + fcb.Overpaid + fcb.InterestPaid as TotalPaid
  from ", tableName,"  fcb
  inner join dwh.DimDate d on fcb.DateSK = d.DateSK
  where fcb.CompleteDate is NULL or
                  fcb.CompleteDate >= d.Date")
  
  output <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query)) %>%
    group_by(CreditAccountSK) %>%
    summarize(MaxDelay = max(CurrentDelay),
              MaxDelayMaxDelay = max(MaxDelay),
              TotalPaid = max(TotalPaid)) %>%
    ungroup() %>%
    mutate(Date = arg1)
  
  print(arg1)
  
  return(output)
}


All_Portfolios <- lapply(Dates$FirstDateSK, downloand_credit_progress)

All_Portfolios_table <- do.call(bind_rows, All_Portfolios) %>%
  arrange(CreditAccountSK, Date)


All_Portfolios_table1 <- left_join(All_Portfolios_table, All_Loans %>% select(CreditAccountSK, CreditBeginDate, Sum, CompleteDate), by = "CreditAccountSK") %>%
  filter(CreditBeginDate >= as.Date("2018-06-30"))%>%
  mutate(Date = ymd(as.character(Date)),
         CreditBeginDate = as.Date(CreditBeginDate),
         MonthsDefault30 = elapsed_months(Date, CreditBeginDate),
         MonthsDefault30 = ifelse(MaxDelay > 30, MonthsDefault30, NA),
         MonthsDefault60 = elapsed_months(Date,CreditBeginDate),
         MonthsDefault60 = ifelse(MaxDelay > 60, MonthsDefault60, NA),
         MonthsDefault90 = elapsed_months(Date, CreditBeginDate),
         MonthsDefault90 = ifelse(MaxDelay > 90, MonthsDefault90, NA)) %>%
  arrange(CreditAccountSK, Date)

Defaults30 <- All_Portfolios_table1 %>%
  filter(is.na(MonthsDefault30) == 0) %>%
  group_by(CreditAccountSK)%>%
  summarize(MonthsDefault30 = min(MonthsDefault30)) %>%
  ungroup()

Defaults60 <- All_Portfolios_table1 %>%
  filter(is.na(MonthsDefault60) == 0)%>%
  group_by(CreditAccountSK) %>%
  summarize(MonthsDefault60 = min(MonthsDefault60)) %>%
  ungroup()

Defaults90 <- All_Portfolios_table1 %>%
  filter(is.na(MonthsDefault90)==0) %>%
  group_by(CreditAccountSK) %>%
  summarize(MonthsDefault90 = min(MonthsDefault90)) %>%
  ungroup()

DefaultsPaid <- All_Portfolios_table1 %>%
  mutate(CompleteDate = as.Date(CompleteDate),
         CompleteDate = as.Date(ifelse(is.na(CompleteDate) == 1, as.Date("2020-03-01"), CompleteDate), origin = "1970-01-01"),
         PaidPerc = TotalPaid/Sum,
         LifeTime = elapsed_months(CompleteDate, CreditBeginDate)) %>%
  filter(LifeTime <= 12) %>%
  group_by(CreditAccountSK) %>%
  mutate(row_number = row_number(),
         last_row = n()) %>%
  ungroup() %>%
  mutate(DefaultsPaid = ifelse(LifeTime <= 3 & PaidPerc >= 1,0,
                               ifelse(LifeTime > 3 & PaidPerc >= 1.2, 0, 1))) %>%
  group_by(CreditAccountSK) %>%
  summarize(DefaultsPaid = min(DefaultsPaid)) %>%
  ungroup()

AllDefaults <- left_join(Defaults30, Defaults60, by = "CreditAccountSK") %>%
  left_join(Defaults90, by = "CreditAccountSK")



Query <- paste0("
  select
  *
  from dwh.FactCreditBalancesCurrent
                where DateSK = '20200301'")

Last_portfolio <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))

Last_portfolio1 <- left_join(Last_portfolio, All_Loans, by = "CreditAccountSK") %>%
  arrange(ClientSK, CreditAccountSK) %>%
  mutate(CreditBeginDate = as.Date(CreditBeginDate),
         CompleteDate.x = as.Date(CompleteDate.x),
         CompleteDate.x = as.Date(ifelse(is.na(CompleteDate.x)==1, as.Date("2020-03-01"), CompleteDate.x), origin = "1970-01-01"))

History <- Last_portfolio1 %>%
  select(ClientSK, CreditAccountSK, CreditBeginDate, CompleteDate.x) %>%
  arrange(ClientSK, CompleteDate.x) %>%
  rename(CreditBeginDateHistory = CreditBeginDate,
         CompleteDateHistory = CompleteDate.x,
         CreditAccountSKHistory = CreditAccountSK)

History1 <- History %>%
  filter(CompleteDateHistory >= as.Date("2018-01-01"))


Poredni <- Last_portfolio1 %>%
  filter(CreditBeginDate > as.Date("2018-06-30"))%>%
  mutate(DateToCheck = floor_date(CreditBeginDate %m-% months(6), "months")) %>%
  left_join(History, by = "ClientSK") %>%
  select(ClientSK, CreditAccountSK, CreditAccountSKHistory, CreditBeginDateHistory, 
         CompleteDateHistory, DateToCheck, CreditBeginDate) %>%
  mutate(Poreden = ifelse(DateToCheck <= CompleteDateHistory & CreditBeginDate > CreditBeginDateHistory,1,0)) %>%
  group_by(CreditAccountSK) %>%
  summarize(Poreden = max(Poreden)) %>%
  ungroup()


data1 <- Last_portfolio1 %>%
  filter(CreditBeginDate > as.Date("2018-06-30")) %>%
  select(CreditAccountSK, ClientSK, CreditBeginDate, Scoring, ScoringType) %>%
  mutate(CreditBeginDate = floor_date(CreditBeginDate, "months"),
         BeginMonth = substr(CreditBeginDate, 1, 7)) %>%
  left_join(Poredni, by = "CreditAccountSK") %>%
  left_join(AllDefaults, by = "CreditAccountSK") %>%
  arrange(CreditBeginDate, CreditAccountSK)


Stats <- data1 %>%
  filter(Poreden == 0) %>%
  group_by(CreditBeginDate, ScoringType) %>%
  summarize(Count = n())%>%
  ungroup()

Stats2 <- data1 %>%
  filter(Poreden == 1) %>%
  group_by(CreditBeginDate, ScoringType) %>%
  summarize(Count = n())%>%
  ungroup()


test <- data1 %>%
  group_by(MonthsDefault90) %>%
  summarise(Broi = n())

test2 <- data1 %>%
  group_by(MonthsDefault60) %>%
  summarise(Broi = n())

data2 <- data1 %>%
  mutate(default = ifelse(MonthsDefault90 < 9 & !is.na(MonthsDefault90) , 1, 0)) %>%
  left_join(Last_portfolio1 %>% 
  select(CreditAccountSK, Sum, ProductName), by = "CreditAccountSK") %>%
  left_join(DefaultsPaid, by = 'CreditAccountSK')


scoring_types <- unique(data2$ScoringType)

ConfidenceLevel <- 0.95
Z <- 1.96

library(ROCR)

score_function <- function(arg1){
  
  dd <- data2 %>%
    filter(ScoringType == arg1) 
  
  statDays <- dd %>%
    mutate(group = cut2(Scoring, m = nrow(dd) / 5)) %>%
    group_by(group) %>%
    summarize(count = n(),
              defaults = sum(default, na.rm = T),
              defPer = defaults/count,
              DefaultPerLower = defPer - Z*sqrt(defPer*(1-defPer)/count),
              DefaultPerUpper = defPer + Z*sqrt(defPer*(1-defPer)/count))
  
  statPaid <- dd %>%
    mutate(group = cut2(Scoring, m = nrow(dd) / 5)) %>%
    group_by(group) %>%
    summarize(count = n(),
              defaults = sum(DefaultsPaid, na.rm = T),
              defPer = defaults/count,
              DefaultPerLower = defPer - Z*sqrt(defPer*(1-defPer)/count),
              DefaultPerUpper = defPer + Z*sqrt(defPer*(1-defPer)/count))

  
  pred_ROCR <- ROCR::prediction(dd$Scoring, dd$default)
  
  auc_ROCR <- performance(pred_ROCR, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]] 
  
  return(list(statDays = statDays, statPaid = statPaid, auc_ROCR = auc_ROCR))
}

ddd <- lapply(scoring_types, score_function)



Scoring_101 <- data2 %>%
  filter(ScoringType == 101)


new_loans <- data2 %>%
  filter(Poreden == 0)


Harold_Easy_BG <- read.csv2(file = "Z:\\Analysis\\3.Data\\2020.02\\Easy BG\\E006_Harold_BG_20200301_v1.csv", 
               stringsAsFactors = F, encoding = "UTF-8")

names(Harold_Easy_BG)[names(Harold_Easy_BG)=="X.U.FEFF.KID"]<-"CodeContract"

data2 <- data2 %>%
  left_join(Last_portfolio1 %>% select(CodeContract, CreditAccountSK), by = "CreditAccountSK") %>%
  left_join(Harold_Easy_BG %>% select(CreditPeriodCountName, ClientEducation, TypeOfContract, StaffType, Automobiles, GetSalaryType, TotalLengthOfService, TypeOfHousing, ClientFamilyStatus, CreditPurpose, WorkExperienceCurrentWork,PartnerConnectionType, ClientSex,YearsOnCurrentAddress, ClientFamilySize, CodeContract), by = "CodeContract")

test <- new_loans %>%
  filter(ScoringType == 101) %>%
  group_by(ProductName) %>%
  summarize(count = n())

test2 <- data2 %>%
  filter(Poreden == 0) %>%
  group_by(ScoringType) %>%
  summarize(count = n())

Scoring_Type_3 <- new_loans %>%
  filter(ScoringType == 3) %>%
  group_by(ProductName) %>%
  summarize(count = n())



all_files <- list.files("Z:\\Analysis\\3.Data", recursive = T, full.names = T)
grepl("ЦКР_Кредитен_BG", all_files)
all_files1 <- all_files[grepl("ЦКР_Кредитен_BG", all_files)]
all_files2 <- all_files1[27:53]

read_files <- function(arg1){
  output <- read.csv2(file = arg1, stringsAsFactors = F, encoding = "UTF-8")
  return(output)
}

NOIInsurance <- lapply(all_files2, read_files)
NOIInsurance <- do.call(bind_rows, NOIInsurance)

names(NOIInsurance)[names(NOIInsurance)=="X.U.FEFF.CodeContract"]<-"CodeContract"

data2 <- data2 %>%
  left_join(NOIInsurance %>% select(NOIInsurance, CodeContract), by = "CodeContract")


Query <- paste0("select BirthDate, ClientSK
                from [BIsmart].[dwh].[DimClient]" )


clients_birth_dates <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))

data2 <- data2 %>%
  left_join(clients_birth_dates, by = "ClientSK")

install.packages("eeptools")
library(eeptools)

data2$BirthDate <- as.Date(data2$BirthDate)

data2 <- data2[-c(20770,280656,368880),]

data2 <- data2 %>%
  mutate(age = age_calc(BirthDate, enddate = CreditBeginDate, units = "years", precise = TRUE))

data2$age <- round(data2$age)
data2$default60 <- ifelse(data2$MonthsDefault60 < 9 & !is.na(data2$MonthsDefault60), 1, 0)
