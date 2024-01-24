rm(list = ls())
options(scipen = 999)
##### Start Block: Load Libraries #####

if(!require(Hmisc)){
  install.packages("Hmisc")
  library(Hmisc)
}

if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}

if(!require(future.apply)){
  install.packages("future.apply")
  library(future.apply)
}

if(!require(scorecard)){
  install.packages("scorecard")
  library(scorecard)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(DBI)){
  install.packages("DBI")
  library(DBI)
}
##### End Block: Load Libraries #####
##### Start Block: Load Input Parameters #####
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
  filter(is.na(DateDenied),
         is.na(DateRefused),
         !is.na(DateApproved), 
         DateApproved >= as.Date("2019-06-01"),
         DateApproved <= as.Date("2019-12-31"))

Scorings_per_month <- All_Loans %>%
  mutate(DateApproved = floor_date(DateApproved, unit = "month")) %>%
  filter(DateApproved > as.Date("2019-01-01"),
         Scoring == -1) %>%
  group_by(DateApproved, ScoringType) %>%
  summarize(Count = n()) %>%
  ungroup()
##### End Block: Get all new loans #####

##### Start Block: Get dates #####
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
         FirstDate <= as.Date("2020-02-01"))
##### End Block: Get dates #####


##### Start Block: Get all portfolios from 2017-12 to 2020-01 from DWH #####
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

test <- All_Portfolios_table %>%
  filter(MaxDelay != MaxDelayMaxDelay)

All_Portfolios_table1 <- left_join(All_Portfolios_table, All_Loans %>% select(CreditAccountSK, CreditBeginDate, Sum, CompleteDate), by = "CreditAccountSK") %>%
  filter(CreditBeginDate >= as.Date("2018-06-30")) %>%
  mutate(Date = ymd(as.character(Date)),
         CreditBeginDate = as.Date(CreditBeginDate),
         MonthsDefault30 = elapsed_months(Date, CreditBeginDate),
         MonthsDefault30 = ifelse(MaxDelay > 30, MonthsDefault30, NA),
         MonthsDefault60 = elapsed_months(Date, CreditBeginDate),
         MonthsDefault60 = ifelse(MaxDelay > 60, MonthsDefault60, NA),
         MonthsDefault90 = elapsed_months(Date, CreditBeginDate),
         MonthsDefault90 = ifelse(MaxDelay > 90, MonthsDefault90, NA)) %>%
  arrange(CreditAccountSK, Date)




Defaults30 <- All_Portfolios_table1 %>%
  filter(is.na(MonthsDefault30) == 0) %>%
  group_by(CreditAccountSK) %>%
  summarise(MonthsDefault30 = min(MonthsDefault30)) %>%
  ungroup()

Defaults60 <- All_Portfolios_table1 %>%
  filter(is.na(MonthsDefault60) == 0) %>%
  group_by(CreditAccountSK) %>%
  summarise(MonthsDefault60 = min(MonthsDefault60)) %>%
  ungroup()

Defaults90 <- All_Portfolios_table1 %>%
  filter(is.na(MonthsDefault90) == 0) %>%
  group_by(CreditAccountSK) %>%
  summarise(MonthsDefault90 = min(MonthsDefault90)) %>%
  ungroup()

DefaultsPaid <- All_Portfolios_table1 %>%
  mutate(CompleteDate = as.Date(CompleteDate),
         CompleteDate = as.Date(ifelse(is.na(CompleteDate) == 1, as.Date("2020-03-01"), CompleteDate), origin = "1970-01-01"),
         PaidPerc = TotalPaid / Sum,
         LifeTime = elapsed_months(CompleteDate, CreditBeginDate)) %>%
  filter(LifeTime <= 12) %>%
  group_by(CreditAccountSK) %>%
  mutate(row_number = row_number(),
         last_row = n())%>%
  ungroup() %>%
  mutate(DefaultPaid = ifelse(LifeTime <= 3 & PaidPerc >= 1, 0,
    ifelse(LifeTime > 3 & PaidPerc >= 1.2, 0, 1))) %>%
  group_by(CreditAccountSK) %>%
  summarise(DefaultPaid = min(DefaultPaid)) %>%
  ungroup()

test <- All_Portfolios_table1 %>%
  filter(CreditAccountSK == 2248965)

AllDefaults <- left_join(Defaults30, Defaults60, by = "CreditAccountSK") %>%
  left_join(Defaults90, by = "CreditAccountSK")
##### End Block: Get all portfolios from 2017-12 to 2020-01 from DWH #####

##### Start Block: Get all portfolios from 2017-12 to 2020-01 from DWH #####
Query <- paste0("
  select
  *
  from dwh.FactCreditBalancesCurrent
                where DateSK = '20200301'")

Last_portfolio <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))
##### End Block: Get all portfolios from 2017-12 to 2020-01 from DWH #####

Last_portfolio1 <- left_join(Last_portfolio, All_Loans, by = "CreditAccountSK") %>%
  arrange(ClientSK, CreditAccountSK) %>%
  mutate(CreditBeginDate = as.Date(CreditBeginDate),
         CompleteDate.x = as.Date(CompleteDate.x),
         CompleteDate.x = as.Date(ifelse(is.na(CompleteDate.x) == 1, as.Date("2020-03-01"), CompleteDate.x), origin = "1970-01-01"))

test_missing <- Last_portfolio1 %>%
  filter(is.na(CodeContract))

History <- Last_portfolio1 %>%
  select(ClientSK, CreditAccountSK, CreditBeginDate, CompleteDate.x) %>%
  arrange(ClientSK, CompleteDate.x) %>%
  rename(CreditBeginDateHistory = CreditBeginDate,
         CompleteDateHistory = CompleteDate.x,
         CreditAccountSKHistory = CreditAccountSK)

History1 <- History %>%
  filter(CompleteDateHistory >= as.Date("2018-01-01"))

Poredni <- Last_portfolio1 %>%
  filter(CreditBeginDate > as.Date("2018-06-30")) %>%
  mutate(DateToCheck = floor_date(CreditBeginDate %m-% months(6), "months")) %>%
  left_join(History1, by = "ClientSK") %>%
  select(ClientSK, CreditAccountSK, CreditAccountSKHistory, CreditBeginDateHistory, 
         CompleteDateHistory, DateToCheck, CreditBeginDate) %>%
  mutate(Poreden = ifelse(DateToCheck <= CompleteDateHistory & CreditBeginDate > CreditBeginDateHistory, 1, 0)) %>%
  group_by(CreditAccountSK) %>%
  summarise(Poreden = max(Poreden)) %>%
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
  summarise(Count = n()) %>%
  ungroup()

test <- data1 %>%
  filter(Poreden == 0,
         ScoringType == 2)


