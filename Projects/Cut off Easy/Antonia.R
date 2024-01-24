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



Query <- paste0("
  select
  fcb.CreditAccountSK,
  fcb.CurrentDelay,
  fcb.MaxDelay,
  fcb.PenaltyPaid + fcb.PrincipalPaid + fcb.Overpaid + fcb.InterestPaid as TotalPaid,
  fcb.CreditPrincipal,
  fcb.CompleteDate
  from dwh.FactCreditBalances202003  fcb
  inner join dwh.DimDate d on fcb.DateSK = d.DateSK
  where fcb.DateSK = '20200331' and
  fcb.CompleteDate is NULL or
                  fcb.CompleteDate >= d.Date")

EasyBG31March <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))




Query <- paste0("
  select
  fcb.CreditAccountSK,
  fcb.CurrentDelay,
  fcb.MaxDelay,
  fcb.PenaltyPaid + fcb.PrincipalPaid + fcb.Overpaid + fcb.InterestPaid as TotalPaid,
  fcb.CreditPrincipal,
  fcb.CompleteDate
  from dwh.FactCreditBalances202002  fcb
  inner join dwh.DimDate d on fcb.DateSK = d.DateSK
  where fcb.DateSK = '20200229' and
  fcb.CompleteDate is NULL or
                  fcb.CompleteDate >= d.Date")

EasyBG29February <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))


Query <- paste0("
  select
  fcb.CreditAccountSK,
  fcb.CurrentDelay,
  fcb.MaxDelay,
  fcb.PenaltyPaid + fcb.PrincipalPaid + fcb.Overpaid + fcb.InterestPaid as TotalPaid,
  fcb.CreditPrincipal,
  fcb.CompleteDate
  from dwh.FactCreditBalances202001  fcb
  inner join dwh.DimDate d on fcb.DateSK = d.DateSK
  where fcb.DateSK = '20200131' and
  fcb.CompleteDate is NULL or
                  fcb.CompleteDate >= d.Date")

EasyBG31January <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))


BucketMar <- EasyBG31March %>%
  mutate(Bucket = case_when(CurrentDelay <= 3 ~ "01.low-3",
                            CurrentDelay <= 30 ~ "02.4-30",
                            CurrentDelay <= 60 ~ "03.31-60",
                            CurrentDelay <= 90 ~ "04.61-90",
                            CurrentDelay <= 120 ~ "05.91-120",
                            CurrentDelay <= 180 ~ "06.121-180",
                            CurrentDelay <= 360 ~ "07.181-360",
                            CurrentDelay > 360 ~ "08.360-Inf",
                            TRUE ~ "Error")) %>%
  group_by(Bucket) %>%
  summarise(Broi = n()) %>%
  ungroup() %>%
  mutate(BroiPerc = Broi / sum(Broi))


BucketFeb <- EasyBG29February %>%
  mutate(Bucket = case_when(CurrentDelay <= 3 ~ "01.low-3",
                            CurrentDelay <= 30 ~ "02.4-30",
                            CurrentDelay <= 60 ~ "03.31-60",
                            CurrentDelay <= 90 ~ "04.61-90",
                            CurrentDelay <= 120 ~ "05.91-120",
                            CurrentDelay <= 180 ~ "06.121-180",
                            CurrentDelay <= 360 ~ "07.181-360",
                            CurrentDelay > 360 ~ "08.360-Inf",
                            TRUE ~ "Error")) %>%
  group_by(Bucket) %>%
  summarise(Broi = n()) %>%
  ungroup() %>%
  mutate(BroiPerc = Broi / sum(Broi))

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