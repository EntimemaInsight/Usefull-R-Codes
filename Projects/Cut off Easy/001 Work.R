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
  filter(LastDate >= as.Date("2017-12-01"),
         LastDate <= as.Date("2020-02-01"))
##### End Block: Get dates #####

##### Start Block: Get all portfolios from 2017-12 to 2020-01 from DWH #####
# downloand_credit_progress <- function(arg1)
# {
#   YYYYMM <- substr(arg1, 1, 6)
#   tableName <- paste("dwh.FactCreditBalances", YYYYMM, sep = "")
#   
#   Query <- paste0("
#   select
# fcb.CreditAccountSK,
# fcb.CurrentDelay,
# fcb.PenaltyPaid + fcb.PrincipalPaid + fcb.Overpaid + fcb.InterestPaid as TotalPaid
# from ", tableName,"  fcb 
#                   where fcb.CompleteDate is NULL")
#   
#   output <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query)) %>%
#     group_by(CreditAccountSK) %>%
#     summarize(MaxDelay = max(CurrentDelay),
#               TotalPaid = max(TotalPaid)) %>%
#     ungroup()
#   
#   print(arg1)
#   
#   return(output)
# }
# 
# All_Portfolios <- lapply(Dates$FirstDateSK, downloand_credit_progress)
##### End Block: Get all portfolios from 2017-12 to 2020-01 from DWH #####

##### Start Block: Get all portfolios from 2017-12 to 2020-01 from Excel #####
All_Excel_files <- list.files(path = "Z:\\Analysis\\3.Data", full.names = TRUE, recursive = TRUE)
All_Excel_files_1 <- All_Excel_files[grepl("E004_Credit_Progress_BG_", All_Excel_files)]


All_Excel_files_2 <- All_Excel_files_1[-c(1:54)]
All_Excel_files_2


All_Excel_files_1 <- All_Excel_files[grepl("E004_Credit_Progress_BG_", All_Excel_files)]


All_Excel_files_1 <- All_Excel_files_1[-c(1:42,55:69)]
All_Excel_files_1 



library(data.table)
import_function <- function(arg){
  output <- fread(arg, select = c("CreditCode", "ClientEGN", "SubStatus","MaxDelayP","CalcDate", "PayedTotal"),stringsAsFactors = F, na.strings = c("NULL", "NA", ""), encoding = "UTF-8")
  print(arg)
  return(output)
  }

All_Portfolios_2018 <- lapply(All_Excel_files_1, import_function)
save(All_Portfolios_2018, file = ".\\Output Data\\Easy_Portfolios_2018.RData")


All_Portfolios_2019 <- lapply(All_Excel_files_2, import_function)
save(All_Portfolios_2019, file = ".\\Output Data\\Easy_Portfolios_2019.RData")


All_Portfolios1 <- lapply(All_Portfolios_2018, function(x) x %>% select(c("CreditCode", "ClientEGN", "SubStatus","MaxDelayP","CalcDate", "PayedTotal")))

get_sub_status <- function(arg1){
  Encoding(arg1$SubStatus) <- "UTF-8"
  return(arg1)
}

All_Portfolios1 <- lapply(All_Portfolios1, get_sub_status)
unique(All_Portfolios_2019[[3]]$SubStatus)

All_Portfolios1 <- lapply(All_Portfolios1, function(x) filter(x, SubStatus == "Усвоен"))

save(All_Portfolios1, file = ".\\Output Data\\Easy_Portfolios_active_2018.RData")


All_Portfolios_2019 <- lapply(All_Portfolios_2019, get_sub_status)
Easy_Portfolios_active_2019 <- lapply(All_Portfolios_2019, function(x) filter(x, SubStatus == "Усвоен"))
save(Easy_Portfolios_active_2019, file = ".\\Output Data\\Easy_Portfolios_active_2019.RData")

All_Portfolios_2018 <- lapply(All_Portfolios_2018, get_sub_status)
Easy_Portfolios_active_2018 <- lapply(All_Portfolios_2018, function(x) filter(x, SubStatus == "Усвоен"))
save(Easy_Portfolios_active_2018, file = ".\\Output Data\\Easy_Portfolios_active_2019.RData")


##### End Block: Get all portfolios from 2017-12 to 2020-01 from Excel #####