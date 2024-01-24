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



number_of_cores <- 12
Reporting_Date <- as.Date("2020-12-01")
##### End Block: Input Parameters #####

##### Start Block: Load Input data #####

##### End Block: Load Input data #####
#### 01 Connect to the database ####
connection_easy_bg <- DBI::dbConnect(odbc::odbc()
                                     , driver= "SQL Server"
                                     , server = 'hidalgo'
                                     , database = "BISmart")


Query <- paste0("select 
                a.CodeContract,
                a.CreditSK,
                a.CreditAccountSK,
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
                a.ClientSK,
                a.CompleteDate,
                a.DateRefused,
                a.DateDenied,
                a.DateApproved,
                a.RefinanceType,
                a.PreviousCreditsCount
                from dwh.dimCredit a
                LEFT JOIN [dwh].[DimProduct] b ON a.ProductSK = b.ProductSK 
                WHERE latest = 1
                ORDER BY ScoringType, Scoring, CreditBeginDate;")

All_Loans <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))


Stats <- All_Loans %>%
  mutate(DateApproved1 = floor_date(DateApproved, unit = "month")) %>%
  filter(DateApproved1 > as.Date("2019-01-01")) %>%
  group_by(DateApproved1, ScoringType) %>%
  summarize(Count = n()) %>%
  ungroup()

Query <- paste0("select  
max(DateSk) as DateSK,
max(Date) as Date
from dwh.dimDate
group by Year,Month
order by Date
;")

Dates <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))
Dates$Date <- as.Date(Dates$Date)
Dates <- Dates[Dates$Date >= as.Date("2019-01-01") & Dates$Date <= as.Date("2020-01-01"),]

loop_dates <- Dates$DateSK

downloand_credit_progress <- function(arg1)
{
  if(!require(DBI)){
    install.packages("DBI")
    library(DBI)
  }
  
  YYYYMM <- substr(arg1, 1, 6)
  tableName <- paste("dwh.FactCreditBalances", YYYYMM, sep = "")
  
  Query <- paste0("select
fcb.CreditSK,
fcb.CreditAccountSK,
fcb.DateSK,
fcb.MaxDelay,
fcb.CurrentDelay,
fcb.PenaltyPaid + fcb.PrincipalPaid + fcb.Overpaid + fcb.InterestPaid as TotalPaid
from ", tableName,"  fcb
where fcb.DateSK = ", arg1,";")
  output <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))
  return(output)
}

plan("multiprocess", workers = 12)
All_Credit_profress <- lapply(loop_dates, downloand_credit_progress)

All_Credits <- do.call(bind_rows, All_Credit_profress) %>%
  arrange(CreditSK, DateSK)

View(All_Credits[1:5000, ])

All_Loans1 <- All_Loans %>%
  filter(StatusSK == 3) %>%
  left_join(All_Credits, by = "CreditSK") %>%
  arrange(CreditSK, DateSK) %>%
  mutate(ObservationDate = ymd(DateSK)) %>%
  select(CreditSK, CreditBeginDate, ObservationDate, MaxDelay)




Query <- paste0("select
fcb.CreditSK,
fcb.CreditAccountSK,
fcb.DateSK,
fcb.PenaltyPaid + fcb.PrincipalPaid + fcb.Overpaid + fcb.InterestPaid as TotalPaid
from dwh.FactCreditBalancesCurrent fcb
where fcb.DateSK = '20200201';")







for(i in 1:20){
  print(i)
}

loop_list <- seq(1:20)
lapply(all_files1, print)

read_credit_progress <- function(arg1){
  output <- read.csv2(file = arg1, stringsAsFactors = F, encoding = "UTF-8")
  return(output)
}


all_files <- list.files("Z:\\Analysis\\3.Data", recursive = T, full.names = T)
grepl("Credit_Progress_BG", all_files)
all_files1 <- all_files[grepl("Credit_Progress_BG", all_files)]
all_files1 <- all_files1[c(1,2,3)]

all_Credit_Progress <- lapply(all_files1, read_credit_progress)



qwe <- read.csv2(file = "Z:\\Analysis\\3.Data\\2020.01\\Easy BG\\E004_Credit_Progress_BG_20200201_v1.csv", 
                 stringsAsFactors = F, encoding = "UTF-8")




Harold_Easy_BG <- read.csv2(file = "Z:\\Analysis\\3.Data\\2020.01\\Easy BG\\E006_Harold_BG_20200201_v1.csv", 
                            stringsAsFactors = F, encoding = "UTF-8")

qwe1<-qwe[c("CreditCode","CreditProduct","SumaP","Weeks","PayedTotal", "DayDelayTotal","LastPayDate","GoodOrBad")]


qwe1$SumaP<-as.numeric(qwe1$SumaP)

qwe1$PayedTotal<-as.numeric(qwe1$PayedTotal)

#qwe1<-qwe1 %>%
group_by(CreditCode) %>%
  mutate(DefaultPayment=as.factor(ifelse(PayedTotal/SumaP>1.2,0,1)),
         DefaultDays= as.factor(ifelse(DayDelayTotal>90,1,0)))


table(qwe1$DefaultDays)
table(qwe1$DefaultPayment)

Harold_Easy_BG1<-Harold_Easy_BG[c("X.U.FEFF.KID","MaxDaysPayedLater","CreditFirstPayment","CreditLastPayment")]
names(Harold_Easy_BG1)[names(Harold_Easy_BG1)=="X.U.FEFF.KID"]<-"CreditCode"

qwh<-left_join(qwe1,Harold_Easy_BG1,by="CreditCode")
qwh<-qwh[qwh$CreditFirstPayment>as.Date("2018-01-01") & qwh$CreditLastPayment<as.Date("2020-02-01"),]

qwh$MaxDaysPayedLater<-as.numeric(qwh$MaxDaysPayedLater)
qwh<-qwh %>%
  group_by(CreditCode) %>%
  mutate(DefaultPayment=as.factor(ifelse(PayedTotal/SumaP>1.2,0,1)),
         DefaultDays= as.factor(ifelse(MaxDaysPayedLater>90,1,0)))

qwh<-na.omit(qwh)

test <- Harold_Easy_BG %>%
  filter(Scoring == "NULL",
         ScoringType != "NULL")

table(test$ScoringType)
