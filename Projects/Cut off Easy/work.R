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
Reporting_Date <- as.Date("2020-01-01")
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

All_Loans_1 <- All_Loans %>%
  filter(is.na(DateDenied)& is.na(DateRefused),!is.na(DateApproved), DateApproved >= as.Date("2018-06-01") & DateApproved <= as.Date("2019-12-31"))


Stats <- All_Loans %>%
  mutate(DateApproved1 = floor_date(DateApproved, unit = "month")) %>%
  filter(DateApproved1 > as.Date("2019-01-01")) %>%
  group_by(DateApproved1, ScoringType) %>%
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
  filter(LastDate >= as.Date("2017-12-01"),
         LastDate <= as.Date("2020-02-01"))

downloand_credit_progress <- function(arg1)
{
  YYYYMM <- substr(arg1, 1, 6)
  tableName <- paste("dwh.FactCreditBalances", YYYYMM, sep = "")
  
  Query <- paste0("
  select
fcb.CreditAccountSK,
fcb.CurrentDelay,
fcb.PenaltyPaid + fcb.PrincipalPaid + fcb.Overpaid + fcb.InterestPaid as TotalPaid
from ", tableName,"  fcb 
                  where fcb.CompleteDate is NULL")
  
  output <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query)) %>%
    group_by(CreditAccountSK) %>%
    summarize(MaxDelay = max(CurrentDelay),
              TotalPaid = max(TotalPaid)) %>%
    ungroup()
  
  return(output)
}







# downloand_credit_progress <- function(arg1)
# {
#   if(!require(DBI)){
#     install.packages("DBI")
#     library(DBI)
#   }
#   
#   YYYYMM <- substr(arg1, 1, 6)
#   tableName <- paste("dwh.FactCreditBalances", YYYYMM, sep = "")
#   
#   Query <- paste0("
#   With helpTable as (
#                 select
#                 tablica1.CreditAccountSK,
#                 tablica2.DateSK
#                 from
#                 (select 
#                 a.CreditAccountSK,
#                 min(a.ValidFrom) as CreditInactiveDate
#                 from dwh.dimCredit a 
#                 where StatusSK = 4
#                 group by a.CreditAccountSK) as tablica1 inner join dwh.DimDate tablica2 on tablica1.CreditInactiveDate = tablica2.Date),
#                 
# table1 as  
#   (select
# fcb.CreditSK,
# fcb.CreditAccountSK,
# fcb.DateSK,
# fcb.MaxDelay,
# fcb.CurrentDelay,
# fcb.PenaltyPaid + fcb.PrincipalPaid + fcb.Overpaid + fcb.InterestPaid as TotalPaid
# from ", tableName,"  fcb) ",
#                   "select
#                   table1.*,
#                   helptable.DateSK as DateInactive
#                   from table1 inner join helpTable on table1.CreditAccountSK = helpTable.CreditAccountSK
#                   where  table1.DateSK < helpTable.DateSK")
#   output <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))
#   return(output)
# }




downloand_credit_progress <- function(arg1)
{
  
  YYYYMM <- substr(arg1, 1, 6)
  tableName <- paste("dwh.FactCreditBalances", YYYYMM, sep = "")
  
  Query <- paste0("
  With helpTable as 
                (select 
                a.CreditAccountSK, 
                max(a.ValidTo) as CreditActiveDate
                from dwh.dimCredit a 
                where StatusSK = 3
                group by a.CreditAccountSK),
                
table1 as  
  (select
fcb.CreditSK,
fcb.CreditAccountSK,
fcb.DateSK,
fcb.MaxDelay,
fcb.CurrentDelay,
fcb.PenaltyPaid + fcb.PrincipalPaid + fcb.Overpaid + fcb.InterestPaid as TotalPaid,
d.Date
from ", tableName,"  fcb inner join dwh.DimDate d on fcb.DateSK = d.DateSK) ",
                  "select
                  table1.*,
                  helptable.CreditActiveDate
                  from table1 inner join helpTable on table1.CreditAccountSK = helpTable.CreditAccountSK
                  where table1.Date <= helpTable.CreditActiveDate")
  output <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))
  return(output)
}

#plan("multiprocess", workers = 12)
All_Credit_progress <- lapply(loop_dates, downloand_credit_progress)
save(All_Credit_progress, file = ".\\Output Data\\All_Credit_progress.RData")

prep<-function(input_dataframe){
  output <- input_dataframe %>%
    mutate(YYYYMM = substr(DateSK,1,6)) %>%
    group_by(CreditAccountSK, YYYYMM) %>%
    summarize(maxDelay=max(CurrentDelay),
              maxTotalPaid=max(TotalPaid)) %>%
    ungroup()
  return(output)
}

All_Credit_progress_2<-lapply(All_Credit_progress, prep)

All_Credit_progress_3<-do.call(bind_rows,All_Credit_progress_2) %>%
  arrange(CreditAccountSK,YYYYMM)

All_Loans_2 <- All_Loans_1 %>%
  left_join(All_Credit_progress_3, by="CreditAccountSK") %>%
  filter(!is.na(YYYYMM))

All_Loans_2$YYYYMM <- paste0(All_Loans_2$YYYYMM,"01")

All_Loans_2$YYYYMM <- ymd(All_Loans_2$YYYYMM)

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

All_Loans_2 <- All_Loans_2 %>%
  filter(CompleteDate <= as.Date("2019-12-31"))%>%
  mutate(hist = elapsed_months(CompleteDate, CreditBeginDate),
         DefaultDays = ifelse(maxDelay>90,1,0),
         DefaultPayment = ifelse(maxTotalPaid/Sum >1.2, 0,1))

All_Loans_3 <- All_Loans_2 %>%
  select(CodeContract,YYYYMM,DefaultDays) %>%
  arrange(CodeContract, YYYYMM)%>%
  filter(DefaultDays == 1) %>%
  group_by(CodeContract) %>%
  summarize(firstDefaultDay = min(YYYYMM)) %>%
  ungroup()


All_Loans_4 <-All_Loans_2 %>%
  left_join(All_Loans_3, by="CodeContract") %>%
 mutate(MonthsDefault = elapsed_months(firstDefaultDay,YYYYMM))


All_Loans_may <- All_Loans_4 %>%
  filter(YYYYMM == "2019-05-01")%>%
  group_by(MonthsDefault,hist)%>%
  summarize(defaultsD = sum(DefaultDays))


OldOrNewClient <- All_Loans_4 %>%
  group_by(ClientSK) %>%
  summarize(min = min(CreditBeginDate),
            max = max(CreditBeginDate),
            diff = ifelse(elapsed_months(max,min) == 0,1,0))






test <- All_Loans_4 %>%
  arrange(ClientSK,CreditBeginDate)%>%
  group_by(ClientSK)%>%
  filter(!duplicated(CreditBeginDate)) %>%
  mutate(OldOrNew = ifelse(n() == 1,1,0) )






test <- All_Loans_4 %>%
  filter(ClientSK == "48527")

loop_list <- as.Date(c("2019-04-01", "2019-07-01"))

loop_table <- data.frame(Date = as.Date(c("2019-04-01", "201 9-07-01", "2019-04-01", "2019-07-01", "2019-04-01", "2019-07-01")), 
                         Scoring = as.character(c("101", "101", "3", "3","2","2")),
                         stringsAsFactors = F)

valentina_function <- function(arg1){
  
  end_date <- arg1[1]
  scoring_type <- arg1[2]
  
  start_date <- as.Date(end_date) %m-% months(3)
  
  New_Loans <- All_Loans_4 %>%
    filter(CreditBeginDate >= start_date,
           CreditBeginDate < end_date,
           ScoringType == scoring_type) %>%
    mutate(Default = ifelse(MonthsDefault >= 0 & MonthsDefault <= 6, 1, 0)%>%
    filter(YYYYMM == start_date)
  
  Scoring_minus_1 <- New_Loans %>%
    filter(Scoring == -1)
  
  
  stat <- New_Loans %>%
    mutate(group = cut2(Scoring, m = nrow(New_Loans) / 10))%>%
    group_by(group) %>%
    summarize(count=n(),
              def = sum(Default, na.rm = T),
              PerDef = def/count,
              DefaultPerLower = PerDef - Z*sqrt(PerDef*(1-PerDef)/count),
              DefaultPerUpper = PerDef + Z*sqrt(PerDef*(1-PerDef)/count))
  
  pred_ROCR <- ROCR::prediction(New_Loans$Scoring, New_Loans$Default)
  #RoC percent
  auc_ROCR <- performance(pred_ROCR, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  
  duplicates <- New_Loans %>%
    group_by(CodeContract) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    filter(count > 1)
  print(nrow(duplicates))
  return(list(stat = stat, auc_ROCR = auc_ROCR, Scoring_minus_1, duplicates))
}

xxx <- apply(loop_table,1, valentina_function)
View(xxx[[1]][[1]])
View(xxx[[1]][[4]])
as.Date("2012/12/31") %m-% months(6)


View(xxx[[3]][[1]])
View(xxx[[4]][[1]])



DefaultAfter <- New_Loans_feb %>%
  group_by(MonthsDefault) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(TotalNewCredits = sum(Count),
         DefaultPercentage = Count / TotalNewCredits,
         DefaultCumulative = cumsum(DefaultPercentage))










stat <- tt%>% 
  mutate(group = cut2(Scoring, m = nrow(New_Loans_feb)/5))%>%
  group_by(group) %>%
  summarize(count=n(),
            def = sum(Default, na.rm = T),
            PerDef = def/count,
            DefaultPerLower = PerDef - Z*sqrt(PerDef*(1-PerDef)/count),
            DefaultPerUpper = PerDef + Z*sqrt(PerDef*(1-PerDef)/count))

iv = iv(tt[ , c("Scoring", "Default")], y = 'Default')
bins = woebin(tt[ , c("Scoring", "Default")], y = 'Default')

bins$Scoring %>%
  knitr::kable()

woebin_plot(bins$Scoring)



install.packages("ROCR")
library(ROCR)

tt$Scoring1 <- tt$Scoring / 100
min(tt$Scoring1)
max(tt$Scoring1)
pred_ROCR <- ROCR::prediction(tt$Scoring, tt$Default)

#Draw ROC Curve
roc_ROCR <- performance(pred_ROCR, measure = "tpr", x.measure = "fpr")
plot(roc_ROCR, main = "ROC curve", colorize = T)
abline(a = 0, b = 1)

#RoC percent
auc_ROCR <- performance(pred_ROCR, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
# 
# All_Credits <- do.call(bind_rows, All_Credit_profress) %>%
#   arrange(CreditSK, DateSK)
# 
# View(All_Credits[1:5000, ])
# 
# All_Loans1 <- All_Loans %>%
#   filter(StatusSK == 3) %>%
#   left_join(All_Credits, by = "CreditAccountSK") %>%
#   arrange(CreditAccountSK, DateSK) %>%
#   mutate(ObservationDate = ymd(DateSK),
#          CreditBeginDate = as.Date(CreditBeginDate)) %>%
#   select(CreditAccountSK, CreditBeginDate, ObservationDate, MaxDelay) %>%
#   mutate(Lifetime = elapsed_months(ObservationDate, CreditBeginDate))
# 
# 
# 
# 
# Query <- paste0("select
# fcb.CreditSK,
# fcb.CreditAccountSK,
# fcb.DateSK,
# fcb.PenaltyPaid + fcb.PrincipalPaid + fcb.Overpaid + fcb.InterestPaid as TotalPaid
# from dwh.FactCreditBalancesCurrent fcb
# where fcb.DateSK = '20200201';")
# 
# 
# 
# 
# 
# 
# 
# for(i in 1:20){
#   print(i)
# }
# 
# loop_list <- seq(1:20)
# lapply(all_files1, print)
# 
# read_credit_progress <- function(arg1){
#   output <- read.csv2(file = arg1, stringsAsFactors = F, encoding = "UTF-8")
#   return(output)
# }
# 
# 
# all_files <- list.files("Z:\\Analysis\\3.Data", recursive = T, full.names = T)
# grepl("Credit_Progress_BG", all_files)
# all_files1 <- all_files[grepl("Credit_Progress_BG", all_files)]
# all_files1 <- all_files1[c(1,2,3)]
# 
# all_Credit_Progress <- lapply(all_files1, read_credit_progress)
# 
# 
# 
# qwe <- read.csv2(file = "Z:\\Analysis\\3.Data\\2020.01\\Easy BG\\E004_Credit_Progress_BG_20200201_v1.csv", 
#                  stringsAsFactors = F, encoding = "UTF-8")
# 
# 
# 
# 
# Harold_Easy_BG <- read.csv2(file = "Z:\\Analysis\\3.Data\\2020.01\\Easy BG\\E006_Harold_BG_20200201_v1.csv", 
#                             stringsAsFactors = F, encoding = "UTF-8")
# 
# qwe1<-qwe[c("CreditCode","CreditProduct","SumaP","Weeks","PayedTotal", "DayDelayTotal","LastPayDate","GoodOrBad")]
# 
# 
# qwe1$SumaP<-as.numeric(qwe1$SumaP)
# 
# qwe1$PayedTotal<-as.numeric(qwe1$PayedTotal)
# 
# #qwe1<-qwe1 %>%
# group_by(CreditCode) %>%
#   mutate(DefaultPayment=as.factor(ifelse(PayedTotal/SumaP>1.2,0,1)),
#          DefaultDays= as.factor(ifelse(DayDelayTotal>90,1,0)))
# 
# 
# table(qwe1$DefaultDays)
# table(qwe1$DefaultPayment)
# 
# Harold_Easy_BG1<-Harold_Easy_BG[c("X.U.FEFF.KID","MaxDaysPayedLater","CreditFirstPayment","CreditLastPayment")]
# names(Harold_Easy_BG1)[names(Harold_Easy_BG1)=="X.U.FEFF.KID"]<-"CreditCode"
# 
# qwh<-left_join(qwe1,Harold_Easy_BG1,by="CreditCode")
# qwh<-qwh[qwh$CreditFirstPayment>as.Date("2018-01-01") & qwh$CreditLastPayment<as.Date("2020-02-01"),]
# 
# qwh$MaxDaysPayedLater<-as.numeric(qwh$MaxDaysPayedLater)
# qwh<-qwh %>%
#   group_by(CreditCode) %>%
#   mutate(DefaultPayment=as.factor(ifelse(PayedTotal/SumaP>1.2,0,1)),
#          DefaultDays= as.factor(ifelse(MaxDaysPayedLater>90,1,0)))
# 
# qwh<-na.omit(qwh)
# 
# test <- Harold_Easy_BG %>%
#   filter(Scoring == "NULL",
#          ScoringType != "NULL")
# 
# table(test$ScoringType)
