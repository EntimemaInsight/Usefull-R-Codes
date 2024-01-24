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

##### End Block: Load Libraries #####

##### Start Block: Load Functions #####
##### End Block: Load Functions #####

##### Start Block: Input Parameters #####
number_of_cores <- 12
Reporting_Date <- as.Date("2020-12-01")
##### End Block: Input Parameters #####

##### Start Block: Load Input data #####

##### End Block: Load Input data #####

Day <- Sys.Date() - 12
#DECLARE_DATE <- paste0("'", Day, "'")
DECLARE_DATE <- "'2019-11-10'"

#### 01 Connect to the database ####
connection_easy_bg <- DBI::dbConnect(odbc::odbc()
                                     , driver= "SQL Server"
                                     , server = 'hidalgo'
                                     , database = "BISmart")
#extract all loans that have gone through R-duck scoring. R-duck scoring is scoring type 1001
Query <- paste0("select 
                a.CodeContract,
                a.CreditSK,
                a.CreditAccountSK,
                b.ProductName, 
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
                a.RefinanceNew,
                a.PreviousCreditsCount
                from dwh.dimCredit a
                LEFT JOIN [dwh].[DimProduct] b ON a.ProductSK = b.ProductSK 
                WHERE ScoringType = 1001 and latest = 1
                ORDER BY ProductName, Scoring, CreditBeginDate;")

All_Loans_1001 <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))
save(All_Loans_1001, file = ".\\Output Data\\All_Loans.RData")

Stats <- ccc %>%
  mutate(DateApproved1 = floor_date(DateApproved, unit = "month")) %>%
  filter(DateApproved1 > as.Date("2019-01-01")) %>%
  group_by(DateApproved1, ScoringType) %>%
  summarize(Count = n())


ClientSK <- unique(All_Loans_1001$ClientSK)
CreditSK1 <- unique(All_Loans_1001$CreditSK)

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


Query <- paste0("select
fcb.CreditSK,
fcb.CreditAccountSK,
fcb.DateSK,
fcb.PenaltyPaid + fcb.PrincipalPaid + fcb.Overpaid + fcb.InterestPaid as TotalPaid
from dwh.FactCreditBalancesCurrent fcb
where fcb.DateSK = '20200201';")
Jan <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))

test <- Jan %>%
  filter(CreditSK %in% unique(All_Loans_1001$CreditSK))

ggg <- anti_join(All_Loans_1001, test, by = "CreditSK")

Get_Payments_Function <- function(arg1){
  m <- substr(arg1, 1, 6)
  Query <- paste0("select
fcb.CreditSK,
fcb.CreditAccountSK,
fcb.DateSK,
fcb.PenaltyPaid + fcb.PrincipalPaid + fcb.Overpaid + fcb.InterestPaid as TotalPaid
from dwh.FactCreditBalances",m," fcb
where fcb.DateSK = '",arg1,"';")
  output_dataset <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))
  return(output_dataset)
}
#DBI::dbDisconnect(connection_easy_bg)
Payments <- lapply(Dates$DateSK, function (x) Get_Payments_Function(x))

save(Payments, file = ".\\Output Data\\Payments.RData")

Keep_only_1001 <- function(arg1){
  output <- arg1 %>%
    filter(CreditSK %in% unique(All_Loans_1001$CreditSK))
  return(output)
}

xxx <- lapply(Payments, function (x) Keep_only_1001(x))

Query <- paste0("select
fcb.CreditSK,
fcb.CreditAccountSK,
fcb.DateSK,
fcb.CreditPaymentCount,
fcb.PenaltyPaid + fcb.PrincipalPaid + fcb.Overpaid + fcb.InterestPaid as TotalPaid,
fcb.MaxDelay
from dwh.FactCreditBalancesCurrent fcb
                inner join(select CreditSK from dwh.dimCredit
                WHERE ScoringType = 1001 and latest = 1) b
                on fcb.CreditSK = b.CreditSK;")

February <- DBI::dbFetch(DBI::dbSendQuery(connection_easy_bg, Query))
save(February, file = ".\\Output Data\\February.RData")

February1 <- February %>%
  arrange(CreditSK, DateSK) %>%
  group_by(CreditSK) %>%
  filter(row_number() == 1) %>%
  ungroup()

Loans <- left_join(All_Loans_1001, February1, by = "CreditSK") %>%
  filter(is.na(CreditBeginDate) == 0,
         CreditBeginDate < as.Date("2019-08-01"),
         ProductName %in% c("EasyMax24", "EasyCredit", "EasyMonth"),
         CreditLastDate < as.Date("2020-03-01")) %>%
  mutate(DefaultDays = as.factor(ifelse(MaxDelay > 90, 1, 0)),
         DefaultPayment = as.factor(ifelse(TotalPaid / Sum > 1, 0, 1)))

table(Loans$DefaultPayment)
table(Loans$DefaultDays)


EasyMax <- Loans %>%
  filter(ProductName == "EasyMax24",is.na(DefaultPayment) == FALSE)
table(EasyMax$DefaultPayment)
table(EasyMax$DefaultPayment)[2] / sum(table(EasyMax$DefaultPayment))

ConfidenceLevel <- 0.95
Z <- 1.96
Stats <- EasyMax %>%
  mutate(Group = cut2(Scoring, m = nrow(EasyMax) / 3)) %>%
  group_by(Group) %>%
  summarize(Count=n(),
            Defaults = sum(as.numeric(DefaultPayment) - 1),
            DefaultPer = Defaults/Count,
            DefaultPerLower = DefaultPer - Z*sqrt(DefaultPer*(1-DefaultPer)/Count),
            DefaultPerUpper = DefaultPer + Z*sqrt(DefaultPer*(1-DefaultPer)/Count))
  
Stats1<- EasyMax %>%
  mutate(Group = cut2(Scoring, m = nrow(EasyMax) / 3)) %>%
  group_by(Group,CreditPaymentCount) %>%
  summarize(Count=n(),
            Defaults = sum(as.numeric(DefaultPayment) - 1),
            DefaultPer = Defaults/Count,
            DefaultPerLower = DefaultPer - Z*sqrt(DefaultPer*(1-DefaultPer)/Count),
            DefaultPerUpper = DefaultPer + Z*sqrt(DefaultPer*(1-DefaultPer)/Count))

Stats2 <- EasyMax %>%
  mutate(Group = cut2(CreditPaymentCount, m = nrow(EasyMax) / 5)) %>%
  group_by(Group) %>%
  summarize(Count=n(),
            Defaults = sum(as.numeric(DefaultPayment) - 1),
            DefaultPer = Defaults/Count,
            DefaultPerLower = DefaultPer - Z*sqrt(DefaultPer*(1-DefaultPer)/Count),
            DefaultPerUpper = DefaultPer + Z*sqrt(DefaultPer*(1-DefaultPer)/Count))

Stats3 <- EasyMax %>%
  mutate(Group = cut2(Scoring, m = nrow(EasyMax) / 4)) %>%
  group_by(Group) %>%
  summarize(Count=n(),
            Defaults = sum(as.numeric(DefaultDays) - 1),
            DefaultPer = Defaults/Count,
            DefaultPerLower = DefaultPer - Z*sqrt(DefaultPer*(1-DefaultPer)/Count),
            DefaultPerUpper = DefaultPer + Z*sqrt(DefaultPer*(1-DefaultPer)/Count))





EasyMonth <- Loans %>%
  filter(ProductName == "EasyMonth",is.na(DefaultPayment) == FALSE)
table(EasyMonth$DefaultPayment)
table(EasyMonth$DefaultPayment)[2] / sum(table(EasyMonth$DefaultPayment))

ConfidenceLevel <- 0.95
Z <- 1.96
Stats <- EasyMonth %>%
  mutate(Group = cut2(Scoring, m = nrow(EasyMonth) / 10)) %>%
  group_by(Group) %>%
  summarize(Count=n(),
            Defaults = sum(as.numeric(DefaultPayment) - 1),
            DefaultPer = Defaults/Count,
            DefaultPerLower = DefaultPer - Z*sqrt(DefaultPer*(1-DefaultPer)/Count),
            DefaultPerUpper = DefaultPer + Z*sqrt(DefaultPer*(1-DefaultPer)/Count))

  
  
  
  
  
  
  
  
























  # summarise(Count = n(),
  #           Defaults = sum(as.numeric(DefaultPayment) - 1),
  #           DefaultRate = 100 * Defaults / Count) %>%
  # ungroup()


bins$Scoring %>%
  knitr::kable()

woebin_plot(bins$Scoring)

hist(EasyMax$Sum)


iv = iv(EasyMax[ , c("Sum", "DefaultPayment")], y = 'DefaultPayment')
bins = woebin(EasyMax[ , c("Sum", "DefaultPayment")], y = 'DefaultPayment')

bins$Sum %>%
  knitr::kable()

woebin_plot(bins$Sum)

test <- EasyMax %>%
  filter(Sum > 1000)


test <- EasyMax %>%
  filter(Scoring > 0,
    Scoring <= 25) %>%
  mutate(Group = cut2(Sum, m = nrow(EasyMax) / 10)) %>%
  group_by(Group) %>%
  summarise(Count = n(),
            Defaults = sum(as.numeric(DefaultPayment) - 1),
            DefaultRate = 100 * Defaults / Count) %>%
  ungroup()


test2 <- EasyMax %>%
  filter(ScoringLimit == 2) 

test2 <- test2 %>%
  mutate(Group = cut2(Sum, m = nrow(test2) / 5)) %>%
  group_by(Group) %>%
  summarise(Count = n(),
            Defaults = sum(as.numeric(DefaultPayment) - 1),
            DefaultRate = 100 * Defaults / Count) %>%
  ungroup()

test3 <- EasyMax %>%
  filter(ScoringLimit == 3) 

test3 <- test3 %>%
  mutate(Group = cut2(Sum, m = nrow(test3) / 5)) %>%
  group_by(Group) %>%
  summarise(Count = n(),
            Defaults = sum(as.numeric(DefaultPayment) - 1),
            DefaultRate = 100 * Defaults / Count) %>%
  ungroup()


test4 <- EasyMax %>%
  filter(ScoringLimit == 4) 

test4<- test4 %>%
  mutate(Group = cut2(Sum, m = nrow(test4) / 5)) %>%
  group_by(Group) %>%
  summarise(Count = n(),
            Defaults = sum(as.numeric(DefaultPayment) - 1),
            DefaultRate = 100 * Defaults / Count) %>%
  ungroup()


iv = iv(EasyMax[ , c("Sum", "DefaultPayment")], y = 'DefaultPayment')
bins = woebin(EasyMax[ , c("Sum", "DefaultPayment")], y = 'DefaultPayment')

bins$Sum %>%
  knitr::kable()

woebin_plot(bins$Sum)
