rm(list = ls())
options(scipen = 999)
##### Start Block: Load Libraries #####
if(!require(Matrix)){
  install.packages("Matrix")
  library(Matrix)
}

if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(vroom)){
  install.packages("vroom")
  library(vroom)
}

if(!require(ROCR)){
  install.packages("ROCR")
  library(ROCR)
}
library(ds.dataloader,lib.loc = "D:\\R Libraries")
library("xgboost",lib.loc = "D:\\R Libraries")
packageVersion("xgboost")
##### End Block: Load Libraries #####

##### Start Block: Load parameters#####
ReportingDate <- "20220901"
FolderPath <- "2022.08"

HaroldPath <- paste0("//hind.smartitbg.int//FileServer//Data Analyses//Analysis//3.Data//2022.08//iCredit MK//E006_Harold_MK_20220901_v1.csv")
CreditProgressPath <- paste0("//hind.smartitbg.int//FileServer//Data Analyses//Analysis//3.Data//2022.08//iCredit MK//E004_Credit_Progress_MK_20220901_v1.csv")
LastSoldPath <- paste0("//hind.smartitbg.int//FileServer//Data Analyses//Analysis//3.Data//2022.08//iCredit MK//E021A_Sold_Grouping_MK_20220901_v1.csv")
LastSoldReturnedPath <- paste0("//hind.smartitbg.int//FileServer//Data Analyses//Analysis//3.Data//2022.08//iCredit MK//E021B_Sold_Grouping_Returned_MK_20220901_v1.csv")

source(".\\Functions\\F_Elapsed_Months.R")
##### End Block: Load parameters#####

##### Start Block: Load Harold files #####
Harold <- ds.dataloader::loadHaroldData(HaroldPath) %>%
  dplyr::rename(CreditCode = KID,
                IncomesMinCosts = `Incomes-Costs`) %>%
  dplyr::select(CreditCode, ClientBirtDate, ProposalDate, ScoringType, Scoring, RefinansNew,
                ClientSex, ClientFamilyStatus, ClientEducation, TypeOfHousing, OtherProperties,
                TypeOfContract, GetSalaryType, ClientSalary, StaffType, OwnProperties, Automobiles,
                PartnerConnectionType, IncomesMinCosts, CompanyArea, WorkExperienceCurrentWork,
                WorkExperienceCurrentWorkMonths) %>% 
  dplyr::mutate(ClientBirtDate = ymd(substr(ClientBirtDate, 1, 10)),
                ProposalDate = parse_date_time(substr(ProposalDate, 1, 10), c("ymd", "dmy", "mdy")),
                Scoring = as.numeric(Scoring),
                IncomesMinCosts = as.numeric(gsub(IncomesMinCosts, pattern = ",", replacement = ".")),
                ClientSalary = as.numeric(gsub(ClientSalary, pattern = ",", replacement = ".")),
                WorkExperienceCurrentWork = as.numeric(gsub(WorkExperienceCurrentWork, pattern = ",", replacement = ".")),
                WorkExperienceCurrentWorkMonths = as.numeric(gsub(WorkExperienceCurrentWorkMonths, pattern = ",", replacement = ".")),
                WorkExperience = WorkExperienceCurrentWork * 12 + WorkExperienceCurrentWorkMonths,
                RefinansNew = as.numeric(RefinansNew),
                RefinansNew = ifelse(is.na(RefinansNew), 0, 1)) %>%
  dplyr::select(CreditCode, ClientBirtDate, Scoring, ScoringType, RefinansNew, ProposalDate) %>%
  dplyr::distinct()
##### End Block: Load Harold files #####

##### Start Block: Load Credit progress files #####
LastCreditProgress <- ds.dataloader::loadCreditProgress(CreditProgressPath)

Encoding(LastCreditProgress$CreditProduct) <- "UTF-8"
LastCreditProgress <- LastCreditProgress %>%
  dplyr::mutate(MaxDelayP = ifelse(is.na(MaxDelayP), 0, MaxDelayP),
         
         PaymentPlan = case_when(CreditProduct %in% c("M Fix", "M FIX", "M Flex 30") ~ "Monthly",
                                 CreditProduct %in% c("FlexRoll 15") ~ "Biweekly15",
                                 CreditProduct %in% c("bilunar", "creditpro") ~ "Biweekly",
                                 CreditProduct %in% c("easycredit", "icredit premium", "icredit", 
                                                      "icredit business", "icredit plus", "icredit vip") ~ "Weekly",
                                 TRUE ~ CreditProduct),
         period = case_when(PaymentPlan == "Weekly" ~ 7,
                            PaymentPlan == "Biweekly" ~ 14,
                            PaymentPlan == "Monthly" ~ 30,
                            TRUE ~ 99999),
         CreditMaturityDate = CreditFirstDateToPay + period * (Weeks - 1),
         CreditMaturity = period * Weeks)
##### End Block: Load Credit progress files #####

##### Start Block: Load Sold files #####
SoldReturned <- read.csv2(file = LastSoldReturnedPath, 
                          stringsAsFactors = F, 
                          header = T, 
                          na.strings = c("N/A", "NULL", ""), 
                          quote = "", 
                          encoding = "UTF-8") %>%
  mutate(IsReturned = 1,
         DateIn = ymd(substr(DateIn,1 , 10))) %>%
  rename(CreditCode = X.U.FEFF.ID) %>%
  select(CreditCode, DateIn, IsReturned)

Sold <- read.csv2(file = LastSoldPath, 
                  stringsAsFactors = F, 
                  header = T, 
                  na.strings = c("N/A", "NULL", ""), 
                  quote = "", 
                  encoding = "UTF-8") %>%
  mutate(IsReturned = 0,
         DateIn = ymd(substr(DateIn,1 , 10))) %>%
  rename(CreditCode = X.U.FEFF.ID) %>%
  select(CreditCode, DateIn, IsReturned)

SoldFinal <- bind_rows(Sold, SoldReturned) %>%
  arrange(CreditCode, DateIn, IsReturned) %>%
  group_by(CreditCode) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  select(CreditCode, DateIn) %>%
  rename(DateSold = DateIn) %>%
  mutate(CreditCode = as.character(CreditCode))
##### End Block: Load Sold files #####

##### Start Block: Load Master files #####
load(file = "//hind.smartitbg.int//FileServer//Data Analyses//Analysis//3.Data//HCC Data//Master2018_RO.RData")
load(file = "//hind.smartitbg.int//FileServer//Data Analyses//Analysis//3.Data//HCC Data//Master2019_RO.RData")
load(file = "//hind.smartitbg.int//FileServer//Data Analyses//Analysis//3.Data//HCC Data//Master2020_RO.RData")
load(file = "//hind.smartitbg.int//FileServer//Data Analyses//Analysis//3.Data//HCC Data//Master2021_RO.RData")
load(file = "//hind.smartitbg.int//FileServer//Data Analyses//Analysis//3.Data//HCC Data//Master2022_RO.RData")

Master <- bind_rows(Master2018, Master2019, Master2020, Master2021, Master2022) %>%
  filter(financestensilname %in% c("Rate pe credit", "Rambursare anticipata")) %>%
  rename(CreditCode = codecontract,
         PaymentDate = documentdate,
         Amount = Sum) %>%
  mutate(PaymentDate = ymd(substr(PaymentDate, 1, 10)),
         PaymentDateMonth = floor_date(PaymentDate, "month"),
         Amount = as.numeric(Amount),
         CreditCode = as.character(CreditCode)) %>%
  select(CreditCode, Amount, PaymentDate, PaymentDateMonth) %>%
  arrange(CreditCode, PaymentDate)

Master1 <- Master %>%
  group_by(CreditCode, PaymentDate) %>%
  summarize(Amount = sum(Amount),
            .groups = 'drop') %>%
  mutate(PaymentDateMonth = floor_date(PaymentDate, "months"))

LastVnoska <- Master1 %>%
  arrange(CreditCode, PaymentDate) %>%
  group_by(CreditCode) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  select(CreditCode, Amount) %>%
  rename(LastVnoska = Amount)

rm(Master2018, Master2019, Master2020, Master2021, Master2022)
##### End Block: Load Master files #####

table(LastCreditProgress$period)

##### Start Block: Prepare files #####


LastCreditProgressPrepared <- LastCreditProgress %>%
  filter(period != 99999) %>%
  left_join(SoldFinal, by = "CreditCode") %>%
  left_join(LastVnoska, by = "CreditCode") %>%
  left_join(Harold1, by = "CreditCode") %>%
  mutate(IsPaid = ifelse(!is.na(CompleteDate) & is.na(DateSold), 1, 0),
         CompleteDate = ifelse(is.na(CompleteDate), ymd(ReportingDate), CompleteDate),
         CompleteDate = as.numeric(CompleteDate),
         CreditBeginDate = as.numeric(CreditBeginDate),
         CreditFirstDateToPay = as.numeric(CreditFirstDateToPay),
         CreditMaturityDate = as.numeric(CreditMaturityDate),
         DateSold = as.numeric(DateSold),
         RealTimeAsClientForThisCredit = CompleteDate - CreditFirstDateToPay,
         TimeAsClientForThisCredit = CompleteDate - CreditBeginDate,
         DailyVnoska = Vnoska / period) %>%
  arrange(ClientEGN, CreditBeginDate) %>%
  select(ClientEGN, CreditCode, CreditBeginDate, SumaP) %>%
  group_by(ClientEGN) %>%
  mutate(Count = n()) %>%
  filter(Count > 15) %>%
  mutate(cum_SumaP10 = zoo::rollapplyr(SumaP, width = 10, FUN = sum, partial = TRUE, align = "right"),
         cum_max_SumaP10 = zoo::rollapplyr(SumaP, width = 10, FUN = max, partial = TRUE, align = "right"))





LastCreditProgressPrepared <- LastCreditProgress %>%
  filter(period != 99999) %>%
  left_join(SoldFinal, by = "CreditCode") %>%
  left_join(LastVnoska, by = "CreditCode") %>%
  left_join(Harold1, by = "CreditCode") %>%
  mutate(IsPaid = ifelse(!is.na(CompleteDate) & is.na(DateSold), 1, 0),
         CompleteDate = ifelse(is.na(CompleteDate), ymd(ReportingDate), CompleteDate),
         CompleteDate = as.numeric(CompleteDate),
         CreditBeginDate = as.numeric(CreditBeginDate),
         CreditFirstDateToPay = as.numeric(CreditFirstDateToPay),
         CreditMaturityDate = as.numeric(CreditMaturityDate),
         DateSold = as.numeric(DateSold),
         RealTimeAsClientForThisCredit = CompleteDate - CreditFirstDateToPay,
         TimeAsClientForThisCredit = CompleteDate - CreditBeginDate,
         DailyVnoska = Vnoska / period) %>%
  arrange(ClientEGN, CreditBeginDate) %>%
  mutate(NextRefinansNew = lead(RefinansNew, default = 0),
         
         IsGood60BasedOnDelay = ifelse(MaxDelayP > 60, 0, 1),
         IsBad60BasedOnDelay = ifelse(MaxDelayP > 60, 1, 0),
         
         IsGood30BasedOnDelay = ifelse(MaxDelayP > 30, 0, 1),
         IsBad30BasedOnDelay = ifelse(MaxDelayP > 30, 1, 0),
         
         BadDate60 = ifelse(IsBad60BasedOnDelay == 1, CompleteDate, 0),
         BadDate30 = ifelse(IsBad30BasedOnDelay == 1, CompleteDate, 0),
         
         SumaPPaidWithoutDelays60 = SumaP * IsGood60BasedOnDelay,
         SumaPPaidWithDelays60 = SumaP * IsBad60BasedOnDelay,
         
         SumaPPaidWithoutDelays30 = SumaP * IsGood30BasedOnDelay,
         SumaPPaidWithDelays30 = SumaP * IsBad30BasedOnDelay,
         
         DailyVnoskaPaidWithoutDelays60 = DailyVnoska * IsGood60BasedOnDelay,
         DailyVnoskaPaidWithDelays60 = DailyVnoska * IsBad60BasedOnDelay,
         
         DailyVnoskaPaidWithoutDelays30 = DailyVnoska * IsGood30BasedOnDelay,
         DailyVnoskaPaidWithDelays30 = DailyVnoska * IsBad30BasedOnDelay,
         
         TimeAsGoodClient60 = TimeAsClientForThisCredit * IsGood60BasedOnDelay,
         TimeAsBadClient60 = TimeAsClientForThisCredit * IsBad60BasedOnDelay,
         TimeAsGoodClient30 = TimeAsClientForThisCredit * IsGood30BasedOnDelay,
         TimeAsBadClient30 = TimeAsClientForThisCredit * IsBad30BasedOnDelay) %>%
  group_by(ClientEGN) %>%
  tidyr::fill(BadDate60, .direction = "down") %>%
  group_by(ClientEGN) %>%
  tidyr::fill(BadDate30, .direction = "down") %>%
  group_by(ClientEGN) %>%
  tidyr::fill(SumaPPaidWithDelays60, .direction = "down") %>%
  group_by(ClientEGN) %>%
  tidyr::fill(SumaPPaidWithDelays30, .direction = "down") %>%
  group_by(ClientEGN) %>%
  mutate(NumberOfPreviousCredits = row_number() - 1,
         
         PreviousDailyVnoska = lag(DailyVnoska, default = 0),
         
         PreviousMaxDailyVnoskaPaidWithoutDelays60 = zoo::rollapplyr(DailyVnoskaPaidWithoutDelays60, width = 11, FUN = max, partial = TRUE, align = "right"),
         PreviousMaxDailyVnoskaPaidWithoutDelays60 = lag(PreviousMaxDailyVnoskaPaidWithoutDelays60, default = 0),
         
         PreviousMaxDailyVnoskaPaidWithoutDelays30 = zoo::rollapplyr(DailyVnoskaPaidWithoutDelays30, width = 11, FUN = max, partial = TRUE, align = "right"),
         PreviousMaxDailyVnoskaPaidWithoutDelays30 = lag(PreviousMaxDailyVnoskaPaidWithoutDelays30, default = 0),
         
         PreviousGood60BasedOnDelay = zoo::rollapplyr(IsGood60BasedOnDelay, width = 11, FUN = sum, partial = TRUE, align = "right"),
         PreviousGood60BasedOnDelay = lag(PreviousGood60BasedOnDelay, default = 0),
         
         PreviousBad60BasedOnDelay = zoo::rollapplyr(IsBad60BasedOnDelay, width = 11, FUN = sum, partial = TRUE, align = "right"),
         PreviousBad60BasedOnDelay = lag(PreviousBad60BasedOnDelay, default = 0),
         
         PreviousGood30BasedOnDelay = zoo::rollapplyr(IsGood30BasedOnDelay, width = 11, FUN = sum, partial = TRUE, align = "right"),
         PreviousGood30BasedOnDelay = lag(PreviousGood30BasedOnDelay, default = 0),
         
         PreviousBad30BasedOnDelay = zoo::rollapplyr(IsBad30BasedOnDelay, width = 11, FUN = sum, partial = TRUE, align = "right"),
         PreviousBad30BasedOnDelay = lag(PreviousBad30BasedOnDelay, default = 0),
         
         PreviousBadDate60 = lag(BadDate60),
         PreviousBadDate30 = lag(BadDate30),
         
         NextCreditBeginDate = lead(CreditBeginDate),
         PreviousDateSold = lag(DateSold),
         PreviousCompleteDate = lag(CompleteDate),
         PreviousTimeAsClient = zoo::rollapplyr(TimeAsClientForThisCredit, width = 11, FUN = sum, partial = TRUE, align = "right"),
         PreviousTimeAsClient = lag(PreviousTimeAsClient, default = 0),
         
         PreviousTimeAsGoodClient60 = zoo::rollapplyr(TimeAsGoodClient60, width = 11, FUN = sum, partial = TRUE, align = "right"),
         PreviousTimeAsGoodClient60 = lag(PreviousTimeAsGoodClient60, default = 0),
         PreviousTimeAsBadClient60 = zoo::rollapplyr(TimeAsBadClient60, width = 11, FUN = sum, partial = TRUE, align = "right"),
         PreviousTimeAsBadClient60 = lag(PreviousTimeAsBadClient60, default = 0),
         
         PreviousTimeAsGoodClient30 = zoo::rollapplyr(TimeAsGoodClient30, width = 11, FUN = sum, partial = TRUE, align = "right"),
         PreviousTimeAsGoodClient30 = lag(PreviousTimeAsGoodClient30, default = 0),
         PreviousTimeAsBadClient30 = zoo::rollapplyr(TimeAsBadClient30, width = 11, FUN = sum, partial = TRUE, align = "right"),
         PreviousTimeAsBadClient30 = lag(PreviousTimeAsBadClient30, default = 0),
         
         PreviousSumaP = lag(SumaP, default = 0),
         PreviousSumaI = lag(SumaI, default = 0),
         
         PreviousMaxSumaP = zoo::rollapplyr(SumaP, width = 11, FUN = max, partial = TRUE, align = "right"),
         PreviousMaxSumaP = lag(PreviousMaxSumaP, default = 0),
         
         PreviousSumaPPaidWithoutDelays60 = zoo::rollapplyr(SumaPPaidWithoutDelays60, width = 11, FUN = max, partial = TRUE, align = "right"),
         PreviousSumaPPaidWithoutDelays60 = lag(PreviousSumaPPaidWithoutDelays60, default = 0),
         PreviousSumaPPaidWithDelays60 = zoo::rollapplyr(SumaPPaidWithDelays60, width = 11, FUN = max, partial = TRUE, align = "right"),
         PreviousSumaPPaidWithDelays60 = lag(PreviousSumaPPaidWithDelays60, default = 0),
         
         PreviousSumaPPaidWithoutDelays30 = zoo::rollapplyr(SumaPPaidWithoutDelays30, width = 11, FUN = max, partial = TRUE, align = "right"),
         PreviousSumaPPaidWithoutDelays30 = lag(PreviousSumaPPaidWithoutDelays30, default = 0),
         PreviousSumaPPaidWithDelays30 = zoo::rollapplyr(SumaPPaidWithDelays30, width = 11, FUN = max, partial = TRUE, align = "right"),
         PreviousSumaPPaidWithDelays30 = lag(PreviousSumaPPaidWithDelays30, default = 0),      
         
         SumaPIncreaseComparedToMax = SumaP - PreviousMaxSumaP,
         SumaPIncreaseComparedToPrevious = SumaP - PreviousSumaP,
         
         SumaPIncreaseComparedToMaxPaid60 = SumaP - PreviousSumaPPaidWithoutDelays60,
         SumaPIncreaseComparedToMaxPaid30 = SumaP - PreviousSumaPPaidWithoutDelays30,
         
         PreviousMaxDelayEver = zoo::rollapplyr(MaxDelayP, width = 11, FUN = max, partial = TRUE, align = "right"),
         PreviousMaxDelayEver = lag(PreviousMaxDelayEver),
         
         PreviousMaxDelay3 = lag(MaxDelayP, default = 0, n = 3),
         PreviousMaxDelay2 = lag(MaxDelayP, default = 0, n = 2),
         PreviousMaxDelay1 = lag(MaxDelayP, default = 0, n = 1),
         
         PreviousLastVnoska = lag(LastVnoska, default = 0),
         PreviousPaidTotal = zoo::rollapplyr(PayedTotal, width = 11, FUN = sum, partial = TRUE, align = "right"),
         PreviousPaidTotal = lag(PreviousPaidTotal, default = 0),
         
         PreviousSumCredits = zoo::rollapplyr(SumaP, width = 11, FUN = sum, partial = TRUE, align = "right"),
         PreviousSumCredits = lag(PreviousSumCredits, default = 0),
         
         PreviousProfit = PreviousPaidTotal - PreviousSumCredits) %>%
  ungroup() %>%
  mutate(TimeToPreviousCredit = as.numeric(CreditBeginDate - PreviousCompleteDate),
         ChangeInDelay = PreviousMaxDelay1 - PreviousMaxDelay2,
         
         TimeToPreviousBadDate60 = CreditBeginDate - PreviousBadDate60,
         TimeToPreviousBadDate30 = CreditBeginDate - PreviousBadDate30,
         
         BadSumaP60LastYear = ifelse(TimeToPreviousBadDate60 <= 365, PreviousSumaPPaidWithDelays60, 0),
         BadSumaP30LastYear = ifelse(TimeToPreviousBadDate30 <= 365, PreviousSumaPPaidWithDelays30, 0),
         
         PreviousMaxDailyVnoskaPaidWithoutDelays60 = ifelse(PreviousMaxDailyVnoskaPaidWithoutDelays60 == 0, 5, PreviousMaxDailyVnoskaPaidWithoutDelays60),
         ChangeInDailyVnoskaMax60 = DailyVnoska / PreviousMaxDailyVnoskaPaidWithoutDelays60,
         
         PreviousMaxDailyVnoskaPaidWithoutDelays30 = ifelse(PreviousMaxDailyVnoskaPaidWithoutDelays30 == 0, 5, PreviousMaxDailyVnoskaPaidWithoutDelays30),
         ChangeInDailyVnoskaMax30 = DailyVnoska / PreviousMaxDailyVnoskaPaidWithoutDelays30,
         
         ChangeInDailyVnoska = DailyVnoska / PreviousDailyVnoska,
         
         IsRefinanced = ifelse(as.numeric(NextCreditBeginDate - CompleteDate) <= 3, 1, 0),
         IsRefinanced = ifelse(is.na(IsRefinanced), 0, IsRefinanced),
         IsRefinanced = ifelse(is.na(DateSold), IsRefinanced, 0),
         
         IsRefinancing = ifelse(as.numeric(CreditBeginDate - PreviousCompleteDate) <= 3, 1, 0),
         IsRefinancing = ifelse(is.na(IsRefinancing), 0, IsRefinancing),
         IsRefinancing = ifelse(is.na(PreviousDateSold), IsRefinancing, 0),
         
         IsRefinancing1 = ifelse(as.numeric(CreditBeginDate - PreviousCompleteDate) <= 0, 1, 0),
         IsRefinancing1 = ifelse(is.na(IsRefinancing1), 0, IsRefinancing1),
         IsRefinancing1 = ifelse(RefinansNew == 1, 1, IsRefinancing1),
         IsRefinancing1 = ifelse(is.na(PreviousDateSold), IsRefinancing1, 0),
         
         IsParallel = ifelse(as.numeric(CreditBeginDate - PreviousCompleteDate) <= -1, 1, 0),
         IsParallel = ifelse(is.na(IsParallel), 0, IsParallel),
         
         LastVnoska = ifelse(is.na(LastVnoska), 0, LastVnoska),
         LastVnoska = ifelse(IsRefinanced == 1, LastVnoska, 0),
         LastVnoska = ifelse(CompleteDate < ymd(ReportingDate) & is.na(DateSold) , 0, LastVnoska),
         
         PaidWithoutLast = PayedTotal - LastVnoska,
         ChainNumber = ifelse(IsRefinancing == 0 & IsParallel == 0, 1, 0)) %>%
  group_by(ClientEGN) %>%
  mutate(ChainNumber = cumsum(ChainNumber),
         PreviousPaidWithoutLast = lag(PaidWithoutLast)) %>%
  group_by(ClientEGN, ChainNumber) %>%
  mutate(SumaPInChain = sum(SumaP),
         PayedTotalInChain = sum(PayedTotal),
         
         SumaPTillNow = cumsum(SumaP),
         PayedTotalTilNow = cumsum(PayedTotal),
         
         TakenBefore = lag(SumaPTillNow, default = 0),
         PaidBefore = lag(PayedTotalTilNow, default = 0),
         
         PreviousCreditsInChain = row_number() - 1,
         PreviousCreditsInChain = pmin(PreviousCreditsInChain, 10),
         FirstCreditCode = first(CreditCode),
         FirstDateInChain = first(CreditBeginDate),
         
         ProfitInChain = PayedTotalInChain - PaidBefore - (SumaPInChain - TakenBefore),
         maxCompleteDate = max(CompleteDate),
         ChainIsFinished = ifelse(maxCompleteDate == ymd(ReportingDate), 0, 1),
         MaxDelayPInChain = max(MaxDelayP),
         TimeAsClientForTheLastCreditInChain = last(TimeAsClientForThisCredit),
         
         TotalTimeInChain = sum(TimeAsClientForThisCredit),
         TotalRealTimeInChain = sum(RealTimeAsClientForThisCredit),
         CreditsInChain = n(),
         
         SumaPTillNow10 = zoo::rollapplyr(SumaP, width = 11, FUN = sum, partial = TRUE, align = "right"), 
         PayedTotalTilNow10 = zoo::rollapplyr(PayedTotal, width = 11, FUN = sum, partial = TRUE, align = "right"), 
         ProfitTillNow10 = PayedTotalTilNow10 - SumaPTillNow10 - PreviousLastVnoska) %>%
  ungroup() %>%
  mutate(TakenAfter = SumaPInChain - TakenBefore,
         ProfitAfter = PayedTotalInChain - PaidBefore - (SumaPInChain - TakenBefore),
         PayedAfter = PayedTotalInChain - PaidBefore,
         
         PreviousPercentagePaid = case_when(NumberOfPreviousCredits == 0 ~ 0,
                                            PreviousSumaP + PreviousSumaI == 0 ~ 0,
                                            TRUE ~ PreviousPaidWithoutLast / (PreviousSumaP + PreviousSumaI)),
         PreviousRemainingToBePaid = case_when(NumberOfPreviousCredits == 0 ~ 0,
                                               TRUE ~ PreviousSumaP + PreviousSumaI - PreviousPaidWithoutLast),
         PreviousChains = ChainNumber - 1,
         ChainID = paste0(ClientEGN,"-",FirstCreditCode),
         ClientType = case_when(NumberOfPreviousCredits == 0 ~ 1,
                                NumberOfPreviousCredits > 0 & IsParallel == 1 ~ 4,
                                NumberOfPreviousCredits > 0 & IsRefinancing1 == 1 ~ 3,
                                NumberOfPreviousCredits > 0 ~ 2,
                                TRUE ~ 999),
         CompleteDate = as.Date(CompleteDate, origin = "1970-01-01"),
         CreditBeginDate = as.Date(CreditBeginDate, origin = "1970-01-01"),
         CreditFirstDateToPay = as.Date(CreditFirstDateToPay, origin = "1970-01-01"),
         BadDate60 = as.Date(BadDate60, origin = "1970-01-01"),
         BadDate30 = as.Date(BadDate30, origin = "1970-01-01"),
         PreviousBadDate60 = as.Date(PreviousBadDate60, origin = "1970-01-01"),
         PreviousBadDate30 = as.Date(PreviousBadDate30, origin = "1970-01-01"),
         DateSold = as.Date(DateSold, origin = "1970-01-01"),
         CreditMaturityDate = as.Date(CreditMaturityDate, origin = "1970-01-01"),
         DefaultFlag = case_when(PayedTotalInChain >= 1.3 * SumaPInChain ~ 0,
                                 PaidWithoutLast > 1.3 * SumaP ~ 0,
                                 PayedTotalTilNow - LastVnoska > 1.3 * SumaPTillNow ~ 0,
                                 SumaP + SumaI == PayedTotal & IsRefinanced == 0 ~ 0,
                                 IsPaid == 1 & IsRefinanced == 0 ~ 0, 
                                 MaxDelayP > 90 ~ 1,
                                 !is.na(DateSold) ~ 1,
                                 TRUE ~ 99999),
         DefaultFlag2 = ifelse(DefaultFlag == 0, 0, NA)) %>%
  group_by(ChainID) %>%
  tidyr::fill(DefaultFlag2, .direction = "up") %>%
  ungroup() %>%
  mutate(DefaultFlag2 = ifelse(is.na(DefaultFlag2), DefaultFlag, DefaultFlag2))
