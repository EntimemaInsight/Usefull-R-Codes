rm(list = ls())
options(scipen = 999)
##### Start Block: Load Libraries #####
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


if(!require(xml2)){
  install.packages("xml2")
  library(xml2)
}

if(!require(Matrix)){
  install.packages("Matrix")
  library(Matrix)
}
library(XML)
library(ds.ccr)
library(ds.dataloader)
library("xgboost",lib.loc = "D:\\R Libraries")
library("xgboost",lib.loc = "D:\\Xgboost Provenir")
packageVersion("xgboost")
library(future.apply)

# library("xgboost",lib.loc = "D:\\R Libraries")
# library(ds.dataloader,lib.loc = "D:\\R Libraries")
# library(ds.ccr,lib.loc = "D:\\R Projects")
##### End Block: Load Libraries #####

##### Start Block: Load parameters#####

source(".\\Functions\\F_Elapsed_Months.R")
source(".\\Functions\\ReadHaroldBG.R")
source(".\\Functions\\SalesFunction.R")
source(".\\Functions\\ReadCreditProgress.R")
ReportingDate <- "20220501"
HaroldPath <- paste0("//hind.smartitbg.int//FileServer//Data Analyses//Analysis//3.Data//2022.04//Easy BG//E006_Harold_BG_", ReportingDate, "_v1.csv")
CreditProgressPath <- paste0("//hind.smartitbg.int//FileServer//Data Analyses//Analysis//3.Data//2022.04//Easy BG//E004_Credit_Progress_BG_", ReportingDate, "_v1.csv")
LastSoldPath <- paste0("//hind.smartitbg.int//FileServer//Data Analyses//Analysis//3.Data//2022.04//Easy BG//E021A_Sold_Grouping_BG_", ReportingDate, "_v1.csv")
LastSoldReturnedPath <- paste0("//hind.smartitbg.int//FileServer//Data Analyses//Analysis//3.Data//2022.04//Easy BG//E021B_Sold_Grouping_Returned_BG_", ReportingDate, "_v1.csv")


source(".\\Parse_CCR.R")
source(".\\Wrangle_CCR_Enc_No_Scale.R")

source(".\\Features_Names_NOI_2.r")
source(".\\Features_Names_NOI_7.r")
source(".\\extractInfoFromXmlChildren.r")
source(".\\age_calc.r")
source(".\\wrangle_NOI_2_Feature_Engineering_xml2.r")
source(".\\wrangle_NOI_7_Feature_Engineering_xml2.r")
source(".\\fill_cols.r")
source(".\\extract_NOI_Data_Feature_Engineering_vladi.r")
##### End Block: Load parameters#####


##### Start Block: Load Master files #####

Master1 <- loadMasterDB(company = "Easy BG", start_date = '20190101', end_date = '20220430')

Master <- Master1 %>%
  dplyr::filter(FinOperation %in% c("Вноска по кредит", "Предсрочно погасяване")) %>%
  dplyr::rename(CreditCode = codecontract,
                PaymentDate = documentdate,
                Amount = Sum) %>%
  dplyr::mutate(PaymentDate = ymd(substr(PaymentDate, 1, 10)),
                PaymentDateMonth = floor_date(PaymentDate, "month"),
                Amount = as.numeric(Amount),
                CreditCode = as.character(CreditCode)) %>%
  dplyr::select(CreditCode, Amount, PaymentDate, PaymentDateMonth) %>%
  dplyr::arrange(CreditCode, PaymentDate) %>%
  dplyr::group_by(CreditCode, PaymentDate) %>%
  dplyr::summarize(Amount = sum(Amount),
                   .groups = 'drop') %>%
  dplyr::mutate(PaymentDateMonth = floor_date(PaymentDate, "months"))

LastVnoska <- Master %>%
  dplyr::arrange(CreditCode, PaymentDate) %>%
  dplyr::group_by(CreditCode) %>%
  dplyr::filter(row_number() == n()) %>%
  dplyr::ungroup() %>%
  dplyr::select(CreditCode, Amount) %>%
  dplyr::rename(LastVnoska = Amount)
##### End Block: Load Master files #####

##### Start Block: Load Sold files #####
SoldFinal <- SoldFilesFunction(LastSoldPath = LastSoldPath, 
                               LastSoldReturnedPath = LastSoldReturnedPath)
##### End Block: Load Sold files #####

##### Start Block: Load Harold files #####
Harold <- ReadHaroldBG(HaroldPath = HaroldPath) %>%
  dplyr::select(CreditCode, ProposalDate, CompleteDate, Age,
                DepartmentCode, Scoring, ScoringType, ClientBirtDate,
                ClientSex, ClientFamilyStatus, ClientEducation, TypeOfHousing,
                WorkExperienceCurrentWork, WorkExperienceCurrentWorkMonths,
                TotalLengthOfService, TypeOfContract, GetSalaryType, ClientSalary,
                PartnerConnectionType, IncomeMinCosts, RefinansNew)
table(Harold$TypeOfContract, useNA = "ifany")
##### End Block: Load Harold files #####

##### Start Block: Load Credit progress files #####
LastCreditProgress <- ReadCreditProgressBG(CreditProgressPath = CreditProgressPath) %>%
  dplyr::select(CreditCode, ClientEGN, CreditProduct, CurrentOfisName, OfficeRegion, 
                CreditBeginDate, CreditFirstDateToPay, CompleteDate, SubStatus,
                SumaP, SumaI, Vnoska, Weeks, Padejirali, CurrentDelay, MaxDelayP, PayedTotal,
                PaymentPlan, period, CreditMaturity, CreditMaturityDate)

LastCreditProgressPrepared <- LastCreditProgress %>%
  filter(period != 99999) %>%
  left_join(SoldFinal, by = "CreditCode") %>%
  left_join(LastVnoska, by = "CreditCode") %>%
  left_join(Harold %>% select(CreditCode, ProposalDate), by = "CreditCode") %>%
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
  group_by(ClientEGN) %>%
  mutate(NumberOfPreviousCredits = row_number() - 1,
         
         NextCreditBeginDate = lead(CreditBeginDate),
         PreviousDateSold = lag(DateSold),
         PreviousCompleteDate = lag(CompleteDate),
         PreviousSumaP = lag(SumaP, default = 0),
         PreviousSumaI = lag(SumaI, default = 0),
         
         PreviousLastVnoska = lag(LastVnoska, default = 0),
         PreviousPaidTotal = lag(cumsum(PayedTotal), default = 0),
         PreviousSumCredits = lag(cumsum(SumaP), default = 0),
         PreviousProfit = PreviousPaidTotal - PreviousSumCredits) %>%
  ungroup() %>%
  mutate(TimeToPreviousCredit = as.numeric(CreditBeginDate - PreviousCompleteDate),
         
         IsRefinanced = ifelse(as.numeric(NextCreditBeginDate - CompleteDate) <= 3, 1, 0),
         IsRefinanced = ifelse(is.na(IsRefinanced), 0, IsRefinanced),
         IsRefinanced = ifelse(is.na(DateSold), IsRefinanced, 0),
         
         IsRefinancing = ifelse(as.numeric(CreditBeginDate - PreviousCompleteDate) <= 3, 1, 0),
         IsRefinancing = ifelse(is.na(IsRefinancing), 0, IsRefinancing),
         IsRefinancing = ifelse(is.na(PreviousDateSold), IsRefinancing, 0),
         
         IsRefinancing1 = ifelse(as.numeric(CreditBeginDate - PreviousCompleteDate) <= 0, 1, 0),
         IsRefinancing1 = ifelse(is.na(IsRefinancing1), 0, IsRefinancing1),
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
         FirstCreditCode = first(CreditCode),
         FirstDateInChain = first(CreditBeginDate),
         
         ProfitInChain = PayedTotalInChain - PaidBefore - (SumaPInChain - TakenBefore),
         maxCompleteDate = max(CompleteDate),
         ChainIsFinished = ifelse(maxCompleteDate == ymd(ReportingDate), 0, 1),
         MaxDelayPInChain = max(MaxDelayP),
         TimeAsClientForTheLastCreditInChain = last(TimeAsClientForThisCredit),
         
         TotalTimeInChain = sum(TimeAsClientForThisCredit),
         TotalRealTimeInChain = sum(RealTimeAsClientForThisCredit),
         CreditsInChain = n()) %>%
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
         DateSold = as.Date(DateSold, origin = "1970-01-01"),
         CreditMaturityDate = as.Date(CreditMaturityDate, origin = "1970-01-01"),
         DefaultFlag = case_when(PayedTotalInChain >= 1.1 * SumaPInChain ~ 0,
                                 PaidWithoutLast > 1.1 * SumaP ~ 0,
                                 PayedTotalTilNow - LastVnoska > 1.1 * SumaPTillNow ~ 0,
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

FirstClients <- LastCreditProgressPrepared %>%
  dplyr::select(CreditCode, ClientEGN, CreditBeginDate, CompleteDate, CreditProduct,
                SumaP, SumaI, PayedTotal, period, Vnoska, NumberOfPreviousCredits) %>%
  dplyr::filter(NumberOfPreviousCredits == 0) %>%
  dplyr::filter(!is.na(ClientEGN))

FirstClientsForCKR <- LastCreditProgressPrepared %>%
  dplyr::select(CreditCode, ClientEGN, CreditBeginDate, CompleteDate, CreditProduct,
                SumaP, SumaI, PayedTotal, period, Vnoska, NumberOfPreviousCredits, TimeToPreviousCredit,
                DefaultFlag2) %>%
  dplyr::filter(NumberOfPreviousCredits == 0 | TimeToPreviousCredit > 180) %>%
  dplyr::filter(!is.na(ClientEGN))
##### End Block: Load Credit progress files #####

##### Start Block: Load CCR files #####

parString <- toString(sprintf("'%s'", FirstClientsForCKR$ClientEGN[1:100])) 
queryTemplate <- "SELECT
      tc.ID_EGN
      ,te.EGN
      ,tc.CCRXmlData
      ,tc.CDate
      ,tc.RequestType
  FROM [CCR].[dbo].[tCCR] tc 
  left join [CCR].[dbo].[tEGN] te on tc.ID_EGN = te.ID
where te.EGN in (%s)
;" 
query <- toString(sprintf(queryTemplate, parString))

dbConn <- DBI::dbConnect(odbc::odbc(),
                         driver = "SQL Server",
                         server = 'dlmsql.smartitbg.int',
                         database = "CCR")
test <- DBI::dbGetQuery(dbConn, query) %>%
  dplyr::mutate(CDate = lubridate::ymd(substr(CDate, 1, 10)))
DBI::dbDisconnect(dbConn)



query <- "SELECT
       te.EGN,
       tc.CDate,
       tc.RequestType,
       tc.CCRXmlData
  FROM [CCR].[dbo].[tCCR] tc
left join [CCR].[dbo].[tEGN] te on tc.ID_EGN = te.ID;" 

dbConn <- DBI::dbConnect(odbc::odbc(),
                         driver = "SQL Server",
                         server = 'dlmsql.smartitbg.int',
                         database = "CCR")

CCRSFromDatabase <- DBI::dbGetQuery(dbConn, query) %>%
  dplyr::mutate(CDate = lubridate::ymd(substr(CDate, 1, 10)))
DBI::dbDisconnect(dbConn)
write_rds(CCRSFromDatabase, "./CCRSFromDatabase.rds")
#CCRSFromDatabase <- readRDS("./CCRSFromDatabase.rds")

CKRWithLoans <- CCRSFromDatabase %>%
  dplyr::left_join(FirstClients %>% select(CreditCode, ClientEGN, CreditBeginDate), 
                   by = c("EGN" = "ClientEGN")) %>%
  dplyr::mutate(Diff = as.numeric(CreditBeginDate - CDate)) %>%
  dplyr::filter(Diff >= 0,
                Diff <= 3) %>%
  dplyr::arrange(CreditCode, Diff) %>%
  dplyr::group_by(CreditCode) %>%
  dplyr::slice_head() %>%
  dplyr::ungroup()
##### End Block: Load CCR files #####

##### Start Block: Wrangle CCR data #####
person_data <- ds.ccr::parallelize(CKRWithLoans$CCRXmlData, ds.ccr::parse_ccr_xml)
write_rds(person_data, "./person_data.rds")
person_data <- readRDS("./person_data.rds")

wrangled_data <- ds.ccr::parallelize(person_data, ds.ccr::wrangle_ccr)
write_rds(wrangled_data, "./wrangled_data.rds")
wrangled_data <- readRDS("./wrangled_data.rds")
colnames(wrangled_data)
#wrangled_data <- bind_cols(CKRWithLoans %>% select(CreditCode), wrangled_data)

# wrangled_data <- wrangled_data %>%
#   left_join(FirstClients %>% select(CreditCode, ClientEGN), by = "ClientEGN")

CredCount_B_INT
CredCount_N_INT

H_CTDelay_INT_B_A
H_CTDelay_INT_B_CL
H_CTDelay_INT_N_A
H_CTDelay_INT_N_CL

H_MaxDPD_FACT_B_A
H_MaxDPD_FACT_B_CL
H_MaxDPD_FACT_N_A
H_MaxDPD_FACT_N_CL



test <- wrangled_data %>%
  filter(!is.na(H_MaxDPD_FACT_B_A),
         CredCount_B_INT == 0,
         CredCount_N_INT == 0)

test <- wrangled_data %>%
  filter(is.na(CredCount_B_INT))

sum(!is.na(test %>% select(-CreditCode, -EGN)))

##### End Block: Wrangle CCR data #####

FactorColumns <- wrangled_data %>%
  select_if(is.factor)
colnames(FactorColumns)
table(FactorColumns$H_MaxDPD_span1_FACT_B_A, useNA = "ifany")


wrangled_data1 <- wrangled_data %>%
  dplyr::mutate(HasCCRData = ifelse(is.na(CredCount_B_INT), 0 , 1)) %>%
  dplyr::mutate(MaxCurrentDPD_B_FACT_NoDPD = ifelse(MaxCurrentDPD_B_FACT == "NoDPD", 1, 0),
                MaxCurrentDPD_B_FACT_DPD90 = ifelse(MaxCurrentDPD_B_FACT == "DPD90", 1, 0),
                MaxCurrentDPD_B_FACT_DPD180 = ifelse(MaxCurrentDPD_B_FACT == "DPD180", 1, 0),
                MaxCurrentDPD_B_FACT_DPD181 = ifelse(MaxCurrentDPD_B_FACT == "DPD181", 1, 0),
                
                MaxCurrentDPD_C_FACT_NoLoan = ifelse(MaxCurrentDPD_C_FACT == "NoLoan", 1, 0),
                MaxCurrentDPD_C_FACT_DPDCurrent = ifelse(MaxCurrentDPD_C_FACT == "DPDCurrent", 1, 0),
                MaxCurrentDPD_C_FACT_DPD90 = ifelse(MaxCurrentDPD_C_FACT == "DPD90", 1, 0),
                MaxCurrentDPD_C_FACT_DPD180 = ifelse(MaxCurrentDPD_C_FACT == "DPD180", 1, 0),
                MaxCurrentDPD_C_FACT_DPD181 = ifelse(MaxCurrentDPD_C_FACT == "DPD181", 1, 0),
                
                MaxCurrentDPD_N_FACT_NoLoan = ifelse(MaxCurrentDPD_N_FACT == "NoLoan", 1, 0),
                MaxCurrentDPD_N_FACT_DPDCurrent = ifelse(MaxCurrentDPD_N_FACT == "DPDCurrent", 1, 0),
                MaxCurrentDPD_N_FACT_DPD90 = ifelse(MaxCurrentDPD_N_FACT == "DPD90", 1, 0),
                MaxCurrentDPD_N_FACT_DPD180 = ifelse(MaxCurrentDPD_N_FACT == "DPD180", 1, 0),
                MaxCurrentDPD_N_FACT_DPD181 = ifelse(MaxCurrentDPD_N_FACT == "DPD181", 1, 0),
                
                H_MaxDPD_FACT_B_A_NoDPD = ifelse(H_MaxDPD_FACT_B_A == "NoDPD", 1, 0),
                H_MaxDPD_FACT_B_A_DPD90 = ifelse(H_MaxDPD_FACT_B_A == "DPD90", 1, 0),
                H_MaxDPD_FACT_B_A_DPD180 = ifelse(H_MaxDPD_FACT_B_A == "DPD180", 1, 0),
                H_MaxDPD_FACT_B_A_DPD181 = ifelse(H_MaxDPD_FACT_B_A == "DPD181", 1, 0),
                
                H_MaxDPD_span1_FACT_B_A_NoDPD = ifelse(H_MaxDPD_span1_FACT_B_A == "NoDPD", 1, 0),
                H_MaxDPD_span1_FACT_B_A_DPD90 = ifelse(H_MaxDPD_span1_FACT_B_A == "DPD90", 1, 0),
                H_MaxDPD_span1_FACT_B_A_DPD180 = ifelse(H_MaxDPD_span1_FACT_B_A == "DPD180", 1, 0),
                H_MaxDPD_span1_FACT_B_A_DPD181 = ifelse(H_MaxDPD_span1_FACT_B_A == "DPD181", 1, 0),
                
                H_MinSpan_FACT_B_A_Never = ifelse(H_MinSpan_FACT_B_A == "Never", 1, 0),
                H_MinSpan_FACT_B_A_Less24M = ifelse(H_MinSpan_FACT_B_A == "Less24M", 1, 0),
                H_MinSpan_FACT_B_A_More24M = ifelse(H_MinSpan_FACT_B_A == "More24M", 1, 0),
                
                H_MaxDPD_span1_FACT_N_A_NoDPD = ifelse(H_MaxDPD_span1_FACT_N_A == "NoDPD", 1, 0),
                H_MaxDPD_span1_FACT_N_A_DPD90 = ifelse(H_MaxDPD_span1_FACT_N_A == "DPD90", 1, 0),
                H_MaxDPD_span1_FACT_N_A_DPD180 = ifelse(H_MaxDPD_span1_FACT_N_A == "DPD180", 1, 0),
                H_MaxDPD_span1_FACT_N_A_DPD181 = ifelse(H_MaxDPD_span1_FACT_N_A == "DPD181", 1, 0),
                
                H_MinSpan_FACT_N_A_Never = ifelse(H_MinSpan_FACT_N_A == "Never", 1, 0),
                H_MinSpan_FACT_N_A_Less24M = ifelse(H_MinSpan_FACT_N_A == "Less24M", 1, 0),
                H_MinSpan_FACT_N_A_More24M = ifelse(H_MinSpan_FACT_N_A == "More24M", 1, 0),
                
                H_MaxDPD_FACT_N_A_NoDPD = ifelse(H_MaxDPD_FACT_N_A == "NoDPD", 1, 0),
                H_MaxDPD_FACT_N_A_DPD90 = ifelse(H_MaxDPD_FACT_N_A == "DPD90", 1, 0),
                H_MaxDPD_FACT_N_A_DPD180 = ifelse(H_MaxDPD_FACT_N_A == "DPD180", 1, 0),
                H_MaxDPD_FACT_N_A_DPD181 = ifelse(H_MaxDPD_FACT_N_A == "DPD181", 1, 0),
                
                H_MaxDPD_FACT_B_CL_NoDPD = ifelse(H_MaxDPD_FACT_B_CL == "NoDPD", 1, 0),
                H_MaxDPD_FACT_B_CL_DPD90 = ifelse(H_MaxDPD_FACT_B_CL == "DPD90", 1, 0),
                H_MaxDPD_FACT_B_CL_DPD180 = ifelse(H_MaxDPD_FACT_B_CL == "DPD180", 1, 0),
                H_MaxDPD_FACT_B_CL_DPD181 = ifelse(H_MaxDPD_FACT_B_CL == "DPD181", 1, 0),
                
                H_MaxDPD_FACT_N_CL_NoDPD = ifelse(H_MaxDPD_FACT_N_CL == "NoDPD", 1, 0),
                H_MaxDPD_FACT_N_CL_DPD90 = ifelse(H_MaxDPD_FACT_N_CL == "DPD90", 1, 0),
                H_MaxDPD_FACT_N_CL_DPD180 = ifelse(H_MaxDPD_FACT_N_CL == "DPD180", 1, 0),
                H_MaxDPD_FACT_N_CL_DPD181 = ifelse(H_MaxDPD_FACT_N_CL == "DPD181", 1, 0),
                
                H_MaxDPD_span1_FACT_B_CL_NoDPD = ifelse(H_MaxDPD_span1_FACT_B_CL == "NoDPD", 1, 0),
                H_MaxDPD_span1_FACT_B_CL_DPD90 = ifelse(H_MaxDPD_span1_FACT_B_CL == "DPD90", 1, 0),
                H_MaxDPD_span1_FACT_B_CL_DPD180 = ifelse(H_MaxDPD_span1_FACT_B_CL == "DPD180", 1, 0),
                H_MaxDPD_span1_FACT_B_CL_DPD181 = ifelse(H_MaxDPD_span1_FACT_B_CL == "DPD181", 1, 0),
 
                H_MaxDPD_span1_FACT_N_CL_NoDPD = ifelse(H_MaxDPD_span1_FACT_N_CL == "NoDPD", 1, 0),
                H_MaxDPD_span1_FACT_N_CL_DPD90 = ifelse(H_MaxDPD_span1_FACT_N_CL == "DPD90", 1, 0),
                H_MaxDPD_span1_FACT_N_CL_DPD180 = ifelse(H_MaxDPD_span1_FACT_N_CL == "DPD180", 1, 0),
                H_MaxDPD_span1_FACT_N_CL_DPD181 = ifelse(H_MaxDPD_span1_FACT_N_CL == "DPD181", 1, 0), 
                
                H_MinSpan_FACT_B_CL_Never = ifelse(H_MinSpan_FACT_B_CL == "Never", 1, 0),
                H_MinSpan_FACT_B_CL_Less24M = ifelse(H_MinSpan_FACT_B_CL == "Less24M", 1, 0),
                H_MinSpan_FACT_B_CL_More24M = ifelse(H_MinSpan_FACT_B_CL == "More24M", 1, 0),
                
                H_MinSpan_FACT_N_CL_Never = ifelse(H_MinSpan_FACT_N_CL == "Never", 1, 0),
                H_MinSpan_FACT_N_CL_Less24M = ifelse(H_MinSpan_FACT_N_CL == "Less24M", 1, 0),
                H_MinSpan_FACT_N_CL_More24M = ifelse(H_MinSpan_FACT_N_CL == "More24M", 1, 0)) %>%
  select(-MaxCurrentDPD_B_FACT, -MaxCurrentDPD_C_FACT, -MaxCurrentDPD_N_FACT,    
         -H_MaxDPD_FACT_B_A,        -H_MaxDPD_span1_FACT_B_A,  -H_MinSpan_FACT_B_A,      
         -H_MaxDPD_span1_FACT_N_A,  -H_MinSpan_FACT_N_A,       -H_MaxDPD_FACT_N_A,       
         -H_MaxDPD_FACT_B_CL,       -H_MaxDPD_FACT_N_CL,       -H_MaxDPD_span1_FACT_B_CL,
         -H_MaxDPD_span1_FACT_N_CL, -H_MinSpan_FACT_B_CL,      -H_MinSpan_FACT_N_CL)

table(FactorColumns$H_MinSpan_FACT_N_CL, useNA = "ifany")

##### Start Block: Prep data data #####
CKRFirstClients <- wrangled_data1 %>%
  left_join(LastCreditProgressPrepared %>% select(CreditCode, DefaultFlag2, CreditBeginDate), by = "CreditCode") %>%
  left_join(Harold %>% select(CreditCode, ScoringType, Scoring))
colnames(CKRFirstClients)


NewClients <- CKRFirstClients %>%
  mutate(Scoring = as.numeric(Scoring),
         Quarter = quarter(CreditBeginDate, with_year = TRUE)) %>%
  rename(DefaultNumeric = DefaultFlag2)
table(NewClients$ScoringType)

Test <- NewClients %>%
  filter(HasCCRData == 0)
table(Test$ScoringType)

NewClients <- NewClients %>%
  filter(HasCCRData == 1)

set.seed(37)
TrainSetNewClients <- NewClients %>%
  as.data.frame()

TrainSetNewClients <- NewClients[sample(nrow(NewClients)),] %>%
  group_by(Quarter, DefaultNumeric) %>%
  mutate(Fold = ntile(x = row_number(), 5)) %>%
  ungroup() %>%
  filter(Quarter < 2021.3) %>%
  select(-ScoringType, -Scoring, -CreditBeginDate)

table(TrainSetNewClients$DefaultNumeric)
Missings <- sapply(TrainSetNewClients, function (x) sum(is.na(x)))
Missings


ValidationSetNewClients <- NewClients %>%
  filter(Quarter >= 2021.3) %>%
  as.data.frame() 

sapply(ValidationSetNewClients, function (x) sum(is.na(x)))


dValidation <- model.matrix(DefaultNumeric ~ ., data = ValidationSetNewClients %>% 
                              select(-CreditCode, -EGN, -Quarter, -CreditBeginDate,
                                     -Scoring, -ScoringType))[, -1]
dValidation <- xgb.DMatrix(dValidation, label = ValidationSetNewClients %>% pull(DefaultNumeric))






eta_param <- seq(from = 0.1, to = 0.1, by = 0.02)
gamma_param <- seq(from = 2, to = 3, by = 0.5)
max.depth_param <- seq(from = 7, to = 8, by = 1)
subsample_param <- seq(from = 0.8, to = 0.8, by = 0.1)
colsample_bytree_param  <- seq(from = 1, to = 1, by = 0.1)
parameters <- expand.grid(eta = eta_param, 
                          gamma = gamma_param,
                          max_depth = max.depth_param,
                          subsample = subsample_param,
                          colsample_bytree = colsample_bytree_param) %>%
  arrange(eta, gamma, max_depth)

do_model <- function(arg1){
  
  output_do_model <- lapply(1:5, function(x) do_cv(x, arg1)) %>%
    bind_rows() %>%
    mutate(eta = arg1[1], 
           gamma = arg1[2],
           max.depth = arg1[3],
           subsample = arg1[4],
           colsample_bytree = arg1[5])
  gc()
  return(output_do_model)
}

do_cv <- function(arg1, arg2){
  
  Test <- arg1
  Train <- 1:5
  Train <- Train[!Train %in% Test]
  
  Train <- TrainSetNewClients %>%
    filter(Fold %in% Train,
           DefaultNumeric < 2) %>%
    select(-Fold)
  
  dTrain <- model.matrix(DefaultNumeric ~ ., data = Train %>% select(-CreditCode, -EGN, -Quarter))[, -1]
  dTrain <- xgb.DMatrix(dTrain, label = Train %>% pull(DefaultNumeric))
  
  Test <- TrainSetNewClients %>%
    filter(Fold %in% Test,
           DefaultNumeric < 2) %>%
    select(-Fold)
  
  dTest <- model.matrix(DefaultNumeric ~ ., data = Test %>% select(-CreditCode, -EGN, -Quarter))[, -1]
  dTest <- xgb.DMatrix(dTest, label = Test %>% pull(DefaultNumeric))
  
  watchlist <- list(train = dTrain, test = dTest)
  
  xgbmodel <- xgb.train(data = dTrain,
                        eta = arg2[1], 
                        gamma = arg2[2],
                        max.depth = arg2[3], 
                        min_child_weight = 1,
                        subsample = arg2[4],
                        colsample_bytree = arg2[5],
                        tree_method = "auto",
                        nthread = 10, 
                        nrounds = 1000,
                        verbose = 0,
                        watchlist = watchlist,
                        early_stopping_rounds = 5,
                        eval_metric = "auc",
                        objective = "binary:logistic")
  
  ValidationSetNewClientsGINI <- ValidationSetNewClients %>%
    select(CreditCode, Quarter) %>%
    mutate(Prediction = predict(xgbmodel, dValidation),
           DefaultNumeric = ValidationSetNewClients$DefaultNumeric) %>%
    filter(Quarter <= 2022.1,
           DefaultNumeric < 2) %>%
    group_by(Quarter) %>%
    summarise(Count = n(),
              AUC = ROCR::performance(ROCR::prediction(Prediction, DefaultNumeric), measure = "auc")@y.values[[1]],
              GINI = 2 * AUC - 1,
              .groups = "drop") %>%
    select(-AUC)
  
  output <- data.frame(BestIteration = xgbmodel$best_iteration,
                       Gini_Train = 2 * xgbmodel$evaluation_log$train_auc[xgbmodel$best_iteration] - 1,
                       Gini_Test = 2 * xgbmodel$evaluation_log$test_auc[xgbmodel$best_iteration] - 1,
                       Gini_Validation2021.3 = ValidationSetNewClientsGINI$GINI[1],
                       Gini_Validation2021.4 = ValidationSetNewClientsGINI$GINI[2],
                       Gini_Validation2022.1 = ValidationSetNewClientsGINI$GINI[3])
  print(arg2[1])
  return(output)
}

system.time({ 
  results <- apply(parameters, 1, do_model) %>%
    bind_rows() 
  
  results1 <- results %>%
    group_by(eta, gamma, max.depth, subsample, colsample_bytree) %>%
    summarize(Gini_Train = mean(Gini_Train),
              Gini_Test_max = max(Gini_Test),
              Gini_Test_min = min(Gini_Test),
              Gini_Test = mean(Gini_Test),
              Gini_Validation2021.3 = mean(Gini_Validation2021.3),
              Gini_Validation2021.4 = mean(Gini_Validation2021.4),
              Gini_Validation2022.1 = mean(Gini_Validation2022.1),
              meanBestIteration = mean(BestIteration),
              maxBestIteration = max(BestIteration),
              minBestIteration = min(BestIteration),
              medianBestIteration = median(BestIteration),
              .groups = "drop") %>%
    arrange(desc(Gini_Test)) %>%
    ungroup()
  
  saveRDS(results, file = "./Output Data//ResultsForNewCustomers.rds")
})
View(results1)



dTrain <- model.matrix(DefaultNumeric ~ ., data = TrainSetNewClients %>% select(-CreditCode, -Quarter, 
                                                                                -EGN, -Fold))[, -1]
dTrain <- xgb.DMatrix(dTrain, label = TrainSetNewClients %>% pull(DefaultNumeric))

xgbmodelNew <- xgboost::xgboost(data = dTrain,
                       eta = 0.1, 
                       gamma = 3,
                       max.depth = 7, 
                       min_child_weight = 1,
                       subsample = 0.8,
                       colsample_bytree = 1,
                       tree_method = "auto",
                       nthread = 1, 
                       nrounds = 56,
                       eval_metric = "auc",
                       objective = "binary:logistic")
class(xgbmodelNew)



Test <- 1
Train <- 1:5
Train <- Train[!Train %in% Test]

Train <- TrainSetNewClients %>%
  filter(Fold %in% Train,
         DefaultNumeric < 2) %>%
  select(-Fold)

dTrain <- model.matrix(DefaultNumeric ~ ., data = Train %>% select(-CreditCode, -EGN, -Quarter))[, -1]
dTrain <- xgb.DMatrix(dTrain, label = Train %>% pull(DefaultNumeric))

Test <- TrainSetNewClients %>%
  filter(Fold %in% Test,
         DefaultNumeric < 2) %>%
  select(-Fold)

dTest <- model.matrix(DefaultNumeric ~ ., data = Test %>% select(-CreditCode, -EGN, -Quarter))[, -1]
dTest <- xgb.DMatrix(dTest, label = Test %>% pull(DefaultNumeric))

watchlist <- list(train = dTrain, test = dTest)

xgbmodel <- xgb.train(data = dTrain,
                      eta = 0.1, 
                      gamma = 3,
                      max.depth = 8, 
                      min_child_weight = 1,
                      subsample = 0.8,
                      colsample_bytree = 1,
                      tree_method = "auto",
                      nthread = 10, 
                      nrounds = 1000,
                      verbose = 1,
                      watchlist = watchlist,
                      early_stopping_rounds = 5,
                      eval_metric = "auc",
                      objective = "binary:logistic")



importance_matrix <- xgb.importance(model = xgbmodel)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
2 * xgbmodelNew$evaluation_log$train_auc[50] - 1
saveRDS(xgbmodelNew, file = "./Output Data//xgbmodelNewProvenir.rds")

xgbmodelNew <- readRDS(file = "./Output Data//xgbmodelNewProvenir.rds")

TrainSetNewClientsWithPredictions <- TrainSetNewClients %>%
  select(CreditCode, EGN, Quarter) %>%
  mutate(Prediction = predict(xgbmodel, dTrain),
         DefaultNumeric = TrainSetNewClients$DefaultNumeric)

ValidationSetNewClientsWithPredictions <- ValidationSetNewClients %>%
  select(CreditCode, EGN, Quarter, ScoringType, Scoring) %>%
  mutate(Prediction = predict(xgbmodel, dValidation),
         DefaultNumeric = ValidationSetNewClients$DefaultNumeric)

table(ValidationSetNewClientsWithPredictions$ScoringType)
table(ValidationSetNewClientsWithPredictions$Quarter, ValidationSetNewClientsWithPredictions$DefaultNumeric)

ValidationSetNewClientsGINI <- ValidationSetNewClientsWithPredictions %>%
  filter(DefaultNumeric < 2) %>%
  group_by(Quarter) %>%
  summarise(CountNewModel = n(),
            AUC = ROCR::performance(ROCR::prediction(Prediction, DefaultNumeric), measure = "auc")@y.values[[1]],
            GININewModel = round((2 * AUC - 1) * 100, digits = 2),
            .groups = "drop") %>%
  select(-AUC)

ValidationSetNewClientsGINI1 <- ValidationSetNewClientsWithPredictions %>%
  filter(DefaultNumeric < 2,
         ScoringType == 1001) %>%
  group_by(Quarter) %>%
  summarise(CountNewModel = n(),
            AUC = ROCR::performance(ROCR::prediction(Prediction, DefaultNumeric), measure = "auc")@y.values[[1]],
            GININewModel = round((2 * AUC - 1) * 100, digits = 2),
            AUC = ROCR::performance(ROCR::prediction(Scoring, DefaultNumeric), measure = "auc")@y.values[[1]],
            GINIOldModel = round((2 * AUC - 1) * 100, digits = 2),
            .groups = "drop") %>%
  select(-AUC)
  View(ValidationSetNewClientsGINI1)
