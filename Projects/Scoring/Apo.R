CreditProgressPath <- paste0("//hind.smartitbg.int//FileServer//Data Analyses//Analysis//3.Data//2022.10//Easy BG//E004_Credit_Progress_BG_20221101_v1.csv")

source(".\\Functions\\ReadCreditProgress.R")

LastCreditProgress <- ReadCreditProgressBG(CreditProgressPath = CreditProgressPath)

LastCreditProgress <- LastCreditProgress %>%
  mutate(CompleteDate = as.numeric(CompleteDate),
         CreditBeginDate = as.numeric(CreditBeginDate)) %>%
  arrange(ClientEGN, CreditBeginDate) %>%
  group_by(ClientEGN) %>%
  mutate(NumberOfPreviousCredits = row_number() - 1,
         
         NextCreditBeginDate = lead(CreditBeginDate),
         
         PreviousCompleteDate = lag(CompleteDate)) %>%
  ungroup() %>%
  mutate(TimeToPreviousCredit = as.numeric(CreditBeginDate - PreviousCompleteDate),
         
         IsRefinanced = ifelse(as.numeric(NextCreditBeginDate - CompleteDate) <= 3, 1, 0),
         IsRefinanced = ifelse(is.na(IsRefinanced), 0, IsRefinanced),
         
         IsRefinancing3 = ifelse(as.numeric(CreditBeginDate - PreviousCompleteDate) <= 3, 1, 0),
         IsRefinancing3 = ifelse(is.na(IsRefinancing3), 0, IsRefinancing3),
         
         IsRefinancing0 = ifelse(as.numeric(CreditBeginDate - PreviousCompleteDate) <= 0, 1, 0),
         IsRefinancing0 = ifelse(is.na(IsRefinancing0), 0, IsRefinancing0),
         
         IsParallel = ifelse(as.numeric(CreditBeginDate - PreviousCompleteDate) <= -1, 1, 0),
         IsParallel = ifelse(is.na(IsParallel), 0, IsParallel),
         
         ClientType0 = case_when(NumberOfPreviousCredits == 0 ~ 1,
                                 NumberOfPreviousCredits > 0 & IsParallel == 1 ~ 4,
                                 NumberOfPreviousCredits > 0 & IsRefinancing0 == 1 ~ 3,
                                 NumberOfPreviousCredits > 0 ~ 2,
                                 TRUE ~ 999),
         
         ClientType3 = case_when(NumberOfPreviousCredits == 0 ~ 1,
                                 NumberOfPreviousCredits > 0 & IsParallel == 1 ~ 4,
                                 NumberOfPreviousCredits > 0 & IsRefinancing3 == 1 ~ 3,
                                 NumberOfPreviousCredits > 0 ~ 2,
                                 TRUE ~ 999))


AllFiles <- list.files(path = "//hind.smartitbg.int//FileServer//Data Analyses//Analysis//3.Data//CCR Data//Automated CCR Processing", 
                       pattern = "rds", full.names = TRUE)


AllFiles2021 <- AllFiles[grepl("2021",AllFiles)]

CCRS <- lapply(AllFiles2021, readRDS)
CCRDataBase <- bind_rows(CCRS)

ApoData <- LastCreditProgress %>% 
  select(ClientEGN, CreditCode, CreditBeginDate, CompleteDate,
         SumaP, 
         PayedTotal, MaxDelayP,
         ClientType0, ClientType3,
         TimeToPreviousCredit) %>%
  left_join(CCRDataBase %>%
              select(EGN,
                     CDateFormatted,
                     Has_CCR_data,
                     ConsumerLoan_MInst_VOL_B,
                     ConsumerLoan_MInst_VOL_N,
                     Revolving_MInst_VOL_B,
                     Revolving_MInst_VOL_N,
                     Other_MInst_VOL_B,
                     Other_MInst_VOL_N,
                     MonthlyInstallment_VOL_B,
                     MonthlyInstallment_VOL_N),
            by = c("ClientEGN" = "EGN")) %>%
  mutate(TotalInst = MonthlyInstallment_VOL_B + MonthlyInstallment_VOL_N,
         CreditBeginDate = as.Date(CreditBeginDate, origin = "1970-01-01"),
         CompleteDate = as.Date(CompleteDate, origin = "1970-01-01"),
         CDateFormatted = ymd(CDateFormatted),
         TimeToCCRCheck =  as.numeric(CreditBeginDate - CDateFormatted),
         CreditBeginDateMonth = floor_date(CreditBeginDate, "month")) %>%
  filter(!is.na(CDateFormatted),
         TimeToCCRCheck >= 0,
         TimeToCCRCheck <= 3) %>%
  arrange(ClientEGN, CreditCode, CreditBeginDate, desc(TimeToCCRCheck)) %>%
  group_by(CreditCode) %>%
  dplyr::slice_tail() %>%
  ungroup()

table(ApoData$ClientType0)
Vint <- ApoData %>%
  filter(CreditBeginDate >= ymd("2021-01-01"),
         CreditBeginDate <= ymd("2021-12-31"))
table(Vint$ClientType3)

VintWeird <- Vint %>%
  filter(TotalInst >= 3000)
table(VintWeird$ClientType3)


VintWeirdTotal <- VintWeird %>%
  group_by(ClientType3, CreditBeginDateMonth) %>%
  summarise(Count = n(),
            SumaP = sum(SumaP),
            PayedTotal = sum(PayedTotal),
            Profit = PayedTotal - 1.2 * SumaP,
            .groups = "drop") %>%
  arrange(ClientType3, CreditBeginDateMonth)


VintNew <- VintWeird %>%
  filter(ClientType3 == 1) %>%
  group_by(CreditBeginDateMonth) %>%
  summarise(Count = n(),
            SumaP = sum(SumaP),
            PayedTotal = sum(PayedTotal),
            Profit = PayedTotal - 1.2 * SumaP,
            .groups = "drop")

VintReturning <- VintWeird %>%
  filter(ClientType3 == 1) %>%
  group_by(CreditBeginDateMonth) %>%
  summarise(Count = n(),
            SumaP = sum(SumaP),
            PayedTotal = sum(PayedTotal),
            Profit = PayedTotal - 1.2 * SumaP,
            .groups = "drop")

Weird <- ApoData %>%
  filter(TotalInst >= 3000)

sum(Weird$SumaP)
sum(Weird$PayedTotal)



Splitted <- base::split(1:nrow(CCRSFromDatabase), ceiling(1:nrow(CCRSFromDatabase)/1000)) 
CCRSFromDatabase1 <- CCRSFromDatabase %>%
  filter(CDate >= ymd("2020-01-01"))

CCRSFromDatabase1$CCRXmlData[1]
k = 1
person_data_list <- list()
wrangled_data_list <- list()
for (i in Splitted){
  print(k)
  person_data_apo <- ds.ccr::parallelize(CCRSFromDatabase$CCRXmlData[i], ds.ccr::parse_ccr_xml)
  # for (w in i) { 
  #   for (col in colnames(person_data_apo[[w]])) { 
  #     Encoding(person_data_apo[[w]][[col]]) <- "Windows-1251" 
  #   } 
  # }
  person_data_list[[k]] <- person_data_apo
  wrangled_data_apo <- ds.ccr::parallelize(person_data_apo, ds.ccr::wrangle_ccr)
  wrangled_data_list[[k]] <- wrangled_data_apo
  k = k + 1
  z = i
}

write_rds(person_data_list, "./person_data_list.rds")
write_rds(wrangled_data_list, "./wrangled_data_list.rds")

View(person_data_apo[[1]])
for (w in 1:1000) { 
  for (col in colnames(person_data_apo[[w]])) { 
    Encoding(person_data_apo[[w]][[col]]) <- "UTF-16" 
  } 
}

person_data_apo <- ds.ccr::parallelize(CCRSFromDatabase$CCRXmlData, ds.ccr::parse_ccr_xml)
write_rds(person_data_apo, "./person_data_apo.rds")
#person_data_apo <- readRDS("./person_data_apo.rds")

wrangled_data_apo <- ds.ccr::parallelize(person_data_apo, ds.ccr::wrangle_ccr)
write_rds(wrangled_data_apo, "./wrangled_data_apo.rds")
#wrangled_data <- readRDS("./wrangled_data.rds")
