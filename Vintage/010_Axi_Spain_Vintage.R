library(tidyverse)
library(data.table)
library(lubridate)
library(readxl)

source('Functions/F_Elapsed_Months.R')

### --- Load Data --- ### 

# Set date parameters and read automatically all Export 02 files from flat files
DateUntill =  floor_date(Sys.Date(), 'month')
DateBegin = as.Date("2021-01-01")

# Define sequence of dates for the automatic reading 
periodForAnalysis = length(seq(from=DateBegin, to=DateUntill, by='month')) 

DatesList = seq.Date(from = DateUntill, length.out = periodForAnalysis, by = '-1 month')

DirectoriesDF <- data.frame(DirPath = list.dirs('//hind.smartitbg.int/FileServer/Data Analyses/Analysis/3.Data', recursive = FALSE)
                            ,stringsAsFactors =  FALSE) %>% 
  mutate(ReportDate = as.Date(paste(gsub(".*./" ,"",DirPath), '01', sep = '.'), format = '%Y.%m.%d')) %>% 
  arrange(desc(ReportDate)) %>%
  filter(!is.na(ReportDate)
         ,ReportDate %in% DatesList
  ) 

DirectoriesFilesList = do.call(c,lapply(file.path(as.list(DirectoriesDF$DirPath),'White Card ESP'), list.files, full.names = TRUE))

white_card_esp_dirs <- DirectoriesFilesList

### --- Card Progress --- ### 

card_progresses_files <- white_card_esp_dirs[grepl('ES_CardProgress_01', white_card_esp_dirs)] #unlist(lapply(white_card_esp_dirs, list.files, pattern = 'ES_CardProgress_01', full.names = TRUE))
#Card_Progress1 = read.csv2(card_progresses_files[length(card_progresses_files)])

read_cp = function(x) {
  
  cp = read.csv2(x, na.strings = c("NULL", "NA", ""))
  
  cp = cp %>%
    filter(AID != '---') %>% 
    rename(CDate = Exportdate) %>%
    mutate(TotalDue = as.numeric(as.character(TotalDue)),
           DayDelay = as.numeric(as.character(DayDelay)),
           CDate = as.Date(CDate, origin = "1970-01-01"))
  
}

CP_df <- bind_rows(lapply(card_progresses_files, read_cp))

### --- Extra Expenses --- ### 

EE_files <- white_card_esp_dirs[grepl('Export FinanceOperations_01', white_card_esp_dirs)] # unlist(lapply(white_card_esp_dirs, list.files, pattern = 'Export ExtraExpenses_01', full.names = TRUE))
EE_df <- bind_rows(lapply(EE_files, read.csv2))

MonthlyPayments = EE_df %>%
  filter(EasyClientNumber != "----------------"
         & FinanceStensilID == 99
         ) %>%
  mutate(DocumentDate = as.Date(DocumentDate)
         , DocumentMonth = floor_date(DocumentDate, unit = "months")
         , Amount = as.numeric(Amount)
         ) %>%
  group_by(EasyClientNumber, DocumentMonth) %>%
  summarise(Paid = sum(Amount)) %>%
  ungroup()

### --- Export 03 --- ### 

Export_03_files <- white_card_esp_dirs[grepl('export-03', white_card_esp_dirs)] # unlist(lapply(white_card_esp_dirs, list.files, pattern = 'Export ExtraExpenses_01', full.names = TRUE))
Export_03_DF <- bind_rows(lapply(Export_03_files, read.csv2, na.strings = c("NULL")))

### --- Calculate the Vintage --- ### 

Activation_Months = Export_03_DF %>%
  filter(easyclientnumber != "----------------") %>%
  mutate(activationdate = as.Date(activationdate)
         , ActivationMonth = floor_date(activationdate, unit = "months")
         ) %>%
  filter(!is.na(ActivationMonth)) %>%
  group_by(easyclientnumber) %>%
  summarise(ActivationMonth = min(ActivationMonth, na.rm = T)) %>%
  ungroup() 

HelperDatesTable = expand.grid(FirstObsPoint = seq(min(Activation_Months$ActivationMonth, na.rm = T), max(Activation_Months$ActivationMonth, na.rm = T), by="months"),
                               NextObsPoint = seq(min(Activation_Months$ActivationMonth, na.rm = T), max(Activation_Months$ActivationMonth, na.rm = T), by="months")) %>%
  mutate(MOB = elapsed_months(NextObsPoint,FirstObsPoint)) %>%
  filter(MOB >= 0)

Vintage_Table_Long = Activation_Months %>%
  left_join(HelperDatesTable, by = c("ActivationMonth" = "FirstObsPoint")) %>%
  left_join(CP_df %>% select(EasyClientNumber, CDate, CurrentMPV, DayDelay), by = c("easyclientnumber" = "EasyClientNumber"
                          , "NextObsPoint" = "CDate"
                          )
            ) %>%
  mutate(CurrentMPV = as.numeric(CurrentMPV)
         , DayDelay = as.numeric(DayDelay)
         ) %>%
  left_join(MonthlyPayments, by = c("easyclientnumber" = "EasyClientNumber"
                                    , "NextObsPoint" = "DocumentMonth")) %>%
  filter(MOB > 0
         & as.Date(ActivationMonth) >= as.Date("2021-06-01")
         ) %>%
  mutate(CurrentMPV = ifelse(is.na(CurrentMPV),0,CurrentMPV)
         , Paid = ifelse(is.na(Paid),0,Paid)
         , Has_MAD_Due = ifelse(CurrentMPV > 0, 1,0)
         , Paid_MAD = case_when(Has_MAD_Due == 0 ~ 1
                                , Has_MAD_Due == 1 & Paid >= CurrentMPV ~ 1
                                , Has_MAD_Due == 1 & Paid < CurrentMPV ~ 0
                                )
         , Bucket1 = ifelse(DayDelay <=0,1,0)
         )

fwrite(Vintage_Table_Long, "//hind.smartitbg.int/FileServer/Data Analyses/Analysis/1.Projects/110_1 WC All Businesses/15 Vintage Analysis/Output Data/010_Axi_Spain_Vintage_Table_Long.csv")


# Load Daily Payments May #

Daily_Pmts_May = read_xlsx('./Input Data/Axi Spain Paid June 2022.xlsx')

Daily_Pmts_May_Agg = Daily_Pmts_May %>%
  group_by(CID,Date = as.Date(`Date of processing`)) %>%
  summarise(Paid = sum(`Amount in euros`)) %>%
  ungroup() %>%
  mutate(CID = as.character(CID))

HelperDatesTableDaily = expand.grid(FirstObsPoint = seq(as.Date("2022-06-01"), as.Date("2022-06-12"), by="days"),
                               NextObsPoint = seq(as.Date("2022-06-01"), as.Date("2022-06-12"), by="days")) %>%
  mutate(DOB = NextObsPoint - FirstObsPoint) %>%
  filter(DOB >= 0)


Vintage_Table_Long_Daily_May = Activation_Months %>%
  filter(ActivationMonth == as.Date("2022-05-01")) %>%
  mutate(ActivationMonth = as.Date("2022-06-01")) %>%
  left_join(HelperDatesTableDaily, by = c("ActivationMonth" = "FirstObsPoint")) %>%
  left_join(CP_df %>% select(EasyClientNumber, CDate, CurrentMPV, DayDelay), by = c("easyclientnumber" = "EasyClientNumber"
                                                                                    , "ActivationMonth" = "CDate"
  )
  ) %>%
  mutate(CurrentMPV = as.numeric(CurrentMPV)
         , DayDelay = as.numeric(DayDelay)
  ) %>%
  left_join(Daily_Pmts_May_Agg, by = c("easyclientnumber" = "CID"
                                    , "NextObsPoint" = "Date")) %>%
  mutate(Paid = ifelse(is.na(Paid),0,Paid)) %>%
  arrange(easyclientnumber, NextObsPoint) %>%
  group_by(easyclientnumber) %>%
  mutate(CumulativePaid = cumsum(Paid)) %>%
  ungroup() %>%
  mutate(HasDueMAD = ifelse(CurrentMPV > 0,1,0)
         , PaidMAD = ifelse(CumulativePaid >= CurrentMPV,1,0)
         ) %>%
  mutate(ActivationMonth = as.Date("2022-05-01"))


fwrite(Vintage_Table_Long_Daily_May, "//hind.smartitbg.int/FileServer/Data Analyses/Analysis/1.Projects/110_1 WC All Businesses/15 Vintage Analysis/Output Data/010_Axi_Spain_Vintage_Table_Long_Daily_May.csv")










