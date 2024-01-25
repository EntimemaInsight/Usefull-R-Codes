
library(readr)
library(data.table)
library(dplyr)
library(kohonen)
library(lubridate)

## Use the last 6 months 
DateOfTheFile <- gsub("-", "",  floor_date(Sys.Date(), 'month'))
#Previous_month <- floor_date(Sys.Date(), "month") -months(1)
DateOfTheFolder <- substr(gsub("-", ".", floor_date(Sys.Date(), "month") - months(1)), start = 1,stop = 7 )

### Set the working paths for Ukraine #### 

Credit_progress_latest <- paste0('//10.254.1.4\\Files\\Analysis\\3.Data/' , DateOfTheFolder,
                                 '/Easy Credit UKR/E004_Credit_Progress_UA_',DateOfTheFile ,'_v1.csv')

Harold_latest <- paste0('//10.254.1.4\\Files\\Analysis\\3.Data/', DateOfTheFolder,'/Easy Credit UKR/E006_Harold_UA_',DateOfTheFile ,'_v1.csv')  

##### 02 Reading Data ####
CreProg_Latest_UKR<- read.csv(file = Credit_progress_latest, head=T, sep = ";", stringsAsFactors = F, na.strings = c("", " ", "NULL"), encoding = 'UTF-8', 
                              quote = "")

Harold_Latest_UKR <- read.csv(file = Harold_latest, head = T, sep = ";", stringsAsFactors = F, na.strings = c("", " ", "NULL"), 
                              encoding = "UTF-8", quote = "")

#### 02.1. Rename some columns ####
setnames(CreProg_Latest_UKR, old = "CreditCode", new = "KID")
colnames(Harold_Latest_UKR)[1] <- "KID"

###Select necessary columns from Credit Progress
CreProg_Latest_UKR_1 <- CreProg_Latest_UKR %>% 
  select (KID, CreditBeginDate, CreditProduct, 
          SumToGetBack, PayedTotal, MaturityT, Weeks, CurrentDelay, MaxDelayP,   
          CompleteDate, IsPrePaid, SumaP) %>% 
  mutate (Delay = case_when(is.na(CompleteDate) ~ CurrentDelay, 
                            TRUE ~ MaxDelayP)) %>%
  mutate (CompleteDate = as.Date(CompleteDate), 
          CreditBeginDate = as.Date(CreditBeginDate)) %>% 
  mutate (Year = year(CreditBeginDate), 
          Month = month(CreditBeginDate), 
          Period = year(CreditBeginDate)*100 +  month(CreditBeginDate))



#### Checking rules

###Select necessary columns from Harold

Harold_Latest_UKR_1 <- Harold_Latest_UKR %>% 
  
  select(KID, TypeOfContract , MoneyGrantedDateByCashDesk, Scoring, ScoringType, ClientEGN, RefinansNew) %>%
  
  left_join(CreProg_Latest_UKR_1, by = "KID") %>%
  arrange(ClientEGN, MoneyGrantedDateByCashDesk)%>%
  group_by(ClientEGN)%>%
  mutate(CreditNumber = seq(n()), 
         Fist_Credit = case_when(CreditNumber == 1 ~ 1, 
                                 TRUE ~ 2), 
         #LastCreditCompleteDate = lag(CompleteDate),
         PreviousDelay = lag(Delay),
         LastP = lag(SumaP))%>% 
  mutate(DelayGroup = case_when(PreviousDelay< 31 ~ "0-30 Days", 
                                PreviousDelay < 61 & PreviousDelay > 30 ~ "31-60 Days", 
                                PreviousDelay < 91 & PreviousDelay >60 ~ "61-90 Days", 
                                PreviousDelay >90 ~"Above 90"))

