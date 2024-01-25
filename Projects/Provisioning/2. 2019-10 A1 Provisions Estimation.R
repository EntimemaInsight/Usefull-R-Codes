options(scipen=999)
rm(list = ls())
gc()
library("xlsx")
library("DBI")
library("odbc")
library("tidyverse")
library("lubridate")
library("ggthemes")




source('./Functions/F_FormatPercentage.R')
source('./Functions/F_DateDiffMonths.R')
source('./Functions/F_CreateDateGrid.R')
source('./Functions/F_InterpolateRecoveryRates.R')
source("./Functions/F_Elapsed_Months.R")

Sys.setlocale("LC_TIME", "English")


ReportDate <- as.Date("2019-10-01")
ReportDay = 1





CreditProgress <- readRDS("20191001 20191029 Card Progress Consolidated.rds")
Transactions <- readRDS("20191001 20191029 Transactions.rds")
SoldDebt <- readRDS("20191001 20191029 DebtSales.rds")



# Also Load A1 Clients

####################################################################################
#                                                                                  #
# Целта на процедурата е да извлече таблица с кид-овете и датата на активация      #
# на картите на клиентите на Бяла Карта България по продукт А1                     #
#                                                                                  #
# Процедурата връща всички клиенти по продукт А1 от началото на историята          #
# на проекта към предходния ден спрямо момента, в който е run-ната                 #
#                                                                                  #  
####################################################################################


# Създаваме връзка с базата данни 
myc = DBI::dbConnect(odbc::odbc()
                     , driver = "SQL Server"
                     , server = "hidalgo.smartitbg.int"
                     , database = "BIsmartWCBG")

# Дефинираме заявка, която ще подадем на базата данни, за да извлечем необходимата ни таблица
QueryA1Clients = paste0("select ECN.EasyClientNumber, DimO.OfferSK, ECN.ActivationDate , 1 as A1Client
                from dwh.DimOffers DimO
                left join
                (select OfferSK, min(EasyClientNumber) as EasyClientNumber, min(ActivationDate) as ActivationDate
                from dwh.DimCards
                where ActivationDate is not null 
                group by OfferSK) as ECN
                on ECN.OfferSK = DimO.OfferSK
                where DimO.ContractAccepted = 1 and DimO.DateRejected is null and DimO.ProductSK = 8
                order by ActivationDate;")

# Извличаме данните, като подаваме заявката към базата
WCBG_A1Clients = DBI::dbFetch(DBI::dbSendQuery(myc, QueryA1Clients))

# Затваряме връзката с базата
DBI::dbDisconnect(myc)

# Премахваме ненужните вече променливи
remove(myc,QueryA1Clients)




# Last day in portfolio

# Създаваме връзка с базата данни 
myc = DBI::dbConnect(odbc::odbc()
                     , driver = "SQL Server"
                     , server = "hidalgo.smartitbg.int"
                     , database = "BIsmartWCBG")

# Дефинираме заявка, която ще подадем на базата данни, за да извлечем необходимата ни таблица
QueryLastDay = paste0("SELECT OfferSK,
                        max(CDate) AS LastDateInPortfolio
                        FROM
                        (SELECT OfferSK,
                          CDate
                          FROM dwh.FactCardProgress2017
                          UNION ALL SELECT OfferSK,
                          CDate
                          FROM dwh.FactCardProgress2018
                          UNION ALL SELECT OfferSK,
                          CDate
                          FROM dwh.FactCardProgressCurrent) FCP
                        GROUP BY OfferSK;;")

# Извличаме данните, като подаваме заявката към базата
LastDayInPortfolio = DBI::dbFetch(DBI::dbSendQuery(myc, QueryLastDay))

# Затваряме връзката с базата
DBI::dbDisconnect(myc)

# Премахваме ненужните вече променливи
remove(myc,QueryLastDay)



# Mapping
# Създаваме връзка с базата данни 
myc = DBI::dbConnect(odbc::odbc()
                     , driver = "SQL Server"
                     , server = "hidalgo.smartitbg.int"
                     , database = "BIsmartWCBG")

# Дефинираме заявка, която ще подадем на базата данни, за да извлечем необходимата ни таблица
MappingQuery = paste0("SELECT EasyClientNumber,
                        min(OfferSK) as OfferSK
                        FROM dwh.DimCards
                        GROUP BY EasyClientNumber; ")

# Извличаме данните, като подаваме заявката към базата
Mapping = DBI::dbFetch(DBI::dbSendQuery(myc, MappingQuery))

# Затваряме връзката с базата
DBI::dbDisconnect(myc)

# Премахваме ненужните вече променливи
remove(myc,MappingQuery)



LastDayInPortfolio1 <- LastDayInPortfolio %>%
  left_join(Mapping) %>%
  select(-OfferSK)





# Обработваме данните
WCBG_A1Clients$ActivationDate = as.Date(WCBG_A1Clients$ActivationDate)
A1Clients <-unique(WCBG_A1Clients$EasyClientNumber)



AggregatedTransactions <- Transactions %>%
  filter(EasyClientNumber %in% A1Clients,
         DID == 99,
         DocumentDate < ReportDate) %>% 
  mutate(Validitydate = lubridate::floor_date(DocumentDate, 'month')) %>% 
  group_by(EasyClientNumber, Validitydate) %>%
  summarise(Amount = sum(Amount)
            , DocumentDate = max(DocumentDate)
  ) %>% 
  ungroup() %>%
  mutate(Flag = 'Payment') %>% 
  bind_rows(
    
    SoldDebt %>%
      filter(ReportDate < ReportDate) %>% 
      mutate(Flag = 'DebtSale') %>% 
      mutate(Validitydate = lubridate::floor_date(ReportDate, 'month')) %>%
      select(EasyClientNumber,Validitydate, Amount = SalesPrice, Flag) 
  )


DefaultDate <- Transactions %>% 
  filter(EasyClientNumber %in% A1Clients,
         DID == 800
         ,DocumentDate < as.Date(ReportDate)) %>% 
  group_by(EasyClientNumber) %>%
  summarise(DefaultDate = min(DocumentDate, na.rm = TRUE)) %>% 
  ungroup()
# arrange(Validitydate)  %>% 
# filter(row_number() == 1) %>% 
# ungroup() %>%
# select(EasyClientNumber, DefaultDate = DocumentDate) %>% 
# distinct()

##


Last_payment_prior_obs_date <- CreditProgress %>%
  # mutate(Exportdate = as.Date(Exportdate)) %>% 
  filter(EasyClientNumber %in% A1Clients,
         as_date(Exportdate) <= as_date(ReportDate))   %>% 
  select(EasyClientNumber, Exportdate)     %>% 
  
  left_join(filter(AggregatedTransactions, Flag == 'Payment') ## Exclude Debt sale !
            , by  = c('EasyClientNumber' = 'EasyClientNumber')) %>% 
  
  # mutate(Datediff   = Exportdate - DocumentDate) %>% 
  filter(Exportdate > DocumentDate) %>%
  # filter(Datediff > 0) %>% 
  group_by(EasyClientNumber, Exportdate) %>% 
  summarise(LastPayDate = max(DocumentDate)) %>% 
  filter(!is.na(LastPayDate))

#####################################
### Create Transition Matrix Data ### 
#####################################


MatrixData <- CreditProgress %>%
  # mutate(Exportdate = as.Date(Exportdate)) %>% 
  filter(EasyClientNumber %in% A1Clients,
         Exportdate <= as.Date(ReportDate)) %>% 
  left_join(DefaultDate, by = 'EasyClientNumber') %>% 
  left_join(Last_payment_prior_obs_date, by = c('EasyClientNumber' = 'EasyClientNumber', 'Exportdate' = 'Exportdate')) %>%
  mutate(Exportdate = as_date(Exportdate)) %>% 
  #### Debt Sale Data ####

left_join(
  SoldDebt %>%
    #filter(ReportDate < as.Date(ReportDate)) %>% # no need for this line
    mutate(Validitydate = lubridate::floor_date(ReportDate, 'month')
           , SoldDebt = 1) %>%
    select(EasyClientNumber,Validitydate,SoldDebt) 
  ,by  = c('EasyClientNumber' = 'EasyClientNumber', 'Exportdate'='Validitydate')) %>% 
  ### Create Days unpaid after default
  mutate(Delinq = case_when(  !is.na(.$DefaultDate) & !is.na(.$LastPayDate) &   .$LastPayDate <  .$DefaultDate ~  .$Exportdate - .$DefaultDate - 1
                              , !is.na(.$DefaultDate) & !is.na(.$LastPayDate) &   .$LastPayDate >= .$DefaultDate ~  .$Exportdate - .$LastPayDate - 1
                              , !is.na(.$DefaultDate) &  is.na(.$LastPayDate)                                    ~  .$Exportdate - .$DefaultDate - 1
                              , TRUE ~ 0)) %>% 
  #### Create buckets
  mutate(DelinquencyGrps = case_when( !is.na(.$DefaultDate) & .$DefaultDate < .$Exportdate & .$Delinq <=  30 ~ 3
                                      ,!is.na(.$DefaultDate) & .$DefaultDate < .$Exportdate & .$Delinq <=  60 ~ 4#5
                                      ,!is.na(.$DefaultDate) & .$DefaultDate < .$Exportdate & .$Delinq <=  90 ~ 5#6
                                      ,!is.na(.$DefaultDate) & .$DefaultDate < .$Exportdate & .$Delinq <= 180 ~ 6#7
                                      ,!is.na(.$DefaultDate) & .$DefaultDate < .$Exportdate & .$Delinq >  180 ~ 7#8
                                      ,(is.na(.$DefaultDate) | .$DefaultDate > .$Exportdate) & .$DayDelay <=   3 ~ 1
                                      ,(is.na(.$DefaultDate) | .$DefaultDate > .$Exportdate) & .$DayDelay >    3 ~ 2
                                      , TRUE ~ 99)) %>% ## 99 - data errors 
  select(Exportdate, CardID, EasyClientNumber, TotalDue, 
         DayDelay, DefaultDate, LastPayDate, 
         Delinq, DelinquencyGrps, SoldDebt)


# TM is per CardID
# Both Add Transitions and
# Add 3 and 12 months probability for Default (~ Transaction DID = 800)

Transitions  <- MatrixData %>%
  group_by(EasyClientNumber) %>%
  arrange(Exportdate) %>%
  
  mutate(Transition_date  = lead(Exportdate,1)
         ,Transition = lead(DelinquencyGrps,1)
  ) %>%
  ungroup() %>%
  mutate( TransitionTo   = case_when(  is.na(.$Transition_date) &  is.na(.$SoldDebt) ~  DelinquencyGrps
                                       , is.na(.$Transition_date) & !is.na(.$SoldDebt) ~  9
                                       ,!is.na(.$Transition) ~ .$Transition
                                       ,  TRUE ~ 99))

 


TidyTransitions <- Transitions %>%
  filter(Exportdate!=ReportDate,
         TransitionTo!=99) %>%
  select(Exportdate, EasyClientNumber, TotalDue, DelinquencyGrps, TransitionTo) %>%
  rename(TransitionFrom=DelinquencyGrps) %>%
  mutate(TransitionFrom = case_when(
      TransitionFrom == 1 ~ "0 DPD",
      TransitionFrom == 2 ~ "1+ DPD",
      TransitionFrom == 3 ~ "M: [0;30]",
      TransitionFrom == 4 ~ "M: [31;60]",
    TransitionFrom ==  5 ~ "M: [61;90]",
    TransitionFrom ==  6 ~ "M: [91;180]",
    TransitionFrom == 7 ~ "M: >= 181",
    TransitionFrom ==  9 ~ "Sold"),
    TransitionTo = case_when(
      TransitionTo == 1 ~ "0 DPD",
      TransitionTo == 2 ~ "1+ DPD",
      TransitionTo == 3 ~ "M: [0;30]",
      TransitionTo == 4 ~ "M: [31;60]",
      TransitionTo ==  5 ~ "M: [61;90]",
      TransitionTo ==  6 ~ "M: [91;180]",
      TransitionTo == 7 ~ "M: >= 181",
      TransitionTo ==  9 ~ "Sold")
  )

table(TidyTransitions$TransitionFrom)
table(TidyTransitions$TransitionTo)


# 
# '%!in%' <- function(x,y)!('%in%'(x,y))
# 
# WC_clients_only_info <- as.data.frame(MatrixData) %>% filter(EasyClientNumber %!in% A1Clients)
#A1_clients_export_info <- as.data.frame(MatrixData) %>% filter(EasyClientNumber %in% A1Clients)

DPDGroups <- c('0 DPD', '1+ DPD', 'M: <= 30', 'M: [31;60]', 'M: [61;90]','M: [91;180]', 'M: >= 181' , 'Total:', 'Error!:')


PortfolioAmountA1 <- MatrixData %>% 
  group_by(Exportdate, DelinquencyGrps) %>% 
  summarise(Amount = sum(TotalDue), Accounts = n())%>%
  mutate(Date = as.Date(Exportdate)) %>% 
  mutate(DelinquencyGrps = factor(DelinquencyGrps
                                  , levels = c(1,2,3,4,5,6,7,8,99), labels = DPDGroups)) %>% 
  ungroup() %>% select(-Exportdate)

PortfolioA1 <- PortfolioAmountA1 %>%
  select(DelinquencyGrps, Date, Amount) %>%
  spread(Date, Amount)


# 
# aa<-CreditProgress %>%
#        filter(EasyClientNumber %in% A1Clients,
#               Exportdate<=as.Date("2018-09-01"))
# 
# 
# bb<-Transactions %>%
#   filter(EasyClientNumber %in% A1Clients,
#          DocumentDate<=as.Date("2018-09-01"))
# 
# sum(aa$TotalDue)




monthsback=1

GetRollRateCountPerc <- function(arg1, monthsback=1) {
  
  arg1 <- ymd(arg1)
  
  # Filter the data for the respective period
  temp <- TidyTransitions %>% 
    filter(Exportdate <= arg1,
           Exportdate > arg1-months(monthsback)) %>% 
    select(Exportdate, EasyClientNumber, TotalDue, TransitionFrom, TransitionTo) 
  
  # Calculate month-to-month percentage based on number of credits (count) transitioning from one bucket to another
  y <- temp %>%
    group_by(TransitionFrom) %>%
    mutate(TotalCount=n())%>%
    group_by(TransitionFrom, TransitionTo) %>%
    summarise(TotalCount = mean(TotalCount),
              Count = n()) %>%
    mutate(Percentage = Count/TotalCount) %>%
    select(TransitionFrom, TransitionTo, Percentage) %>%
    spread(TransitionTo, Percentage, fill = 0) %>%
    ungroup
  
   y <- rbind(y, c('Sold', rep(0, times=as.numeric(ncol(y)-2)), 1))

      y[,-1] <- sapply(y[,-1], as.numeric)

   return(y)
}

date_list <- seq.Date(from = as.Date("2018-09-01"), to = as.Date("2019-09-01"), by = "month")

RollRateList <- list()
RollRateList <- lapply(unique(date_list),GetRollRateCountPerc)
names(RollRateList) <- unique(date_list)[1:length(unique(date_list))]

calcrowsums<-function(x){apply(x[,-1],1,sum)}
lapply(RollRateList, calcrowsums)



# Estimating future roll rate matrices
# First month for estimation
startEstimationDate <- as.Date(tail(names(RollRateList),1))+months(1)
endEstimationDate <- as.Date("2021-01-01")
periodEstimation <- seq.Date(from = startEstimationDate, endEstimationDate, by = "month")
lengthEstimation <- elapsed_months(endEstimationDate, startEstimationDate)+1
lengthHistData <- length(RollRateList)

DPDGroupDF <- data.frame("DPDGroup"=c("0 DPD","1+ DPD","M: [0;30]", "M: [31;60]", "M: [61;90]", "M: [91;180]", "M: >= 181", "Sold"))



EstimatedRollRateList <- RollRateList

# perc_cession_better<-0.01

for (i in (lengthHistData+1):(lengthHistData+lengthEstimation)){
  EstimatedRollRateList[[i]]<-(EstimatedRollRateList[[i-1]][,-1] +
                                 EstimatedRollRateList[[i-2]][,-1] +
                                 EstimatedRollRateList[[i-3]][,-1]) / 3
  # EstimatedRollRateList[[i-4]][,-1] +
  # EstimatedRollRateList[[i-5]][,-1] +
  # EstimatedRollRateList[[i-6]][,-1]) / 6
  
  EstimatedRollRateList[[i]] <- as_tibble(EstimatedRollRateList[[i]]) %>%
    bind_cols(DPDGroupDF) %>%
    select(DPDGroup, everything())
  
  
  # EstimatedRollRateList[[i]][6,8]<-EstimatedRollRateList[[i]][6,8]+perc_cession_better
  # EstimatedRollRateList[[i]][6,9]<-EstimatedRollRateList[[i]][6,9]-perc_cession_better
  # 
  # 
  # EstimatedRollRateList[[i]][7,8]<-EstimatedRollRateList[[i]][7,8]+perc_cession_better
  # EstimatedRollRateList[[i]][7,9]<-EstimatedRollRateList[[i]][7,9]-perc_cession_better
  
  names(EstimatedRollRateList[[i]])<-names(EstimatedRollRateList[[i-1]])
  names(EstimatedRollRateList)[i]<-as.character(paste0('Estimation ',as.Date(periodEstimation[i-lengthHistData])))
  
}

# Check validity of the roll rate matrices - all row sums should be equal to 1
calcrowsums<-function(x){apply(x[,-1],1,sum)}
lapply(EstimatedRollRateList, calcrowsums)



##########################
#
# Get Roll Rate Matrices
#
##########################


InitialPortfolio <- read.csv2(file = "Initial Portfolio A1.csv", header = T, stringsAsFactors = F) %>%
  mutate(TotalDue = as.numeric(gsub(" ", "", TotalDue, fixed = TRUE)),
         Observations = as.numeric(gsub(" ", "", Observations, fixed = TRUE))) %>%
  select(-Observations) %>%
  rename(`2019-10-01` = TotalDue)
InitialPortfolio <- t(as.matrix(InitialPortfolio[, -1]))
#xxx <- InitialPortfolio
#InitialPortfolio[1] <- InitialPortfolio[1] + PlannedGrowth[,1]


#Forecast <- read.csv2(file = "Forecast.csv", header = T, row.names = 1, stringsAsFactors = F)
library(readxl)
AssumptionsBudget <- as.data.frame(read_excel("./Assumptions from Budget A1 Oct 2019.xlsx", sheet = "From Budget", col_names = TRUE))[,-1]
colnames(AssumptionsBudget) <- c(seq.Date(from = as.Date("2019-11-01"), to = as.Date("2021-01-01"), by = "month"))



PlannedPortfolio <- AssumptionsBudget[1,]
PlannedSales <- AssumptionsBudget[2,]
Accruals <- AssumptionsBudget[3,]
GrantedCredits_Principal <- AssumptionsBudget[4,]
Paid <- AssumptionsBudget[5,]

# Check Budgeting Assumptions - SHOULD BE EQUAL TO ZERO
round(lag(PlannedPortfolio)+(Accruals-PlannedSales-Paid+GrantedCredits_Principal)-PlannedPortfolio,2)


###############################################
#
# Calculate Protfolios Distribution For 2020
#
###############################################

InitialPortfolio_Sold<-c(InitialPortfolio,0)

PortfoliosList <- list()

PortfoliosList[[1]] <- InitialPortfolio_Sold %*% as.matrix(EstimatedRollRateList[[lengthHistData+1]][,-1])
PortfoliosList[[1]][1, 1] <- PortfoliosList[[1]][1, 1] + Accruals[, 1]
PortfoliosList[[1]][1, 1] <- PortfoliosList[[1]][1, 1] - Paid[, 1]
PortfoliosList[[1]][1, 1] <- PortfoliosList[[1]][1, 1] + GrantedCredits_Principal[, 1]
#PortfoliosList[[1]][1, 7] <- PortfoliosList[[1]][1, 7] + PlannedSales[, 1] 




# 
# for (i in 2:15){
#   PortfoliosList[[i]] <- PortfoliosList[[i-1]] %*% RollRate
#   PortfoliosList[[i]][1,1] <- PortfoliosList[[i]][1,1] + PlannedGrowth[, i] #New Production
#   PortfoliosList[[i]][1,1] <- PortfoliosList[[i]][1,1] - PlannedClosed[, i] #Closed accounts
#   PortfoliosList[[i]][1,7] <- PortfoliosList[[i]][1,7] - PlannedSales[, i] #Sold accounts
# }



for (i in 2:15){
  
  # Multiply last month's portfolio by estimated roll rate
  PortfoliosList[[i]] <- PortfoliosList[[i-1]] %*% as.matrix(EstimatedRollRateList[[lengthHistData+i]][,-1])
  
  # Add accruals, paid nad granted credits to the first bucket
  PortfoliosList[[i]][1, 1] <- PortfoliosList[[i]][1, 1] + Accruals[, i]
  PortfoliosList[[i]][1, 1] <- PortfoliosList[[i]][1, 1] - Paid[, i]
  PortfoliosList[[i]][1, 1] <- PortfoliosList[[i]][1, 1] + GrantedCredits_Principal[, i]
  # PortfoliosList[[i]][1, 6] <- PortfoliosList[[i]][1, 6] * 0.55
  # PortfoliosList[[i]][1, 7] <- PortfoliosList[[i]][1, 7] * 0.7
  
  
  # # Subtract Cessions (Sold) ammount from last bucket
  # # In case the amount of the cession is more than the amount in the last bucket, subtract the difference from the previous one (see while loop below)
  # SalesForMonth <- -PlannedSales[, i]
  # k=0
  # 
  # while (SalesForMonth > 0){
  #   SoldFromBucket <- PortfoliosList[[i]][1,(7-k)]
  #   
  #   # If the sales amount is more than the amount of the current bucket, assign 0 as amount for the bucket
  #   PortfoliosList[[i]][1,(7-k)] <- ifelse((PortfoliosList[[i]][1,(7-k)] - SalesForMonth)<0,0,(PortfoliosList[[i]][1,(7-k)] - SalesForMonth)) #Sold accounts
  #   
  #   print(paste0("Ajdustment done for month ", i,", for bucket ", 7-k ))
  #   
  #   # If the sales amount is more than the amount of the current bucket, calculate the difference and repeat with the next lower bucket
  #   SalesForMonth <- SalesForMonth - SoldFromBucket
  #   k=k+1
}



names(PortfoliosList)<-c(seq.Date(from = as.Date("2019-11-01"), to = as.Date("2021-01-01"), by = "month"))
PortfoliosDF<-bind_rows(PortfoliosList)
DPDGroupDF <- data.frame("DPDGroup"=c("0 DPD","1+ DPD","M: [0;30]", "M: [31;60]", "M: [61;90]", "M: [91;180]", "M: >= 181", "Sold"))
PortfoliosDF <- PortfoliosDF %>%
  bind_cols(DPDGroupDF) %>%
  select(DPDGroup, everything())

View(PortfoliosDF)

#write.csv(EstimatedRollRateList, "./roll.csv")
library(openxlsx)

write.xlsx(EstimatedRollRateList, "./A1 roll.xlsx", asTable = F)

