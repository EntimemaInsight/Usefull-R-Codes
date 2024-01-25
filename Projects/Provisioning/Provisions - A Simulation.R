# Simulating a portfolio consisting of two loans issued per day - one good and one bad one
library(tidyverse)
library(lubridate)
library(magrittr)
require(expm)


#library(future.apply)

beginDate <- as.Date("2019-01-01")
monthsForAnalysis <- 6
monthsForPrediction <- 12

simulate_loans<- function(x) {
      # Scenario 1 - paid loan
      dates <- seq.Date(from = beginDate+x, to = beginDate+x + 30, by = 'day')
      outstanding <- c(rep(100, length(dates)-1), 0)
      delay <- rep(0, length(dates))
      names <- rep(paste0("good_loan_", x), length(dates))
      sold_or_paid <- c(rep(NA, length(dates)-1), "Paid") ############
      
     #data.frame(loan_id = paste0("good_credit_", x), dates = dates, outstanding = outstanding, delay = delay)
      
      # Scenario 2 - delayed loan
      dates_del <- seq.Date(from = beginDate+x, to = beginDate+x + 390, by = 'day')
      outstanding_del <- c(rep(100, length(dates_del)-1), 0)
      delay_del <- c(rep(0, 30), seq(from = 1, to = 360, by = 1), 0)
      names_del <- rep(paste0("bad_loan_", x), length(dates_del))
      sold_or_paid_del <- c(rep(NA, length(dates_del)-1), "Sold") ###########
      
      loan_id = c(names, names_del) 
      dates = c(dates, dates_del)
      outstanding = c(outstanding, outstanding_del)
      delay = c(delay, delay_del)
      sold_or_paid = c(sold_or_paid, sold_or_paid_del)
      
      df <- data.frame(loan_id = loan_id, 
                             dates = dates,
                             outstanding = outstanding,
                             delay = delay,
                              sold_or_paid = sold_or_paid)
      
      
      return(df)
      
}

number_of_simulated_loans <- seq(from = 0, to = 360*4, by = 1)

# Simulate loans
#plan(multiprocess)
simulated_list <- lapply(number_of_simulated_loans, simulate_loans)
simulatedDF <- bind_rows(simulated_list)

Portfolio_per_day <- simulatedDF %>%
  group_by(dates) %>%
  summarise(outstanding = sum(outstanding))

plot(Portfolio_per_day, type = 'l')


Portfolios <- simulatedDF %>%
  mutate(DPDGroup = case_when(
    sold_or_paid == "Paid" ~ "Paid",
    sold_or_paid == "Sold" ~ "Sold",
    delay <=3 ~ "low - 3",
    delay <= 30 ~ "4-30",
    delay <= 60 ~ "31-60",
    delay <= 90 ~ "61-90",
    delay <= 120 ~ "91-120",
    delay <= 150 ~ "121-150",
    delay <= 180 ~ "151-180",
    delay <= 210 ~ "181-210",
    delay <= 240 ~ "211-240",
    delay <= 270 ~ "241-270",
    delay <= 300 ~ "271-300",
    delay <= 330 ~ "301-330",
    delay <= 360 ~ "331-360",
    delay > 360 ~ "361-high"
    )) 


DPDGroupLevels <- c("Paid", "low - 3", "4-30", "31-60", "61-90",
                    "91-120", "121-150", "151-180", "181-210",
                    "211-240", "241-270", "271-300", "301-330",
                    "331-360", "Sold")

Portfolios <- Portfolios %>% mutate(DPDGroup = fct_relevel(DPDGroup, DPDGroupLevels))

Portfolios <- Portfolios %>%
  group_by(loan_id) %>%
  mutate(dates1 = case_when(
    dates == max(dates) ~ as.Date(ceiling_date(dates, 'month')) - days(1)),
    dates = as.Date(ifelse(!is.na(dates1), dates1, dates), origin = "1970-01-01")) %>%
  select(-dates1)




reporting_dates <- seq.Date(from = floor_date(as.Date('2020-01-01'), 'month'),
                            to = floor_date(as.Date('2021-12-01'), 'month'),
                            by = 'month') + months(1) - days(1)

Portfolios_Clean <- Portfolios %>%
  filter(dates %in% reporting_dates) %>%
  select(loan_id, DPDGroup, dates, outstanding) %>%
  group_by(dates, DPDGroup) %>%
  # summarise(outstanding = sum(outstanding, na.rm = T)) %>%
  # spread(dates, outstanding)
  summarise(number_of_credits = n())

Summary <- Portfolios %>%
  filter(dates %in% reporting_dates) %>%
  select(loan_id, DPDGroup, dates, outstanding) %>%
  group_by(dates, DPDGroup) %>%
  # summarise(outstanding = sum(outstanding, na.rm = T)) %>%
  # spread(dates, outstanding)
  summarise(number_of_credits = n()) %>%
  spread(dates, number_of_credits)

Summary_amount <- Portfolios %>%
  filter(dates %in% reporting_dates) %>%
  select(loan_id, DPDGroup, dates, outstanding) %>%
  group_by(dates, DPDGroup) %>%
  summarise(outstanding = sum(outstanding, na.rm = T)) %>%
  spread(dates, outstanding)
  # summarise(number_of_credits = n()) %>%
  # spread(dates, number_of_credits)

Plot <- Portfolios_Clean %>%
  filter(!DPDGroup %in% c("Paid", "Sold")) 

library(ggplot2)

ggplot(Plot, aes(x = dates, y = number_of_credits, fill = DPDGroup)) +
  geom_area(alpha = 0.6, size = 1, col = 'black')

# Base Portfolios
BasePortfolios <- Portfolios %>%
  filter(dates %in% reporting_dates) %>%
  arrange(loan_id, dates) %>%
  group_by(loan_id) %>%
  mutate(TransitionFrom = as.character(DPDGroup),
         TransitionTo = as.character(lead(TransitionFrom, order_by = dates))) %>%
  filter(TransitionFrom != "Paid")

PeriodsForAnalysis <- tibble(reporting_dates) %>%
  rename(EndDate = reporting_dates) %>%
  mutate(StartDate = floor_date(EndDate, unit="month")-months(monthsForAnalysis-1)-days(1)) %>%
  filter(StartDate>"2020-01-01",
         EndDate <= "2021-12-31") %>%
  select(StartDate, EndDate) %>%
  arrange(StartDate)

# Introducing function to filling in missing columns in a dataframe with a specific set of columns
fill_cols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  if(length(add)!=0) data[add] <- NA
  data
}

TotalCoefficients <- tibble(DPDGroupLevels) %>%
  filter(DPDGroupLevels != "Paid")

names(TotalCoefficients) <- "DPDGroup"




for (k in 1:nrow(PeriodsForAnalysis)) {
  
  #########################################################################
  #
  # Analyzing transition between buckets 
  #
  #########################################################################
  
  TempPortfolios <- BasePortfolios %>%
    filter(dates >= as.Date(PeriodsForAnalysis$StartDate)[k],
           dates < as.Date(PeriodsForAnalysis$EndDate)[k])
  
  
  DF_M1 <- TempPortfolios %>%
    group_by(TransitionFrom) %>%
    mutate(RowSumm = n()) %>%
    group_by(TransitionFrom, TransitionTo) %>%
    summarise(RowSumm = mean(RowSumm)
              , Trans = n()
    )%>%
    mutate(RR = Trans / RowSumm) %>%
    select(TransitionFrom, TransitionTo, RR) %>%
    spread(TransitionTo, RR, fill = 0)  %>%
    ungroup()
  

  
  DF_M1 <- DF_M1 %>% 
    # Adding the missing columns in case there are no observations in each group to transfer to
    fill_cols(c("TransitionFrom", DPDGroupLevels)) %>%
    # Next, adding the missing rows in a similar manner, by transposing the matrix 
    # Transpose
    t() %>%
    
    # Set columnames to the transposed matrix
    set_colnames(.[1,]) %>%
    .[-1,] %>%
    
    # And add the missing rows (now columns in the transposed matrix) with the same function 
    as.data.frame()%>%
    fill_cols(c(DPDGroupLevels)) %>%
    
    # Transpose back to normal state
    t() %>%
    as.data.frame()%>%
    rownames_to_column("TransitionFrom") %>%
    
    # Arrange all columns
    select(c("TransitionFrom", DPDGroupLevels)) %>%
    
    # Dealing with classes - convert all columns to numeric (through character)
    mutate_at(DPDGroupLevels, as.character) %>%
    mutate_at(DPDGroupLevels, as.numeric) %>%
    
    # Convert the rownames to factor
    mutate(TransitionFrom = fct_relevel(TransitionFrom, DPDGroupLevels)) %>%
    
    # Arrange all rows
    arrange(TransitionFrom)
  
  
  # if (!file.exists(file = "./Result/TransitionMatrix_Apr-Aug19.csv")) {
  #   write_csv(DF_M1,"./Result/TransitionMatrix_Apr-Aug19.csv")
  # } else {
  #   print("The file was not saved, as it already exists in the wd")}
  # 
  
  #########################################################################
  #
  # Markov Chain - assumptions for future transition coefficients
  #
  #########################################################################
  
  M1 <- as.matrix(sapply(select(DF_M1, -TransitionFrom), as.numeric))
  ## Transform M1 to square
  #M2 <- rbind(c(1, rep(0, ncol(M1)-1)), M1)
  M2 <- M1
  M2[1,]<-c(1, rep(0, ncol(M1)-1))
  M2[nrow(M2),]<-c(rep(0, ncol(M1)-1),1)
  
  
  DiagM <-diag(15)
  
  for(i in 1:(nrow(M2)-1)) {
    for(j in 1:ncol(M2)) {
      
      DiagM[i,j] <- M2[i,j]
      
    }
  }
  
  # Creating a matrix with coefficients forecast after six periods
  M2_Pow6 <- round(DiagM %^% monthsForPrediction, 2)
  
  # Provisional coefficients per bucket
  ProvisionalCoefficients<-tibble(DPDGroupLevels)
  names(ProvisionalCoefficients)<-"DPDGroup"
  ProvisionalCoefficients$Coefficients<-M2_Pow6[,ncol(M2_Pow6)]
  
  # Taking into consideration assumption for the cession price in Ukraine
  #ProvisionalCoefficients$Coefficients[1:(nrow(ProvisionalCoefficients)-1)]<-ProvisionalCoefficients$Coefficients[1:(nrow(ProvisionalCoefficients)-1)]*(1-CessionPrice)
  
  ProvisionalCoefficients$Coefficients<-round(ProvisionalCoefficients$Coefficients, 4)
  
  names(ProvisionalCoefficients$Coefficients) <- as.character(as.Date(PeriodsForAnalysis$StartDate)[k])
  
  
  # if (!file.exists(file = "./Result/ProvisionalCoefs.csv")) {
  #   write_csv(ProvisionalCoefficients,"./Result/ProvisionalCoefs.csv")
  # } else {
  #   print("The file was not saved, as it already exists in the wd")}
  
  
  TotalCoefficients <- TotalCoefficients %>%
    left_join(ProvisionalCoefficients , by="DPDGroup")
  
}


names(TotalCoefficients) <- c("DPDGroup", as.character(PeriodsForAnalysis$EndDate))

TotalCoefficients
