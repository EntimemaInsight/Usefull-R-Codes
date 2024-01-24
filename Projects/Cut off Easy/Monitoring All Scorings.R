# Monitoring Scoring 101/ default flag 90 days

#Creating a list of dates
dates_list <-as.Date(c("2018-06-01", "2018-09-01", "2018-12-01", "2019-03-01", "2019-06-01", "2019-09-01", "2019-12-01"))

monitoring101_90days_func <- function(arg1){
  start_date = arg1
  end_date = start_date %m+% months(2)
  
  Final <- Final %>%
    filter(CreditBeginDate >= start_date,
          CreditBeginDate <= end_date,
          ScoringType == 101)

  stat <- Final %>%
    mutate(group = cut2(Scoring, m = nrow(Final) / 5))%>%
    group_by(group) %>%
    summarize(count=n(),
            def = sum(default90, na.rm = T),
            PerDef = def/count,
            DefaultPerLower = PerDef - Z*sqrt(PerDef*(1-PerDef)/count),
            DefaultPerUpper = PerDef + Z*sqrt(PerDef*(1-PerDef)/count))

 check<- Final%>%
   group_by(MonthsDefault90) %>%
   summarise(Defaults = n()) %>%
   ungroup() %>%
   mutate(TotalObservations = sum(Defaults),
         DefaultRate = Defaults / TotalObservations)


 iv <- iv(Final[ , c("Scoring", "default90")], y = 'default90')
 bins <- woebin(Final[ , c("Scoring", "default90")], y = 'default90')

 #Calculating AUC
 pred_ROCR <- ROCR::prediction(Final$Scoring, Final$default90)
 auc_ROCR <- performance(pred_ROCR, measure = "auc")
 auc_ROCR <- auc_ROCR@y.values[[1]]

 #Calculating KS
 library(InformationValue)
 KS = ks_stat(actuals=Final$default90, predictedScores=Final$Scoring) 
 
 return(list(stat = stat, check = check, auc_ROCR = auc_ROCR, KS = KS))

}

results_list_90days <- lapply(dates_list, monitoring101_90days_func)

# Monitoring Scoring 101/ default flag 60 days

monitoring101_60days_func <- function(arg1){
  start_date = arg1
  end_date = start_date %m+% months(2)
  
  Final <- Final %>%
    filter(CreditBeginDate >= start_date,
           CreditBeginDate <= end_date,
           ScoringType == 101)
  
  stat <- Final %>%
    mutate(group = cut2(Scoring, m = nrow(Final) / 5))%>%
    group_by(group) %>%
    summarize(count=n(),
              def = sum(default60, na.rm = T),
              PerDef = def/count,
              DefaultPerLower = PerDef - Z*sqrt(PerDef*(1-PerDef)/count),
              DefaultPerUpper = PerDef + Z*sqrt(PerDef*(1-PerDef)/count))
  
  check<- Final%>%
    group_by(MonthsDefault60) %>%
    summarise(Defaults = n()) %>%
    ungroup() %>%
    mutate(TotalObservations = sum(Defaults),
           DefaultRate = Defaults / TotalObservations)
  
  
  iv <- iv(Final[ , c("Scoring", "default60")], y = 'default60')
  bins <- woebin(Final[ , c("Scoring", "default60")], y = 'default60')
  
  #Calculating AUC
  pred_ROCR <- ROCR::prediction(Final$Scoring, Final$default60)
  auc_ROCR <- performance(pred_ROCR, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  
  #Calculating KS
  library(InformationValue)
  KS = ks_stat(actuals=Final$default60, predictedScores=Final$Scoring) 
  
  return(list(stat = stat, check = check, auc_ROCR = auc_ROCR, KS = KS))
  
}

results_list101_60days <- lapply(dates_list, monitoring101_60days_func)

#Monitoring scoring 3/ 90 days default flag

monitoring3_90days_func <- function(arg1){
  start_date = arg1
  end_date = start_date %m+% months(2)
  
  Final <- Final %>%
    filter(CreditBeginDate >= start_date,
           CreditBeginDate <= end_date,
           ScoringType == 3)
  
  stat <- Final %>%
    mutate(group = cut2(Scoring, m = nrow(Final) / 5))%>%
    group_by(group) %>%
    summarize(count=n(),
              def = sum(default90, na.rm = T),
              PerDef = def/count,
              DefaultPerLower = PerDef - Z*sqrt(PerDef*(1-PerDef)/count),
              DefaultPerUpper = PerDef + Z*sqrt(PerDef*(1-PerDef)/count))
  
  check <- Final%>%
    group_by(MonthsDefault90) %>%
    summarise(Defaults = n()) %>%
    ungroup() %>%
    mutate(TotalObservations = sum(Defaults),
           DefaultRate = Defaults / TotalObservations)
  
  
  iv <- iv(Final[ , c("Scoring", "default90")], y = 'default90')
  bins <- woebin(Final[ , c("Scoring", "default90")], y = 'default90')
  
  #Calculating AUC
  pred_ROCR <- ROCR::prediction(Final$Scoring, Final$default90)
  auc_ROCR <- performance(pred_ROCR, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  
  #Calculating KS
  library(InformationValue)
  KS = ks_stat(actuals=Final$default90, predictedScores=Final$Scoring) 
  
  return(list(stat = stat, check = check, auc_ROCR = auc_ROCR, KS = KS))
  
}

results_list_scoring3_90days <- lapply(dates_list, monitoring3_90days_func)


# Monitoring Scoring 3/ default flag 60 days

monitoring3_60days_func <- function(arg1){
  start_date = arg1
  end_date = start_date %m+% months(2)
  
  Final <- Final %>%
    filter(CreditBeginDate >= start_date,
           CreditBeginDate <= end_date,
           ScoringType == 3)
  
  stat <- Final %>%
    mutate(group = cut2(Scoring, m = nrow(Final) / 5))%>%
    group_by(group) %>%
    summarize(count=n(),
              def = sum(default60, na.rm = T),
              PerDef = def/count,
              DefaultPerLower = PerDef - Z*sqrt(PerDef*(1-PerDef)/count),
              DefaultPerUpper = PerDef + Z*sqrt(PerDef*(1-PerDef)/count))
  
  check <- Final %>%
    group_by(MonthsDefault60) %>%
    summarise(Defaults = n()) %>%
    ungroup() %>%
    mutate(TotalObservations = sum(Defaults),
           DefaultRate = Defaults / TotalObservations)
  
  iv <- iv(Final[ , c("Scoring", "default60")], y = 'default60')
  bins <- woebin(Final[ , c("Scoring", "default60")], y = 'default60')
  
  #Calculating AUC
  pred_ROCR <- ROCR::prediction(Final$Scoring, Final$default60)
  auc_ROCR <- performance(pred_ROCR, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  
  #Calculating KS
  library(InformationValue)
  KS = ks_stat(actuals=Final$default60, predictedScores=Final$Scoring) 
  
  return(list(stat = stat, check = check, auc_ROCR = auc_ROCR, KS = KS))
  
}

results_list3_60days <- lapply(dates_list, monitoring101_60days_func)

#Monitoring scoring 2/ 90 days default flag

#Creating a list of dates
dates_list_scoring2 <-as.Date(c("2018-06-01", "2018-09-01", "2018-12-01", "2019-03-01", "2019-06-01"))

monitoring2_90days_func <- function(arg1){
  start_date = arg1
  end_date = start_date %m+% months(2)
  
  Final <- Final %>%
    filter(CreditBeginDate >= start_date,
           CreditBeginDate <= end_date,
           ScoringType == 2)
  
  stat <- Final %>%
    mutate(group = cut2(Scoring, m = nrow(Final) / 5))%>%
    group_by(group) %>%
    summarize(count=n(),
              def = sum(default90, na.rm = T),
              PerDef = def/count,
              DefaultPerLower = PerDef - Z*sqrt(PerDef*(1-PerDef)/count),
              DefaultPerUpper = PerDef + Z*sqrt(PerDef*(1-PerDef)/count))
  
  check<- Final%>%
    group_by(MonthsDefault90) %>%
    summarise(Defaults = n()) %>%
    ungroup() %>%
    mutate(TotalObservations = sum(Defaults),
           DefaultRate = Defaults / TotalObservations)
  
  
  iv <- iv(Final[ , c("Scoring", "default90")], y = 'default90')
  bins <- woebin(Final[ , c("Scoring", "default90")], y = 'default90')
  
  #Calculating AUC
  pred_ROCR <- ROCR::prediction(Final$Scoring, Final$default90)
  auc_ROCR <- performance(pred_ROCR, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  
  #Calculating KS
  library(InformationValue)
  KS = ks_stat(actuals=Final$default90, predictedScores=Final$Scoring) 
  
  return(list(stat = stat, check = check, auc_ROCR = auc_ROCR, KS = KS))
  
}

results_list_scoring2_90days <- lapply(dates_list_scoring2, monitoring101_90days_func)

# Monitoring Scoring 3/ default flag 60 days

monitoring2_60days_func <- function(arg1){
  start_date = arg1
  end_date = start_date %m+% months(2)
  
  Final <- Final %>%
    filter(CreditBeginDate >= start_date,
           CreditBeginDate <= end_date,
           ScoringType == 2)
  
  stat <- Final %>%
    mutate(group = cut2(Scoring, m = nrow(Final) / 5))%>%
    group_by(group) %>%
    summarize(count=n(),
              def = sum(default60, na.rm = T),
              PerDef = def/count,
              DefaultPerLower = PerDef - Z*sqrt(PerDef*(1-PerDef)/count),
              DefaultPerUpper = PerDef + Z*sqrt(PerDef*(1-PerDef)/count))
  
  check <- Final %>%
    group_by(MonthsDefault60) %>%
    summarise(Defaults = n()) %>%
    ungroup() %>%
    mutate(TotalObservations = sum(Defaults),
           DefaultRate = Defaults / TotalObservations)
  
  iv <- iv(Final[ , c("Scoring", "default60")], y = 'default60')
  bins <- woebin(Final[ , c("Scoring", "default60")], y = 'default60')
  
  #Calculating AUC
  pred_ROCR <- ROCR::prediction(Final$Scoring, Final$default60)
  auc_ROCR <- performance(pred_ROCR, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  
  #Calculating KS
  library(InformationValue)
  KS = ks_stat(actuals=Final$default60, predictedScores=Final$Scoring) 
  
  return(list(stat = stat, check = check, auc_ROCR = auc_ROCR, KS = KS))
  
}

results_list2_60days <- lapply(dates_list, monitoring2_60days_func)


#Monitoring scoring 1001 / 90 days default flag

#Creating a list of dates

dates_list_scoring1001 <-as.Date(c( "2019-02-01", "2019-05-01", "2019-08-01", "2019-11-01"))

monitoring1001_90days_func <- function(arg1){
  start_date = arg1
  end_date = start_date %m+% months(2)
  
  Final1 <- Final %>%
    filter(CreditBeginDate >= start_date,
           CreditBeginDate <= end_date,
           ScoringType == 1001)
  
  stat <- Final %>%
    mutate(group = cut2(Scoring, m = nrow(Final) / 5))%>%
    group_by(group) %>%
    summarize(count=n(),
              def = sum(default90, na.rm = T),
              PerDef = def/count,
              DefaultPerLower = PerDef - Z*sqrt(PerDef*(1-PerDef)/count),
              DefaultPerUpper = PerDef + Z*sqrt(PerDef*(1-PerDef)/count))
  
  check<- Final%>%
    group_by(MonthsDefault90) %>%
    summarise(Defaults = n()) %>%
    ungroup() %>%
    mutate(TotalObservations = sum(Defaults),
           DefaultRate = Defaults / TotalObservations)
  
  
  iv <- iv(Final[ , c("Scoring", "default90")], y = 'default90')
  bins <- woebin(Final[ , c("Scoring", "default90")], y = 'default90')
  
  #Calculating AUC
  pred_ROCR <- ROCR::prediction(Final$Scoring, Final$default90)
  auc_ROCR <- performance(pred_ROCR, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  
  #Calculating KS
  library(InformationValue)
  KS = ks_stat(actuals=Final$default90, predictedScores=Final$Scoring) 
  
  return(list(stat = stat, check = check, auc_ROCR = auc_ROCR, KS = KS))
  
}

results_list_scoring1001_90days <- lapply(dates_list_scoring1001, monitoring1001_90days_func)

# Monitoring Scoring 1001 / default flag 60 days

monitoring1001_60days_func <- function(arg1){
  start_date = arg1
  end_date = start_date %m+% months(2)
  
  Final <- Final %>%
    filter(CreditBeginDate >= start_date,
           CreditBeginDate <= end_date,
           ScoringType == 1001)
  
  stat <- Final %>%
    mutate(group = cut2(Scoring, m = nrow(Final) / 5))%>%
    group_by(group) %>%
    summarize(count=n(),
              def = sum(default60, na.rm = T),
              PerDef = def/count,
              DefaultPerLower = PerDef - Z*sqrt(PerDef*(1-PerDef)/count),
              DefaultPerUpper = PerDef + Z*sqrt(PerDef*(1-PerDef)/count))
  
  check <- Final %>%
    group_by(MonthsDefault60) %>%
    summarise(Defaults = n()) %>%
    ungroup() %>%
    mutate(TotalObservations = sum(Defaults),
           DefaultRate = Defaults / TotalObservations)
  
  iv <- iv(Final[ , c("Scoring", "default60")], y = 'default60')
  bins <- woebin(Final[ , c("Scoring", "default60")], y = 'default60')
  
  #Calculating AUC
  pred_ROCR <- ROCR::prediction(Final$Scoring, Final$default60)
  auc_ROCR <- performance(pred_ROCR, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  
  #Calculating KS
  library(InformationValue)
  KS = ks_stat(actuals=Final$default60, predictedScores=Final$Scoring) 
  
  return(list(stat = stat, check = check, auc_ROCR = auc_ROCR, KS = KS))
  
}

results_list1001_60days <- lapply(dates_list_scoring1001, monitoring1001_60days_func)
