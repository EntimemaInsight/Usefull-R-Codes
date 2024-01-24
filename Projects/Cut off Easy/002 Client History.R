

# 
# Harold_Easy <- read.csv2(file = "//10.254.1.4\\Files\\Analysis\\3.Data\\2020.01\\Easy BG\\E006_Harold_BG_20200201_v1.csv", 
#                          stringsAsFactors = F, na.strings = c("NULL", "NA", "", "N/A"), encoding = "UTF-8")



History2019 <- do.call(bind_rows,Easy_Portfolios_active_2019) 
History2018 <- do.call(bind_rows, Easy_Portfolios_active_2018)

History <- rbind(History2019, History2018)

History1 <- New_Loans %>%
  left_join(History, by = c("CodeContract" = "CreditCode"))
  


History1 <- History1 %>%
      arrange(EGN,CalcDate) %>%
      mutate(date = as.Date(CreditBeginDate %m-% months(6)),
             OldOrNew = ifelse(date < CalcDate & CalcDate < CreditBeginDate, 1, 0),
                           defaultDays = ifelse(MaxDelayP > 90, 1, 0),
                           defaultPayment = ifelse(PayedTotal/Sum > 1.2,0,1))


History2 <- History1 %>%
  select(CodeContract,CalcDate,defaultDays) %>%
  arrange(CodeContract, CalcDate) %>%
  filter(defaultDays == 1) %>%
  group_by(CodeContract) %>%
  summarize(FirstDefaultDate = min(CalcDate)) %>%
  ungroup()


History3 <- History1 %>%
  left_join(History2, by = "CodeContract") %>%
  arrange(CodeContract, desc(CalcDate)) %>%
  group_by(CodeContract) %>%
  filter(row_number()==1) %>%
  ungroup()
   
History3 <- History3 %>%
  select("CodeContract", "ClientSK", "CreditBeginDate", "ScoringType", "Scoring","OldOrNew", "Sum","PeriodSK","ProductName", "MaxDelayP", "PayedTotal","defaultDays","defaultPayment")

History3 <- na.omit(History3, cols = c("MaxDelayP", "PayedTotal"))



list_scoring_type <- c(1001,-1,2,101)

library(ROCR)

score_function <- function(arg1){
  
  dd <- History3 %>%
    filter(ScoringType == arg1) 
  
  statDays <- dd %>%
    mutate(group = cut2(Scoring, m = nrow(dd) / 5)) %>%
    group_by(group) %>%
    summarize(count = n(),
              defaults = sum(defaultDays, na.rm = T),
              defPer = defaults/count,
              DefaultPerLower = defPer - Z*sqrt(defPer*(1-defPer)/count),
              DefaultPerUpper = defPer + Z*sqrt(defPer*(1-defPer)/count))
  
  statPayment <- dd %>%
    mutate(group = cut2(Scoring, m = nrow(dd) / 5)) %>%
    group_by(group) %>%
    summarize(count = n(),
              defaults = sum(defaultPayment, na.rm = T),
              defPer = defaults/count,
              DefaultPerLower = defPer - Z*sqrt(defPer*(1-defPer)/count),
              DefaultPerUpper = defPer + Z*sqrt(defPer*(1-defPer)/count))
  
  dd$defaultDays <- factor(dd$defaultDays)
  dd$defaultPayment <- factor(dd$defaultPayment)
  
  pred_ROCR1 <- ROCR::prediction(dd$Scoring, dd$defaultDays)
  
 
  auc_ROCR1 <- performance(pred_ROCR1, measure = "auc")
  auc_ROCR1 <- auc_ROCR1@y.values[[1]] 
  
  
  pred_ROCR2 <- ROCR::prediction(dd$Scoring, dd$defaultPayment)
  
  #RoC percent
  auc_ROCR2 <- performance(pred_ROCR2, measure = "auc")
  auc_ROCR2 <- auc_ROCR2@y.values[[1]] 
  
  return(list(statDays = statDays, statPayment = statPayment, auc_ROCR1 = auc_ROCR1, auc_ROCR2 = auc_ROCR2))
}

ddd <- lapply(list_scoring_type, score_function)


test <- History %>%
  filter(CreditCode == "3541615")

test2 <- History3 %>%
  filter(CodeContract == "3541615")

