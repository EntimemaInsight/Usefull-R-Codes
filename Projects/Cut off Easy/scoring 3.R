
#age

data2$age1 <- ifelse(data2$age <= 20, 42, 
                          ifelse(data2$age >= 21 & data2$age <=30, 22,
                                 ifelse(data2$age >= 31 & data2$age <= 45,0,
                                        ifelse(data2$age >= 46 & data2$age <=51, -13,
                                               ifelse(data2$age >= 52 & data2$age <= 58, -28, -36)))))

#Automobiles 

data2$Automobiles1 <- ifelse(data2$Automobiles <= 0, 13, -53)

#Client Family Size

data2$ClientFamilySize1 <- ifelse(data2$ClientFamilySize >= 2 & data2$ClientFamilySize <= 4, -13, 36 )

#Work Experience Current Work

data2$WorkExperienceCurrentWork1 <- ifelse(data2$WorkExperienceCurrentWork <= 1, 17,
                                                ifelse(data2$WorkExperienceCurrentWork >= 2 & data2$WorkExperienceCurrentWork <=5, 0, -22))


#Sum

data2$Sum1 <- ifelse(data2$Sum <= 150, -30,
                          ifelse(data2$Sum >= 151 & data2$Sum <= 400, 0, 3))

#Client Education

data2$ClientEducation1 <- ifelse(data2$ClientEducation %in% c("Висше","Висше бакалавър","Висше доктор и др. академични степени","Висше магистър"), -134,
                                      ifelse(data2$ClientEducation %in% c("Полувисше / Специалист"),-85,
                                             ifelse(data2$ClientEducation %in% c("Средно"),-23,
                                                    ifelse(data2$ClientEducation %in% c("Основно"), 62, 105 ))))


#Client Family Status

data2$ClientFamilyStatus1 <- ifelse(data2$ClientFamilyStatus %in% c("Женен / Омъжена"), -39,
                                         ifelse(data2$ClientFamilyStatus %in% c("Вдовец / Вдовица"), -28, 14))

#Get Salary Type 

data2$GetSalaryType1 <- ifelse(data2$GetSalaryType %in% c("По сметка в банка"), -18, 9)


#Type of Contract

data2$TypeOfContract1 <- ifelse(data2$TypeOfContract %in% c("Пенсионер по стаж и възраст неработещ","Пенсионер по стаж и възраст работещ"), -56,
                                           ifelse(data2$TypeOfContract %in% c("Безсрочен трудов договор","Договор за управление"), -20, 32))

#Weeks by Product

data2$WeeksByProduct <- ifelse(data2$ProductName == "EasyCredit" & data2$CreditPeriodCountName == 43,58,
                                             ifelse(data2$ProductName == "EasyCredit" & data2$CreditPeriodCountName == 34,58,
                                                    ifelse(data2$ProductName == "EasyCredit" & data2$CreditPeriodCountName == 27, 58,
                                                           ifelse(data2$ProductName == "EasyCredit" & data2$CreditPeriodCountName == 21,36,
                                                                  ifelse(data2$ProductName == "EasyCredit" & data2$CreditPeriodCountName == 16, 11,
                                                                         ifelse(data2$ProductName == "EasyCredit" & data2$CreditPeriodCountName == 12, -31,
                                                                                ifelse(data2$ProductName == "EasyCredit" & data2$CreditPeriodCountName == 8, -185,
                                                                                       ifelse(data2$ProductName == "EasyMonth" & data2$CreditPeriodCountName == 8, 58,
                                                                                              ifelse(data2$ProductName == "EasyMonth" & data2$CreditPeriodCountName == 6, 36,
                                                                                                     ifelse(data2$ProductName == "EasyMonth" & data2$CreditPeriodCountName == 4, 11,
                                                                                                            ifelse(data2$ProductName == "Pensioner" & data2$CreditPeriodCountName == 12, 36,
                                                                                                                ifelse(data2$ProductName == "Pensioner" & data2$CreditPeriodCountName == 10, 11,
                                                                                                                   ifelse(data2$ProductName == "Pensioner" & data2$CreditPeriodCountName == 8,11,
                                                                                                                       ifelse(data2$ProductName == "Pensioner" & data2$CreditPeriodCountName == 6, -61,
                                                                                                                           ifelse(data2$ProductName == "Pensioner" & data2$CreditPeriodCountName == 4, -133,
                                                                                                                              ifelse(data2$ProductName == "Pensioner" & data2$CreditPeriodCountName == 3, -197, NA))))))))))))))))
                                                                                                  
                                                                                       
data2 <- data2 %>%
  filter(!is.na(WeeksByProduct))


data2$Score3 <- 277 + data2$age1 + data2$Automobiles1 + data2$ClientFamilySize1 + data2$WorkExperienceCurrentWork1 + data2$Sum1 + data2$ClientEducation1 + data2$ClientFamilyStatus1 + data2$GetSalaryType1 + data2$TypeOfContract1 + data2$WeeksByProduct

data2$PD3 = round(0.0012760306917*data2$Score - 0.0622822414375, 2)

data2$PD3 <- ifelse(data2$PD < 0, 0, data2$PD)

#AUC for all credits(old and new)

library(ROCR)
pred_ROCR_3_all <- ROCR::prediction(data2$PD3, data2$default)

auc_ROCR_3_all <- performance(pred_ROCR_3, measure = "auc")
auc_ROCR_3_all <- auc_ROCR_3_all@y.values[[1]] 

#AUC for score 3 credits

data2_score3 <- data2 %>%
  filter(ScoringType == 3,
         ProductName %in% c("EasyCredit", "EasyMonth", "EasyMax"))

pred_ROCR_3 <- ROCR::prediction(data2_score3$PD3, data2_score3$default)

auc_ROCR_3 <- performance(pred_ROCR_3, measure = "auc")
auc_ROCR_3 <- auc_ROCR_3@y.values[[1]] 

#AUC for score 101 credits(scoring 3 model applied on scoring 101 credits)

data2_101 <- data2 %>%
  filter(ProductName %in% c("EasyCredit"),
         ScoringType == 101)

pred_ROCR_3_101 <- ROCR::prediction(data2_101$PD3, data2_101$default)

auc_ROCR_3_101 <- performance(pred_ROCR_3_101, measure = "auc")
auc_ROCR_3_101 <- auc_ROCR_3_101@y.values[[1]] 

#AUC for credits that are not scored

pred_ROCR_3_minus_1 <- ROCR::prediction(data2_minus_1$PD3, data2_minus_1$default)

auc_ROCR_3_minus_1 <- performance(pred_ROCR_3_minus_1, measure = "auc")
auc_ROCR_3_minus_1 <- auc_ROCR_3_minus_1@y.values[[1]] 

#AUC for new credits that are not scored

pred_ROCR_3_new <- ROCR::prediction(data2_new$PD3, data2_new$default)

auc_ROCR_3_new <- performance(pred_ROCR_3_new, measure = "auc")
auc_ROCR_3_new <- auc_ROCR_3_new@y.values[[1]] 

#AUC for Easy Credit product

pred_ROCR_3_EasyCredit <- ROCR::prediction(data2_Easycredit$PD3, data2_Easycredit$default)

auc_ROCR_3_EasyCredit <- performance(pred_ROCR_3_EasyCredit, measure = "auc")
auc_ROCR_3_EasyCredit <- auc_ROCR_3_EasyCredit@y.values[[1]] 

#AUC for Easy Month product

pred_ROCR_3_EasyMonth<- ROCR::prediction(data2_EasyMonth$PD3, data2_EasyMonth$default)

auc_ROCR_3_EasyMonth <- performance(pred_ROCR_3_EasyMonth, measure = "auc")
auc_ROCR_3_EasyMonth <- auc_ROCR_3_EasyMonth@y.values[[1]] 

#AUC for Easy Month product

pred_ROCR_3_Pensioner<- ROCR::prediction(data2_Pensioner$PD3, data2_Pensioner$default)

auc_ROCR_3_Pensioner <- performance(pred_ROCR_3_Pensioner, measure = "auc")
auc_ROCR_3_Pensioner <- auc_ROCR_3_Pensioner@y.values[[1]] 

#AUC for all credits(old and new)

library(ROCR)
pred_ROCR_3_all_60days <- ROCR::prediction(data2$PD3, data2$default60)

auc_ROCR_3_all_60days <- performance(pred_ROCR_3_all_60days, measure = "auc")
auc_ROCR_3_all_60days <- auc_ROCR_3_all_60days@y.values[[1]] 

#AUC for score 3 credits

data2_score3 <- data2 %>%
  filter(ScoringType == 3,
         ProductName %in% c("EasyCredit", "EasyMonth", "EasyMax"))

pred_ROCR_3_60days <- ROCR::prediction(data2_score3$PD3, data2_score3$default60)

auc_ROCR_3_60days <- performance(pred_ROCR_3_60days, measure = "auc")
auc_ROCR_3_60days <- auc_ROCR_3_60days@y.values[[1]] 

#AUC for score 101 credits(scoring 3 model applied on scoring 101 credits)

data2_101 <- data2 %>%
  filter(ProductName %in% c("EasyCredit"),
         ScoringType == 101)

pred_ROCR_3_101_60days <- ROCR::prediction(data2_101$PD3, data2_101$default60)

auc_ROCR_3_101_60days <- performance(pred_ROCR_3_101_60days, measure = "auc")
auc_ROCR_3_101_60days <- auc_ROCR_3_101_60days@y.values[[1]] 

#AUC for credits that are not scored

pred_ROCR_3_minus_1_60days <- ROCR::prediction(data2_minus_1$PD3, data2_minus_1$default60)

auc_ROCR_3_minus_1_60days <- performance(pred_ROCR_3_minus_1_60days, measure = "auc")
auc_ROCR_3_minus_1_60days <- auc_ROCR_3_minus_1_60days@y.values[[1]] 

#AUC for new credits that are not scored

pred_ROCR_3_new_60days <- ROCR::prediction(data2_new$PD3, data2_new$default60)

auc_ROCR_3_new_60days <- performance(pred_ROCR_3_new_60days, measure = "auc")
auc_ROCR_3_new_60days <- auc_ROCR_3_new_60days@y.values[[1]] 

#AUC for Easy Credit product

pred_ROCR_3_EasyCredit_60days <- ROCR::prediction(data2_Easycredit$PD3, data2_Easycredit$default60)

auc_ROCR_3_EasyCredit_60days <- performance(pred_ROCR_3_EasyCredit_60days, measure = "auc")
auc_ROCR_3_EasyCredit_60days <- auc_ROCR_3_EasyCredit_60days@y.values[[1]] 

#AUC for Easy Month product

pred_ROCR_3_EasyMonth_60days<- ROCR::prediction(data2_EasyMonth$PD3, data2_EasyMonth$default60)

auc_ROCR_3_EasyMonth_60days <- performance(pred_ROCR_3_EasyMonth_60days, measure = "auc")
auc_ROCR_3_EasyMonth_60days <- auc_ROCR_3_EasyMonth_60days@y.values[[1]] 

#AUC for Pensioner product

pred_ROCR_3_Pensioner_60days<- ROCR::prediction(data2_Pensioner$PD3, data2_Pensioner$default60)

auc_ROCR_3_Pensioner_60days <- performance(pred_ROCR_3_Pensioner_60days, measure = "auc")
auc_ROCR_3_Pensioner_60days <- auc_ROCR_3_Pensioner_60days@y.values[[1]] 

scoring_3_products <- data2 %>%
  filter(ScoringType == 3) %>%
  group_by(ProductName) %>%
  summarise(count = n())


#AUC for all scoring 3 credits

pred_ROCR_3_paid <- ROCR::prediction(data2_score3$PD3, data2_score3$DefaultsPaid)

auc_ROCR_3_paid <- performance(pred_ROCR_3_paid, measure = "auc")
auc_ROCR_3_paid <- auc_ROCR_3_paid@y.values[[1]] 

#AUC for all credits

pred_ROCR_3_all_paid <- ROCR::prediction(data2$PD3, data2$DefaultsPaid)

auc_ROCR_3_all_paid <- performance(pred_ROCR_3_all_paid, measure = "auc")
auc_ROCR_3_all_paid <- auc_ROCR_3_all_paid@y.values[[1]] 

#AUC for score 3 credits
pred_ROCR_3_101_paid <- ROCR::prediction(data2_score3$PD3, data2_score3$DefaultsPaid)

auc_ROCR_3_101_paid <- performance(pred_ROCR_3_101_paid, measure = "auc")
auc_ROCR_3_101_paid <- auc_ROCR_3_101_paid@y.values[[1]] 

#AUC for credits that are not scored

pred_ROCR_3_minus_1_paid <- ROCR::prediction(data2_minus_1$PD3, data2_minus_1$DefaultsPaid)

auc_ROCR_3_minus_1_paid <- performance(pred_ROCR_3_minus_1_paid, measure = "auc")
auc_ROCR_3_minus_1_paid <- auc_ROCR_3_minus_1_paid@y.values[[1]] 

#AUC for new credits that are not scored

pred_ROCR_3_new_paid <- ROCR::prediction(data2_new$PD3, data2_new$DefaultsPaid)

auc_ROCR_3_new_paid <- performance(pred_ROCR_3_new_paid, measure = "auc")
auc_ROCR_3_new_paid <- auc_ROCR_3_new_paid@y.values[[1]] 


#AUC for Easy Credit product

pred_ROCR_3_EasyCredit_paid <- ROCR::prediction(data2_Easycredit$PD3, data2_Easycredit$DefaultsPaid)

auc_ROCR_3_EasyCredit_paid <- performance(pred_ROCR_3_EasyCredit_paid, measure = "auc")
auc_ROCR_3_EasyCredit_paid <- auc_ROCR_3_EasyCredit_paid@y.values[[1]] 

#AUC for Easy Month product

pred_ROCR_3_EasyMonth_paid<- ROCR::prediction(data2_EasyMonth$PD3, data2_EasyMonth$DefaultsPaid)

auc_ROCR_3_EasyMonth_paid <- performance(pred_ROCR_3_EasyMonth_paid, measure = "auc")
auc_ROCR_3_EasyMonth_paid <- auc_ROCR_3_EasyMonth_paid@y.values[[1]] 

#AUC for Pensioner product

pred_ROCR_3_Pensioner_paid<- ROCR::prediction(data2_Pensioner$PD3, data2_Pensioner$DefaultsPaid)

auc_ROCR_3_Pensioner_paid <- performance(pred_ROCR_3_Pensioner_paid, measure = "auc")
auc_ROCR_3_Pensioner_paid <- auc_ROCR_3_Pensioner_paid@y.values[[1]] 




Scorinng3_90days <- c(auc_ROCR_3, auc_ROCR_3_101, auc_ROCR_3_all,auc_ROCR_3_minus_1, auc_ROCR_3_new, auc_ROCR_3_EasyCredit, auc_ROCR_3_EasyMonth, auc_ROCR_3_Pensioner)
Scorinng3_60days <- c(auc_ROCR_3_60days, auc_ROCR_3_101_60days, auc_ROCR_3_all_60days,auc_ROCR_3_minus_1_60days, auc_ROCR_3_new_60days, auc_ROCR_3_EasyCredit_60days, auc_ROCR_3_EasyMonth_60days, auc_ROCR_3_Pensioner_60days)
Scorinng3_paid <- c(auc_ROCR_3_paid, auc_ROCR_3_101_paid, auc_ROCR_3_all_paid,auc_ROCR_3_minus_1_paid, auc_ROCR_3_new_paid, auc_ROCR_3_EasyCredit_60days, auc_ROCR_3_EasyMonth_paid, auc_ROCR_3_Pensioner_paid)


AUC_results <- data.frame(cbind(Scoring101_90days,Scoring101_60days,Scoring101_Paid, Scorinng3_90days,Scorinng3_60days,Scorinng3_paid))
