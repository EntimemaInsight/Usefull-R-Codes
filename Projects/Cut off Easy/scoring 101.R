
# Client Education

data2$ClientEducation1 <- ifelse(data2$ClientEducation %in% c("Неизвестно","Начално", "Основно","Без образование","NA"), 0.42294,
                                                 ifelse(data2$ClientEducation %in% c("Висше магистър","Полувисше / Специалист","Висше","Висше бакалавър","Висше доктор и др. академични степени"),-0.58534,0 ))
data2$ClientEducation1[is.na(data2$ClientEducation)] <- 0.42294

# Type of contract

data2$TypeOfContract1 <- ifelse(data2$TypeOfContract %in% c("Работещ без договор"),0.48459,
                                                ifelse(data2$TypeOfContract %in% c("Безсрочен трудов договор"), 0, 0.32724))


# Staff type

data2$StaffType1 <- ifelse(data2$StaffType %in% c("Нискоквалифициран и неквалифициран персонал"),0.14627, 0)


# Automobiles

data2$Automobiles1 <- ifelse(data2$Automobiles <= 0, 0, -0.38088)
data2$Automobiles1[is.na(data2$Automobiles1)] <- 0

# Get salary type

data2$GetSalaryType1 <- ifelse(data2$GetSalaryType %in% c("По сметка в банка"), -0.22545, 0)


# Total length of service
data2$TotalLengthOfService <- as.numeric(data2$TotalLengthOfService)
data2$TotalLengthOfService1 <- ifelse(data2$TotalLengthOfService > 19, -0.22378, 0)

data2$TotalLengthOfService1[is.na(data2$TotalLengthOfService1)] <- 0

# Type of housing 

data2$TypeOfHousing1 <- ifelse(data2$TypeOfHousing %in% c("При Родители", "Неизвестно", "Собствено жилище"), 0, 0.22179)
                                               
                                               
# Client family status

data2$ClientFamilyStatus1 <- ifelse(data2$ClientFamilyStatus %in% c("Женен / Омъжена"), -0.18191, 0)
data2$ClientFamilyStatus1[is.na(data2$ClientFamilyStatus1)] <- 0



# Credit Purpose

data2$CreditPurpose1 <- ifelse(data2$CreditPurpose %in% c("Ремонт и обзавеждане", "Лечение и други медицински разходи","За покриване на друг кредит","Друга цел/ опишете подробно","Неизвестна","NA"), 0.24747,
                                             ifelse(data2$CreditPurpose %in% c("За образование", "Разходи за автомобила/транспортни","Разходи за автомобила","Покупка на бяла, черна или дребна техника","Разходи за забавления, пътуване, почивки и гостувания"), -0.1581, 0)) 
data2$CreditPurpose1[is.na(data2$CreditPurpose)]<- 0.24747


# Work experience current work
data2$WorkExperienceCurrentWork <- as.numeric(data2$WorkExperienceCurrentWork)
data2$WorkExperienceCurrentWork1 <- ifelse(data2$WorkExperienceCurrentWork >3,0, 0.1953)
data2$WorkExperienceCurrentWork1[is.na(data2$WorkExperienceCurrentWork1)] <- 0.1953


# Age

data2$age1 <- ifelse(data2$ age<= 22, 0.15476, 0)

# Partner connection type

data2$PartnerConnectionType1 <- ifelse(data2$PartnerConnectionType %in% c("Син / Дъщеря", "Съпруг / Съпруга","Родител"),0, 0.23749)
                                                       
                                                       
# Client sex

data2$ClientSex1 <- ifelse(data2$ClientSex %in% c("Жена"),-0.28059, 0)


# Noi insurance
data2$NOIInsurance <- as.numeric(data2$NOIInsurance)
data2$NOIInsurance1 <- ifelse(data2$NOIInsurance > 0, 0, 0.20238)
data2$NOIInsurance1[is.na(data2$NOIInsurance)] <- 0.20238

# Years on current address
data2$YearsOnCurrentAddress <- as.numeric(data2$YearsOnCurrentAddress)
data2$YearsOnCurrentAddress1 <- ifelse(data2$YearsOnCurrentAddress < 8,0.10777,0)
data2$YearsOnCurrentAddress1[is.na(data2$YearsOnCurrentAddress1)] <- 0.10777



# Client family size
data2$ClientFamilySize <- as.numeric(data2$ClientFamilySize)
data2$ClientFamilySize1 <- ifelse(data2$ClientFamilySize > 4,0.21315, 0)
data2$ClientFamilySize1[is.na(data2$ClientFamilySize)] <- 0


data2$score101 = -1.76381 + data2$ClientEducation1 + data2$TypeOfContract1 + data2$StaffType1 + data2$Automobiles1 + data2$GetSalaryType1 + data2$TotalLengthOfService1 + data2$TypeOfHousing1 + data2$ClientFamilyStatus1 + data2$CreditPurpose1 + data2$WorkExperienceCurrentWork1 + data2$age1 + data2$PartnerConnectionType1 + data2$ClientSex1 + data2$NOIInsurance1 + data2$YearsOnCurrentAddress1 +data2$ClientFamilySize1

data2$PD101 = 100/(1+exp(-data2$score))

data2$PD101 <- round(data2$PD, 2)

#AUC for all credits
pred_ROCR_101_all <- ROCR::prediction(data2$PD101, data2$default)

auc_ROCR_101_all <- performance(pred_ROCR_101_all, measure = "auc")
auc_ROCR_101_all <- auc_ROCR_101_all@y.values[[1]] 


#AUC for all scoring 101 credits
pred_ROCR_101 <- ROCR::prediction(data2_101$PD101, data2_101$default)

auc_ROCR_101 <- performance(pred_ROCR_101, measure = "auc")
auc_ROCR_101 <- auc_ROCR_101@y.values[[1]] 

#AUC for score 3 credits
pred_ROCR_101_3 <- ROCR::prediction(data2_score3$PD101, data2_score3$default)

auc_ROCR_101_3 <- performance(pred_ROCR_101_3, measure = "auc")
auc_ROCR_101_3 <- auc_ROCR_101_3@y.values[[1]] 

#AUC for credits that are not scored

data2_minus_1 <- data2 %>%
  filter(ScoringType == -1)

pred_ROCR_101_minus_1 <- ROCR::prediction(data2_minus_1$PD101, data2_minus_1$default)

auc_ROCR_101_minus_1 <- performance(pred_ROCR_101_minus_1, measure = "auc")
auc_ROCR_101_minus_1 <- auc_ROCR_101_minus_1@y.values[[1]] 

#AUC for new credits that are not scored

data2_new <- data2 %>%
  filter(ScoringType == -1,
         Poreden == 0)

pred_ROCR_101_new <- ROCR::prediction(data2_new$PD101, data2_new$default)

auc_ROCR_101_new <- performance(pred_ROCR_101_new, measure = "auc")
auc_ROCR_101_new <- auc_ROCR_101_new@y.values[[1]] 

#AUC for Easy Credit product

data2_Easycredit <- data2 %>%
  filter(ProductName == "EasyCredit")

pred_ROCR_101_EasyCredit <- ROCR::prediction(data2_Easycredit$PD101, data2_Easycredit$default)

auc_ROCR_101_EasyCredit <- performance(pred_ROCR_101_EasyCredit, measure = "auc")
auc_ROCR_101_EasyCredit <- auc_ROCR_101_EasyCredit@y.values[[1]] 

#AUC for Easy Month product

data2_EasyMonth <- data2 %>%
  filter(ProductName == "EasyMonth")

pred_ROCR_101_EasyMonth<- ROCR::prediction(data2_EasyMonth$PD101, data2_EasyMonth$default)

auc_ROCR_101_EasyMonth <- performance(pred_ROCR_101_EasyMonth, measure = "auc")
auc_ROCR_101_EasyMonth <- auc_ROCR_101_EasyMonth@y.values[[1]] 

#AUC for Pensioner product

data2_Pensioner <- data2 %>%
  filter(ProductName == "Pensioner")

pred_ROCR_101_Pensioner<- ROCR::prediction(data2_Pensioner$PD101, data2_Pensioner$default)

auc_ROCR_101_Pensioner <- performance(pred_ROCR_101_Pensioner, measure = "auc")
auc_ROCR_101_Pensioner <- auc_ROCR_101_Pensioner@y.values[[1]] 


#AUC for all credits(old and new)

library(ROCR)
pred_ROCR_101_all_60days <- ROCR::prediction(data2$PD101, data2$default60)

auc_ROCR_101_all_60days <- performance(pred_ROCR_101_all_60days, measure = "auc")
auc_ROCR_101_all_60days <- auc_ROCR_101_all_60days@y.values[[1]] 

#AUC for score 3 credits

pred_ROCR_101_60days <- ROCR::prediction(data2_score3$PD101, data2_score3$default60)

auc_ROCR_101_60days <- performance(pred_ROCR_101_60days, measure = "auc")
auc_ROCR_101_60days <- auc_ROCR_101_60days@y.values[[1]] 

#AUC for score 101 credits(scoring 3 model applied on scoring 101 credits)

pred_ROCR_101_3_60days <- ROCR::prediction(data2_score3$PD101, data2_score3$default60)

auc_ROCR_101_3_60days <- performance(pred_ROCR_101_3_60days, measure = "auc")
auc_ROCR_101_3_60days <- auc_ROCR_101_3_60days@y.values[[1]] 

#AUC for credits that are not scored

pred_ROCR_101_minus_1_60days <- ROCR::prediction(data2_minus_1$PD101, data2_minus_1$default60)

auc_ROCR_101_minus_1_60days <- performance(pred_ROCR_101_minus_1_60days, measure = "auc")
auc_ROCR_101_minus_1_60days <- auc_ROCR_101_minus_1_60days@y.values[[1]] 

#AUC for new credits that are not scored

pred_ROCR_101_new_60days <- ROCR::prediction(data2_new$PD101, data2_new$default60)

auc_ROCR_101_new_60days <- performance(pred_ROCR_101_new_60days, measure = "auc")
auc_ROCR_101_new_60days <- auc_ROCR_101_new_60days@y.values[[1]] 

#AUC for Easy Credit product

pred_ROCR_101_EasyCredit_60days <- ROCR::prediction(data2_Easycredit$PD101, data2_Easycredit$default60)

auc_ROCR_101_EasyCredit_60days <- performance(pred_ROCR_101_EasyCredit_60days, measure = "auc")
auc_ROCR_101_EasyCredit_60days <- auc_ROCR_101_EasyCredit_60days@y.values[[1]] 

#AUC for Easy Month product

pred_ROCR_101_EasyMonth_60days<- ROCR::prediction(data2_EasyMonth$PD101, data2_EasyMonth$default60)

auc_ROCR_101_EasyMonth_60days <- performance(pred_ROCR_101_EasyMonth_60days, measure = "auc")
auc_ROCR_101_EasyMonth_60days <- auc_ROCR_101_EasyMonth_60days@y.values[[1]] 

#AUC for Pensioner product

pred_ROCR_101_Pensioner_60days<- ROCR::prediction(data2_Pensioner$PD101, data2_Pensioner$default60)

auc_ROCR_101_Pensioner_60days <- performance(pred_ROCR_101_Pensioner_60days, measure = "auc")
auc_ROCR_101_Pensioner_60days <- auc_ROCR_101_Pensioner_60days@y.values[[1]] 

scoring_101_products <- data2 %>%
  filter(ScoringType == 101) %>%
  group_by(ProductName) %>%
  summarise(count = n())

scorings <- data2 %>%
  group_by(ScoringType)%>%
  summarise(count = n())


scoring5 <- data2 %>%
  filter(ScoringType == 5)


#AUC for all credits

data2$DefaultsPaid[is.na(data2$DefaultsPaid)] <- 0
library(ROCR)
pred_ROCR_101_paid <- ROCR::prediction(data2$PD101, data2$DefaultsPaid)

auc_ROCR_101_paid <- performance(pred_ROCR_101_paid, measure = "auc")
auc_ROCR_101_paid <- auc_ROCR_101_paid@y.values[[1]] 
unique(data2_1$default)

#AUC for score 3 credits
pred_ROCR_101_3_paid <- ROCR::prediction(data2_score3$PD101, data2_score3$DefaultsPaid)

auc_ROCR_101_3_paid <- performance(pred_ROCR_101_3_paid, measure = "auc")
auc_ROCR_101_3_paid <- auc_ROCR_101_3_paid@y.values[[1]] 

#AUC for credits that are not scored

data2_minus_1 <- data2 %>%
  filter(ScoringType == -1)

pred_ROCR_101_minus_1_paid <- ROCR::prediction(data2_minus_1$PD101, data2_minus_1$DefaultsPaid)

auc_ROCR_101_minus_1_paid <- performance(pred_ROCR_101_minus_1_paid, measure = "auc")
auc_ROCR_101_minus_1_paid <- auc_ROCR_101_minus_1_paid@y.values[[1]] 

#AUC for new credits that are not scored

data2_new <- data2 %>%
  filter(ScoringType == -1,
         Poreden == 0)

pred_ROCR_101_new_paid <- ROCR::prediction(data2_new$PD101, data2_new$DefaultsPaid)

auc_ROCR_101_new_paid <- performance(pred_ROCR_101_new_paid, measure = "auc")
auc_ROCR_101_new_paid <- auc_ROCR_101_new_paid@y.values[[1]] 


#AUC for Easy Credit product

pred_ROCR_101_EasyCredit_paid <- ROCR::prediction(data2_Easycredit$PD101, data2_Easycredit$DefaultsPaid)

auc_ROCR_101_EasyCredit_paid <- performance(pred_ROCR_101_EasyCredit_paid, measure = "auc")
auc_ROCR_101_EasyCredit_paid <- auc_ROCR_101_EasyCredit_paid@y.values[[1]] 

#AUC for Easy Month product

pred_ROCR_101_EasyMonth_paid<- ROCR::prediction(data2_EasyMonth$PD101, data2_EasyMonth$DefaultsPaid)

auc_ROCR_101_EasyMonth_paid <- performance(pred_ROCR_101_EasyMonth_paid, measure = "auc")
auc_ROCR_101_EasyMonth_paid <- auc_ROCR_101_EasyMonth_paid@y.values[[1]] 

#AUC for Pensioner product

pred_ROCR_101_Pensioner_paid<- ROCR::prediction(data2_Pensioner$PD101, data2_Pensioner$DefaultsPaid)

auc_ROCR_101_Pensioner_paid <- performance(pred_ROCR_101_Pensioner_paid, measure = "auc")
auc_ROCR_101_Pensioner_paid <- auc_ROCR_101_Pensioner_paid@y.values[[1]] 


library(ROCR)
pred_ROCR_101_all_paid <- ROCR::prediction(data2$PD101, data2$DefaultsPaid)

auc_ROCR_101_all_paid <- performance(pred_ROCR_101_all_paid, measure = "auc")
auc_ROCR_101_all_paid <- auc_ROCR_101_all_paid@y.values[[1]] 


Scoring101_90days <- c(auc_ROCR_101,auc_ROCR_101_3, auc_ROCR_101_all, auc_ROCR_101_minus_1,auc_ROCR_101_new, auc_ROCR_101_EasyCredit, auc_ROCR_101_EasyMonth, auc_ROCR_101_Pensioner)
Scoring101_60days <- c(auc_ROCR_101_60days, auc_ROCR_101_3_60days, auc_ROCR_101_all_60days, auc_ROCR_101_new_60days, auc_ROCR_101_new_60days,auc_ROCR_101_EasyCredit_60days, auc_ROCR_101_EasyMonth_60days, auc_ROCR_3_Pensioner_60days)
Scoring101_Paid <- c(auc_ROCR_101_paid, auc_ROCR_101_3_paid, auc_ROCR_101_all_paid,auc_ROCR_101_minus_1_paid, auc_ROCR_101_new_paid,auc_ROCR_3_EasyCredit_paid, auc_ROCR_3_EasyMonth_paid, auc_ROCR_3_Pensioner_paid)

