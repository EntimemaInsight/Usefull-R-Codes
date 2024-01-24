
# Adding the variables needed for scoring model 101
Final$YearsOnCurrentAddress <- as.numeric(Final$YearsOnCurrentAddress)
Final$TotalLengthOfService <- as.numeric(Final$TotalLengthOfService)

Final <- Final %>%
  mutate(AgeSore101 = case_when(Age <= 22 ~ 0.15476,
                                Age > 22 ~ 0),
         ClientEducationScore101 = case_when(ClientEducation %in% c("Неизвестно","Начално","Основно","Без образование","NA") ~ 0.42294,
                                             ClientEducation %in% c("Висше магистър","Полувисше / Специалист","Висше","Висше бакалавър","Висше доктор и др. академични степени") ~ -0.58534,
                                             ClientEducation %in% c("Средно") ~ 0),
         TypeOfContractScore101 = case_when(TypeOfContract %in% c("Работещ без договор") ~ 0.48459,
                                            TypeOfContract %in% c("Безсрочен трудов договор") ~ 0,
                                            TypeOfContract %nin% c("Работещ без договор","Безсрочен трудов договор") ~ 0.32724),
         StaffTypeScore101 = case_when(StaffType %in% c("Нискоквалифициран и неквалифициран персонал") ~ 0.14627,
                                       StaffType %nin% c("Нискоквалифициран и неквалифициран персонал") ~ 0),
         AutomobilesScore101 = case_when(Automobiles <= 0 ~ 0,
                                         Automobiles >= 1 ~ -0.38088),
         GetSalaryTypeScore101 = case_when(GetSalaryType %in% c("По сметка в банка") ~ -0.22545,
                                           GetSalaryType %nin% c("По сметка в банка") ~ 0),
         TotalLengthOfServiceScore101 = case_when(TotalLengthOfService < 19 ~ 0,
                                                  TotalLengthOfService >= 19 ~ -0.22378,
                                                  TotalLengthOfService == "NA" ~ 0),
         TypeOfHousingScore101 = case_when(TypeOfHousing %in% c("При Родители", "Неизвестно", "Собствено жилище") ~ 0,
                                           TypeOfHousing %nin% c("При Родители", "Неизвестно", "Собствено жилище") ~ 0.22179),
         ClientFamilyStatusScore101 = case_when(ClientFamilyStatus %in% c("Женен / Омъжена") ~ -0.18191,
                                                ClientFamilyStatus %nin% c("Женен / Омъжена") ~ 0),
         CreditPurposeScore101 = case_when(CreditPurpose %in% c("Ремонт и обзавеждане", "Лечение и други медицински разходи","За покриване на друг кредит","Друга цел/ опишете подробно","Неизвестна","NA") ~ 0.24747,
                                           CreditPurpose %in% c("За образование", "Разходи за автомобила/транспортни","Разходи за автомобила","Покупка на бяла, черна или дребна техника","Разходи за забавления, пътуване, почивки и гостувания") ~ -0.1581,
                                           CreditPurpose %in% c("Консумативи, сметки, отопление","Разширяване на бизнеса","Оборотен капитал и др. бизнес нужди","Разходи за персонал","Покупка на машини и обурудване") ~ 0),
         WorkExperienceCurrentWorkScore101 = case_when(WorkExperienceCurrentWork <= 3.3 ~ 0.1953,
                                                       WorkExperienceCurrentWork > 3.3 ~ 0),
         PartnerConnectionTypeScore101 = case_when(PartnerConnectionType %in% c("Син / Дъщеря", "Съпруг / Съпруга","Родител") ~ 0,
                                                  PartnerConnectionType %nin% c("Син / Дъщеря", "Съпруг / Съпруга","Родител") ~ 0.23749),
         ClientSexScore101 = case_when(ClientSex == "Жена" ~ -0.28059,
                               ClientSex == "Мъж" ~ 0),
         NOIInsuranceScore101 = case_when(NOIInsurance > 0 ~ 0,
                                          NOIInsurance %in% c(0, NA) ~ 0.20238),
         YearsOnCurrentAddressScore101 = case_when(YearsOnCurrentAddress < 8 ~ 0.10777,
                                                   YearsOnCurrentAddress >= 8 ~ 0,
                                                   is.na(YearsOnCurrentAddress) ~ 0.10777),
         ClientFamilySizeScore101 = case_when(ClientFamilySize > 4 ~ 0.21315,
                                              ClientFamilySize <= 4 ~ 0),
         Score101 = -1.76381 + ClientEducationScore101 + TypeOfContractScore101 + StaffTypeScore101 + AutomobilesScore101 + GetSalaryTypeScore101 + TotalLengthOfServiceScore101 + TypeOfHousingScore101 + ClientFamilyStatusScore101 + CreditPurposeScore101 + WorkExperienceCurrentWorkScore101 + AgeSore101 + PartnerConnectionTypeScore101 + ClientSexScore101 + NOIInsuranceScore101 + YearsOnCurrentAddressScore101 + ClientFamilySizeScore101,
         PD101 = round(100/(1+exp(-Score101)),2))


#Adding the variables for scoring model 3 and calculating the score
Final <- Final %>%
  mutate(AgeScore3 = case_when(Age <= 20 ~ 42,
                                Age <= 30 ~ 22,
                                Age <= 45 ~ 0,
                                Age <= 51 ~ -13,
                                Age <= 58 ~ -28,
                                Age > 58 ~ -36),
         AutomobilesScore3 = case_when(Automobiles <= 0 ~ 13,
                                       Automobiles >= 1 ~ -53),
         ClientFamilySizeScore3 = case_when(ClientFamilySize >= 2 & ClientFamilySize <= 4 ~ -13,
                                            TRUE ~ 36),
         WorkExperienceCurrentWorkScore3 = case_when(WorkExperienceCurrentWork <= 1 ~ 17,
                                                     WorkExperienceCurrentWork >= 2 & WorkExperienceCurrentWork <= 5 ~ 0,
                                                     WorkExperienceCurrentWork >= 6 ~ -22),
         SumaPScore3 = case_when(SumaP <= 150 ~ -30,
                                 SumaP >=  151 & SumaP <= 400 ~ 0,
                                 SumaP >= 401 ~ 3),
         ClientEducationScore3 = case_when(ClientEducation %in% c("Висше","Висше бакалавър","Висше доктор и др. академични степени","Висше магистър") ~ -134,
                                           ClientEducation %in% c("Полувисше / Специалист") ~ -85,
                                           ClientEducation %in% c("Средно") ~ -23,
                                           ClientEducation %in% c("Основно") ~ 62,
                                           ClientEducation %in% c("Без образование", "Начално", "Неизвестно", "Друго") ~ 105),
         ClientFamilyStatusScore3 = case_when(ClientFamilyStatus %in% c("Женен / Омъжена") ~ -39,
                                              ClientFamilyStatus %in% c("Вдовец / Вдовица") ~ -28,
                                              TRUE ~ 14),
         GetSalaryTypeScore3 = case_when(GetSalaryType %in% c("По сметка в банка") ~ -18,
                                         TRUE ~ 9),
         TypeOfContractScore3 = case_when(TypeOfContract %in% c("Пенсионер по стаж и възраст неработещ","Пенсионер по стаж и възраст работещ") ~ -56,
                                          TypeOfContract %in% c("Безсрочен трудов договор","Договор за управление") ~ -20,
                                          TRUE ~ 32),
         WeeksScore3 = case_when(Weeks == 8 & CreditProduct == "EasyMonth" ~ 58,
                                 Weeks == 6 & CreditProduct == "EasyMonth" ~ 36,
                                 Weeks == 4 & CreditProduct == "EasyMonth" ~ 11,
                                 Weeks == 43 & CreditProduct == "EasyCredit" ~ 58,
                                 Weeks == 34 & CreditProduct == "EasyCredit" ~ 58,
                                 Weeks == 27 & CreditProduct == "EasyCredit" ~ 58,
                                 Weeks == 21 & CreditProduct == "EasyCredit" ~ 36,
                                 Weeks == 16 & CreditProduct == "EasyCreit" ~ 11,
                                 Weeks == 12 & CreditProduct == "EasyCredit" ~ -31,
                                 Weeks == 8 & CreditProduct == "EasyCredit" ~ -185,
                                 Weeks == 12 & CreditProduct == "Pensioner" ~ 36,
                                 Weeks == 10 & CreditProduct == "Pensioner" ~ 11,
                                 Weeks == 8 & CreditProduct == "Pensioner" ~ 11,
                                 Weeks == 6 & CreditProduct == "Pensioner" ~ -61,
                                 Weeks == 4 & CreditProduct == "Pensioner" ~ -133,
                                 Weeks == 3 & CreditProduct == "Pensioner" ~ -197),
         
         Score3 = 277 + AgeScore3 + AutomobilesScore3 + ClientFamilySizeScore3 +WorkExperienceCurrentWorkScore3 + SumaPScore3 + ClientEducationScore3 +  ClientFamilyStatusScore3 + GetSalaryTypeScore3 + TypeOfContractScore3 + WeeksScore3,
         PD3 = round(0.0012760306917*Score3 - 0.0622822414375, 2),
         PD3 = ifelse(PD3 < 0, 0, PD3))


test101 <- Final %>%
  filter(ScoringType == 101,
         PD101 != Scoring,
         CreditCode == "3382395")

test101$default90 <- as.factor(test101$default90)
pred_ROCR_101 <- ROCR::prediction(test101$Scoring, test101$default90)

auc_ROCR_101 <- performance(pred_ROCR_101, measure = "auc")
auc_ROCR_101 <- auc_ROCR_101@y.values[[1]] 

test3 <- Final %>%
  filter(ScoringType == 3,
         PD3!=Scoring,
         !Scoring > 1,
         !Scoring <0)

#Calculating AUC for Scoring Model 101

#AUC for all credits (90 days default flag)
pred_ROCR_101_all_90_days <- ROCR::prediction(Final$PD101, Final$default90)

auc_ROCR_101_all_90days <- performance(pred_ROCR_101_all_90_days, measure = "auc")
auc_ROCR_101_all_90days <- auc_ROCR_101_all_90days@y.values[[1]] 

# AUC for all credits (60 days default flag)

pred_ROCR_101_all_60days <- ROCR::prediction(Final$PD101, Final$default60)

auc_ROCR_101_all_60days <- performance(pred_ROCR_101_all_60days, measure = "auc")
auc_ROCR_101_all_60days <- auc_ROCR_101_all_60days@y.values[[1]] 

#AUC for credits scored with scoring 101 (90 days flag)
FinalScore101 <- Final %>%
  filter(ScoringType == 101,
         CreditProduct == "EasyCredit")

pred_ROCR_101_90_days <- ROCR::prediction(FinalScore101$PD101, FinalScore101$default90)

auc_ROCR_101_90days <- performance(pred_ROCR_101_90_days, measure = "auc")
auc_ROCR_101_90days <- auc_ROCR_101_90days@y.values[[1]] 

#AUC for credits scored with scoring 101 (60 days flag)

pred_ROCR_101_60_days <- ROCR::prediction(FinalScore101$PD101, FinalScore101$default60)

auc_ROCR_101_60days <- performance(pred_ROCR_101_60_days, measure = "auc")
auc_ROCR_101_60days <- auc_ROCR_101_60days@y.values[[1]] 


# AUC for score 3 credits (applying score 101 model on credits scored with score 3, 90 days default flag)

FinalScore3 <- Final %>%
  filter(ScoringType == 3)

pred_ROCR_101_3_90days <- ROCR::prediction(FinalScore3$PD101, FinalScore3$default90)

auc_ROCR_101_3_90days <- performance(pred_ROCR_101_3_90days, measure = "auc")
auc_ROCR_101_3_90days <- auc_ROCR_101_3_90days@y.values[[1]] 

# AUC for score 3 credits (applying score 101 model on credits scored with score 3, 60 days default flag)

pred_ROCR_101_3_60days <- ROCR::prediction(FinalScore3$PD101, FinalScore3$default60)

auc_ROCR_101_3_60days <- performance(pred_ROCR_101_3_60days, measure = "auc")
auc_ROCR_101_3_60days <- auc_ROCR_101_3_60days@y.values[[1]] 

#AUC for new credits (90 days default flag)

Final_new <- Final %>%
  filter(Poreden == 0)

pred_ROCR_101_new_90Days <- ROCR::prediction(Final_new$PD101, Final_new$default90)

auc_ROCR_101_new_90days <- performance(pred_ROCR_101_new_90Days, measure = "auc")
auc_ROCR_101_new_90days <- auc_ROCR_101_new_90days@y.values[[1]] 

#AUC for new credits (60 days default flag)

pred_ROCR_101_new_60days <- ROCR::prediction(Final_new$PD101, Final_new$default60)

auc_ROCR_101_new_60days <- performance(pred_ROCR_101_new_60days, measure = "auc")
auc_ROCR_101_new_60days <- auc_ROCR_101_new_60days@y.values[[1]] 

#AUC for EasyCredit product (90 days default flag)

Final_EasyCredit <- Final %>%
  filter(CreditProduct == "EasyCredit")

pred_ROCR_101_EasyCredit_90days <- ROCR::prediction(Final_EasyCredit$PD101, Final_EasyCredit$default90)

auc_ROCR_101_EasyCredit_90days <- performance(pred_ROCR_101_EasyCredit_90days, measure = "auc")
auc_ROCR_101_EasyCredit_90days <- auc_ROCR_101_EasyCredit_90days@y.values[[1]] 

#AUC for EasyCredit product (60 days default flag)

pred_ROCR_101_EasyCredit_60days <- ROCR::prediction(Final_EasyCredit$PD101, Final_EasyCredit$default60)

auc_ROCR_101_EasyCredit_60days <- performance(pred_ROCR_101_EasyCredit_60days, measure = "auc")
auc_ROCR_101_EasyCredit_60days <- auc_ROCR_101_EasyCredit_60days@y.values[[1]] 

#AUC for EasyMonth product (90 days default flag)

Final_EasyMonth <- Final %>%
  filter(CreditProduct == "EasyMonth")

pred_ROCR_101_EasyMonth_90days <- ROCR::prediction(Final_EasyMonth$PD101, Final_EasyMonth$default90)

auc_ROCR_101_EasyMonth_90days <- performance(pred_ROCR_101_EasyMonth_90days, measure = "auc")
auc_ROCR_101_EasyMonth_90days <- auc_ROCR_101_EasyMonth_90days@y.values[[1]] 

#AUC for EasyMax product (60 days default flag)

pred_ROCR_101_EasyMonth_60days <- ROCR::prediction(Final_EasyMonth$PD101, Final_EasyMonth$default60)

auc_ROCR_101_EasyMonth_60days <- performance(pred_ROCR_101_EasyMonth_60days, measure = "auc")
auc_ROCR_101_EasyMonth_60days <- auc_ROCR_101_EasyMonth_60days@y.values[[1]] 


#AUC for Pensioner product (90 days default flag)
Final_Pensioner <- Final %>%
  filter(CreditProduct == "Pensioner")

pred_ROCR_101_Pensioner_90days <- ROCR::prediction(Final_Pensioner$PD101, Final_Pensioner$default90)

auc_ROCR_101_Pensioner_90days <- performance(pred_ROCR_101_Pensioner_90days, measure = "auc")
auc_ROCR_101_Pensioner_90days <- auc_ROCR_101_Pensioner_90days@y.values[[1]] 

#AUC for Pensioner product (60 days default flag)

pred_ROCR_101_Pensioner_60days <- ROCR::prediction(Final_Pensioner$PD101, Final_Pensioner$default60)

auc_ROCR_101_Pensioner_60days <- performance(pred_ROCR_101_Pensioner_60days, measure = "auc")
auc_ROCR_101_Pensioner_60days <- auc_ROCR_101_Pensioner_60days@y.values[[1]] 

#Calculating AUC for Scoring Model 3

#AUC for score 3 model (90 days default flag)

pred_ROCR_3 <- ROCR::prediction(FinalScore3$Scoring, FinalScore3$default90)

auc_ROCR_3 <- performance(pred_ROCR_3, measure = "auc")
auc_ROCR_3 <- auc_ROCR_3@y.values[[1]] 

#AUC for all credits (90 days default flag)

pred_ROCR_3_all_90_days <- ROCR::prediction(Final$PD3, Final$default90)

auc_ROCR_3_all_90days <- performance(pred_ROCR_3_all_90_days, measure = "auc")
auc_ROCR_3_all_90days <- auc_ROCR_3_all_90days@y.values[[1]] 

# AUC for all credits (60 days default flag)

pred_ROCR_3_all_60days <- ROCR::prediction(Final$PD3, Final$default60)

auc_ROCR_3_all_60days <- performance(pred_ROCR_3_all_60days, measure = "auc")
auc_ROCR_3_all_60days <- auc_ROCR_3_all_60days@y.values[[1]] 

#AUC for credits scored with scoring 3 (90 days flag)

pred_ROCR_3_90_days <- ROCR::prediction(FinalScore3$PD3, FinalScore3$default90)

auc_ROCR_3_90days <- performance(pred_ROCR_3_90_days, measure = "auc")
auc_ROCR_3_90days <- auc_ROCR_3_90days@y.values[[1]] 

#AUC for credits scored with scoring 101 (60 days flag)

pred_ROCR_3_60_days <- ROCR::prediction(FinalScore3$PD3, FinalScore3$default60)

auc_ROCR_3_60days <- performance(pred_ROCR_3_60_days, measure = "auc")
auc_ROCR_3_60days <- auc_ROCR_3_60days@y.values[[1]] 

# AUC for score 101 credits (applying score 3 model on credits scored with score 101, 90 days default flag)

pred_ROCR_3_101_90days <- ROCR::prediction(FinalScore3$PD3, FinalScore3$default90)

auc_ROCR_3_101_90days <- performance(pred_ROCR_3_101_90days, measure = "auc")
auc_ROCR_3_101_90days <- auc_ROCR_3_101_90days@y.values[[1]] 

# AUC for score 3 credits (applying score 101 model on credits scored with score 3, 60 days default flag)

pred_ROCR_3_101_60days <- ROCR::prediction(FinalScore3$PD3, FinalScore3$default60)

auc_ROCR_3_101_60days <- performance(pred_ROCR_3_101_60days, measure = "auc")
auc_ROCR_3_101_60days <- auc_ROCR_3_101_60days@y.values[[1]] 

#AUC for new credits (90 days default flag)

pred_ROCR_3_new_90Days <- ROCR::prediction(Final_new$PD3, Final_new$default90)

auc_ROCR_3_new_90days <- performance(pred_ROCR_3_new_90Days, measure = "auc")
auc_ROCR_3_new_90days <- auc_ROCR_3_new_90days@y.values[[1]] 

#AUC for new credits (60 days default flag)

pred_ROCR_3_new_60days <- ROCR::prediction(Final_new$PD3, Final_new$default60)

auc_ROCR_3_new_60days <- performance(pred_ROCR_3_new_60days, measure = "auc")
auc_ROCR_3_new_60days <- auc_ROCR_3_new_60days@y.values[[1]] 

#AUC for EasyCredit product (90 days default flag)

pred_ROCR_3_EasyCredit_90days <- ROCR::prediction(Final_EasyCredit$PD3, Final_EasyCredit$default90)

auc_ROCR_3_EasyCredit_90days <- performance(pred_ROCR_3_EasyCredit_90days, measure = "auc")
auc_ROCR_3_EasyCredit_90days <- auc_ROCR_3_EasyCredit_90days@y.values[[1]] 

#AUC for EasyCredit product (60 days default flag)

pred_ROCR_3_EasyCredit_60days <- ROCR::prediction(Final_EasyCredit$PD3, Final_EasyCredit$default60)

auc_ROCR_3_EasyCredit_60days <- performance(pred_ROCR_3_EasyCredit_60days, measure = "auc")
auc_ROCR_3_EasyCredit_60days <- auc_ROCR_3_EasyCredit_60days@y.values[[1]] 

#AUC for EasyMonth product (90 days default flag)

pred_ROCR_3_EasyMonth_90days <- ROCR::prediction(Final_EasyMonth$PD3, Final_EasyMonth$default90)

auc_ROCR_3_EasyMonth_90days <- performance(pred_ROCR_3_EasyMonth_90days, measure = "auc")
auc_ROCR_3_EasyMonth_90days <- auc_ROCR_3_EasyMonth_90days@y.values[[1]] 

#AUC for EasyMonth product (60 days default flag)

pred_ROCR_3_EasyMonth_60days <- ROCR::prediction(Final_EasyMonth$PD3, Final_EasyMonth$default60)

auc_ROCR_3_EasyMonth_60days <- performance(pred_ROCR_3_EasyMonth_60days, measure = "auc")
auc_ROCR_3_EasyMonth_60days <- auc_ROCR_3_EasyMonth_60days@y.values[[1]] 


#AUC for Pensioner product (90 days default flag)

pred_ROCR_3_Pensioner_90days <- ROCR::prediction(Final_Pensioner$PD3, Final_Pensioner$default90)

auc_ROCR_3_Pensioner_90days <- performance(pred_ROCR_3_Pensioner_90days, measure = "auc")
auc_ROCR_3_Pensioner_90days <- auc_ROCR_3_Pensioner_90days@y.values[[1]] 

#AUC for Pensioner product (60 days default flag)

pred_ROCR_3_Pensioner_60days <- ROCR::prediction(Final_Pensioner$PD3, Final_Pensioner$default60)

auc_ROCR_3_Pensioner_60days <- performance(pred_ROCR_3_Pensioner_60days, measure = "auc")
auc_ROCR_3_Pensioner_60days <- auc_ROCR_3_Pensioner_60days@y.values[[1]] 

#Creating a dataframe with all AUC results

AUC_Score_101 <- c(auc_ROCR_101, auc_ROCR_101_60days, auc_ROCR_101_90days, auc_ROCR_101_all_60days, auc_ROCR_101_all_90days, auc_ROCR_101_3_60days, auc_ROCR_101_3_90days, auc_ROCR_101_new_60days, auc_ROCR_101_new_90days, auc_ROCR_101_EasyCredit_60days, auc_ROCR_101_EasyCredit_90days, auc_ROCR_101_EasyMonth_60days, auc_ROCR_101_EasyMonth_90days, auc_ROCR_101_Pensioner_60days, auc_ROCR_101_Pensioner_90days)
AUC_Score_3 <- c(auc_ROCR_3, auc_ROCR_3_60days, auc_ROCR_3_90days, auc_ROCR_3_all_60days, auc_ROCR_3_all_90days, auc_ROCR_3_101_60days, auc_ROCR_3_101_90days, auc_ROCR_3_new_60days, auc_ROCR_3_new_90days, auc_ROCR_3_EasyCredit_60days, auc_ROCR_3_EasyCredit_90days, auc_ROCR_3_EasyMonth_60days, auc_ROCR_3_EasyMonth_90days, auc_ROCR_3_Pensioner_60days, auc_ROCR_3_Pensioner_90days)
AUC_results <- data.frame(rows_labels,AUC_Score_101, AUC_Score_3)
rows_labels <- c("AUC (system scoring)",  "AUC calc scoring (60days)",  "AUC calc scoring (90days)", "AUC all credits (60days)", "AUC all credits (90days)", "AUC model on the other scoring credits (60days)", "AUC model on the other scoring credits (90days)", "AUC new credits (60 days)", "AUC new credits (90 days)", "AUC EasyCredit (60 days)", "AUC EasyCredit (90 days)", "AUC EasyMonth (60days)", "AUC EasyMonth (90days)", "AUC Pensioner (60 days)", "AUC Pensioner (90 days)")

AUC_results <- data.frame(rows_labels,AUC_Score_101, AUC_Score_3)
AUC_results$AUC_Score_101 <- round(AUC_results$AUC_Score_101, 3)
AUC_results$AUC_Score_3 <- round(AUC_results$AUC_Score_3, 3)

write.xlsx(AUC_results, "Z:/Analysis/1.Projects/120. Cutt off Easy")
