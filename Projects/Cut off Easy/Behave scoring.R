behave_scoring <- data2 %>%
  filter(ProductName %in% c("EasyCredit", "EasyMax", "EasyMonth", "EasyVIP", "Pensioner"))

behave_scoring <- behave_scoring %>%
  left_join(Last_portfolio1 %>% select(CodeContract, CreditAccountSK), by = "CreditAccountSK") %>%
  left_join(Harold_Easy_BG %>% select(CreditPeriodCountName, ClientEducation, TypeOfContract, StaffType, Automobiles, GetSalaryType, TotalLengthOfService, TypeOfHousing, ClientFamilyStatus, CreditPurpose, WorkExperienceCurrentWork,PartnerConnectionType, ClientSex,YearsOnCurrentAddress, ClientFamilySize, CodeContract), by = "CodeContract")

behave_scoring <- behave_scoring %>%
  left_join(NOIInsurance %>% select(NOIInsurance, CodeContract), by = "CodeContract") %>%
  left_join(clients_birth_dates, by = "ClientSK")


All_Excel_files <- list.files(path = "Z:\\Analysis\\3.Data", full.names = TRUE, recursive = TRUE)
All_Excel_files <- All_Excel_files[grepl("E004_Credit_Progress_BG_", All_Excel_files)]

All_Excel_files_1 <- All_Excel_files[c(49:60)]
All_Excel_files_2 <- All_Excel_files[c(61:70)]

library(data.table)
import_function <- function(arg){
  output <- fread(arg, select = c("CalcDate", "ClientEGN","SubStatus", "PayedTotal", "SumToGetBack", "RestTotal", "CreditCode", "CreditBeginDate", "CreditFirstDateToPay", "CreditProduct", "Weeks", "Vnoska", "CompleteDate", "CurrentDelay", "MaxDelayP", "SumaP", "Ostavashti", "Padejirali"), na.strings = c("NULL", "NA", ""), encoding = "UTF-8")
  print(arg)
  return(output)
}

All_Credit_Progress1 <- lapply(All_Excel_files_1, import_function)


All_Credit_Progress1 <- lapply(All_Credit_Progress1, function(x) filter(x, SubStatus == "Усвоен"))
save(All_Credit_Progress1, file = ".\\Output Data\\All_Credit_Progress1.RData")


All_Credit_Progress2 <- lapply(All_Excel_files_2, import_function)
All_Credit_Progress2 <- lapply(All_Credit_Progress2, function(x) filter(x, SubStatus == "Усвоен"))
save(All_Credit_Progress2, file = ".\\Output Data\\All_Credit_Progress2.RData")


all_credit_progress2 <- do.call(rbind, All_Credit_Progress2)
all_credit_progress1 <- do.call(rbind, All_Credit_Progress1)


All_Credit_Progress <- rbind(all_credit_progress1, all_credit_progress2)
save(All_Credit_Progress, file = ".\\Output Data\\All_Credit_Progress.RData")


All_Credit_Progress$CreditBeginDate <- as.Date(All_Credit_Progress$CreditBeginDate)
All_Credit_Progress$CreditFirstDateToPay <- as.Date(All_Credit_Progress$CreditFirstDateToPay)
All_Credit_Progress$CalcDate <- as.Date(All_Credit_Progress$CalcDate)
All_Credit_Progress$Weeks <- as.numeric(All_Credit_Progress$Weeks)

All_Credit_Progress <- All_Credit_Progress %>%
  filter(CreditBeginDate >= as.Date('2018-07-01'))

#Ratio of Paid Amount till observation point and total due till obs. point

All_Credit_Progress$Vnoska = as.numeric(All_Credit_Progress$Vnoska)
All_Credit_Progress = mutate(All_Credit_Progress, TotalDue_OBS_P=Padejirali*Vnoska)
All_Credit_Progress = mutate(All_Credit_Progress, Perc_PaidAmount_var = round(PayedTotal/TotalDue_OBS_P,2))

#Passed - Padejirali vnoski/Total vnoski

All_Credit_Progress$Passed <- All_Credit_Progress$Padejirali/All_Credit_Progress$Weeks

#Current delay fluctuation

All_Credit_Progress <- All_Credit_Progress %>%
  arrange(CreditCode,desc(CalcDate)) %>%
  group_by(CreditCode) %>%
  mutate(CurrentDelayFluctuation = CurrentDelay - lag(CurrentDelay, default = 0))

#Count Previous Bad Credits

All_Credit_Progress$MaxDelayP = as.numeric(All_Credit_Progress$MaxDelayP)

test <- All_Credit_Progress %>%
  select(CalcDate,ClientEGN, CreditCode, MaxDelayP) %>%
  group_by(ClientEGN, CreditCode) %>%
  mutate(bads = ifelse(MaxDelayP > 90, 1, 0)) %>%
  filter(row_number()==1) %>%
  group_by(ClientEGN) %>%
  summarize(BadCounts =sum(bads))


All_Credit_Progress <- All_Credit_Progress %>%
  left_join(test, by = "ClientEGN")
  

#Previous Credits

All_Credit_Progress <- All_Credit_Progress %>%
  left_join(Last_portfolio1 %>% select(PreviousCreditsCount,CodeContract), by = c("CreditCode" = "CodeContract"))

behave_scoring <- behave_scoring %>%
  left_join(All_Credit_Progress %>% select("MaxDelayP","CalcDate","Perc_PaidAmount_var","Passed", "CurrentDelayFluctuation", "BadCounts","PreviousCreditsCount", "CreditCode"), by = c("CodeContract" = "CreditCode")) %>%
  group_by(ClientSK,CreditAccountSK) %>%
  filter(row_number() == 1)


#Ratio Paid amount until observation

behave_scoring$Perc_PaidAmount_var1 <- ifelse(behave_scoring$Perc_PaidAmount_var >= 0.68 & behave_scoring$Perc_PaidAmount_var <= 0.80,-0.4337,
                                         ifelse(behave_scoring$Perc_PaidAmount_var >= 0.81 & behave_scoring$Perc_PaidAmount_var <= 0.89, -0.93467,
                                                ifelse(behave_scoring$Perc_PaidAmount_var >= 0.90 & behave_scoring$Perc_PaidAmount_var <= 1, -1.45906,
                                                       ifelse(behave_scoring$Perc_PaidAmount_var >= 1.01, -1.77958,
                                                              ifelse(behave_scoring$Perc_PaidAmount_var == -99, -0.93467,
                                                                     ifelse(behave_scoring$Perc_PaidAmount_var == -999, -0.93467,0))))))
behave_scoring$Perc_PaidAmount_var1[is.na(behave_scoring$Perc_PaidAmount_var)] <- 0


# Passed

behave_scoring$Passed1 <- ifelse(behave_scoring$Passed >= 0.30 & behave_scoring$Passed <= 0.42,-0.65949,
                            ifelse(behave_scoring$Passed >= 0.43 & behave_scoring$Passed <= 0.58, -1.10634,
                                   ifelse(behave_scoring$Passed >= 0.59 & behave_scoring$Passed <= 1, -1.41085, 0)))

behave_scoring$Passed1[is.na(behave_scoring$Passed)] <- 0


# MaxDelay

behave_scoring$MaxDelayP1 <- ifelse(behave_scoring$MaxDelayP >= 4 & behave_scoring$MaxDelayP <= 10, 0.31081,
                               ifelse(behave_scoring$MaxDelayP >= 11 & behave_scoring$MaxDelayP <= 17, 0.51107,
                                      ifelse(behave_scoring$MaxDelayP >= 18, 1.06919, 0)))

behave_scoring$MaxDelayP1[is.na(behave_scoring$MaxDelayP)] <- 0


# Current Delay Fluctuation

behave_scoring$CurrentDelayFluctuation1 <- ifelse(behave_scoring$CurrentDelayFluctuation >= 2 & behave_scoring$CurrentDelayFluctuation <= 8, 0.29203,
                                             ifelse(behave_scoring$CurrentDelayFluctuation >= 9 & behave_scoring$CurrentDelayFluctuation <=15, 0.32646,
                                                    ifelse(behave_scoring$CurrentDelayFluctuation >= 16 & behave_scoring$CurrentDelayFluctuation <= 22, 0.57872,
                                                           ifelse(behave_scoring$CurrentDelayFluctuation >= 23,0.75941, 0))))

behave_scoring$CurrentDelayFluctuation1[is.na(behave_scoring$CurrentDelayFluctuation)] <- 0

#Count Previous Credits

behave_scoring$PreviousCreditsCount1 <- ifelse(behave_scoring$PreviousCreditsCount == 0, 0.30483,
                                          ifelse(behave_scoring$PreviousCreditsCount == 1, 0,
                                                 ifelse(behave_scoring$PreviousCreditsCount == 2, -0.12499,
                                                        ifelse(behave_scoring$PreviousCreditsCount == 3, -0.31961,
                                                               ifelse(behave_scoring$PreviousCreditsCount == 4, -0.45149,
                                                                      ifelse(behave_scoring$PreviousCreditsCount >= 5 & behave_scoring$PreviousCreditsCount <= 6, -0.57752,
                                                                             ifelse(behave_scoring$PreviousCreditsCount == 7, -0.8047,
                                                                                    ifelse(behave_scoring$PreviousCreditsCount == 8, -0.71439,
                                                                                           ifelse(behave_scoring$PreviousCreditsCount >= 9 & behave_scoring$PreviousCreditsCount >= 13, -0.80655, -1.02696)))))))))
behave_scoring$PreviousCreditsCount1[is.na(behave_scoring$PreviousCreditsCount)] <- 0

# Count Previous Bads

behave_scoring$BadCounts1 <- ifelse(behave_scoring$BadCounts >= 1, 0, -0.5712)
behave_scoring$BadCounts1[is.na(behave_scoring$BadCounts)] <- -0.5712

#Get Salary Type

behave_scoring$GetSalaryType1 <- ifelse(behave_scoring$GetSalaryType %in% c("По сметка в банка"), -0.07856, 0)


#Client Education

behave_scoring$ClientEducation1 <- ifelse(behave_scoring$ClientEducation %in% c("Висше",
                                                                      "Висше доктор и др. академични степени",
                                                                      "Висше магистър",
                                                                      "Висше бакалавър",
                                                                      "Полувисше / Специалист"), -0.65956,
                                     ifelse(behave_scoring$ClientEducation %in% c("Средно"),-0.38419,0))
                                     
                                     
                                     
# Type of Contract

behave_scoring$TypeOfContract1 <- ifelse(behave_scoring$TypeOfContract %in% c("Пенсионер по стаж и възраст неработещ",
                                                                    "Пенсионер по стаж и възраст работещ",
                                                                    "Пенсионер по болест работещ",
                                                                    "Пенсионер по болест неработещ",
                                                                    "Безсрочен трудов договор",
                                                                    "Договор за управление",
                                                                    "Допълнително споразумение"), -0.33832, 0)
                                    
                                    
#Client sex

behave_scoring$ClientSex1 <- ifelse(behave_scoring$ClientSex %in% c("Мъж"),0.11793,0)
                                    
                                    
#PD

behave_scoring$PD = 0.40015 + behave_scoring$Perc_PaidAmount_var1 + behave_scoring$Passed1 + behave_scoring$MaxDelayP1 + behave_scoring$CurrentDelayFluctuation1 + behave_scoring$PreviousCreditsCount1 + behave_scoring$GetSalaryType1 + behave_scoring$ClientEducation1 + behave_scoring$TypeOfContract1 + behave_scoring$ClientSex1 + behave_scoring$BadCounts1

behave_scoring$PD = exp(behave_scoring$PD)/(1+exp(behave_scoring$PD))

behave_scoring$Score <-round(behave_scoring$PD*1000, 0)                                    


