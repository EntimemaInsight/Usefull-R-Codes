scoring_2 <- data2 %>%
  filter(ScoringType %in% c(2,-1,5,3,0),
         Poreden == 1,
         ProductName %in% c("EasyCredit", "EasyMonth", "Pensioner"))


scoring_2 <- scoring_2 %>%
  left_join(Last_portfolio1 %>% select(CodeContract, CreditAccountSK), by = "CreditAccountSK") %>%
  left_join(Harold_Easy_BG %>% select(CreditPeriodCountName, ClientEducation, TypeOfContract, StaffType, Automobiles, GetSalaryType, TotalLengthOfService, TypeOfHousing, ClientFamilyStatus, CreditPurpose, WorkExperienceCurrentWork,PartnerConnectionType, ClientSex,YearsOnCurrentAddress, ClientFamilySize, CodeContract), by = "CodeContract")%>%
  left_join(NOIInsurance %>% select(NOIInsurance, CodeContract), by = "CodeContract") %>%
  left_join(clients_birth_dates, by = "ClientSK")

scoring_2$BirthDate <- as.Date(scoring_2$BirthDate)

library(eeptools)

scoring_2 <- scoring_2 %>%
  mutate(age = age_calc(BirthDate, enddate = CreditBeginDate, units = "years", precise = TRUE))


#age

scoring_2$age1 <- ifelse(scoring_2$age <= 20, 42, 
                                   ifelse(scoring_2$age >= 21 & scoring_2$age <=30, 22,
                                          ifelse(scoring_2$age >= 31 & scoring_2$age <= 45,0,
                                                 ifelse(scoring_2$age >= 46 & scoring_2$age <=51, -13,
                                                        ifelse(scoring_2$age >= 52 & scoring_2$age <= 58, -28, -36)))))

#Automobiles 

scoring_2$Automobiles1 <- ifelse(scoring_2$Automobiles <= 0, 13, -53)

#Client Family Size

scoring_2$ClientFamilySize1 <- ifelse(scoring_2$ClientFamilySize >= 2 & scoring_2$ClientFamilySize <= 4, -13, 36 )

#Work Experience Current Work

scoring_2$WorkExperienceCurrentWork1 <- ifelse(scoring_2$WorkExperienceCurrentWork <= 1, 17,
                                                         ifelse(scoring_2$WorkExperienceCurrentWork >= 2 & scoring_2$WorkExperienceCurrentWork <=5, 0, -22))

#Sum

scoring_2$Sum1 <- ifelse(scoring_2$Sum <= 150, -30,
                                   ifelse(scoring_2$Sum >= 151 & scoring_2$Sum <= 400, 0, 3))

#Client Education

scoring_2$ClientEducation1 <- ifelse(scoring_2$ClientEducation %in% c("Висше","Висше бакалавър","Висше доктор и др. академични степени","Висше магистър"), -134,
                                               ifelse(scoring_2$ClientEducation %in% c("Полувисше / Специалист"),-85,
                                                      ifelse(scoring_2$ClientEducation %in% c("Средно"),-23,
                                                             ifelse(scoring_2$ClientEducation %in% c("Основно"), 62, 105 ))))


#Client Family Status

scoring_2$ClientFamilyStatus1 <- ifelse(scoring_2$ClientFamilyStatus %in% c("Женен / Омъжена"), -39,
                                                  ifelse(scoring_2$ClientFamilyStatus %in% c("Вдовец / Вдовица"), -28, 14))

#Get Salary Type 

scoring_2$GetSalaryType1 <- ifelse(scoring_2$GetSalaryType %in% c("По сметка в банка"), -18, 9)


#Type of Contract

scoring_2$TypeOfContract1 <- ifelse(scoring_2$TypeOfContract %in% c("Пенсионер по стаж и възраст неработещ","Пенсионер по стаж и възраст работещ"), -56,
                                              ifelse(scoring_2$TypeOfContract %in% c("Безсрочен трудов договор","Договор за управление"), -20, 32))



scoring_2$WeeksByProduct <- ifelse(scoring_2$ProductName == "EasyCredit" & scoring_2$CreditPeriodCountName == 43,58,
                                             ifelse(scoring_2$ProductName == "EasyCredit" & scoring_2$CreditPeriodCountName == 34,58,
                                                    ifelse(scoring_2$ProductName == "EasyCredit" & scoring_2$CreditPeriodCountName == 27, 58,
                                                           ifelse(scoring_2$ProductName == "EasyCredit" & scoring_2$CreditPeriodCountName == 21,36,
                                                                  ifelse(scoring_2$ProductName == "EasyCredit" & scoring_2$CreditPeriodCountName == 16, 11,
                                                                         ifelse(scoring_2$ProductName == "EasyCredit" & scoring_2$CreditPeriodCountName == 12, -31,
                                                                                ifelse(scoring_2$ProductName == "EasyCredit" & scoring_2$CreditPeriodCountName == 8, -185,
                                                                                       ifelse(scoring_2$ProductName == "EasyMonth" & scoring_2$CreditPeriodCountName == 8, 58,
                                                                                              ifelse(scoring_2$ProductName == "EasyMonth" & scoring_2$CreditPeriodCountName == 6, 36,
                                                                                                     ifelse(scoring_2$ProductName == "EasyMonth" & scoring_2$CreditPeriodCountName == 4, 11,
                                                                                                            ifelse(scoring_2$ProductName == "Pensioner" & scoring_2$CreditPeriodCountName == 12, 36,
                                                                                                                   ifelse(scoring_2$ProductName == "Pensioner" & scoring_2$CreditPeriodCountName == 10, 11,
                                                                                                                       ifelse(scoring_2$ProductName == "Pensioner" & scoring_2$CreditPeriodCountName == 8,11,
                                                                                                                                 ifelse(scoring_2$ProductName == "Pensioner" & scoring_2$CreditPeriodCountName == 6, -61,
                                                                                                                                        ifelse(scoring_2$ProductName == "Pensioner" & scoring_2$CreditPeriodCountName == 4, -133,
                                                                                                                                               ifelse(scoring_2$ProductName == "Pensioner" & scoring_2$CreditPeriodCountName == 3, -197, NA))))))))))))))))
scoring_2 <-scoring_2 %>%
  filter(!is.na(WeeksByProduct))


scoring_2 <- scoring_2 %>%
  left_join(Last_portfolio1 %>% select(MaxDelay, CreditAccountSK, PreviousCreditsCount), by = "CreditAccountSK") 

# LagMaxDelayP variable

#install.packages("DataCombine")
library(DataCombine)

scoring_2 <- arrange(scoring_2, ClientSK, as.Date(CreditBeginDate))
scoring_2 <- slide(scoring_2, Var ="MaxDelay", TimeVar = "CreditBeginDate", GroupVar ="ClientSK", NewVar = "LagMaxDelayP", slideBy = -1) 
scoring_2$LagMaxDelayP <- replace(scoring_2$LagMaxDelayP, is.na(scoring_2$LagMaxDelayP), 0) # replace NA`s with zeros
sum(is.na(scoring_2$LagMaxDelayP)) # 0 - check

scoring_2$LagMaxDelayP1 <- ifelse(scoring_2$LagMaxDelayP <= 5, -77,
                                  ifelse(scoring_2$LagMaxDelayP >= 6 & scoring_2$LagMaxDelayP <= 8, -42,
                                         ifelse(scoring_2$LagMaxDelayP >= 9 & scoring_2$LagMaxDelayP <= 24, 3,
                                                ifelse(scoring_2$LagMaxDelayP >= 25 & scoring_2$LagMaxDelayP <= 50, 44, 96))))  

#Count variable

scoring_2$Count <- ifelse(scoring_2$PreviousCreditsCount == 1, 22,
                          ifelse(scoring_2$PreviousCreditsCount >= 2 & scoring_2$PreviousCreditsCount <= 3, 2,
                                 ifelse(scoring_2$PreviousCreditsCount >= 4 & scoring_2$PreviousCreditsCount <= 5, -18,
                                        ifelse(scoring_2$PreviousCreditsCount >=6 & scoring_2$PreviousCreditsCount <= 7, -32, -61))))


scoring_2$Score <- 237 + scoring_2$age1 + scoring_2$Automobiles1 + scoring_2$ClientFamilySize1 + scoring_2$WorkExperienceCurrentWork1 + scoring_2$Sum1 + scoring_2$ClientEducation1 + scoring_2$ClientFamilyStatus1 + scoring_2$GetSalaryType1 + scoring_2$TypeOfContract1 + scoring_2$WeeksByProduct + scoring_2$LagMaxDelayP1 + scoring_2$Count
scoring_2$PD <- round(0.0012760306917*scoring_2$Score - 0.0622822414375, 2)

scoring_2$PD <- ifelse(scoring_2$PD < 0, 0, scoring_2$PD)


scoring_2 <- scoring_2 %>%
  left_join(poreden %>% select(CreditAccountSK, PD), by = "CreditAccountSK")


test <- scoring_2 %>%
  select(CreditAccountSK, ScoringType, Scoring, PD.x, PD.y) %>%
  filter(ScoringType == 2)

test2 <- scoring_2 %>%
  filter(CreditAccountSK == "2250300")
