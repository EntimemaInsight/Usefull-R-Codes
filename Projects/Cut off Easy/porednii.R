poreden <- Last_portfolio1 %>%
  select(Sum,CodeContract,CreditAccountSK, ClientSK, Scoring, ScoringType, CreditBeginDate, MaxDelay,PreviousCreditsCount, ProductName) %>%
  left_join(Harold_Easy_BG %>% select(ClientEGN,CreditPeriodCountName, ClientEducation, TypeOfContract, StaffType, Automobiles, GetSalaryType, TotalLengthOfService, TypeOfHousing, ClientFamilyStatus, CreditPurpose, WorkExperienceCurrentWork,PartnerConnectionType, ClientSex,YearsOnCurrentAddress, ClientFamilySize, CodeContract), by = "CodeContract")%>%
  left_join(NOIInsurance %>% select(NOIInsurance, CodeContract), by = "CodeContract") %>%
  left_join(clients_birth_dates, by = "ClientSK")

poreden<- poreden[!grepl("9999", poreden$BirthDate),]

poreden<- poreden[!grepl("2045", poreden$BirthDate),]

poreden$BirthDate <- as.Date(poreden$BirthDate)

poreden <- poreden[!poreden$BirthDate > poreden$CreditBeginDate,]

poreden <- poreden[!is.na(poreden$BirthDate),]

library(eeptools)

poreden <- poreden %>%
  mutate(age = age_calc(BirthDate, enddate = CreditBeginDate, units = "years", precise = TRUE))


#age
poreden$age <- round(poreden$age)
poreden$age1 <- ifelse(poreden$age <= 20, 42, 
                         ifelse(poreden$age >= 21 & poreden$age <=30, 22,
                                ifelse(poreden$age >= 31 & poreden$age <= 45,0,
                                       ifelse(poreden$age >= 46 & poreden$age <=51, -13,
                                              ifelse(poreden$age >= 52 & poreden$age <= 58, -28, -36)))))

#Automobiles 

poreden$Automobiles1 <- ifelse(poreden$Automobiles <= 0, 13, -53)

#Client Family Size

poreden$ClientFamilySize1 <- ifelse(poreden$ClientFamilySize >= 2 & poreden$ClientFamilySize <= 4, -13, 36 )

#Work Experience Current Work

poreden$WorkExperienceCurrentWork1 <- ifelse(poreden$WorkExperienceCurrentWork <= 1, 17,
                                               ifelse(poreden$WorkExperienceCurrentWork >= 2 & poreden$WorkExperienceCurrentWork <=5, 0, -22))

#Sum

poreden$Sum1 <- ifelse(poreden$Sum <= 150, -30,
                         ifelse(poreden$Sum >= 151 & poreden$Sum <= 400, 0, 3))

#Client Education

poreden$ClientEducation1 <- ifelse(poreden$ClientEducation %in% c("Висше","Висше бакалавър","Висше доктор и др. академични степени","Висше магистър"), -134,
                                     ifelse(poreden$ClientEducation %in% c("Полувисше / Специалист"),-85,
                                            ifelse(poreden$ClientEducation %in% c("Средно"),-23,
                                                   ifelse(poreden$ClientEducation %in% c("Основно"), 62, 105 ))))


#Client Family Status

poreden$ClientFamilyStatus1 <- ifelse(poreden$ClientFamilyStatus %in% c("Женен / Омъжена"), -39,
                                        ifelse(poreden$ClientFamilyStatus %in% c("Вдовец / Вдовица"), -28, 14))

#Get Salary Type 

poreden$GetSalaryType1 <- ifelse(poreden$GetSalaryType %in% c("По сметка в банка"), -18, 9)


#Type of Contract

poreden$TypeOfContract1 <- ifelse(poreden$TypeOfContract %in% c("Пенсионер по стаж и възраст неработещ","Пенсионер по стаж и възраст работещ"), -56,
                                    ifelse(poreden$TypeOfContract %in% c("Безсрочен трудов договор","Договор за управление"), -20, 32))



poreden$WeeksByProduct <- ifelse(poreden$ProductName == "EasyCredit" & poreden$CreditPeriodCountName == 43,58,
                                   ifelse(poreden$ProductName == "EasyCredit" & poreden$CreditPeriodCountName == 34,58,
                                          ifelse(poreden$ProductName == "EasyCredit" & poreden$CreditPeriodCountName == 27, 58,
                                                 ifelse(poreden$ProductName == "EasyCredit" & poreden$CreditPeriodCountName == 21,36,
                                                        ifelse(poreden$ProductName == "EasyCredit" & poreden$CreditPeriodCountName == 16, 11,
                                                               ifelse(poreden$ProductName == "EasyCredit" & poreden$CreditPeriodCountName == 12, -31,
                                                                      ifelse(poreden$ProductName == "EasyCredit" & poreden$CreditPeriodCountName == 8, -185,
                                                                             ifelse(poreden$ProductName == "EasyMonth" & poreden$CreditPeriodCountName == 8, 58,
                                                                                    ifelse(poreden$ProductName == "EasyMonth" & poreden$CreditPeriodCountName == 6, 36,
                                                                                           ifelse(poreden$ProductName == "EasyMonth" & poreden$CreditPeriodCountName == 4, 11,
                                                                                                  ifelse(poreden$ProductName == "Pensioner" & poreden$CreditPeriodCountName == 12, 36,
                                                                                                         ifelse(poreden$ProductName == "Pensioner" & poreden$CreditPeriodCountName == 10, 11,
                                                                                                                ifelse(poreden$ProductName == "Pensioner" & poreden$CreditPeriodCountName == 8,11,
                                                                                                                       ifelse(poreden$ProductName == "Pensioner" & poreden$CreditPeriodCountName == 6, -61,
                                                                                                                              ifelse(poreden$ProductName == "Pensioner" & poreden$CreditPeriodCountName == 4, -133,
                                                                                                                                     ifelse(poreden$ProductName == "Pensioner" & poreden$CreditPeriodCountName == 3, -197, NA))))))))))))))))
poreden <-poreden %>%
  filter(!is.na(WeeksByProduct))


poreden <- poreden %>%
  left_join(Last_portfolio1 %>% select(MaxDelay, CreditAccountSK, PreviousCreditsCount), by = "CreditAccountSK") 

# LagMaxDelayP variable

#install.packages("DataCombine")
library(DataCombine)

poreden <- arrange(poreden, ClientSK, as.Date(CreditBeginDate))
poreden <- slide(poreden, Var ="MaxDelay.x", TimeVar = "CreditBeginDate", GroupVar ="ClientSK", NewVar = "LagMaxDelayP", slideBy = -1) 
poreden$LagMaxDelayP <- replace(poreden$LagMaxDelayP, is.na(poreden$LagMaxDelayP), 0) # replace NA`s with zeros
sum(is.na(poreden$LagMaxDelayP)) # 0 - check

poreden$LagMaxDelayP1 <- ifelse(poreden$LagMaxDelayP <= 5, -77,
                                  ifelse(poreden$LagMaxDelayP >= 6 & poreden$LagMaxDelayP <= 8, -42,
                                         ifelse(poreden$LagMaxDelayP >= 9 & poreden$LagMaxDelayP <= 24, 3,
                                                ifelse(poreden$LagMaxDelayP >= 25 & poreden$LagMaxDelayP <= 50, 44, 96))))  

#Count variable

poreden$Count <- ifelse(poreden$PreviousCreditsCount.x == 1, 22,
                          ifelse(poreden$PreviousCreditsCount.x >= 2 & poreden$PreviousCreditsCount.x <= 3, 2,
                                 ifelse(poreden$PreviousCreditsCount.x >= 4 & poreden$PreviousCreditsCount.x <= 5, -18,
                                        ifelse(poreden$PreviousCreditsCount.x >=6 & poreden$PreviousCreditsCount.x <= 7, -32, -61))))


poreden$Score <- 237 + poreden$age1 + poreden$Automobiles1 + poreden$ClientFamilySize1 + poreden$WorkExperienceCurrentWork1 + poreden$Sum1 + poreden$ClientEducation1 + poreden$ClientFamilyStatus1 + poreden$GetSalaryType1 + poreden$TypeOfContract1 + poreden$WeeksByProduct + poreden$LagMaxDelayP1 + poreden$Count
poreden$PD <- round(0.0012760306917*poreden$Score - 0.0622822414375, 2)

poreden$PD <- ifelse(poreden$PD < 0, 0, poreden$PD)


testtt <- poreden %>%
  filter(ScoringType == 2)
  
