library(Hmisc)
library(tidyverse)
library(RODBC)
library(lubridate)
library(caret)
library(dplyr)
library(future.apply)
library(scorecard)
library(eeptools)
library(InformationValue)

#Selecting the variables needed

Group_var <- Final %>%
  select(CreditCode,ClientEducation, TypeOfContract, StaffType, Automobiles, GetSalaryType, TotalLengthOfService, TypeOfHousing, ClientFamilyStatus, CreditPurpose, WorkExperienceCurrentWork, ClientSex, YearsOnCurrentAddress, ClientFamilySize, PartnerConnectionType, CountPreviousCredits, Age,CreditBeginDate, default90)%>%
  mutate(ClientEducation_cat = case_when(ClientEducation %in%  c("Висше","Висше бакалавър","Висше доктор и др. академични степени","Висше магистър", "Полувисше / Специалист", "Средно") ~ "Висше/Средно",
                                         ClientEducation %in% c("Без образование", "Начално", "Неизвестно", "Друго", "Основно") ~ "Основно/Начално/Без образование"),
         TypeOfContract_cat = case_when(TypeOfContract %in% c("Безсрочен трудов договор") ~ "Безсрочен трудов договор",
                                        TRUE ~ "Друго"),
         StaffType_cat = case_when(StaffType %in% c("Нискоквалифициран и неквалифициран персонал") ~ "Нискоквалифициран и неквалифициран персонал",
                                   TRUE ~ "Kвалифициран персонал"),
         Automobiles_cat = case_when(Automobiles == 0 ~ "0",
                                     Automobiles > 0 ~ "Повече от 0"),
         GetSalaryType_cat = case_when(GetSalaryType %in% c("По сметка в банка") ~ "По сметка в банка",
                                       TRUE ~ "Друго"),
         TotalLengthOfService_cat = case_when(TotalLengthOfService < 19 ~ "Под 19",
                                              TRUE ~ "Над 19"),
         TypeOfHousing_cat = case_when(TypeOfHousing %in% c("Собствено жилище") ~ "Собствено жилище",
                                       is.na(TypeOfHousing) ~ "Друго",
                                       TRUE ~ "Друго"), 
         ClientFamilyStatus_cat = case_when(ClientFamilyStatus %in% c("Женен / Омъжена") ~ "Женен / Омъжена",
                                            is.na(ClientFamilyStatus) ~ "Неженен / Неомъжена",
                                            TRUE ~ "Неженен / Неомъжена"),
         WorkExperienceCurrentWork_cat = case_when(WorkExperienceCurrentWork <= 3 ~ "Под 3",
                                                   WorkExperienceCurrentWork > 3 ~ "Над 3"),
         ClientSex_cat = case_when(ClientSex %in% c("Жена") ~ "Жена",
                                   ClientSex %in% c("Мъж") ~ "Мъж"),
         YearsOnCurrentAddress_cat = case_when(YearsOnCurrentAddress < 8 ~ "Под 8",
                                               YearsOnCurrentAddress >= 8 ~ "Над 8"),
         ClientFamilySize_cat = case_when(ClientFamilySize > 4 ~ "Над 4",
                                          ClientFamilySize <= 4 ~ "Под 4"),
         PartnerConnectionType_cat = case_when(PartnerConnectionType %in% c("Братовчед/ Братовчедка", "Внук/Внучка", "Колега", "Неизвестен", "Партньор", "Приятел/Познат", "Семейни начала", "Съпруг / Съпруга") ~ "Семейство",
                                               is.na(PartnerConnectionType) ~ "Семейство",
                                               TRUE ~ "Друго"),
         CountPreviousCredits_cat = case_when(CountPreviousCredits == 0 ~ "Няма",
                                              CountPreviousCredits > 0 ~ "Има"),
         Age_cat = case_when(Age <= 22 ~ "Под 22",
                             Age > 22 ~ "Над 22"))

First_Period <- Group_var %>%
  filter(CreditBeginDate >= as.Date("2018-09-01"), CreditBeginDate <= as.Date("2018-11-01"))

variables_list <- names(Group_var[,20:34])

iv_ks_function <- function(arg1){

  iv <- iv(First_Period[ , c(arg1, "default90")], y = 'default90')
  bins <- woebin(First_Period[ , c(arg1, "default90")], y = 'default90')

  Default_Rate_90 <- First_Period %>%
    group_by(First_Period[,arg1]) %>%
    summarise(Numbers = n(),
            Number_Default = sum(default90, na.rm = T)) %>%
    ungroup() %>%
    mutate(TotalSex = sum(Numbers),
         DefaultRate = Number_Default / Numbers)

  ks = ks_stat(actuals=First_Period[,arg1], predictedScores=First_Period$default90)
  ks_plot(actuals=First_Period[,arg1], predictedScores=First_Period$default90)

  return(list(bins = bins, Default_Rate_90 = Default_Rate_90, ks = ks ))
}

results_iv_ks <- lapply(variables_list, iv_ks_function)
