AllCreditsLimits <- ValidationSetNewClientsWithPredictions %>%
  dplyr::left_join(LastCreditProgressPrepared %>% select(CreditCode, PayedTotal, PaidWithoutLast, SumaP, PayedTotalInChain, SumaPInChain, 
                                                         CreditsInChain, TakenAfter, PayedAfter, ProfitInChain, TotalTimeInChain,
                                                         ChainIsFinished, IsRefinanced, CompleteDate), by = "CreditCode") %>%
  dplyr::filter(Quarter <= 2021.3)

FinishedCreditsLimits <- ValidationSetNewClientsWithPredictions %>%
  dplyr::left_join(LastCreditProgressPrepared %>% select(CreditCode, PayedTotal, PaidWithoutLast, SumaP, PayedTotalInChain, SumaPInChain, 
                                                         CreditsInChain, TakenAfter, PayedAfter, ProfitInChain, TotalTimeInChain,
                                                         ChainIsFinished, IsRefinanced, CompleteDate), by = "CreditCode") %>%
  dplyr::filter(DefaultNumeric < 2)


AllCreditsLimits <- ValidationSetNewClientsWithPredictions %>%
  dplyr::left_join(LastCreditProgressPrepared %>% select(CreditCode, PayedTotal, PaidWithoutLast, SumaP, PayedTotalInChain, SumaPInChain, 
                                                         CreditsInChain, TakenAfter, PayedAfter, ProfitInChain, MaxDelayP, TotalTimeInChain,
                                                         ChainIsFinished, IsRefinanced, CompleteDate), by = "CreditCode") %>%
  dplyr::filter(Quarter <= 2021.4)

Z <- 1.96
xxxFinishedCreditsLimits <- AllCreditsLimits %>%
  dplyr::filter(Quarter <= 2021.4,
                DefaultNumeric < 2) %>%
  dplyr::mutate(#DefaultNumeric = ifelse(MaxDelayP > 90, 1, 0),
                FivePercentClass = Hmisc::cut2(Prediction, m = round(n() / 20)),
                Fraud = ifelse(PayedTotal < 50, 1, 0)) %>%
  dplyr::group_by(FivePercentClass) %>%
  dplyr::summarise(TotalObs = n(),
                   Defaults = sum(as.numeric(DefaultNumeric)),
                   DefaultRate = Defaults / TotalObs,
                   DefaultPerLower = scales::percent(DefaultRate - Z * sqrt(DefaultRate * (1 - DefaultRate) / TotalObs), accuracy = 0.1),
                   DefaultPerUpper = scales::percent(DefaultRate + Z * sqrt(DefaultRate * (1 - DefaultRate) / TotalObs), accuracy = 0.1),
                   DefaultRate = scales::percent(DefaultRate, accuracy = 0.1),
                   SumaPMean = mean(SumaP),
                   SumaPTotal = sum(SumaP),
                   PayedTotal = sum(PayedTotal),
                   .groups = "drop") %>%
  dplyr::ungroup() %>%
  dplyr::arrange(FivePercentClass)

View(xxxFinishedCreditsLimits)



Z <- 1.96
xxxAllCreditsLimits <- AllCreditsLimits %>%
  dplyr::group_by(Quarter) %>%
  dplyr::mutate(FivePercentClass = Hmisc::cut2(Prediction, m = round(n() / 20)),
                Fraud = ifelse(PayedTotal < 50, 1, 0)) %>%
  dplyr::group_by(FivePercentClass) %>%
  dplyr::summarise(TotalObs = n(),
                   Defaults = sum(as.numeric(DefaultNumeric)),
                   Frauds = sum(as.numeric(Fraud)),
                   DefaultRate = Defaults / TotalObs) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(FivePercentClass)


DefaultPerLower = scales::percent(DefaultRate - Z * sqrt(DefaultRate * (1 - DefaultRate) / TotalObs), accuracy = 0.1),
DefaultPerUpper = scales::percent(DefaultRate + Z * sqrt(DefaultRate * (1 - DefaultRate) / TotalObs), accuracy = 0.1),
DefaultRate = scales::percent(DefaultRate, accuracy = 0.1),
SumaPMean = mean(SumaP),
SumaPTotal = sum(SumaP),
PayedTotal = sum(PayedTotal),
ProfitWithTenPercCost = PayedTotal - SumaPTotal * 1.1,
Refinanced = sum(IsRefinanced),
.groups = "drop") %>%
  dplyr::ungroup() %>%
  dplyr::arrange(FivePercentClass)
View(xxxAllCreditsLimits)





Z <- 1.96
xxxFinishedCreditsLimits <- FinishedCreditsLimits %>%
  dplyr::group_by(Quarter) %>%
  dplyr::mutate(FivePercentClass = Hmisc::cut2(Prediction, m = round(n() / 20)),
                Fraud = ifelse(PayedTotal < 50, 1, 0)) %>%
  dplyr::group_by(Quarter, FivePercentClass) %>%
  dplyr::summarise(TotalObs = n(),
                   Defaults = sum(as.numeric(DefaultNumeric)),
                   Frauds = sum(as.numeric(Fraud)),
                   DefaultRate = Defaults / TotalObs,
                   DefaultPerLower = scales::percent(DefaultRate - Z * sqrt(DefaultRate * (1 - DefaultRate) / TotalObs), accuracy = 0.1),
                   DefaultPerUpper = scales::percent(DefaultRate + Z * sqrt(DefaultRate * (1 - DefaultRate) / TotalObs), accuracy = 0.1),
                   DefaultRate = scales::percent(DefaultRate, accuracy = 0.1),
                   SumaPMean = mean(SumaP),
                   SumaPTotal = sum(SumaP),
                   PayedTotal = sum(PayedTotal),
                   ProfitWithTenPercCost = PayedTotal - SumaPTotal * 1.1,
                   Refinanced = sum(IsRefinanced),
                   .groups = "drop") %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Quarter, FivePercentClass) %>%
  dplyr::filter(Quarter == 2021.3)
View(xxxFinishedCreditsLimits)


sss <- FinishedCreditsLimits %>%
  dplyr::filter(Prediction >= 0.7157)

sss %>%
  dplyr::group_by(Quarter)

Z <- 1.96
xxxFinishedCreditsLimits <- FinishedCreditsLimits %>%
  mutate(FivePercentClass = Hmisc::cut2(Prediction, m = round(nrow(limits) / 20)),
         CreditsInChainAfter = CreditsInChain,
         Weight = SumaP / sum(SumaP),
         WeightedProfit = Weight * ProfitPerc,
         Fraud = ifelse(PayedTotal < 50, 1, 0)) %>%
  group_by(FivePercentClass) %>%
  summarise(TotalObs = n(),
            Frauds = sum(as.numeric(Fraud)),
            Frauds = sum(as.numeric(Fraud)),
            FraudRate = Frauds / TotalObs,
            FraudPerLower = scales::percent(FraudRate - Z * sqrt(FraudRate * (1 - FraudRate) / TotalObs), accuracy = 0.1),
            FraudPerUpper = scales::percent(FraudRate + Z * sqrt(FraudRate * (1 - FraudRate) / TotalObs), accuracy = 0.1),
            FraudRate = scales::percent(FraudRate, accuracy = 0.1),
            SumaPMean = mean(SumaP),
            SumaPTotal = sum(SumaP),
            PayedTotal = sum(PayedTotal),
            ProfitWithTenPercCost = PayedTotal - SumaPTotal * 1.1,
            Refinanced = sum(IsRefinanced),
            .groups = "drop") %>%
  ungroup() %>%
  arrange(FivePercentClass)
View(xxxlimits)





xxxlimits <- limits %>%
  mutate(GroupRazdelenie = Hmisc::cut2(Prediction, m = round(nrow(limits) / 20)),
         CreditsInChainAfter = CreditsInChain,
         Weight = SumaP / sum(SumaP),
         WeightedProfit = Weight * ProfitPerc,
         Fraud = ifelse(PayedTotal < 50, 1, 0)) %>%
  group_by(GroupRazdelenie) %>%
  summarise(Defaults = sum(as.numeric(DefaultNumeric)),
            TotalObs = n(),
            DefaultRate = Defaults / TotalObs,
            DefaultPerLower = percent(DefaultRate - Z * sqrt(DefaultRate * (1 - DefaultRate) / TotalObs), accuracy = 0.1),
            DefaultPerUpper = percent(DefaultRate + Z * sqrt(DefaultRate * (1 - DefaultRate) / TotalObs), accuracy = 0.1),
            DefaultRate = percent(DefaultRate, accuracy = 0.1),
            SumaPMean = mean(SumaP),
            SumaPInChain = sum(SumaP),
            PaidForProfit = sum(PaidForProfit),
            PayedTotal = sum(PayedTotal),
            ProfitInChain = sum(PaidForProfit - SumaPInChain),
            ProfitPerc = sum(WeightedProfit),
            Refinanced = sum(IsRefinanced),
            Frauds = sum(Fraud),
            .groups = "drop") %>%
  ungroup() %>%
  arrange(GroupRazdelenie)


test <- ValidationSetNewClientsWithPredictions %>%
  filter(Prediction >= 0.5812) %>%
  left_join(LastCreditProgressPrepared %>% select(CreditCode, CreditBeginDate, CompleteDate,
                                                  CreditMaturityDate, CreditProduct, Weeks, period,
                                                  SumaP, SumaI, PayedTotal))



test <- TrainSetNewClientsWithPredictions %>%
  filter(Prediction >= 0.5812) 
table(test$DefaultNumeric, useNA = "ifany")

%>%
  left_join(LastCreditProgressPrepared %>% select(CreditCode, CreditBeginDate, CompleteDate,
                                                  CreditMaturityDate, CreditProduct, Weeks, period,
                                                  SumaP, SumaI, PayedTotal))


ForCheck <- test %>%
  select(EGN) %>%
  inner_join()


limitsRefinancingClients <- limitsRefinancingClients %>%
  mutate(RiskClass = case_when(Prediction < 0.0278 ~ 1,
                               Prediction < 0.0544 ~ 2,
                               Prediction < 0.0811 ~ 3,
                               Prediction < 0.1358 ~ 4,
                               Prediction < 0.1678 ~ 5,
                               Prediction < 0.2173 ~ 6,
                               Prediction <= 1 ~ 7,
                               TRUE ~ 999)) 

ProfitsRefinancing <- limitsRefinancingClients %>%
  group_by(RiskClass) %>%
  mutate(Weight = SumaP / sum(SumaP),
         WeightedProfit = Weight * ProfitPerc) %>%
  summarise(Defaults = sum(as.numeric(DefaultNumeric)),
            TotalObs = n(),
            DefaultRate = Defaults / TotalObs,
            DefaultPerLower = percent(DefaultRate - Z * sqrt(DefaultRate * (1 - DefaultRate) / TotalObs), accuracy = 0.1),
            DefaultPerUpper = percent(DefaultRate + Z * sqrt(DefaultRate * (1 - DefaultRate) / TotalObs), accuracy = 0.1),
            DefaultRate = percent(DefaultRate, accuracy = 0.1),
            SumaPMean = mean(SumaP),
            SumaPInChain = sum(SumaPInChain),
            PaidInChain = sum(PayedTotalInChain),
            ProfitInChain = sum(PaidInChain - 1.3 * SumaPInChain),
            ProfitPerc = sum(WeightedProfit),
            .groups = "drop") %>%
  ungroup() %>%
  arrange(RiskClass)
View(ProfitsRefinancing)
saveRDS(ProfitsRefinancing, "./ProfitsRefinancing.rds")
