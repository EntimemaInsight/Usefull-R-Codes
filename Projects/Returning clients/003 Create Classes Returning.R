library(scales)
limitsReturningClients <- ValidationSetReturningClientsWithPredictions %>%
  left_join(LastCreditProgressPrepared %>% select(CreditCode, PayedTotal, PaidWithoutLast, SumaP, PayedTotalInChain, 
                                                  SumaPInChain, TakenAfter, PayedAfter, ProfitInChain,
                                                  TotalTimeInChain, CreditsInChain, PreviousCreditsInChain,
                                                  ChainIsFinished, IsRefinanced), by = "CreditCode") %>%
  filter(Quarter %in% c(2020.4, 2021.1, 2021.2),
         #!is.infinite(ProfitPerc),
         DefaultNumeric < 2) %>%
  mutate( PaidForProfit = case_when(DefaultNumeric == 0 ~ PayedTotal,
                                    DefaultNumeric == 1 & IsRefinanced == 1 ~ PaidWithoutLast,
                                    TRUE ~ PayedTotal),
          ProfitPerc = PaidForProfit / SumaP / 1.3 - 1)


Z <- 1.96
xxxReturning <- limitsReturningClients %>%
  mutate(GroupRazdelenie = cut2(Prediction, m = round(nrow(limitsReturningClients) / 20)),
         CreditsInChainAfter = CreditsInChain - PreviousCreditsInChain) %>%
  group_by(GroupRazdelenie) %>%
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
  arrange(GroupRazdelenie)
View(xxxReturning)
saveRDS(xxxReturning, "./xxxReturning.rds")

limitsReturningClients <- limitsReturningClients %>%
  mutate(RiskClass = case_when(Prediction < 0.0150 ~ 1,
                               Prediction < 0.0364 ~ 2,
                               Prediction < 0.0630 ~ 3,
                               Prediction < 0.1109 ~ 4,
                               Prediction < 0.1687 ~ 5,
                               Prediction < 0.2415 ~ 6,
                               Prediction <= 1 ~ 7,
                               TRUE ~ 999)) 

limitsReturningClients %>%
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
  arrange(RiskClass) %>%
  View()

ProfitsReturning <- limitsReturningClients %>%
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
View(ProfitsReturning)
saveRDS(ProfitsReturning, "./ProfitsReturning.rds")


ProfitGood <- limitsReturningClients %>%
  filter(DefaultNumeric == 0,
         IsRefinanced == 0) %>%
  mutate(Profit = PayedTotal / SumaP) %>%
  group_by(RiskClass) %>%
  mutate(Weight = SumaP / sum(SumaP),
         WeightedProfit = Weight * ProfitPerc) %>%
  summarise(Defaults = sum(as.numeric(DefaultNumeric)),
            TotalObs = n(),
            SumaPMean = mean(SumaP),
            SumaPInChain = sum(SumaPInChain),
            PaidInChain = sum(PayedTotalInChain),
            ProfitInChain = sum(PaidInChain - 1.3 * SumaPInChain),
            PayedTotal = sum(PayedTotal),
            PaidForProfit = sum(PaidForProfit),
            ProfitPerc = sum(WeightedProfit),
            Profit = mean(Profit),
            .groups = "drop") %>%
  ungroup() %>%
  arrange(RiskClass) %>%
  summarise(Profit = mean(ProfitPerc)) %>%
  ungroup()

ProfitBad <- limitsReturningClients %>%
  filter(DefaultNumeric == 1,
         IsRefinanced == 0) %>%
  mutate(Profit = PayedTotal / SumaP) %>%
  group_by(RiskClass) %>%
  mutate(Weight = SumaP / sum(SumaP),
         WeightedProfit = Weight * ProfitPerc) %>%
  summarise(Defaults = sum(as.numeric(DefaultNumeric)),
            TotalObs = n(),
            SumaPMean = mean(SumaP),
            SumaP1 = sum(SumaP),
            SumaPInChain = sum(SumaPInChain),
            PaidInChain = sum(PayedTotalInChain),
            ProfitInChain = sum(PaidInChain - 1.3 * SumaPInChain),
            PayedTotal = sum(PayedTotal),
            PaidForProfit = sum(PaidForProfit),
            ProfitPerc = sum(WeightedProfit),
            Profit = mean(Profit),
            .groups = "drop") %>%
  ungroup() %>%
  arrange(RiskClass) %>%
  summarise(Profit = mean(ProfitPerc)) %>%
  ungroup()

ProfitDB <- data.frame(RiskClass = 1:7) %>%
  mutate(ProfitGood = 1.6,
         ProfitBad = case_when(RiskClass %in% c(1, 2, 3) ~ 0.63,
                               RiskClass %in% c(4) ~ 0.50,
                               RiskClass %in% c(5) ~ 0.45,
                               RiskClass %in% c(6, 7) ~ 0.35,
                               TRUE ~ 0.5))

ProfitsReturning <- ProfitsReturning %>%
  left_join(ProfitDB, by = "RiskClass") %>%
  mutate(Goods = TotalObs - Defaults,
         Bads = Defaults,
         ProfitPerc = (Goods / TotalObs) * ProfitGood + (Bads / TotalObs) * ProfitPerc) %>%
  select(RiskClass, ProfitPerc)
  
