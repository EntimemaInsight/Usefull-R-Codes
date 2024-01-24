AllProducts <- LastCreditProgressPrepared %>%
  filter(CreditBeginDate >= as.Date("2021-01-01")) %>%
  select(CreditProduct, SumaP, Vnoska, Weeks, DailyVnoska) %>%
  distinct() %>%
  arrange(CreditProduct, SumaP) %>%
  mutate(ID = 1) %>%
  distinct() %>%
  rename(SumaPSuggested = SumaP,
         DailyVnoskaSuggested = DailyVnoska,
         VnoskaSuggested = Vnoska,
         WeeksSuggested = Weeks) %>%
  distinct(ID, SumaPSuggested, CreditProduct, VnoskaSuggested, WeeksSuggested, DailyVnoskaSuggested) %>%
  mutate(Product = case_when(CreditProduct == "bilunar" ~ 21,
                             CreditProduct == "ibusiness" ~ -12,
                             CreditProduct == "icredit" ~ 0,
                             CreditProduct == "icredit premium" ~ 13,
                             CreditProduct == "icredit vip" ~ 0,
                             CreditProduct == "pensioner" ~ 1,
                             CreditProduct == "staffro" ~ 10,
                             TRUE ~ 0))
saveRDS(AllProducts, file = "./Output Data/AllProducts.rds")

AllProducts <- AllProducts %>%
  select(-Product)
hist(AllProducts$SumaPSuggested)

AllProducts %>%
  group_by(CreditProduct) %>%
  summarise(MinOffer = min(SumaPSuggested)) %>%
  View()

ReturningOver1000 <- ValidationSetReturningClients %>%
  inner_join(ValidationSetReturningClientsWithPredictions %>% select(CreditCode, Prediction)) %>%
  inner_join(LastCreditProgressPrepared %>% select(CreditCode, ClientEGN, CreditProduct,
                                                    CreditsInChain, ChainIsFinished, IsRefinanced)) %>%
  #filter(SumaP > 1000) %>%
  mutate(ID = 1) %>%
  left_join(AllProducts, by = c("ID", "CreditProduct")) %>%
  filter(DailyVnoskaSuggested / PreviousMaxDailyVnoskaPaidWithoutDelays30 <= 1.25) %>%
  mutate(SumaPOriginal = SumaP,
         SumaP = SumaPSuggested,
         DailyVnoskaOriginal = DailyVnoska,
         DailyVnoska = DailyVnoskaSuggested,
         RiskClassOriginal = case_when(Prediction < 0.0150 ~ 1,
                                       Prediction < 0.0364 ~ 2,
                                       Prediction < 0.0630 ~ 3,
                                       Prediction < 0.1109 ~ 4,
                                       Prediction < 0.1687 ~ 5,
                                       Prediction < 0.2415 ~ 6,
                                       Prediction <= 1 ~ 7,
                                       TRUE ~ 999),
         SumaPIncreaseComparedToMaxPaid30 = SumaP - PreviousSumaPPaidWithoutDelays30,
         SumaPIncreaseComparedToPrevious = SumaP - PreviousSumaP,
         ChangeInDailyVnoska = DailyVnoska / PreviousDailyVnoska,
         ChangeInDailyVnoskaMax30 = DailyVnoska / PreviousMaxDailyVnoskaPaidWithoutDelays30) %>%
  select(CreditCode, ClientEGN, CreditBeginDateMonth, Quarter, DefaultNumeric, SumaPOriginal, 
         CreditsInChain, ChainIsFinished, IsRefinanced,
         CreditProduct, RiskClassOriginal, DailyVnoskaOriginal, 
         Age, SumaP, DailyVnoska, PreviousDailyVnoska, ChangeInDailyVnoska, ChangeInDailyVnoskaMax30,
         ChangeInDelay, 
         TimeToPreviousBadDate30,
         PreviousProfit,
         PreviousGood30BasedOnDelay, PreviousBad30BasedOnDelay, 
         PreviousChains,
         PreviousSumaP,
         PreviousSumaPPaidWithoutDelays30, PreviousSumaPPaidWithDelays30, PreviousMaxDailyVnoskaPaidWithoutDelays30,
         PreviousTimeAsGoodClient30, PreviousTimeAsBadClient30,
         TimeToPreviousCredit,
         SumaPIncreaseComparedToMaxPaid30, SumaPIncreaseComparedToPrevious,
         PreviousMaxDelay1, PreviousMaxDelay2, PreviousMaxDelay3, PreviousMaxDelayEver) %>%
  as.data.frame() %>%
  na.omit()

length(unique(ReturningOver1000$CreditCode))

dReturningOver1000 <- model.matrix(DefaultNumeric ~ ., data = ReturningOver1000 %>% 
                                       select(-CreditCode, -ClientEGN, -Quarter,-CreditBeginDateMonth,
                                              -SumaPOriginal, -RiskClassOriginal, -CreditProduct, -DailyVnoskaOriginal,
                                              -CreditsInChain, -ChainIsFinished, -IsRefinanced))[, -1]
dReturningOver1000 <- xgb.DMatrix(dReturningOver1000, label = ReturningOver1000 %>% pull(DefaultNumeric))
xgbmodelReturning$feature_names

colnames(dReturningOver1000)
ReturningOver1000WithPredictions <- ReturningOver1000 %>%
  select(CreditCode, ClientEGN, Quarter, PreviousSumaP, SumaPOriginal, SumaP, CreditProduct, 
         DailyVnoskaOriginal, DailyVnoska, ChangeInDailyVnoska, ChangeInDailyVnoskaMax30,
         CreditsInChain, ChainIsFinished, IsRefinanced,
         PreviousSumaPPaidWithoutDelays30, PreviousSumaPPaidWithDelays30,
         RiskClassOriginal) %>%
  mutate(Prediction = predict(xgbmodelReturning, dReturningOver1000),
         DefaultNumeric = ReturningOver1000$DefaultNumeric,
         RiskClass = case_when(Prediction < 0.0150 ~ 1,
                               Prediction < 0.0364 ~ 2,
                               Prediction < 0.0630 ~ 3,
                               Prediction < 0.1109 ~ 4,
                               Prediction < 0.1687 ~ 5,
                               Prediction < 0.2415 ~ 6,
                               Prediction <= 1 ~ 7,
                               TRUE ~ 999)) %>%
  arrange(CreditCode, RiskClass, desc(SumaP)) 

OgranicheniqReturningOver1000 <- ReturningOver1000WithPredictions %>%
  mutate(MaxOffer = case_when(RiskClass = 1 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 1750, 8000),
                              RiskClass = 2 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 1500, 8000),
                              RiskClass = 3 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 1250, 8000),
                              RiskClass = 4 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 1000, 8000),
                              RiskClass = 5 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 750, 8000),
                              RiskClass = 6 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 500, 8000),
                              RiskClass = 7 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 0, 8000),
                              TRUE ~ 0),
         MinOffer = case_when(CreditProduct == "bilunar" ~ 500,
                              CreditProduct == "ibusiness" ~ 500,
                              CreditProduct == "icredit" ~ 500,
                              CreditProduct == "icredit premium" ~ 1500,
                              CreditProduct == "icredit vip" ~ 2000,
                              CreditProduct == "pensioner" ~ 500,
                              CreditProduct == "staffro" ~ 1200,
                              TRUE ~ MaxOffer),
         MaxOffer = pmax(MinOffer, MaxOffer),
         MaxOffer = pmin(8000, MaxOffer)) %>%
  arrange(CreditCode, SumaP)
length(unique(OgranicheniqReturningOver1000$CreditCode))

ViewWeirdBehaviour <- OgranicheniqReturningOver1000 %>%
  group_by(CreditCode) %>%
  mutate(ProblemBehavior = ifelse(lag(RiskClass) > RiskClass, 1, 0),
         ProblemBehavior = ifelse(is.na(ProblemBehavior), 0, ProblemBehavior),
         MaxProblem = max(ProblemBehavior),
         .groups = "drop")



OgranicheniqReturningOver1000 <- OgranicheniqReturningOver1000 %>%
  filter(SumaP <= MaxOffer,
         ChangeInDailyVnoskaMax30 <= 1.5) %>%
  left_join(ProfitsReturning %>% select(RiskClass, ProfitPerc), by = "RiskClass") %>%
  rename(ExpectedProfitPercent = ProfitPerc) %>%
  mutate(ExpectedProfit = SumaP * ExpectedProfitPercent) %>%
  arrange(CreditCode, desc(ExpectedProfit)) %>%
  group_by(CreditCode) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  mutate(SumaP = ifelse(RiskClass >= 7, 0, SumaP))
length(unique(OgranicheniqReturningOver1000$CreditCode))

test <- OgranicheniqReturningOver1000 %>%
  filter(ChangeInDailyVnoska > 1.25)


FinalChoiceReturningOver1000 <- OgranicheniqReturningOver1000
length(unique(FinalChoiceReturningOver1000$CreditCode))


FinalChoiceReturningOver1000 %>%
  filter(SumaP > 0)


FinalChoiceReturningOver1000 %>%
  filter(Quarter < 2020.4) %>%
  group_by(RiskClass) %>%
  summarise(Defaults = sum(as.numeric(DefaultNumeric)),
            TotalObs = n(),
            DefaultRate = round(Defaults / TotalObs, digits = 5),
            DefaultRate = scales::percent(DefaultRate, accuracy = 0.01),
            MeanSumaP = mean(SumaP),
            TotalSumaP = sum(SumaP),
            MeanSumaPOriginal = mean(SumaPOriginal),
            TotalSumaPOriginal = sum(SumaPOriginal),
            PreviousSumaPPaidWithoutDelays60 = mean(PreviousSumaPPaidWithoutDelays60),
            .groups = "drop") %>%
  mutate(Total = sum(TotalSumaP),
         TotalOriginal = sum(TotalSumaPOriginal)) %>%
  arrange(RiskClass) %>%
  View()

FinalChoiceReturningOver1000 %>%
  filter(Quarter < 2020.4) %>%
  group_by(RiskClassOriginal) %>%
  summarise(Defaults = sum(as.numeric(DefaultNumeric)),
            TotalObs = n(),
            DefaultRate = round(Defaults / TotalObs, digits = 5),
            DefaultRate = scales::percent(DefaultRate, accuracy = 0.01),
            MeanSumaP = mean(SumaP),
            MeanSumaPOriginal = mean(SumaPOriginal),
            TotalSumaP = sum(SumaP),
            TotalSumaPOriginal = sum(SumaPOriginal),
            PreviousSumaPPaidWithoutDelays60 = mean(PreviousSumaPPaidWithoutDelays60),
            PreviousSumaPPaidWithDelays60 = mean(PreviousSumaPPaidWithDelays60),
            .groups = "drop") %>%
  mutate(Total = sum(TotalSumaP),
         TotalOriginal = sum(TotalSumaPOriginal)) %>%
  arrange(RiskClassOriginal) %>%
  View()

FinalChoiceReturningOver1000 %>%
  filter(Quarter < 2021.3,
         DefaultNumeric < 2) %>%
  group_by(RiskClassOriginal, RiskClass) %>%
  summarise(Defaults = sum(as.numeric(DefaultNumeric)),
            TotalObs = n(),
            DefaultRate = round(Defaults / TotalObs, digits = 5),
            DefaultRate = scales::percent(DefaultRate, accuracy = 0.01),
            MeanSumaP = mean(SumaP),
            TotalSumaP = sum(SumaP),
            MeanSumaPOriginal = mean(SumaPOriginal),
            TotalSumaPOriginal = sum(SumaPOriginal),
            PreviousSumaPPaidWithoutDelays30 = mean(PreviousSumaPPaidWithoutDelays30),
            PreviousSumaPPaidWithDelays30 = mean(PreviousSumaPPaidWithDelays30),
            .groups = "drop") %>%
  group_by(RiskClassOriginal) %>%
  mutate(Total = sum(TotalSumaP),
         TotalOriginal = sum(TotalSumaPOriginal)) %>%
  arrange(RiskClassOriginal) %>%
  View()


FinalChoiceReturningOver1000 %>%
  filter(Quarter < 2021.3,
         DefaultNumeric == 0) %>%
  group_by(RiskClassOriginal, RiskClass) %>%
  summarise(Defaults = sum(as.numeric(DefaultNumeric)),
            TotalObs = n(),
            DefaultRate = round(Defaults / TotalObs, digits = 5),
            DefaultRate = scales::percent(DefaultRate, accuracy = 0.01),
            MeanSumaP = mean(SumaP),
            TotalSumaP = sum(SumaP),
            MeanSumaPOriginal = mean(SumaPOriginal),
            TotalSumaPOriginal = sum(SumaPOriginal),
            PreviousSumaPPaidWithoutDelays60 = mean(PreviousSumaPPaidWithoutDelays60),
            PreviousSumaPPaidWithDelays60 = mean(PreviousSumaPPaidWithDelays60),
            .groups = "drop") %>%
  mutate(Total = sum(TotalSumaP),
         TotalOriginal = sum(TotalSumaPOriginal)) %>%
  arrange(RiskClassOriginal) %>%
  View()



Test <- FinalChoiceReturningOver1000 %>%
  filter(RiskClassOriginal < RiskClass,
         SumaPOriginal >= SumaP)


