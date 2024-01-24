ReturningClients <- LastCreditProgressPrepared  %>%
  filter(CreditBeginDate >= as.Date("2019-07-01"),
         CreditBeginDate <= as.Date("2021-12-31"),
         ClientType == 2) %>%
  mutate(DefaultNumeric = DefaultFlag2,
         Default = factor(DefaultNumeric,
                          levels = c("0", "1"),
                          labels = c("Good", "Bad")),
         CreditMaturity = as.numeric(CreditMaturity),
         #FirstDateInChainMonth = floor_date(FirstDateInChain, "month"),
         CreditBeginDateMonth = floor_date(CreditBeginDate, "month"),
         Quarter = quarter(CreditBeginDate, with_year = TRUE),
         Age = as.numeric(CreditBeginDate - ClientBirtDate) / 365)


set.seed(37)
TrainSetReturningClients <- ReturningClients %>%
  filter(CreditBeginDate >= as.Date("2019-07-01"),
         CreditBeginDate <= as.Date("2020-09-30"),
         DefaultNumeric < 2) %>%
  mutate(CreditBeginDateMonth = floor_date(CreditBeginDate, "month")) %>%
  select(CreditCode, CreditBeginDateMonth, Quarter, DefaultNumeric,
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
  as.data.frame()

TrainSetReturningClients <- TrainSetReturningClients[sample(nrow(TrainSetReturningClients)),] %>%
  group_by(CreditBeginDateMonth, DefaultNumeric) %>%
  mutate(Fold = ntile(x = row_number(), 5)) %>%
  ungroup() %>%
  select(-CreditBeginDateMonth) 

Missings <- sapply(TrainSetReturningClients, function (x) sum(is.na(x)))
Missings

TrainSetReturningClients <- TrainSetReturningClients %>%
  na.omit() %>%
  as.data.frame()

ValidationSetReturningClients <- ReturningClients %>%
  filter(CreditBeginDate >= as.Date("2020-10-01"),
         CreditBeginDate <= as.Date("2022-03-31")) %>%
  mutate(CreditBeginDateMonth = floor_date(CreditBeginDate, "month")) %>%
  select(CreditCode, CreditBeginDateMonth, Quarter, DefaultNumeric,
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

ValidationSetReturningClients  %>%
  group_by(CreditBeginDateMonth, DefaultNumeric) %>%
  summarise(Count = n()) %>%
  View()

dValidation <- model.matrix(DefaultNumeric ~ ., data = ValidationSetReturningClients %>% select(-CreditCode, -Quarter, -CreditBeginDateMonth))[, -1]
dValidation <- xgb.DMatrix(dValidation, label = ValidationSetReturningClients %>% pull(DefaultNumeric))

eta_param <- seq(from = 0.02, to = 0.2, by = 0.02)
gamma_param <- seq(from = 2, to = 6, by = 0.5)
max.depth_param <- seq(from = 5, to = 10, by = 1)
subsample_param <- seq(from = 0.8, to = 0.8, by = 0.1)
colsample_bytree_param  <- seq(from = 1, to = 1, by = 0.1)
parameters <- expand.grid(eta = eta_param, 
                          gamma = gamma_param,
                          max_depth = max.depth_param,
                          subsample = subsample_param,
                          colsample_bytree = colsample_bytree_param) %>%
  arrange(eta, gamma, max_depth)

do_model <- function(arg1){
  
  output_do_model <- lapply(1:5, function(x) do_cv(x, arg1)) %>%
    bind_rows() %>%
    mutate(eta = arg1[1], 
           gamma = arg1[2],
           max.depth = arg1[3],
           subsample = arg1[4],
           colsample_bytree = arg1[5])
  gc()
  return(output_do_model)
}

do_cv <- function(arg1, arg2){
  
  Test <- arg1
  Train <- 1:5
  Train <- Train[!Train %in% Test]
  
  Train <- TrainSetReturningClients %>%
    filter(Fold %in% Train) %>%
    select(-Fold)
  
  dTrain <- model.matrix(DefaultNumeric ~ ., data = Train %>% select(-CreditCode, -Quarter))[, -1]
  dTrain <- xgb.DMatrix(dTrain, label = Train %>% pull(DefaultNumeric))
  
  Test <- TrainSetReturningClients %>%
    filter(Fold %in% Test) %>%
    select(-Fold)
  
  dTest <- model.matrix(DefaultNumeric ~ ., data = Test %>% select(-CreditCode, -Quarter))[, -1]
  dTest <- xgb.DMatrix(dTest, label = Test %>% pull(DefaultNumeric))
  
  watchlist <- list(train = dTrain, test = dTest)
  
  xgbmodel <- xgb.train(data = dTrain,
                        eta = arg2[1], 
                        gamma = arg2[2],
                        max.depth = arg2[3], 
                        min_child_weight = 1,
                        subsample = arg2[4],
                        colsample_bytree = arg2[5],
                        tree_method = "auto",
                        nthread = 10, 
                        nrounds = 1000,
                        verbose = 0,
                        watchlist = watchlist,
                        early_stopping_rounds = 5,
                        eval_metric = "auc",
                        objective = "binary:logistic")
  
  ValidationSetReturningClientsGINI <- ValidationSetReturningClients %>%
    select(CreditCode, Quarter) %>%
    mutate(Prediction = predict(xgbmodel, dValidation),
           DefaultNumeric = ValidationSetReturningClients$DefaultNumeric) %>%
    filter(Quarter < 2021.3,
           DefaultNumeric < 2) %>%
    group_by(Quarter) %>%
    summarise(Count = n(),
              AUC = ROCR::performance(prediction(Prediction, DefaultNumeric), measure = "auc")@y.values[[1]],
              GINI = 2 * AUC - 1,
              .groups = "drop") %>%
    select(-AUC)
  
  output <- data.frame(BestIteration = xgbmodel$best_iteration,
                       Gini_Train = 2 * xgbmodel$evaluation_log$train_auc[xgbmodel$best_iteration] - 1,
                       Gini_Test = 2 * xgbmodel$evaluation_log$test_auc[xgbmodel$best_iteration] - 1,
                       Gini_Validation2020.4 = ValidationSetReturningClientsGINI$GINI[1],
                       Gini_Validation2021.1 = ValidationSetReturningClientsGINI$GINI[2],
                       Gini_Validation2021.2 = ValidationSetReturningClientsGINI$GINI[3])
  print(arg2[1])
  return(output)
}

system.time({ 
  results <- apply(parameters, 1, do_model) %>%
    bind_rows() 
  
  results1 <- results %>%
    group_by(eta, gamma, max.depth, subsample, colsample_bytree) %>%
    summarize(Gini_Train = mean(Gini_Train),
              Gini_Test_max = max(Gini_Test),
              Gini_Test_min = min(Gini_Test),
              Gini_Test = mean(Gini_Test),
              Gini_Validation2020.4 = mean(Gini_Validation2020.4),
              Gini_Validation2021.1 = mean(Gini_Validation2021.1),
              Gini_Validation2021.2 = mean(Gini_Validation2021.2),
              meanBestIteration = mean(BestIteration),
              maxBestIteration = max(BestIteration),
              minBestIteration = min(BestIteration),
              medianBestIteration = median(BestIteration),
              .groups = "drop") %>%
    arrange(desc(Gini_Test)) %>%
    ungroup()
  
  saveRDS(results, file = "./Output Data//ResultsForReturningCustomers.rds")
})

dTrain <- model.matrix(DefaultNumeric ~ ., data = TrainSetReturningClients %>% select(-CreditCode, -Quarter, -Fold))[, -1]
dTrain <- xgb.DMatrix(dTrain, label = TrainSetReturningClients %>% pull(DefaultNumeric))

xgbmodelReturning <- xgboost(data = dTrain,
                             eta = 0.20, 
                             gamma = 2.5,
                             max.depth = 5, 
                             min_child_weight = 1,
                             subsample = 0.8,
                             colsample_bytree = 1,
                             tree_method = "auto",
                             nthread = 10, 
                             nrounds = 30,
                             eval_metric = "auc",
                             objective = "binary:logistic")

importance_matrix <- xgb.importance(model = xgbmodelReturning)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
2 * xgbmodelReturning$evaluation_log$train_auc[30] - 1

saveRDS(xgbmodelReturning, file = "./Output Data//xgbmodelReturningProvenir.rds")
xgbmodelReturning <- readRDS(file = "./Output Data//xgbmodelReturningProvenir.rds")

ValidationSetReturningClientsWithPredictions <- ValidationSetReturningClients %>%
  select(CreditCode, Quarter, CreditBeginDateMonth) %>%
  mutate(Prediction = predict(xgbmodelReturning, dValidation),
         DefaultNumeric = ValidationSetReturningClients$DefaultNumeric)

ValidationSetReturningClientsGINI <- ValidationSetReturningClientsWithPredictions %>%
  filter(Quarter < 2021.4,
         DefaultNumeric < 2) %>%
  group_by(Quarter) %>%
  summarise(CountNewModel = n(),
            AUC = ROCR::performance(prediction(Prediction, DefaultNumeric), measure = "auc")@y.values[[1]],
            GININewModel = round((2 * AUC - 1) * 100, digits = 2),
            DefaultNumeric = sum(DefaultNumeric),
            .groups = "drop") %>%
  select(-AUC)