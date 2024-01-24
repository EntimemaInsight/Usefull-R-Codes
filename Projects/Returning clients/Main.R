tryCatch(
  expr = {
    library(xgboost)
    library(tidyverse)
    library(XML)
    library(jsonlite)
    library(lubridate)
    
    currentDate <- Sys.Date()
    input_data <- read.delim(file="./input_data.txt", na.strings = "NULL", quote="")
    
    AllProducts <- readRDS("./AllProducts.rds") %>%
      distinct(CreditProduct, Product, SumaPSuggested, VnoskaSuggested, WeeksSuggested, DailyVnoskaSuggested)
    
    ProfitsReturning <- readRDS("./ProfitsReturning.rds")
    
    ProfitsRefinancing <- readRDS("./ProfitsRefinancing.rds")
    
    previous_credits <- as.character(input_data$previous_credits)
    Encoding(previous_credits) <- "UTF-8"
    
    ClientBirthDate <- ymd(as.character(input_data$ClientBirthDate))
    Age <- as.numeric((currentDate - ClientBirthDate) / 365)
    ClientFamilyStatus <- as.character(input_data$ClientFamilyStatus)
    ClientEducation <- as.character(input_data$ClientEducation)
    ClientSex <- ""
    TypeOfContract <- as.character(input_data$TypeOfContract)
    IncomeMinCosts <- as.numeric(input_data$IncomeMinCosts)
    ClientSalary <- as.numeric(input_data$ClientSalary)
    ClientEGN <- as.character(input_data$ClientEGN)
    CreditProductx <- as.numeric(input_data$CreditProduct)
    #ScoringDecision <<- CreditProductx
    #CreditProductx <- ifelse(CreditProductx > -34 | CreditProductx < - 37, -34, CreditProductx)
    
    AllProducts <- AllProducts %>%
      filter(Product == CreditProductx) %>%
      select(-CreditProduct)
    
    if(nrow(AllProducts) == 0){
      ErrorMessage <<- "No such product in product table"
      scoreResultNum <<- -999
      scoreResultProbabilityByDefault <<- -999
      break
    }
    
    current_credit <- data.frame(CreditCode = "9999999",
                                 CreditBeginDate = currentDate,
                                 CompleteDate = NA,
                                 SumaTotal = 0,
                                 SumaP = 0,
                                 SumaI = 0,
                                 PayedTotal = 0,
                                 earlyRepaymentAmount = 0,
                                 CurrentDelay = 0,
                                 MaxDelayP = 0,
                                 Vnoska = 0,
                                 stringsAsFactors = FALSE)
    
    previous_credits_history <- jsonlite::fromJSON(previous_credits, simplifyDataFrame = TRUE)
    previous_credits_history <- previous_credits_history$data$allClientApplications
    
    NewClientCheck <- 0
    
    if(class(previous_credits_history) == "list"){
      NewClientCheck <- 1
    } else {
      previous_credits_history <- previous_credits_history     %>%
        dplyr::filter(!is.na(beginDate)) %>%
        dplyr::mutate(creditCode = as.character(creditCode))
      
      if(nrow(previous_credits_history) == 0){
        NewClientCheck <- 1
      }
    }
    
    if(NewClientCheck == 1){
      
      data <- current_credit
      dataforpd <- bind_rows(data, data)
      
      # xgbmodelNew <- readRDS("./xgbmodelNew.rds")
      # #xgb.Booster.complete(xgbmodel, saveraw = TRUE)
      # dataforpd <- dataforpd[, c(xgbmodel$feature_names)]
      # dataforpd <- model.matrix(~ ., data = dataforpd)[,-1]
      # dataforpd <- xgb.DMatrix(dataforpd)
      
      scoringID <<- ""
      scoreName <<- ""
      # scoreResultProbabilityByDefault <<- unique(round(predict(xgbmodelNew, dataforpd), digits = 2))
      # scoreResultNum <<- 100 * unique(round(predict(xgbmodelNew, dataforpd), digits = 4))
      # maxAcceptableCreditSum <<- as.character(data$SumaP)
      # inputParameters <<- ""
    } else {
      
      productxxxs <- previous_credits_history$product$key
      
      previous_credits_history <- previous_credits_history %>%
        dplyr::select(creditCode, createDate, beginDate, completeDate,
                      principalAmount, totalAmountDue, totalAmountPaid, earlyRepaymentAmount,
                      currentDelay, maxDelay, creditInstallment) %>%
        dplyr::mutate(product = productxxxs,
                      creditCode = as.character(creditCode),
                      CreditBeginDate = ymd(beginDate),
                      CompleteDate = ymd(completeDate),
                      createDate = ymd(createDate),
                      SumaTotal = as.numeric(totalAmountDue),
                      SumaP = as.numeric(principalAmount),
                      SumaI = SumaTotal - SumaP,
                      PayedTotal = as.numeric(totalAmountPaid),
                      earlyRepaymentAmount = as.numeric(earlyRepaymentAmount),
                      CurrentDelay = as.numeric(currentDelay),
                      creditInstallment = as.numeric(creditInstallment),
                      MaxDelayP = as.numeric(maxDelay)) %>%
        dplyr::rename(CreditCode = creditCode,
                      CreditProduct = product,
                      Vnoska = creditInstallment) %>%
        dplyr::select(CreditCode, CreditProduct, CreditBeginDate, CompleteDate,
                      SumaTotal, SumaP, SumaI, PayedTotal, earlyRepaymentAmount,
                      CurrentDelay, MaxDelayP, Vnoska) %>%
        dplyr::filter(!is.na(CreditBeginDate)) %>%
        dplyr::bind_rows(current_credit) %>%
        dplyr::arrange(CreditBeginDate, CreditCode) %>%
        dplyr::mutate(period = case_when(CreditProduct == 21 ~ 14,
                                         CreditProduct == -12 ~ 30,
                                         CreditProduct == 0 ~ 7,
                                         CreditProduct == 13 ~ 7,
                                         CreditProduct == 1 ~ 30,
                                         CreditProduct == 10 ~ 30,
                                         TRUE ~ 30),
                      
                      CompleteDateOriginal = CompleteDate,
                      CompleteDate = ifelse(is.na(CompleteDate), currentDate, CompleteDate),
                      CompleteDate = as.numeric(CompleteDate),
                      CreditBeginDate = as.numeric(CreditBeginDate),
                      
                      DailyVnoska = Vnoska / period,
                      
                      TimeAsClientForThisCredit = as.numeric(CompleteDate - CreditBeginDate),
                      
                      IsGood90BasedOnDelay = ifelse(MaxDelayP > 90, 0, 1),
                      IsBad90BasedOnDelay = ifelse(MaxDelayP > 90, 1, 0),
                      
                      IsGood60BasedOnDelay = ifelse(MaxDelayP > 60, 0, 1),
                      IsBad60BasedOnDelay = ifelse(MaxDelayP > 60, 1, 0),
                      
                      IsGood40BasedOnDelay = ifelse(MaxDelayP > 40, 0, 1),
                      IsBad40BasedOnDelay = ifelse(MaxDelayP > 40, 1, 0),
                      
                      IsGood30BasedOnDelay = ifelse(MaxDelayP > 30, 0, 1),
                      IsBad30BasedOnDelay = ifelse(MaxDelayP > 30, 1, 0),
                      
                      BadDate90 = case_when(IsBad90BasedOnDelay == 1 ~ CompleteDate, 
                                            TRUE ~ 0), 
                      
                      BadDate60 = case_when(IsBad60BasedOnDelay == 1 ~ CompleteDate, 
                                            TRUE ~ 0),
                      
                      BadDate40 = case_when(IsBad40BasedOnDelay == 1 ~ CompleteDate, 
                                            TRUE ~ 0), 
                      
                      BadDate30 = case_when(IsBad30BasedOnDelay == 1 ~ CompleteDate, 
                                            TRUE ~ 0),
                      
                      SumaPPaidWithoutDelays90 = SumaP * IsGood90BasedOnDelay,
                      SumaPPaidWithDelays90 = SumaP * IsBad90BasedOnDelay,
                      
                      SumaPPaidWithoutDelays60 = SumaP * IsGood60BasedOnDelay,
                      SumaPPaidWithDelays60 = SumaP * IsBad60BasedOnDelay,
                      
                      SumaPPaidWithoutDelays40 = SumaP * IsGood40BasedOnDelay,
                      SumaPPaidWithDelays40 = SumaP * IsBad40BasedOnDelay,
                      
                      SumaPPaidWithoutDelays30 = SumaP * IsGood30BasedOnDelay,
                      SumaPPaidWithDelays30 = SumaP * IsBad30BasedOnDelay,
                      
                      DailyVnoskaPaidWithoutDelays90 = DailyVnoska * IsGood90BasedOnDelay,
                      DailyVnoskaPaidWithDelays90 = DailyVnoska * IsBad90BasedOnDelay,
                      
                      DailyVnoskaPaidWithoutDelays60 = DailyVnoska * IsGood60BasedOnDelay,
                      DailyVnoskaPaidWithDelays60 = DailyVnoska * IsBad60BasedOnDelay,
                      
                      DailyVnoskaPaidWithoutDelays40 = DailyVnoska * IsGood40BasedOnDelay,
                      DailyVnoskaPaidWithDelays40 = DailyVnoska * IsBad40BasedOnDelay,
                      
                      DailyVnoskaPaidWithoutDelays30 = DailyVnoska * IsGood30BasedOnDelay,
                      DailyVnoskaPaidWithDelays30 = DailyVnoska * IsBad30BasedOnDelay,
                      
                      TimeAsGoodClient60 = TimeAsClientForThisCredit * IsGood60BasedOnDelay,
                      TimeAsBadClient60 = TimeAsClientForThisCredit * IsBad60BasedOnDelay,
                      TimeAsGoodClient30 = TimeAsClientForThisCredit * IsGood30BasedOnDelay,
                      TimeAsBadClient30 = TimeAsClientForThisCredit * IsBad30BasedOnDelay) %>%
        tidyr::fill(BadDate90, .direction = "down") %>%
        tidyr::fill(BadDate60, .direction = "down") %>%
        tidyr::fill(BadDate40, .direction = "down") %>%
        tidyr::fill(BadDate30, .direction = "down") %>%
        tidyr::fill(SumaPPaidWithDelays60, .direction = "down") %>%
        tidyr::fill(SumaPPaidWithDelays30, .direction = "down") %>%
        dplyr::mutate(NumberOfPreviousCredits = row_number() - 1,
                      
                      MaxBadDate90 = max(BadDate90),
                      MaxBadDate60 = max(BadDate60),
                      MaxBadDate40 = max(BadDate40),
                      MaxBadDate30 = max(BadDate30),
                      
                      LastBadDate = max(MaxBadDate90, MaxBadDate60, MaxBadDate40, MaxBadDate30),
                      # LastBadType = case_when(LastBadDate == MaxBadDate90 ~ 90,
                      #                         LastBadDate == MaxBadDate60 ~ 60,
                      #                         LastBadDate == MaxBadDate40 ~ 40,
                      #                         LastBadDate == MaxBadDate30 ~ 30,
                      #                         TRUE ~ 0),
                      # 
                      # PreviousMaxDailyVnoskaForCalculation0 = zoo::rollapplyr(DailyVnoskaPaidWithoutDelays30, width = 6, FUN = max, partial = TRUE, align = "right"),
                      # PreviousMaxDailyVnoskaForCalculation30 = zoo::rollapplyr(DailyVnoskaPaidWithoutDelays30, width = 6, FUN = max, partial = TRUE, align = "right"),
                      # PreviousMaxDailyVnoskaForCalculation40 = zoo::rollapplyr(DailyVnoskaPaidWithoutDelays30, width = 6, FUN = max, partial = TRUE, align = "right"),
                      # PreviousMaxDailyVnoskaForCalculation60 = zoo::rollapplyr(DailyVnoskaPaidWithoutDelays30, width = 6, FUN = max, partial = TRUE, align = "right"),
                      # PreviousMaxDailyVnoskaForCalculation90 = zoo::rollapplyr(DailyVnoskaPaidWithoutDelays30, width = 6, FUN = max, partial = TRUE, align = "right"),
                      
                      PreviousDailyVnoska = lag(DailyVnoska, default = 0),
                      
                      PreviousMaxDailyVnoskaPaidWithoutDelays60 = zoo::rollapplyr(DailyVnoskaPaidWithoutDelays60, width = 11, FUN = max, partial = TRUE, align = "right"),
                      PreviousMaxDailyVnoskaPaidWithoutDelays60 = lag(PreviousMaxDailyVnoskaPaidWithoutDelays60, default = 0),
                      
                      PreviousMaxDailyVnoskaPaidWithoutDelays30 = zoo::rollapplyr(DailyVnoskaPaidWithoutDelays30, width = 11, FUN = max, partial = TRUE, align = "right"),
                      PreviousMaxDailyVnoskaPaidWithoutDelays30 = lag(PreviousMaxDailyVnoskaPaidWithoutDelays30, default = 0),
                      
                      PreviousGood60BasedOnDelay = zoo::rollapplyr(IsGood60BasedOnDelay, width = 11, FUN = sum, partial = TRUE, align = "right"),
                      PreviousGood60BasedOnDelay = lag(PreviousGood60BasedOnDelay, default = 0),
                      
                      PreviousBad60BasedOnDelay = zoo::rollapplyr(IsBad60BasedOnDelay, width = 11, FUN = sum, partial = TRUE, align = "right"),
                      PreviousBad60BasedOnDelay = lag(PreviousBad60BasedOnDelay, default = 0),
                      
                      PreviousGood30BasedOnDelay = zoo::rollapplyr(IsGood30BasedOnDelay, width = 11, FUN = sum, partial = TRUE, align = "right"),
                      PreviousGood30BasedOnDelay = lag(PreviousGood30BasedOnDelay, default = 0),
                      
                      PreviousBad30BasedOnDelay = zoo::rollapplyr(IsBad30BasedOnDelay, width = 11, FUN = sum, partial = TRUE, align = "right"),
                      PreviousBad30BasedOnDelay = lag(PreviousBad30BasedOnDelay, default = 0),
                      
                      PreviousBadDate60 = lag(BadDate60),
                      PreviousBadDate30 = lag(BadDate30),
                      
                      NextCreditBeginDate = lead(CreditBeginDate),
                      PreviousCompleteDate = lag(CompleteDate),
                      PreviousTimeAsClient = zoo::rollapplyr(TimeAsClientForThisCredit, width = 11, FUN = sum, partial = TRUE, align = "right"),
                      PreviousTimeAsClient = lag(PreviousTimeAsClient, default = 0),
                      
                      PreviousTimeAsGoodClient60 = zoo::rollapplyr(TimeAsGoodClient60, width = 11, FUN = sum, partial = TRUE, align = "right"),
                      PreviousTimeAsGoodClient60 = lag(PreviousTimeAsGoodClient60, default = 0),
                      PreviousTimeAsBadClient60 = zoo::rollapplyr(TimeAsBadClient60, width = 11, FUN = sum, partial = TRUE, align = "right"),
                      PreviousTimeAsBadClient60 = lag(PreviousTimeAsBadClient60, default = 0),
                      
                      PreviousTimeAsGoodClient30 = zoo::rollapplyr(TimeAsGoodClient30, width = 11, FUN = sum, partial = TRUE, align = "right"),
                      PreviousTimeAsGoodClient30 = lag(PreviousTimeAsGoodClient30, default = 0),
                      PreviousTimeAsBadClient30 = zoo::rollapplyr(TimeAsBadClient30, width = 11, FUN = sum, partial = TRUE, align = "right"),
                      PreviousTimeAsBadClient30 = lag(PreviousTimeAsBadClient30, default = 0),
                      
                      PreviousSumaP = lag(SumaP, default = 0),
                      PreviousSumaI = lag(SumaI, default = 0),
                      
                      PreviousMaxSumaP = zoo::rollapplyr(SumaP, width = 11, FUN = max, partial = TRUE, align = "right"),
                      PreviousMaxSumaP = lag(PreviousMaxSumaP, default = 0),
                      
                      PreviousSumaPPaidWithoutDelays60 = zoo::rollapplyr(SumaPPaidWithoutDelays60, width = 11, FUN = max, partial = TRUE, align = "right"),
                      PreviousSumaPPaidWithoutDelays60 = lag(PreviousSumaPPaidWithoutDelays60, default = 0),
                      PreviousSumaPPaidWithDelays60 = zoo::rollapplyr(SumaPPaidWithDelays60, width = 11, FUN = max, partial = TRUE, align = "right"),
                      PreviousSumaPPaidWithDelays60 = lag(PreviousSumaPPaidWithDelays60, default = 0),
                      
                      PreviousSumaPPaidWithoutDelays30 = zoo::rollapplyr(SumaPPaidWithoutDelays30, width = 11, FUN = max, partial = TRUE, align = "right"),
                      PreviousSumaPPaidWithoutDelays30 = lag(PreviousSumaPPaidWithoutDelays30, default = 0),
                      PreviousSumaPPaidWithDelays30 = zoo::rollapplyr(SumaPPaidWithDelays30, width = 11, FUN = max, partial = TRUE, align = "right"),
                      PreviousSumaPPaidWithDelays30 = lag(PreviousSumaPPaidWithDelays30, default = 0),      
                      
                      SumaPIncreaseComparedToMax = SumaP - PreviousMaxSumaP,
                      SumaPIncreaseComparedToPrevious = SumaP - PreviousSumaP,
                      
                      SumaPIncreaseComparedToMaxPaid60 = SumaP - PreviousSumaPPaidWithoutDelays60,
                      SumaPIncreaseComparedToMaxPaid30 = SumaP - PreviousSumaPPaidWithoutDelays30,
                      
                      PreviousMaxDelayEver = zoo::rollapplyr(MaxDelayP, width = 11, FUN = max, partial = TRUE, align = "right"),
                      PreviousMaxDelayEver = lag(PreviousMaxDelayEver),
                      
                      PreviousMaxDelay3 = lag(MaxDelayP, default = 0, n = 3),
                      PreviousMaxDelay2 = lag(MaxDelayP, default = 0, n = 2),
                      PreviousMaxDelay1 = lag(MaxDelayP, default = 0, n = 1),
                      
                      PreviousPaidTotal = zoo::rollapplyr(PayedTotal, width = 11, FUN = sum, partial = TRUE, align = "right"),
                      PreviousPaidTotal = lag(PreviousPaidTotal, default = 0),
                      
                      PreviousSumCredits = zoo::rollapplyr(SumaP, width = 11, FUN = sum, partial = TRUE, align = "right"),
                      PreviousSumCredits = lag(PreviousSumCredits, default = 0),
                      
                      PreviousProfit = PreviousPaidTotal - PreviousSumCredits,
                      
                      TimeToPreviousCredit = as.numeric(CreditBeginDate - PreviousCompleteDate),
                      ChangeInDelay = PreviousMaxDelay1 - PreviousMaxDelay2,
                      
                      TimeToPreviousBadDate60 = CreditBeginDate - PreviousBadDate60,
                      TimeToPreviousBadDate30 = CreditBeginDate - PreviousBadDate30,
                      
                      BadSumaP60LastYear = ifelse(TimeToPreviousBadDate60 <= 365, PreviousSumaPPaidWithDelays60, 0),
                      BadSumaP30LastYear = ifelse(TimeToPreviousBadDate30 <= 365, PreviousSumaPPaidWithDelays30, 0),
                      
                      PreviousMaxDailyVnoskaPaidWithoutDelays60 = ifelse(PreviousMaxDailyVnoskaPaidWithoutDelays60 == 0, 5, PreviousMaxDailyVnoskaPaidWithoutDelays60),
                      ChangeInDailyVnoskaMax60 = DailyVnoska / PreviousMaxDailyVnoskaPaidWithoutDelays60,
                      
                      PreviousMaxDailyVnoskaPaidWithoutDelays30 = ifelse(PreviousMaxDailyVnoskaPaidWithoutDelays30 == 0, 5, PreviousMaxDailyVnoskaPaidWithoutDelays30),
                      ChangeInDailyVnoskaMax30 = DailyVnoska / PreviousMaxDailyVnoskaPaidWithoutDelays30,
                      
                      ChangeInDailyVnoska = DailyVnoska / PreviousDailyVnoska,
                      
                      IsRefinanced = ifelse(as.numeric(NextCreditBeginDate - CompleteDate) <= 3, 1, 0),
                      IsRefinanced = ifelse(is.na(IsRefinanced), 0, IsRefinanced),
                      
                      IsRefinancing = ifelse(as.numeric(CreditBeginDate - PreviousCompleteDate) <= 3, 1, 0),
                      IsRefinancing = ifelse(is.na(IsRefinancing), 1, IsRefinancing),
                      
                      IsRefinancing1 = ifelse(as.numeric(CreditBeginDate - PreviousCompleteDate) <= 0, 1, 0),
                      IsRefinancing1 = ifelse(is.na(IsRefinancing1), 1, IsRefinancing1),
                      
                      ChainNumber = ifelse(IsRefinancing == 0, 1, 0),
                      ChainNumber = cumsum(ChainNumber)) %>%
        group_by(ChainNumber) %>%
        mutate(SumaPInChain = sum(SumaP),
               PayedTotalInChain = sum(PayedTotal),
               
               SumaPTillNow = cumsum(SumaP),
               PayedTotalTilNow = cumsum(PayedTotal),
               
               TakenBefore = lag(SumaPTillNow, default = 0),
               PaidBefore = lag(PayedTotalTilNow, default = 0),
               
               PreviousCreditsInChain = row_number() - 1,
               PreviousCreditsInChain = pmin(PreviousCreditsInChain, 10),
               FirstCreditCode = first(CreditCode),
               FirstDateInChain = first(CreditBeginDate),
               
               ProfitInChain = PayedTotalInChain - PaidBefore - (SumaPInChain - TakenBefore),
               maxCompleteDate = max(CompleteDate),
               ChainIsFinished = ifelse(maxCompleteDate == currentDate, 0, 1),
               MaxDelayPInChain = max(MaxDelayP),
               TimeAsClientForTheLastCreditInChain = last(TimeAsClientForThisCredit),
               
               TotalTimeInChain = sum(TimeAsClientForThisCredit),
               CreditsInChain = n(),
               
               SumaPTillNow10 = zoo::rollapplyr(SumaP, width = 11, FUN = sum, partial = TRUE, align = "right"), 
               PayedTotalTilNow10 = zoo::rollapplyr(PayedTotal, width = 11, FUN = sum, partial = TRUE, align = "right")) %>%
        ungroup() %>%
        mutate(TakenAfter = SumaPInChain - TakenBefore,
               ProfitAfter = PayedTotalInChain - PaidBefore - (SumaPInChain - TakenBefore),
               PayedAfter = PayedTotalInChain - PaidBefore,
               
               
               RemainingToPay = SumaP + SumaI - PayedTotal,
               PreviousRemainingToBePaid = lag(RemainingToPay),
               
               PayedPercentage = PayedTotal / (SumaP + SumaI),
               PreviousPercentagePaid = lag(PayedPercentage),
               
               PreviousChains = ChainNumber - 1,
               CompleteDate = CompleteDateOriginal,
               CompleteDate = as.Date(CompleteDate, origin = "1970-01-01"),
               CreditBeginDate = as.Date(CreditBeginDate, origin = "1970-01-01"),
               BadDate60 = as.Date(BadDate60, origin = "1970-01-01"),
               BadDate30 = as.Date(BadDate30, origin = "1970-01-01"),
               PreviousBadDate60 = as.Date(PreviousBadDate60, origin = "1970-01-01"),
               PreviousBadDate30 = as.Date(PreviousBadDate30, origin = "1970-01-01"),
               IsRefinancingCheck = lag(CompleteDate))
      
      test <- previous_credits_history %>%
        dplyr::filter(CreditCode == "9999999")
      
      TimeT <- test$TimeToPreviousCredit
      TimeT <- ifelse(is.na(test$IsRefinancingCheck), -999999999, TimeT)
      
      if(test$PreviousMaxDelay1 > 30){
        
        VnoskaMax = case_when(TimeT >= 0 & test$PreviousMaxDelay1 > 90 ~ test$PreviousDailyVnoska * 0.7,
                              TimeT >= 0 & test$PreviousMaxDelay1 > 40 ~ test$PreviousDailyVnoska * 1,
                              TimeT >= 0 & test$PreviousMaxDelay1 > 30 ~ test$PreviousDailyVnoska * 1.3,
                              #TimeT >= 0 & test$PreviousMaxDelay1 <= 30 ~ test$PreviousDailyVnoska * 1.5,
                              TimeT < 0 & test$PreviousMaxDelay1 > 40 ~ test$PreviousDailyVnoska * 1,
                              TimeT < 0 & test$PreviousMaxDelay1 > 30 ~ test$PreviousDailyVnoska * 1.3,
                              #TimeT < 0 & test$PreviousMaxDelay1 <= 30 ~ test$PreviousDailyVnoska * 1.5,
                              TRUE ~ 5)
        
        VnoskaMax = pmax(VnoskaMax, 5)
        
      } else {
        ForVnoska <- previous_credits_history %>%
          dplyr::filter(CreditBeginDate >= LastBadDate) %>%
          dplyr::mutate(VnoskaMax = zoo::rollapplyr(DailyVnoskaPaidWithoutDelays30, width = 6, FUN = max, partial = TRUE, align = "right"),
                        VnoskaMax = lag(VnoskaMax, default = 0)) %>%
          dplyr::filter(CreditCode == "9999999")
        
        VnoskaMax = pmax(ForVnoska$VnoskaMax, 5)
      }
      
      AllProducts <- AllProducts %>%
        dplyr::filter(DailyVnoskaSuggested <= VnoskaMax)

      previous_credits_history <- previous_credits_history %>%
        dplyr::filter(CreditCode == "9999999")
      
      data <- data.frame(Product = as.numeric(CreditProductx),
                         Age = as.numeric(Age),
                         SumaP = as.numeric(previous_credits_history$SumaP),
                         DailyVnoska = 0,
                         PreviousDailyVnoska = as.numeric(previous_credits_history$PreviousDailyVnoska),
                         PreviousMaxDailyVnoskaPaidWithoutDelays30 = as.numeric(previous_credits_history$PreviousMaxDailyVnoskaPaidWithoutDelays30),
                         # ChangeInDailyVnoska = as.numeric(previous_credits_history$ChangeInDailyVnoska),
                         # ChangeInDailyVnoskaMax30 = as.numeric(previous_credits_history$ChangeInDailyVnoskaMax30),
                         ChangeInDelay = as.numeric(previous_credits_history$ChangeInDelay),
                         PreviousPercentagePaid = as.numeric(previous_credits_history$PreviousPercentagePaid),
                         PreviousProfit = as.numeric(previous_credits_history$PreviousProfit),
                         PreviousGood30BasedOnDelay = as.numeric(previous_credits_history$PreviousGood30BasedOnDelay),
                         PreviousBad30BasedOnDelay = as.numeric(previous_credits_history$PreviousBad30BasedOnDelay),
                         PreviousChains = as.numeric(previous_credits_history$PreviousChains),
                         PreviousSumaP = as.numeric(previous_credits_history$PreviousSumaP),
                         PreviousSumaPPaidWithoutDelays30 = as.numeric(previous_credits_history$PreviousSumaPPaidWithoutDelays30),
                         PreviousSumaPPaidWithDelays30 = as.numeric(previous_credits_history$PreviousSumaPPaidWithDelays30),
                         PreviousTimeAsGoodClient30 = as.numeric(previous_credits_history$PreviousTimeAsGoodClient30),
                         PreviousTimeAsBadClient30 = as.numeric(previous_credits_history$PreviousTimeAsBadClient30),
                         TimeToPreviousCredit = as.numeric(previous_credits_history$TimeToPreviousCredit),
                         TimeToPreviousBadDate30 = as.numeric(previous_credits_history$TimeToPreviousBadDate30),
                         SumaPIncreaseComparedToMaxPaid30 = as.numeric(previous_credits_history$SumaPIncreaseComparedToMaxPaid30),
                         SumaPIncreaseComparedToPrevious = as.numeric(previous_credits_history$SumaPIncreaseComparedToPrevious),
                         PreviousMaxDelay1 = as.numeric(previous_credits_history$PreviousMaxDelay1),
                         PreviousMaxDelay2 = as.numeric(previous_credits_history$PreviousMaxDelay2),
                         PreviousMaxDelay3 = as.numeric(previous_credits_history$PreviousMaxDelay3),
                         PreviousMaxDelayEver = as.numeric(previous_credits_history$PreviousMaxDelayEver),
                         PreviousRemainingToBePaid = as.numeric(previous_credits_history$PreviousRemainingToBePaid))
      
      # if(data$PreviousMaxDailyVnoskaPaidWithoutDelays30 <= 5){
      #   AllProducts <- AllProducts %>%
      #     dplyr::filter(DailyVnoskaSuggested <= 5)
      # } else {
      #   AllProducts <- AllProducts %>%
      #     dplyr::filter(DailyVnoskaSuggested / data$PreviousMaxDailyVnoskaPaidWithoutDelays30 <= 1.25)
      # }
      

      if(TimeT >= 0){
        
        xgbmodelReturning <- readRDS("./xgbmodelReturningProvenir.rds")
        
        data <- data %>%
          dplyr::left_join(AllProducts %>% select(Product, SumaPSuggested, DailyVnoskaSuggested, WeeksSuggested), by = c("Product")) %>%
          dplyr::mutate(SumaP = SumaPSuggested,
                        SumaPIncreaseComparedToMaxPaid30 = SumaP - PreviousSumaPPaidWithoutDelays30,
                        SumaPIncreaseComparedToPrevious = SumaP - PreviousSumaP,
                        DailyVnoska = DailyVnoskaSuggested,
                        ChangeInDailyVnoska = DailyVnoska - PreviousDailyVnoska,
                        ChangeInDailyVnoskaMax30 = DailyVnoska / PreviousMaxDailyVnoskaPaidWithoutDelays30) %>%
          dplyr::select(-SumaPSuggested, -DailyVnoskaSuggested)
        
        dataforpd <- dplyr::bind_rows(data)
        dataforpd <- dataforpd[, c(xgbmodelReturning$feature_names)]
        dataforpd <- model.matrix(~ ., data = dataforpd)[,-1]
        dataforpd <- xgb.DMatrix(dataforpd)
        
        data <- data %>%
          dplyr::mutate(Product = as.numeric(CreditProductx),
                        Prediction = predict(xgbmodelReturning, dataforpd),
                        RiskClass = case_when(Prediction < 0.0150 ~ 1,
                                              Prediction < 0.0364 ~ 2,
                                              Prediction < 0.0630 ~ 3,
                                              Prediction < 0.1109 ~ 4,
                                              Prediction < 0.1687 ~ 5,
                                              Prediction < 0.2415 ~ 6,
                                              Prediction <= 1 ~ 7,
                                              TRUE ~ 7)) %>%
          dplyr::mutate(MaxOffer = case_when(RiskClass = 1 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 1750, 8000),
                                             RiskClass = 2 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 1500, 8000),
                                             RiskClass = 3 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 1250, 8000),
                                             RiskClass = 4 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 1000, 8000),
                                             RiskClass = 5 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 750, 8000),
                                             RiskClass = 6 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 500, 8000),
                                             RiskClass = 7 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 0, 8000),
                                             TRUE ~ 3000),
                        MinOffer = case_when(Product == 21 ~ 500,
                                             Product == -12 ~ 500,
                                             Product == 0 ~ 500,
                                             Product == 13 ~ 1500,
                                             Product == 0 ~ 2000,
                                             Product == 1 ~ 500,
                                             Product == 10 ~ 1200,
                                             TRUE ~ MaxOffer),
                        MaxOffer = pmax(MinOffer, MaxOffer),
                        MaxOffer = pmin(8000, MaxOffer),
                        MinOffer = pmin(MaxOffer, MinOffer)) %>%
          dplyr::arrange(SumaP) %>%
          dplyr::filter(SumaP <= MaxOffer,
                        SumaP >= MinOffer) %>%
          dplyr::left_join(ProfitsReturning %>% select(RiskClass, ProfitPerc), by = "RiskClass") %>%
          dplyr::rename(ExpectedProfitPercent = ProfitPerc) %>%
          dplyr::mutate(ExpectedProfit = SumaP * ExpectedProfitPercent) %>%
          dplyr::arrange(desc(ExpectedProfit)) %>%
          dplyr::slice_head()
        
        scoringID <<- "4141004"
        scoreName <<- "iCredit RO Returning"
        scoreResultProbabilityByDefault <<- unique(round(data$Prediction, digits = 2))
        scoreResultNum <<- unique(round(100 * data$Prediction, digits = 2))
        maxAcceptableCreditSum <<- ifelse(data$RiskClass >= 7, 0, as.character(data$SumaP))
        period <- ifelse(data$RiskClass >= 7, 0, as.character(data$WeeksSuggested))
        inputParameters <<- ""
        
        scoringClass <<- ""
        scoringDecision <<- "Allowed"
      } else {
        xgbmodelRefinancing <- readRDS("./xgbmodelRefinancingProvenir.rds")
        
        data <- data %>%
          dplyr::left_join(AllProducts %>% select(Product, SumaPSuggested, DailyVnoskaSuggested, WeeksSuggested), by = c("Product")) %>%
          dplyr::mutate(SumaP = SumaPSuggested,
                        SumaPIncreaseComparedToMaxPaid30 = SumaP - PreviousSumaPPaidWithoutDelays30,
                        SumaPIncreaseComparedToPrevious = SumaP - PreviousSumaP,
                        DailyVnoska = DailyVnoskaSuggested,
                        ChangeInDailyVnoska = DailyVnoska - PreviousDailyVnoska,
                        ChangeInDailyVnoskaMax30 = DailyVnoska / PreviousMaxDailyVnoskaPaidWithoutDelays30,
                        TimeToPreviousCredit = 0) %>%
          dplyr::select(-SumaPSuggested, -DailyVnoskaSuggested)
        
        
        dataforpd <- dplyr::bind_rows(data)
        dataforpd <- dataforpd[, c(xgbmodelRefinancing$feature_names)]
        dataforpd <- model.matrix(~ ., data = dataforpd)[,-1]
        dataforpd <- xgb.DMatrix(dataforpd)
        
        data <- data %>%
          dplyr::mutate(Product = as.numeric(CreditProductx),
                        Prediction = predict(xgbmodelRefinancing, dataforpd),
                        RiskClass = case_when(Prediction < 0.01509 ~ 1,
                                              Prediction < 0.02481 ~ 2,
                                              Prediction < 0.04065 ~ 3,
                                              Prediction < 0.06553 ~ 4,
                                              Prediction < 0.13374 ~ 5,
                                              Prediction < 0.24457 ~ 6,
                                              Prediction < 0.35395 ~ 7,
                                              Prediction <= 1 ~ 8,
                                              TRUE ~ 8)) %>%
          dplyr::mutate(MaxOffer = case_when(RiskClass = 1 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 1750, 8000),
                                             RiskClass = 2 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 1500, 8000),
                                             RiskClass = 3 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 1250, 8000),
                                             RiskClass = 4 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 1000, 8000),
                                             RiskClass = 5 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 750, 8000),
                                             RiskClass = 6 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 500, 8000),
                                             RiskClass = 7 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 0, 8000),
                                             RiskClass = 8 & PreviousSumaPPaidWithoutDelays30 >= 0 ~ pmin(PreviousSumaPPaidWithoutDelays30 + 0, 8000),
                                             TRUE ~ 3000),
                        MinOffer = case_when(Product == 21 ~ 500,
                                             Product == -12 ~ 500,
                                             Product == 0 ~ 500,
                                             Product == 13 ~ 1500,
                                             Product == 0 ~ 2000,
                                             Product == 1 ~ 500,
                                             Product == 10 ~ 1200,
                                             TRUE ~ MaxOffer),
                        MinOffer = pmin(MinOffer, PreviousRemainingToBePaid + 1000),
                        MaxOffer = pmax(MinOffer, MaxOffer),
                        MaxOffer = pmin(8000, MaxOffer),
                        MinOffer = pmin(MaxOffer, MinOffer)) %>%
          dplyr::arrange(SumaP) %>%
          dplyr::filter(SumaP <= MaxOffer,
                        SumaP >= MinOffer) %>%
          dplyr::left_join(ProfitsRefinancing %>% select(RiskClass, ProfitPerc), by = "RiskClass") %>%
          dplyr::rename(ExpectedProfitPercent = ProfitPerc) %>%
          dplyr::mutate(ExpectedProfit = SumaP * ExpectedProfitPercent) %>%
          dplyr::arrange(desc(ExpectedProfit)) %>%
          dplyr::slice_head()
        
        
        scoringID <<- "4141005"
        scoreName <<- "iCredit RO Refinancing"
        scoreResultProbabilityByDefault <<- unique(round(data$Prediction, digits = 2))
        scoreResultNum <<- unique(round(100 * data$Prediction, digits = 2))
        maxAcceptableCreditSum <<- ifelse(data$RiskClass >= 7, 0, as.character(data$SumaP))
        inputParameters <<- ""
        period <- ifelse(data$RiskClass >= 7, 0, as.character(data$WeeksSuggested))
        
        scoringClass <<- ""
        scoringDecision <<- "Allowed"
      }
    }
  },
  # warning = function(w) {
  #   WarningMessage <<- as.character(w)
  # }, 
  error = function(e) {
    if (ErrorMessage == "No such product in product table") {
      ErrorMessage <<- "No such product in product table"
    } else {
      ErrorMessage <<- as.character(e)
    }
  }
)