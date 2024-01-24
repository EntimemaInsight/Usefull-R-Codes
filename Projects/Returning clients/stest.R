library(XML)

xxx <- xmlToList("C:\\Users\\vladimir.zakov\\Desktop\\Ro_test\\20220307084948025_2560417240037_7245479c-1b53-4a6b-b1a0-dbcbf61b3538.xml")

yyy <- xxx[["ExternalRawData"]][["InternalDataLayer"]][["SmartCheckAPI"]][["GetAllClientApplications"]][[".attrs"]]
previous_credits <- yyy
previous_credits_history <- jsonlite::fromJSON(yyy, simplifyDataFrame = TRUE)
previous_credits_history <- previous_credits_history$data$allClientApplications

xxx <- xmlToList("D:\\R Projects\\HCC Ukraine Scoring Repeating and Returing Customers Vladi\\For Provenir\\Test xml\\WIth previous history\\arch_20220110\\20220110080251987_2802013425_ad9d45ae-8d4a-47d5-a818-f63ed157af78.xml")
yyy <- xxx[["ExternalRawData"]][["InternalDataLayer"]][["SmartCheckAPI"]][["GetAllClientApplications"]][[".attrs"]]
previous_credits <- yyy
previous_credits_history <- jsonlite::fromJSON(yyy, simplifyDataFrame = TRUE)
previous_credits_history <- previous_credits_history$data$allClientApplications



AllProducts <- AllProducts %>%
  filter(Product == -24) %>%
  select(-CreditProduct)



productxxxs <- previous_credits_history$product$key
# previous_credits_history <- previous_credits_history %>%
#   dplyr::select(creditCode, product, createDate, beginDate, completeDate,
#                 principalAmount, totalAmountDue, totalAmountPaid, earlyRepaymentAmount,
#                 currentDelay, maxDelay, creditInstallment) %>%
#   dplyr::select(creditCode, product.key, createDate, beginDate, completeDate,
#                 principalAmount, totalAmountDue, totalAmountPaid, earlyRepaymentAmount,
#                 currentDelay, maxDelay, creditInstallment)
# 
# class(previous_credits_history$product)
currentDate <- Sys.Date()
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
  dplyr::mutate(period = case_when(CreditProduct == -38 ~ 30,
                                   CreditProduct == -37 ~ 30,
                                   CreditProduct == -36 ~ 14,
                                   CreditProduct == -35 ~ 30,
                                   CreditProduct == -34 ~ 7,
                                   CreditProduct == -29 ~ 7,
                                   TRUE ~ 7),
                TimeAsClientForThisCredit = ifelse(is.na(CompleteDate), as.numeric(currentDate - CreditBeginDate),
                                                   as.numeric(CompleteDate - CreditBeginDate)),
                
                IsGood60BasedOnDelay = ifelse(MaxDelayP > 60, 0, 1),
                IsBad60BasedOnDelay = ifelse(MaxDelayP > 60, 1, 0),
                
                IsGood30BasedOnDelay = ifelse(MaxDelayP > 30, 0, 1),
                IsBad30BasedOnDelay = ifelse(MaxDelayP > 30, 1, 0),
                
                BadDate60 = case_when(IsBad60BasedOnDelay == 1 & is.na(CompleteDate) ~ currentDate,
                                      IsBad60BasedOnDelay == 1 & !is.na(CompleteDate) ~ CompleteDate, 
                                      TRUE ~ ymd("1970-01-01")), 
                
                BadDate30 = case_when(IsBad30BasedOnDelay == 1 & is.na(CompleteDate) ~ currentDate,
                                      IsBad30BasedOnDelay == 1 & !is.na(CompleteDate) ~ CompleteDate, 
                                      TRUE ~ ymd("1970-01-01"))) %>%
  tidyr::fill(BadDate60, .direction = "down") %>%
  tidyr::fill(BadDate30, .direction = "down") %>%
  dplyr::mutate(NumberOfPreviousCredits = row_number() - 1,
                
                PreviousPaidTotal = cumsum(PayedTotal) - PayedTotal,
                PreviousSumCredits = cumsum(SumaP) - SumaP,
                PreviousProfit = PreviousPaidTotal - PreviousSumCredits,
                
                RemainingToPay = SumaP + SumaI - PayedTotal,
                PreviousRemainingToBePaid = lag(RemainingToPay),
                PayedPercentage = PayedTotal / (SumaP + SumaI),
                PreviousPercentagePaid = lag(PayedPercentage),
                
                NextCreditBeginDate = lead(CreditBeginDate),
                PreviousCompleteDate = lag(CompleteDate),
                
                PreviousBadDate60 = lag(BadDate60),
                PreviousBadDate30 = lag(BadDate30),
                
                TimeToPreviousCredit = as.numeric(CreditBeginDate - PreviousCompleteDate),
                
                TimeToPreviousBadDate60 = CreditBeginDate - PreviousBadDate60,
                TimeToPreviousBadDate30 = CreditBeginDate - PreviousBadDate30,
                
                PreviousSumaP = lag(SumaP, default = 0),
                
                PreviousGood60BasedOnDelay = cumsum(IsGood60BasedOnDelay),
                PreviousGood60BasedOnDelay = lag(PreviousGood60BasedOnDelay, default = 0),
                PreviousBad60BasedOnDelay = cumsum(IsBad60BasedOnDelay),
                PreviousBad60BasedOnDelay = lag(PreviousBad60BasedOnDelay, default = 0),
                
                PreviousGood30BasedOnDelay = cumsum(IsGood30BasedOnDelay),
                PreviousGood30BasedOnDelay = lag(PreviousGood30BasedOnDelay, default = 0),
                PreviousBad30BasedOnDelay = cumsum(IsBad30BasedOnDelay),
                PreviousBad30BasedOnDelay = lag(PreviousBad30BasedOnDelay, default = 0),
                
                SumaPPaidWithoutDelays60 = SumaP * IsGood60BasedOnDelay,
                SumaPPaidWithDelays60 = SumaP * IsBad60BasedOnDelay,
                SumaPPaidWithoutDelays30 = SumaP * IsGood30BasedOnDelay,
                SumaPPaidWithDelays30 = SumaP * IsBad30BasedOnDelay,
                
                TimeAsGoodClient60 = TimeAsClientForThisCredit * IsGood60BasedOnDelay,
                TimeAsBadClient60 = TimeAsClientForThisCredit * IsBad60BasedOnDelay,
                TimeAsGoodClient30 = TimeAsClientForThisCredit * IsGood30BasedOnDelay,
                TimeAsBadClient30 = TimeAsClientForThisCredit * IsBad30BasedOnDelay,
                
                PreviousTimeAsGoodClient60 = cumsum(TimeAsGoodClient60) - TimeAsGoodClient60,
                PreviousTimeAsBadClient60 = cumsum(TimeAsBadClient60) - TimeAsBadClient60,
                PreviousTimeAsGoodClient30 = cumsum(TimeAsGoodClient60) - TimeAsGoodClient30,
                PreviousTimeAsBadClient30 = cumsum(TimeAsBadClient60) - TimeAsBadClient30,
                
                PreviousSumaPPaidWithoutDelays60 = cummax(SumaPPaidWithoutDelays60),
                PreviousSumaPPaidWithoutDelays60 = lag(PreviousSumaPPaidWithoutDelays60, default = 0),
                PreviousSumaPPaidWithDelays60 = cummax(SumaPPaidWithDelays60),
                PreviousSumaPPaidWithDelays60 = lag(PreviousSumaPPaidWithDelays60, default = 0),
                
                PreviousSumaPPaidWithoutDelays30 = cummax(SumaPPaidWithoutDelays30),
                PreviousSumaPPaidWithoutDelays30 = lag(PreviousSumaPPaidWithoutDelays30, default = 0),
                PreviousSumaPPaidWithDelays30 = cummax(SumaPPaidWithDelays30),
                PreviousSumaPPaidWithDelays30 = lag(PreviousSumaPPaidWithDelays30, default = 0),
                
                DailyVnoska = Vnoska / period,
                
                DailyVnoskaPaidWithoutDelays60 = DailyVnoska * IsGood60BasedOnDelay,
                DailyVnoskaPaidWithDelays60 = DailyVnoska * IsBad60BasedOnDelay,
                
                DailyVnoskaPaidWithoutDelays30 = DailyVnoska * IsGood30BasedOnDelay,
                DailyVnoskaPaidWithDelays30 = DailyVnoska * IsBad30BasedOnDelay,
                
                PreviousDailyVnoska = lag(DailyVnoska, default = 0),
                
                PreviousMaxDailyVnoskaPaidWithoutDelays60 = cummax(DailyVnoskaPaidWithoutDelays60),
                PreviousMaxDailyVnoskaPaidWithoutDelays60 = lag(PreviousMaxDailyVnoskaPaidWithoutDelays60, default = 0),
                
                PreviousMaxDailyVnoskaPaidWithoutDelays30 = cummax(DailyVnoskaPaidWithoutDelays30),
                PreviousMaxDailyVnoskaPaidWithoutDelays30 = lag(PreviousMaxDailyVnoskaPaidWithoutDelays30, default = 0),
                
                PreviousMaxDailyVnoskaPaidWithoutDelays60 = ifelse(PreviousMaxDailyVnoskaPaidWithoutDelays60 == 0, 5, PreviousMaxDailyVnoskaPaidWithoutDelays60),
                ChangeInDailyVnoskaMax60 = DailyVnoska / PreviousMaxDailyVnoskaPaidWithoutDelays60,
                PreviousMaxDailyVnoskaPaidWithoutDelays30 = ifelse(PreviousMaxDailyVnoskaPaidWithoutDelays30 == 0, 5, PreviousMaxDailyVnoskaPaidWithoutDelays30),
                ChangeInDailyVnoskaMax30 = DailyVnoska / PreviousMaxDailyVnoskaPaidWithoutDelays30,
                
                PreviousMaxDelayEver = cummax(MaxDelayP),
                PreviousMaxDelayEver = lag(PreviousMaxDelayEver),
                
                SumaPIncreaseComparedToMaxPaid60 = SumaP - PreviousSumaPPaidWithoutDelays60,
                SumaPIncreaseComparedToMaxPaid30 = SumaP - PreviousSumaPPaidWithoutDelays30,
                SumaPIncreaseComparedToPrevious = SumaP - PreviousSumaP,
                
                PreviousMaxDelay3 = lag(MaxDelayP, default = -999, n = 3),
                PreviousMaxDelay2 = lag(MaxDelayP, default = -999, n = 2),
                PreviousMaxDelay1 = lag(MaxDelayP, default = -999, n = 1),
                
                ChangeInDelay = PreviousMaxDelay1 - PreviousMaxDelay2,
                
                IsRefinanced = ifelse(as.numeric(NextCreditBeginDate - CompleteDate) <= 3, 1, 0),
                IsRefinanced = ifelse(is.na(IsRefinanced), 0, IsRefinanced),
                
                IsRefinancing = ifelse(as.numeric(CreditBeginDate - PreviousCompleteDate) <= 3, 1, 0),
                IsRefinancing = ifelse(is.na(IsRefinancing), 0, IsRefinancing),
                
                IsParallel = ifelse(as.numeric(CreditBeginDate - PreviousCompleteDate) <= -1, 1, 0),
                IsParallel = ifelse(is.na(IsParallel), 1, 0),
                
                ChainNumber = ifelse(IsRefinancing == 0 & IsParallel == 0, 1, 0),
                ChainNumber = cumsum(ChainNumber),
                PreviousChains = ChainNumber - 1) %>%
  dplyr::mutate(SumaPInChain = sum(SumaP)) %>%
  dplyr::group_by(ChainNumber) %>%
  dplyr::mutate(SumaPInChain = sum(SumaP),
                PayedTotalInChain = sum(PayedTotal),
                PreviousCreditsInChain = row_number() - 1,
                FirstCreditCode = first(CreditCode),
                
                TakenBefore = lag(cumsum(SumaP), default = 0),
                PaidBefore = lag(cumsum(PayedTotal), default = 0),
                
                ProfitInChain = PayedTotalInChain - PaidBefore - (SumaPInChain - TakenBefore),
                MaxDelayPInChain = max(MaxDelayP),
                TimeAsClientForTheLastCreditInChain = last(TimeAsClientForThisCredit)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ChainID = paste0(FirstCreditCode))
  
