
#### Checking rules

###Select necessary columns from Harold

Harold_Latest_UKR_1 <- Harold_Latest_UKR %>% 
  
  select(KID, TypeOfContract , MoneyGrantedDateByCashDesk, Scoring, ScoringType, ClientEGN, RefinansNew) %>%
  
  left_join(CreProg_Latest_UKR_1, by = "KID") %>%
  arrange(ClientEGN, MoneyGrantedDateByCashDesk)%>%
  group_by(ClientEGN)%>%
  mutate(CreditNumber = seq(n()), 
         Fist_Credit = case_when(CreditNumber == 1 ~ 1, 
                                 TRUE ~ 2), 
         #LastCreditCompleteDate = lag(CompleteDate),
         PreviousDelay = lag(Delay),
         LastP = lag(SumaP))%>% 
  mutate(DelayGroup = case_when(PreviousDelay< 31 ~ "0-30 Days", 
                                PreviousDelay < 61 & PreviousDelay > 30 ~ "31-60 Days", 
                                PreviousDelay < 91 & PreviousDelay >60 ~ "61-90 Days", 
                                PreviousDelay >90 ~"Above 90"))


max_amount_table_overruled <- Harold_Latest_UKR_1 %>% filter (CreditNumber == 1) %>% filter(SumaP >7500)%>%
                        group_by(Period, TypeOfContract) %>% 
                        summarise(max(SumaP), Total = n(), TermAvg = round(mean(Weeks),0), 
                                  AvgDelay=round(mean(Delay), 2),
                                  median(Delay),
                                  RR_Total = round(sum(PayedTotal, na.rm = T)/ sum(SumToGetBack, na.rm = T),2), 
                                  RR_SumaP = round(sum(PayedTotal, na.rm = T)/ sum(SumaP, na.rm = T), 2), 
                                  RR_Maturity = round(sum(PayedTotal, na.rm = T)/ sum(SumaP, na.rm = T), 2), 
                                  AvgSummaP = mean(SumaP, na.rm = T))



#### 7500 Rule max amount and 10000 for client   
max_amount_table <- Harold_Latest_UKR_1 %>% filter (CreditNumber == 1) %>%
 # filter(SumaP >7500)%>%
  group_by(Year, CreditProduct) %>% 
  summarise(max(SumaP)
            # Total = n(), TermAvg = round(mean(Weeks),0), 
            # AvgDelay=round(mean(Delay), 2),
            # median(Delay),
            # RR_Total = round(sum(PayedTotal, na.rm = T)/ sum(SumToGetBack, na.rm = T),2), 
            # RR_SumaP = round(sum(PayedTotal, na.rm = T)/ sum(SumaP, na.rm = T), 2), 
            # RR_Maturity = round(sum(PayedTotal, na.rm = T)/ sum(SumaP, na.rm = T), 2)
            )%>% arrange(max(SumaP), Year)


## Util f for copy: 

WriteExcel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}
WriteExcel(max_amount_table)


#write.csv(max_amount_table_overruled, file = "max_amount_table.csv")

Rule2 <- Harold_Latest_UKR_1 

### 


Stats_Rule2 <- Rule2 %>% filter(CreditNumber == 2) %>% 
                              filter(Year >2012)%>%
                              filter(Period <201803)%>%
                              
                              mutate( GotSumaP_max = case_when(LastP >= 7500 ~ 1, TRUE ~ 0), 
                                     
                                      Overruled = case_when (LastP <7500 & SumaP > 7500 ~ "Override 7500",
                                      LastP >=7500 & SumaP > 10500 ~ "Override 10 500", 
                                      TRUE ~ "Not override")) %>% 
                              group_by(RefinansNew, Overruled, DelayGroup ) %>% 
                             
                              summarise(Total = n(), GotSumaP_max_avg = sum(GotSumaP_max), 
                                        AvgSumaP = mean(SumaP), 
                                        AvgLastSumaP = mean(LastP), 
                                        RR_Total = round(sum(PayedTotal, na.rm = T)/ sum(SumToGetBack, na.rm = T),2), 
                                        RR_SumaP = round(sum(PayedTotal, na.rm = T)/ sum(SumaP, na.rm = T), 2), 
                                        RR_Maturity = round(sum(PayedTotal, na.rm = T)/ sum(SumaP, na.rm = T), 2),
                                        TermAvg = round(mean(Weeks),0),
                                        DelayAvg = round(mean(Delay),0),
                                        ClosedAccounts = sum(!is.na(CompleteDate)), 
                                        ClosedAccounts = round(sum(!is.na(CompleteDate))/ Total, 2))


WriteExcel(Stats_Rule2)

