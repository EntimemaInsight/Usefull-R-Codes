
WriteExcel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}
##### Scoring accounts for rule 3 ####
#### All credits ####
Rule2_scored <- Harold_Latest_UKR_1 %>% left_join(sample, by = c("KID"))

#### Test group difference with wilcox test ####

Rule2_scored_sample <- Harold_Latest_UKR_1 %>% left_join(sample, by = c("KID"))%>%
                        filter(CreditNumber == 2) %>% 
                        filter(Year >2012)%>%
                        filter(Period <201803) %>%
                        filter(is.na(RefinansNew)) %>%
                        mutate( Overruled = case_when (LastP <7500 & SumaP > 7500 ~ "Override 7500",
                                                       LastP >=7500 & SumaP > 10500 ~ "Override 10 500", 
                                                       TRUE ~ "Not override"))%>%
                        select(KID, Overruled,DelayGroup, Scoring.y )

table(Rule2_scored_sample$Overruled)

### Extract vectors for the test ###
GroupOverride <- Rule2_scored_sample[Rule2_scored_sample$Overruled =='Override 7500', ]

Group1 <-  Rule2_scored_sample[Rule2_scored_sample$Overruled =='Not override'& 
                        Rule2_scored_sample$DelayGroup == "0-30 Days", ]

x<- GroupOverride$Scoring.y
y<- Group1$Scoring.y
###apply test ####
res <- wilcox.test(x, y)
res

################
Stats_Rule2_scored <- Rule2_scored %>% filter(CreditNumber == 2) %>% 
  filter(Year >2012)%>%
  filter(Period <201803)%>%
  
  mutate( GotSumaP_max = case_when(LastP >= 7500 ~ 1, TRUE ~ 0), 
          
          Overruled = case_when (LastP <7500 & SumaP > 7500 ~ "Override 7500",
                                 LastP >=7500 & SumaP > 10500 ~ "Override 10 500", 
                                 TRUE ~ "Not override")) %>% 
  group_by(RefinansNew, Overruled, DelayGroup ) %>% 
  
  summarise(Total = n(), GotSumaP_max_avg = sum(GotSumaP_max), 
            AvgScore = mean(Scoring.y, na.rm = T),
            AvgSumaP = mean(SumaP), 
            AvgLastSumaP = mean(LastP), 
            RR_Total = round(sum(PayedTotal, na.rm = T)/ sum(SumToGetBack, na.rm = T),2), 
            RR_SumaP = round(sum(PayedTotal, na.rm = T)/ sum(SumaP, na.rm = T), 2), 
            RR_Maturity = round(sum(PayedTotal, na.rm = T)/ sum(SumaP, na.rm = T), 2),
            TermAvg = round(mean(Weeks),0),
            DelayAvg = round(mean(Delay),0),
            ClosedAccounts = sum(!is.na(CompleteDate)), 
            ClosedAccounts = round(sum(!is.na(CompleteDate))/ Total, 2))


WriteExcel(Stats_Rule2_scored)


Rule2_ICREDIT_scored <- Harold_Latest_UKR_1 %>% left_join(sample, by = c("KID")) %>% 
                            filter(CreditProduct == "ICREDIT")

Stats_Rule2_ICREDIT_scored <- Rule2_ICREDIT_scored %>% filter(CreditNumber == 2) %>% 
  filter(Year >2012)%>%
  filter(Period <201803)%>%
  
  mutate( GotSumaP_max = case_when(LastP >= 7500 ~ 1, TRUE ~ 0), 
          
          Overruled = case_when (LastP <7500 & SumaP > 7500 ~ "Override 7500",
                                 LastP >=7500 & SumaP > 10500 ~ "Override 10 500", 
                                 TRUE ~ "Not override")) %>% 
  group_by(RefinansNew, Overruled, DelayGroup ) %>% 
  
  summarise(Total = n(), GotSumaP_max_avg = sum(GotSumaP_max), 
            AvgScore = mean(Scoring.y, na.rm = T),
            AvgSumaP = mean(SumaP), 
            AvgLastSumaP = mean(LastP), 
            RR_Total = round(sum(PayedTotal, na.rm = T)/ sum(SumToGetBack, na.rm = T),2), 
            RR_SumaP = round(sum(PayedTotal, na.rm = T)/ sum(SumaP, na.rm = T), 2), 
            RR_Maturity = round(sum(PayedTotal, na.rm = T)/ sum(SumaP, na.rm = T), 2),
            TermAvg = round(mean(Weeks),0),
            DelayAvg = round(mean(Delay),0),
            ClosedAccounts = sum(!is.na(CompleteDate)), 
            ClosedAccounts = round(sum(!is.na(CompleteDate))/ Total, 2))


WriteExcel(Stats_Rule2_ICREDIT_scored)

#### export outpts ####
setwd("//10.254.1.4/Files/Analysis/9. Personal folders/Ivelina Vasileva/1. Projects/008 Ukraine EasyCredit/Checking Rules/Outputs")

rm(wb)
wb = createWorkbook()
sheet = createSheet(wb, sheetName = "AllCredits")
addDataFrame(Rule2_scored, sheet, showNA = T, characterNA = '0')

sheet = createSheet(wb, sheetName = "ICredit")
addDataFrame(Rule2_ICREDIT_scored, sheet, showNA = T, characterNA = '0')

saveWorkbook(wb, file = "Scored sample rule 3.xlsx")

