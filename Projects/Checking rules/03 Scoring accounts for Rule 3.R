library(xlsx)
library(tidyr)
##### Scoring accounts for rule 3 ####

### select only accounts only for ICREDIT ####
table(Rule3.1$CreditProduct)

Rule3.1_Icredit_scored <- Rule3.1 %>% 
                        filter(CreditProduct =="ICREDIT") %>% 
                        left_join(sample, by = c("KID"))
### SAMPLE TO SAVE ###
Rule3.1_Icredit_scored_sample <- Rule3.1_Icredit_scored%>% 
                                  filter(Period <= 201803) %>%
                                  select(KID, Scoring.y, Overrides) 
                                  
Group_1_BEP<- Rule3.1_Icredit_scored_sample[Rule3.1_Icredit_scored_sample$Overrides == 'Group 1 BEP', ]
x <- Group_1_BEP$Scoring.y

Normal_Group_1<- Rule3.1_Icredit_scored_sample[Rule3.1_Icredit_scored_sample$Overrides == 'Normal Group 1', ]
y <- Normal_Group_1$Scoring.y


#### Apply test #####

res1 <- wilcox.test(x,y)
res1

#### Create full data stats for all products ####

Rule3_Icredit_scored_stats <-
  Rule3.1_Icredit_scored %>% filter(Period <= 201803)%>%
  group_by(Overrides)%>% 
  summarise(Total = n(), 
            AvgScore = mean(Scoring.y, na.rm = T),
            AvgSumaP = mean(SumaP, na.rm = T), 
            AvgLastSumaP = mean(LastP, na.rm = T), 
            RR_Total = round(sum(PayedTotal, na.rm = T)/ sum(SumToGetBack, na.rm = T),2), 
            RR_SumaP = round(sum(PayedTotal, na.rm = T)/ sum(SumaP, na.rm = T), 2), 
            AvgDaySiff = round(mean(DiffDays, na.rm = T), 2),
            TermAvg = round(mean(Weeks),0),
            DelayAvg = round(mean(Delay),2),
            meanTarget = round(mean(Target),2),
            MedianDelayAvg = round(median(Delay, na.rm = T),0),
            AvgPreviousDelay = round(mean(PreviousDelay, na.rm = T),0),
            ClosedAccounts = sum(!is.na(CompleteDate)), 
            ClosedAccounts = round(sum(!is.na(CompleteDate))/ Total, 2))

#### All credits scored #####

Rule3.1_scored <-  Rule3.1 %>% left_join(sample, by = c("KID"))
#### SAMPLE TO SAVE ####
Rule3.1_scored_sample <- Rule3.1_scored %>%
                          filter(Period <= 201803) %>%
                          select(KID, Scoring.y, Overrides) 


### Create vectors for test ####

Group_1_BEP<- Rule3.1_scored_sample[Rule3.1_scored_sample$Overrides == 'Group 1 BEP', ]
x <- Group_1_BEP$Scoring.y

Normal_Group_1<- Rule3.1_scored_sample[Rule3.1_scored_sample$Overrides == 'Normal Group 1', ]
y <- Normal_Group_1$Scoring.y


#### Apply test #####

res1 <- wilcox.test(x,y)
res1

### Create vectors for test ####

Group_1_BEP<- Rule3.1_scored_sample[Rule3.1_scored_sample$Overrides == 'Override Group 2', ]
x <- Group_1_BEP$Scoring.y

Normal_Group_1<- Rule3.1_scored_sample[Rule3.1_scored_sample$Overrides == 'Normal Group 2', ]
y <- Normal_Group_1$Scoring.y


#### Apply test #####

res2 <- wilcox.test(x,y)
res2

### Create vectors for test ####

Group_1_BEP<- Rule3.1_scored_sample[Rule3.1_scored_sample$Overrides == 'Group 2 BEP', ]
x <- Group_1_BEP$Scoring.y

Normal_Group_1<- Rule3.1_scored_sample[Rule3.1_scored_sample$Overrides == 'Normal Group 2', ]
y <- Normal_Group_1$Scoring.y


#### Apply test #####

res3 <- wilcox.test(x,y)
res3
#### Create the stats #####


Rule3_scored_stats <-
  Rule3.1_scored %>% filter(Period <= 201803)%>%
  group_by(Overrides)%>% 
  summarise(Total = n(), 
            AvgScore = mean(Scoring.y, na.rm = T),
            AvgSumaP = mean(SumaP, na.rm = T), 
            AvgLastSumaP = mean(LastP, na.rm = T), 
            RR_Total = round(sum(PayedTotal, na.rm = T)/ sum(SumToGetBack, na.rm = T),2), 
            RR_SumaP = round(sum(PayedTotal, na.rm = T)/ sum(SumaP, na.rm = T), 2), 
            AvgDaySiff = round(mean(DiffDays, na.rm = T), 2),
            TermAvg = round(mean(Weeks),0),
            DelayAvg = round(mean(Delay),2),
            meanTarget = round(mean(Target),2),
            MedianDelayAvg = round(median(Delay, na.rm = T),0),
            AvgPreviousDelay = round(mean(PreviousDelay, na.rm = T),0),
            ClosedAccounts = sum(!is.na(CompleteDate)), 
            ClosedAccounts = round(sum(!is.na(CompleteDate))/ Total, 2))

WriteExcel(Rule3_Icredit_scored_stats)

# 
# #### export outpts ####
# setwd("//10.254.1.4/Files/Analysis/9. Personal folders/Ivelina Vasileva/1. Projects/008 Ukraine EasyCredit/Checking Rules/Outputs")
# 
# rm(wb)
# wb = createWorkbook()
# sheet = createSheet(wb, sheetName = "AllCredits")
# addDataFrame(Rule3.1_scored_sample, sheet, showNA = T, characterNA = '0')
# 
# sheet = createSheet(wb, sheetName = "ICredit")
# addDataFrame(Rule3.1_Icredit_scored_sample, sheet, showNA = T, characterNA = '0')
# 
# saveWorkbook(wb, file = "Scored sample rule 3.xlsx")
