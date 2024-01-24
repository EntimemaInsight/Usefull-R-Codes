library(tidyverse)
library(writexl)
library(data.table)

# Full CRIF Fraud Score Scored 
Axi_PL_CRIF_Fraud_Scored = readRDS("//hind.smartitbg.int/FileServer/Data Analyses/Analysis/1.Projects/113.WCard PL/1.Scoring/11 CRIF Scoring Fraud/Output Data/070_Scored_Data_2019-12-11_2022-07-20.rds")

# Distinct duplicate Offer IDs
Axi_PL_CRIF_Fraud_Scored = Axi_PL_CRIF_Fraud_Scored %>%
  distinct(OfferID, .keep_all = T)

# Export to excel and csv 
write_xlsx(Axi_PL_CRIF_Fraud_Scored, "./Output Data/030_Axi_PL_CRIF_Fraud_Scored.xlsx")
fwrite(Axi_PL_CRIF_Fraud_Scored,"./Output Data/030_Axi_PL_CRIF_Fraud_Scored.csv")
saveRDS(Axi_PL_CRIF_Fraud_Scored,"./Output Data/030_Axi_PL_CRIF_Fraud_Scored.rds")


### Combine data about all rules found to lead to losses ### 

Provenir_Scoring = readRDS("//hind.smartitbg.int/FileServer/Data Analyses/Analysis/1.Projects/113.WCard PL/7.Data/01 Automate Scoring Info Extraction from Provenir xmls/Output Data/020_WC_PL_Full_Scoring_Data.rds")

CRIF_Rules = fread("./Input Data/axi_ovd_pr.csv")

Total_CRIF_New_Rules_Sample = Provenir_Scoring %>%
  select(application_number, application_date, Crif_Score_Class, Crif_Score_PD) %>%
  mutate(application_number = as.character(application_number)) %>%
  left_join(Axi_PL_CRIF_Fraud_Scored %>% 
              select(OfferID, CRIF_Fraud_Score_Class = scoringClass, CRIF_Fraud_Score_PD = PD)
            , by = c("application_number" = "OfferID")) %>%
  mutate(application_number = as.numeric(application_number)) %>%
  left_join(CRIF_Rules, by = c("application_number" = "ApplicationID")) %>%
  mutate(Flag_New_Rules = case_when(Crif_Score_Class %in% c(8,9,10) & pwip_overdue_closed_loans_number_p0_720_pr_or_pwip_overdue_active_loans_ovd_amount_p15_360_pr == TRUE ~ 1
                                    , Crif_Score_Class %in% c(8,9,10) & CRIF_Fraud_Score_Class %in% c(8,9,10) ~ 1
                                    , TRUE ~ 0)
         )

fwrite(Total_CRIF_New_Rules_Sample,"./Output Data/030_Total_CRIF_New_Rules_Sample.csv")

Check_Correlation = Total_CRIF_New_Rules_Sample %>%
  filter(as.Date(application_date) >= as.Date("2022-02-01")
         & !is.na(Crif_Score_PD)
         & !is.na(CRIF_Fraud_Score_PD)
         )

cor(Check_Correlation$Crif_Score_Class, Check_Correlation$CRIF_Fraud_Score_Class, method = "spearman") # 58.5% 





