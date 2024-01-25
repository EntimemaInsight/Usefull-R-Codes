######### Feature Selection ######### 

library(Hmisc)
library(forcats)


names(training_cut)

# Remove the keys and leave a dataframe containing only the selected features after fine classing and the target
training_cut_FS =training_cut
names(training_cut_FS)

training_cut_FS = training_cut_FS[,3:ncol(training_cut_FS)]
# names(dev_sample_cut_train_haveCKR_FS)
# ncol(dev_sample_cut_train_haveCKR_FS)
# str(dev_sample_cut_train_haveCKR_FS)
#  
# Convert to factor while taking care for NA-values - they should be part of the levels in the factor variable


# NA values are turned into another level- "missing"
training_cut_FS=as.data.frame(apply(training_cut_FS,2,function(x){fct_explicit_na(x,"missing")}))
str(training_cut_FS)
names(training_cut_FS)

#--- Run from here ---#
vec.data<-c() #concatenates data to be modelled on
maxGinisteps = double(length=103) #stores maximum GINI on each iteration
minGinisteps = double(length=103)

for (j in 1:103) 
{
  GINI_c=double(length=103) #stores the adjusted r squared
  
  for (i in 1:103) 
  {
    if (! i %in% vec.data) 
    {
      model = lm(GB_flag ~ .,data = training_cut_FS[,c(i,vec.data,104)])
      training_cut_FS$prediction <- predict(model, training_cut_FS, type = "response")
      GINI_c[i]= rcorr.cens(training_cut_FS$prediction , training_cut_FS$GB_flag)[2]  
    }
  }
  #summary(model)
  round(GINI_c,2)
  maxGini = which.max(GINI_c)
  minGini = which(GINI_c==min(GINI_c[GINI_c>0]))
  maxGinisteps[j] = GINI_c[maxGini]
  minGinisteps[j] = GINI_c[maxGini]
  vec.data <- c(vec.data,maxGini)
}

#There were 50 or more warnings (use warnings() to see the first 50)

#---    Till here    ---#

maxGinisteps
# [1] 0.2897468 0.4136506 0.4862617 0.5408030 0.5794736 0.6195259 0.6538252 0.6754557 0.6980827
# [10] 0.7192815 0.7447917 0.7636187 0.7796822 0.7948953 0.8092182 0.8223719 0.8330277 0.8442947
# [19] 0.8536485 0.8645833 0.8765147 0.8872901 0.8942655 0.9025962 0.9096514 0.9155772 0.9223932
# [28] 0.9275616 0.9324378 0.9365699 0.9411405 0.9457775 0.9502418 0.9531649 0.9571907 0.9609906
# [37] 0.9640333 0.9663053 0.9682053 0.9703975 0.9721115 0.9736527 0.9749548 0.9765492 0.9778646
# [46] 0.9788079 0.9797646 0.9806415 0.9813457 0.9822226 0.9829932 0.9837638 0.9844414 0.9852652
# [55] 0.9857834 0.9863281 0.9869127 0.9873113 0.9876834 0.9881218 0.9884407 0.9888127 0.9892512
# [64] 0.9896631 0.9899819 0.9902211 0.9906330 0.9908987 0.9911511 0.9913770 0.9916959 0.9921742
# [73] 0.9924532 0.9929581 0.9932371 0.9934763 0.9934896 0.9934896 0.9934630 0.9935560 0.9936490
# [82] 0.9937420 0.9937553 0.9939413 0.9943266 0.9943266 0.9945126 0.9944994 0.9944728 0.9944595
# [91] 0.9943931 0.9943665 0.9943399 0.9942336 0.9941539 0.9941008 0.9939679 0.9937155 0.9936490
# [100] 0.9934497 0.9930910 0.9924134 0.9915232

colnames(training_cut_FS)[vec.data[1:103]] #
# [1] "Total_amount_all"                                                  
# [2] "total_outstanding_overdue_principal"                               
# [3] "CompanyArea"                                                       
# [4] "Max_delay_3m"                                                      
# [5] "WorkExperienceCurrentWorkMonths_new"                               
# [6] "max_outstanding_overdue_principal_L2"                              
# [7] "TypeOfContract"                                                    
# [8] "PercOutstanding"                                                   
# [9] "total_outstanding_performing_principal"                            
# [10] "TotalLengthOfService"                                              
# [11] "total_amount_approved"                                             
# [12] "Age"                                                               
# [13] "max_months_count_FALSE_L2"                                         
# [14] "SumaP"                                                             
# [15] "percentage_paid_amount"                                            
# [16] "Incomes.Costs"                                                     
# [17] "Total_credits_all"                                                 
# [18] "SumaP_asPerc"                                                      
# [19] "total_amount_drawn"                                                
# [20] "ClientSalary"                                                      
# [21] "CreditPurpose"                                                     
# [22] "Total_oustanding_all"                                              
# [23] "max_outstanding_overdue_principal"                                 
# [24] "max_outstanding_overdue_principal_Curr"                            
# [25] "MaxDelayLastCredit"                                                
# [26] "max_outstanding_overdue_principal_FALSE_L2"                        
# [27] "ClientEducation"                                                   
# [28] "worst_category_Curr"                                               
# [29] "credits_ft_entities"                                               
# [30] "Count_previous_credits_12m"                                        
# [31] "max_outstanding_overdue_principal_TRUE"                            
# [32] "max_outstanding_overdue_interest_and_others"                       
# [33] "max_months_count_TRUE_Curr"                                        
# [34] "PreviousCreditsCount"                                              
# [35] "max_months_count_TRUE"                                             
# [36] "Count_previous_credits_6m"                                         
# [37] "max_outstanding_overdue_interest_and_others_FALSE"                 
# [38] "ClientFamilyStatus"                                                
# [39] "total_credits"                                                     
# [40] "total_amount_approved_after"                                       
# [41] "PercOverduePrincipal"                                              
# [42] "max_outstanding_overdue_interest_and_others_FALSE_Curr"            
# [43] "max_max_cred_count_TRUE_Curr"                                      
# [44] "max_max_cred_count_FALSE_L2"                                       
# [45] "Други вземания"                                                    
# [46] "TypeOfHousing"                                                     
# [47] "Кредит, свързан с платежни услуги по чл.19 ЗПУПС"                  
# [48] "Weeks"                                                             
# [49] "max_outstanding_overdue_interest_and_others_FALSE_L2"              
# [50] "max_outstanding_overdue_interest_and_others_L2"                    
# [51] "max_outstanding_overdue_principal_TRUE_L2"                         
# [52] "Max_delay_6m"                                                      
# [53] "Count_previous_credits_3m"                                         
# [54] "max_outstanding_overdue_interest_and_others_TRUE"                  
# [55] "max_outstanding_overdue_interest_and_others_TRUE_Curr"             
# [56] "MaxNumMonthsOverdueStatus"                                         
# [57] "total_entities"                                                    
# [58] "RejectedCreditsCount"                                              
# [59] "total_credits_after"                                               
# [60] "worst_curr_category_actives"                                       
# [61] "max_off_balance_sheet_dues_TRUE_Curr"                              
# [62] "MaxNumCreditsOverdueStatus"                                        
# [63] "Max_delay_12m"                                                     
# [64] "worst_category_TRUE_Curr"                                          
# [65] "Паричен заем от всякакъв вид"                                      
# [66] "max_off_balance_sheet_dues_FALSE_Curr"                             
# [67] "worst_category_FALSE"                                              
# [68] "worst_category_L2"                                                 
# [69] "max_off_balance_sheet_dues"                                        
# [70] "GetSalaryType"                                                     
# [71] "Кредитна карта"                                                    
# [72] "max_off_balance_sheet_dues_FALSE"                                  
# [73] "max_months_count_FALSE"                                            
# [74] "max_months_count_FALSE_Curr"                                       
# [75] "max_outstanding_overdue_principal_FALSE_Curr"                      
# [76] "worst_category_TRUE_L2"                                            
# [77] "max_off_balance_sheet_dues_L2"                                     
# [78] "total_monthly_instalment"                                          
# [79] "worst_category_FALSE_L2"                                           
# [80] "max_off_balance_sheet_dues_Curr"                                   
# [81] "max_off_balance_sheet_dues_FALSE_L2"                               
# [82] "max_max_cred_count_FALSE_Curr"                                     
# [83] "max_off_balance_sheet_dues_TRUE_L2"                                
# [84] "StaffType"                                                         
# [85] "max_outstanding_overdue_principal_TRUE_Curr"                       
# [86] "max_max_cred_count_TRUE_L2"                                        
# [87] "StatusLastToCurr"                                                  
# [88] "Поръчителство-авал"                                                
# [89] "max_off_balance_sheet_dues_TRUE"                                   
# [90] "ClientSex"                                                         
# [91] "worst_category_TRUE"                                               
# [92] "worst_category"                                                    
# [93] "max_months_count_TRUE_L2"                                          
# [94] "Финансов лизинг"                                                   
# [95] "max_outstanding_overdue_interest_and_others_TRUE_L2"               
# [96] "Кредитна линия"                                                    
# [97] "Прехвърлено вземане чрез цесия, встъпване в дълг и поемане на дълг"
# [98] "worst_category_FALSE_Curr"                                         
# [99] "max_max_cred_count_TRUE"                                           
# [100] "max_max_cred_count_FALSE"                                          
# [101] "Овърдрафт"                                                         
# [102] "max_outstanding_overdue_interest_and_others_Curr"                  
# [103] "max_outstanding_overdue_principal_FALSE"        
rm(model)

# --------> Leave the features till reaching 0.66 GINI (the value converges after that and the rest of the variables have less added value)

chosenfeatures_FS = colnames(training_cut_FS)[vec.data[1:30]] #
# [1] "Total_amount_all"                           "total_outstanding_overdue_principal"       
# [3] "CompanyArea"                                "Max_delay_3m"                              
# [5] "WorkExperienceCurrentWorkMonths_new"        "max_outstanding_overdue_principal_L2"      
# [7] "TypeOfContract"                             "PercOutstanding"                           
# [9] "total_outstanding_performing_principal"     "TotalLengthOfService"                      
# [11] "total_amount_approved"                      "Age"                                       
# [13] "max_months_count_FALSE_L2"                  "SumaP"                                     
# [15] "percentage_paid_amount"                     "Incomes.Costs"                             
# [17] "Total_credits_all"                          "SumaP_asPerc"                              
# [19] "total_amount_drawn"                         "ClientSalary"                              
# [21] "CreditPurpose"                              "Total_oustanding_all"                      
# [23] "max_outstanding_overdue_principal"          "max_outstanding_overdue_principal_Curr"    
# [25] "MaxDelayLastCredit"                         "max_outstanding_overdue_principal_FALSE_L2"
# [27] "ClientEducation"                            "worst_category_Curr"                       
# [29] "credits_ft_entities"                        "Count_previous_credits_12m"            

# Create a separate dataframe which contains the chosen features after Feature Seleciton, the target and unique key (kid)
names(training_cut)

training_forwardFS = training_cut[, colnames(training_cut) %in% 
                                          c(chosenfeatures_FS,"CreditCode","GB_flag")]


names(training_forwardFS) # 32vars, 30 features
str(training_forwardFS)

# Create a CA Report

# 1st convert to character all columns
for (i in 1:ncol(training_forwardFS)) {
  training_forwardFS[,i] = as.character(training_forwardFS[,i])
}

str(training_forwardFS)
training_forwardFS$GB_flag = as.integer(training_forwardFS$GB_flag)

# create a CA report
#m.CA.Report(training_forwardFS,"C:/Users/Gloria Hristova/Desktop/CSV v3 - FS/", exportToCSV = TRUE)

# Convert to factors, taking care for NA-s  (they will be a separate category)
str(training_forwardFS)
training_forwardFS= as.data.frame(apply(training_forwardFS,2,function(x){fct_explicit_na(x,"missing")}))
str(training_forwardFS)

# Test model on fine classing
model_forwardFS = glm(GB_flag ~ . - CreditCode , data = training_forwardFS, family = "binomial")
summary(model_forwardFS)

training_forwardFS$prediction <- predict(model_forwardFS, training_forwardFS, type = "response")
rcorr.cens(training_forwardFS$prediction , training_forwardFS$GB_flag)[2]  # 0.7112246 (features are more)  

################################




