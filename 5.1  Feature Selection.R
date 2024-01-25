########## Try another feature selection approach ########## 

#install.packages("Boruta")
library(Boruta)
library(Hmisc)

# Apply to ALL of the features (including these with low information value)

#### If necessary clear the dataset before applying the Boruta algorithm for feature selection
names(training_cut_FS) # only features and the target are left

# Count number of different values in a column (using the data type "factor")
# NA values are turned into another level- "missing"

boruta_featureselection = training_cut_FS
str(boruta_featureselection)
names(boruta_featureselection)

# Remove prediction column
boruta_featureselection$prediction = NULL
names(boruta_featureselection)

# # Store num of levels in each feature in a vector
# Numlevels = c()
# for (i in 1:ncol(boruta_featureselection_factors)) {
#   Numlevels[i] = length(levels(boruta_featureselection_factors[,i]))
# }
# 
# # If a feature has <= 20 levels  => turn it into a factor variable 
# 
# convert <- which(Numlevels <= 20) # features having less than or equal to 20 levels
# boruta_featureselection[,convert] <- data.frame(apply(boruta_featureselection[convert], 2, function(x){fct_explicit_na(x,"missing")}))
# 
# str(boruta_featureselection)


# Apply boruta feature selection
set.seed(11235)
boruta.train <- Boruta(GB_flag~., data = boruta_featureselection, doTrace = 2)

print(boruta.train)
# Boruta performed 99 iterations in 1.5283 mins.
# 28 attributes confirmed important: Count_previous_credits_12m,
# Count_previous_credits_3m, Count_previous_credits_6m, credits_ft_entities,
# Max_delay_3m and 23 more;
# 61 attributes confirmed unimportant: `Други вземания`, `Кредит, свързан с
# платежни услуги по чл.19 ЗПУПС`, `Кредитна карта`, `Кредитна линия`,
# `Паричен заем от всякакъв вид` and 56 more;
# 14 tentative attributes left: Age, Max_delay_12m,
# max_months_count_FALSE_Curr, max_off_balance_sheet_dues_TRUE_Curr,
# max_outstanding_overdue_interest_and_others_FALSE and 9 more;

# Variable Importance plot

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
  names(lz) <- colnames(boruta.train$ImpHistory)
  Labels <- sort(sapply(lz,median))
  axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

# Fix for Tentative variables

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta) #
# Boruta performed 99 iterations in 1.5283 mins.
# Tentatives roughfixed over the last 99 iterations.
# 28 attributes confirmed important: Count_previous_credits_12m,
# Count_previous_credits_3m, Count_previous_credits_6m, credits_ft_entities,
# Max_delay_3m and 23 more;
# 75 attributes confirmed unimportant: `Други вземания`, `Кредит, свързан с
# платежни услуги по чл.19 ЗПУПС`, `Кредитна карта`, `Кредитна линия`,
# `Паричен заем от всякакъв вид` and 70 more;

# Create a dataframe with final decision from boruta feature selection
boruta.df <- attStats(final.boruta)
table(boruta.df$decision)
# Tentative Confirmed  Rejected 
# 0           28        75 

boruta.df_accepted = boruta.df[boruta.df$decision == "Confirmed",]
boruta.df_accepted$Feature = row.names(boruta.df_accepted)
features = boruta.df_accepted$Feature
features = gsub("`","",features)
features

# [1] "total_amount_approved"                                
# [2] "total_amount_drawn"                                   
# [3] "total_outstanding_performing_principal"               
# [4] "total_credits"                                        
# [5] "total_entities"                                       
# [6] "max_max_cred_count_TRUE"                              
# [7] "max_outstanding_overdue_principal_TRUE_Curr"          
# [8] "max_outstanding_overdue_interest_and_others_TRUE_Curr"
# [9] "max_months_count_TRUE_Curr"                           
# [10] "max_max_cred_count_TRUE_Curr"                         
# [11] "Total_amount_all"                                     
# [12] "Total_oustanding_all"                                 
# [13] "Total_credits_all"                                    
# [14] "max_outstanding_overdue_principal_L2"                 
# [15] "max_outstanding_overdue_interest_and_others_L2"       
# [16] "worst_category_Curr"                                  
# [17] "max_outstanding_overdue_interest_and_others_Curr"     
# [18] "max_off_balance_sheet_dues_Curr"                      
# [19] "StatusLastToCurr"                                     
# [20] "credits_ft_entities"                                  
# [21] "PercOutstanding"                                      
# [22] "TotalLengthOfService"                                 
# [23] "Count_previous_credits_3m"                            
# [24] "Count_previous_credits_6m"                            
# [25] "Count_previous_credits_12m"                           
# [26] "Max_delay_3m"                                         
# [27] "Max_delay_6m"                                         
# [28] "SumaP_asPerc"      

training_BorutaFS = training_cut[,colnames(training_cut) %in% c(features,"CreditCode","GB_flag")]

names(training_BorutaFS) # 30 vars 
str(training_BorutaFS)  # 1064 obs, 30 variables, 28 features

# Convert to factors, taking care for NA-s  (they will be a separate category)
training_BorutaFS= as.data.frame(apply(training_BorutaFS,2,function(x){fct_explicit_na(x,"missing")}))
str(training_BorutaFS)

# Test model on fine classing
model_boruta = glm(GB_flag ~ . - CreditCode , data = training_BorutaFS, family = "binomial")
summary(model_boruta)

training_BorutaFS$prediction <- predict(model_boruta, training_BorutaFS, type = "response")
rcorr.cens(training_BorutaFS$prediction , training_BorutaFS$GB_flag)[2]  # 0.6103229  

# Create CA report on the chosen features from Boruta feature selection
# Remove factors first
training_BorutaFS$prediction = NULL
training_BorutaFS_careport = training_BorutaFS

for (i in 1:ncol(training_BorutaFS_careport)) {
  training_BorutaFS_careport[,i] = as.character(training_BorutaFS_careport[,i])
}

str(training_BorutaFS_careport)
training_BorutaFS_careport$GB_flag = as.integer(training_BorutaFS_careport$GB_flag)

# replace "missing" with NA-s
for (i in 1:ncol(training_BorutaFS_careport)) {
  training_BorutaFS_careport[,i] = with(training_BorutaFS_careport, ifelse(training_BorutaFS_careport[,i] == "missing", NA,training_BorutaFS_careport[,i] ))
}


# m.CA.Report(training_BorutaFS_careport,
#              dir = "E:/Projects/VIVA/Application Scorecards - Installment product - Reassessment Jul 2017/Reassessment of VIVA Scorecard New Clients/Output tables/CA Report/After new features addition/CSV v2/",exportToCSV = TRUE)
# 












