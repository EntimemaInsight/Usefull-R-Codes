########### Reassessment of VIVA Scorecard for Existing clients ########### 

########### Script 1: Read and Transform CKR Data ########### 
####### ################# 
# Load libraries

library(lubridate)
library(plyr)
library(dplyr)
library(gtools)
library(magrittr)
library(stringr)
library(reshape2)
library(data.table)

##### Import Data #####
# Used data - CKR data for VIVA credit clients part of the development samples in both scorecards created in 2016 (two scorecards - for New and Existing clients)

#XML Transformed data for Recent sample - xml data
xml.data<-read.csv(file = "E:/Projects/VIVA/Application Scorecards - Installment product - Reassessment Jul 2017/Data/CKR/CCR_Viva04_07_2017.csv",header = T,sep=";",
                   stringsAsFactors = F,na.strings = c(NA,"NULL")) # 43102 obs., 36 columns
sum(!duplicated(xml.data$CreditCode)) # 4261

# Import list of kid-s part of dev sample with existing clients
devsample_ExClients = read.csv(file = "E:/Projects/VIVA/Application Scorecards - Installment product - Reassessment Jul 2017/Data/List of KID-s and EGN-s for both scorecards/List_KID_VivaCredit_Excl.csv",header = T,sep=";",
                                stringsAsFactors = F,na.strings = c(NA,"NULL")) # 5941 obs.

# In CKR data leave kid-s only part of the development sample for existing clients
xml.data = filter(xml.data, CreditCode %in% devsample_ExClients$CreditCode) # 17653 obs.
sum(!duplicated(xml.data$CreditCode)) # 1511 obs. - Information is available for 1511 kid-s out of 5941 in the dev sample for this model

## Obtain CreditBeginDate for the kid-s in the sample (it will be necessary for calculating several measures from CKR data)
# Import latest credit progress (as of 01.07.2017)

creditprogress = read.csv("E:/Data/2017.06/VIVA/E004_Credit_Progress_VIVA_20170701_v1.csv",header = T,sep=";",
                          stringsAsFactors = F,na.strings = c(NA,"NULL"), encoding = "UTF-8" )
app.data = select(creditprogress, CreditCode, ClientEGN, CreditBeginDate)

# Leave kid-s of interest (1511)
app.data = app.data[app.data$CreditCode %in% xml.data$CreditCode,] # 1511 unique kid-s
sum(!duplicated(app.data$ClientEGN)) # 1394

# Obtain two variables which will be used later in the analysis - current_year/last_year
app.data$current_year <- year(as.Date(app.data$CreditBeginDate)) # year of application
app.data$last_year <- year(as.Date(app.data$CreditBeginDate)-365) # previous year of application
 
# Rename one of the columns
names(app.data)[2] <- "code" # egn

# Summary information for "xml.data" dataframe
str(xml.data)
summary(xml.data)

# Remove redundant columns in CKR data - all these columns are NA (system data)
xml.data.clean <- select(xml.data,  - c(format,req_file_name, req_file_time, req_file_user, req_file_entity,
                                        out_file_time, borr_count, code_valid))

# Create a list of unique EGN in CKR data - to be used for merging
id.list<- select(xml.data,code) %>% filter(!duplicated(code)) # 1394 unique EGN-s - for these clients, we have information from CKR 
kid.list <- select(xml.data,CreditCode) %>% filter(!duplicated(CreditCode)) # 1511 obs.

##########--- Start deriving variables ---##########

# !!! Run the code below three times for CKR data obtained after October 2016 - in this way you will 
# obtain three separate dataframes giving information on: banks, non-banks and both.

# !!! Run the code below once for CKR data obtained before October 2016 (during this period there was no separation between banks and non-banks).

### ----> In the current case, the code below will be ran only once because there exist no entity type for this dataset (data is obtained before October 2016).

institution = c("banks","nonbanks",NA) 
#institution = c("banks")
#institution = c("nonbanks")

#### Section I). Information on active credits

## 1) Total Amount Approved, Principal and Instalment on active credits
type_of_credit <- xml.data.clean[which(xml.data.clean$grouping_attribute_ID %in%  c("3BD9C927-F081-4898-B104-E081CEB59F00") # key for "I. Information on active credits" - "Type of credit" section
                                       &xml.data.clean$entity_type %in% institution)
                                 ,c("CreditCode","amount_drawn","monthly_installment","amount_approved","cred_count","source_entity_count",
                                    "outstanding_performing_principal","outstanding_overdue_principal","grouping_DATA")] 

## 2) Get grouping_DATA (it gives info on the exact type of credit)

# For active credits
type_of_credit_grouping_DATA = type_of_credit$grouping_DATA # type of credit

# For new credits (newly approved)
type_of_credit_new<-xml.data.clean[!is.na(xml.data.clean$amount_approvedNew) & xml.data.clean$entity_type %in% institution, c("CreditCode","type")]

# Summarize - place in one vector the different(unique) types of credits (for both currently active and recently approved credits)
type_of_credit_grouping_DATA = unique(c(unique(type_of_credit_new$type),unique(type_of_credit_grouping_DATA))) # 9 types - number of observed type of credits (grouping types) in the data
type_of_credit_grouping_DATA = type_of_credit_grouping_DATA[!is.na(type_of_credit_grouping_DATA)]

# Summarize "type_of_credit" dataframe
# Sum up all columns and obtain number of active credits and number of entities

type_of_credit_summ<-summarise(group_by(type_of_credit,CreditCode),
                               total_amount_approved = sum(amount_approved,na.rm = T),
                               total_amount_drawn = sum(amount_drawn,na.rm = T),
                               total_outstanding_performing_principal = sum(outstanding_performing_principal,na.rm = T),
                               total_outstanding_overdue_principal = sum(outstanding_overdue_principal,na.rm = T),
                               total_monthly_instalment = sum(monthly_installment,na.rm = T), ### missing in this CKR data
                               total_credits = min(cred_count,na.rm = T), 
                               total_entities = min(source_entity_count,na.rm = T)
)

# Fill-in data for credit type in the "type_of_credit_summ" dataframe
# Each credit type is a separate column. The column is binary -> 1 - the client has a credit from this type; 0 - the client does not have credit from this type.

for (i in 1:length(type_of_credit_grouping_DATA))
{
  type_of_credit_summ[,type_of_credit_grouping_DATA[i]]<-with(type_of_credit_summ,
                                                              ifelse(CreditCode %in% type_of_credit[type_of_credit$grouping_DATA %in% type_of_credit_grouping_DATA[i],"CreditCode"]
                                                                     | CreditCode %in% type_of_credit_new[type_of_credit_new$type %in% type_of_credit_grouping_DATA[i],"CreditCode"], 1, 0))
}

# 3) Total amount given after the last reporting period and number of newly approved credits
post_rep_per_summ = summarise(group_by(xml.data.clean[xml.data.clean$entity_type %in% institution,],CreditCode),
                              total_amount_approved_after = sum(amount_approvedNew,na.rm = T),
                              total_credits_after = sum(!is.na(amount_approvedNew),na.rm=T)
) # 1511 obs.

# 4) Период на просрочие (overdue payment period)
overdue_payment_period = xml.data.clean[xml.data.clean$grouping_attribute_ID %in%  c("B0E30EDB-C999-4350-A5FF-58EC32334695") # key for "I. Information on active credits" - "Overdue period" section
                                        & xml.data.clean$entity_type %in% institution,c("CreditCode","grouping_DATA")] 
#table(overdue_payment_period$grouping_DATA)

# recode the "grouping_DATA" field according to the old format of "overdue payment period" field (codes 401,402,403,404)
overdue_payment_period$grouping_DATA_new = with(overdue_payment_period, ifelse(grouping_DATA %in% c("от 0 до 30 дни","няма просрочие","от 1 до 30 дни"), 401,
                                                                               ifelse(grouping_DATA %in% c("от 31 до 60 дни", "от 31 до 90 дни"), 402,
                                                                                      ifelse(grouping_DATA == "от 61 до 90 дни", 403,
                                                                                             ifelse(grouping_DATA %in%  c("от 91 до 180 дни","от 181 до 360 дни",
                                                                                                                          "над 360 дни","над 180 дни"),404,grouping_DATA)))))
overdue_payment_period$grouping_DATA_new = as.integer(substr(overdue_payment_period$grouping_DATA_new,1,3))

# Summarize information - obtain worst status from the currently active credits
overdue_payment_period_summ = summarize(group_by(overdue_payment_period, CreditCode),
                                        worst_curr_category_actives = max(grouping_DATA_new)) # 1473 obs.

# A check: 
# The length of "overdue_payment_period_summ" data frame should be equal to the length of "type_of_credit_summ" data frame length (number of rows).

#### Section II). Information on overdues (for both active and completed credits) 

## Take columns of interest from this section
# If "active" field = TRUE - an active credit
# If "active" field = FALSE - a completed credit

overdues <- xml.data.clean[!is.na(xml.data.clean$active) & xml.data.clean$entity_type  %in% institution, # "active" column - active credit or not
                            c("CreditCode","active","year","months_count","max_cred_count","max_outstanding_overdue_principal",
                              "max_outstanding_overdue_interest_and_others","max_off_balance_sheet_dues",
                              "category","overdue_payment_period")] 

## Mapping the field "category" of overdues dataframe
# Create a new column which contains all categories (both new and old format of "overdue payment period" can be included here)
overdues$category_new = with(overdues, ifelse(is.na(category), overdue_payment_period, category))
overdues$overdue_payment_period = NULL

# Recode the "category_new" field according to the old format of "overdue payment period" field (codes 401,403,403,404)
overdues$category_new = with(overdues, ifelse(category_new %in% c("от 0 до 30 дни","няма просрочие","от 1 до 30 дни"), 401,
                                              ifelse(category_new %in% c("от 31 до 60 дни", "от 31 до 90 дни"), 402,
                                                     ifelse(category_new == "от 61 до 90 дни", 403,
                                                            ifelse(category_new %in%  c("от 91 до 180 дни","от 181 до 360 дни",
                                                                                        "над 360 дни","над 180 дни"),404,category_new)))))

# Extract just the number from the status 
overdues$category_trimmed <- as.numeric(substr(overdues$category_new, start = 1, stop = 3))

# Summarize "overdues" dataframe - take max of all fields from this section (separately for active and completed credits)
overdues_summ_max <- summarize(group_by(overdues,CreditCode,active),
                           worst_category = max(category_trimmed, na.rm = T),
                           max_outstanding_overdue_principal = max(max_outstanding_overdue_principal,na.rm=T),
                           max_outstanding_overdue_interest_and_others = max(max_outstanding_overdue_interest_and_others,na.rm=T),
                           max_off_balance_sheet_dues = max(max_off_balance_sheet_dues,na.rm=T),
                           max_months_count = max(months_count,na.rm = T),
                           max_max_cred_count = max(max_cred_count, na.rm = T)
)

# Transpose, create separate columns for active and completed credits
names(overdues_summ_max)
overdues_summ_max_trans = dcast(as.data.table(overdues_summ_max), CreditCode ~ active, value.var = c("worst_category","max_outstanding_overdue_principal","max_outstanding_overdue_interest_and_others",
                                            "max_off_balance_sheet_dues","max_months_count","max_max_cred_count"))

# Add year of application and the year before that (created earlier in the script above in "app.data" dataframe) to the "overdues" dataframe in order to obtain some features
overdues<-merge(overdues,app.data[,c("CreditCode","current_year","last_year")],by = "CreditCode",all.x=T)

# Create features 1 -  for the previous(last) year - take max of all fields
# _L2 - meaning last 2 years
overdues_summ_max_L2 <- summarize(group_by(overdues[overdues$year == overdues$last_year,],CreditCode,active),
                              worst_category = max(category_trimmed, na.rm = T),
                              max_outstanding_overdue_principal = max(max_outstanding_overdue_principal,na.rm=T),
                              max_outstanding_overdue_interest_and_others = max(max_outstanding_overdue_interest_and_others,na.rm=T),
                              max_off_balance_sheet_dues = max(max_off_balance_sheet_dues,na.rm=T),
                              max_months_count = max(months_count,na.rm = T),
                              max_max_cred_count = max(max_cred_count, na.rm = T)
                              
)

names(overdues_summ_max_L2)
overdues_summ_max_trans_L2 = dcast(as.data.table(overdues_summ_max_L2), CreditCode ~ active, value.var = c("worst_category","max_outstanding_overdue_principal","max_outstanding_overdue_interest_and_others",
                                                                                               "max_off_balance_sheet_dues","max_months_count","max_max_cred_count")) # 1057 obs.
names(overdues_summ_max_trans_L2)[-1] %<>% paste0("_L2")

# Create features 2 -  for the current year - take max of all fields
# _Curr - meaning current year (the year of application)     
overdues_summ_max_Curr <- summarize(group_by(overdues[overdues$year == overdues$current_year,],CreditCode,active),
                                worst_category = max(category_trimmed, na.rm = T),
                                max_outstanding_overdue_principal = max(max_outstanding_overdue_principal,na.rm=T),
                                max_outstanding_overdue_interest_and_others = max(max_outstanding_overdue_interest_and_others,na.rm=T),
                                max_off_balance_sheet_dues = max(max_off_balance_sheet_dues,na.rm=T),
                                max_months_count = max(months_count,na.rm = T),
                                max_max_cred_count = max(max_cred_count, na.rm = T)
)

overdues_summ_max_trans_Curr = dcast(as.data.table(overdues_summ_max_Curr), CreditCode ~ active, value.var = c("worst_category","max_outstanding_overdue_principal","max_outstanding_overdue_interest_and_others",
                                                                                                     "max_off_balance_sheet_dues","max_months_count","max_max_cred_count")) # 510 vars.
names(overdues_summ_max_trans_Curr)[-1] %<>% paste0("_Curr") # 539 vars.


##########--- Combine all data and derive several additional variables ---##########

all_transposed = join_all(dfs = list(kid.list,type_of_credit_summ,
                                     post_rep_per_summ, overdue_payment_period_summ,
                                     overdues_summ_max_trans,
                                     overdues_summ_max_trans_Curr,
                                     overdues_summ_max_trans_L2,
                                     app.data
),type = "left",
by = "CreditCode") # 1511 obs, 60vars

# Check if the dataframe has all of the necessary columns
overdues_names <- readRDS("E:/Projects/CKR data project/Scripts/All rds names.rds") #36 columns names

#get missing columns in the all_transposed file
get_missing_columns = overdues_names[!overdues_names %in% colnames(all_transposed)]

# Cbind 
get_missing_columns_frame = as.data.frame(matrix(data=NA,nrow = nrow(all_transposed) ,ncol = length(get_missing_columns)))
colnames(get_missing_columns_frame) <- get_missing_columns

# Actualize "all_transposed" data frame (in case there are missing columns else it won`t happen anything)
all_transposed<-cbind(all_transposed,get_missing_columns_frame)

# Group variables
group_vars = ddply(all_transposed,"CreditCode", summarize,
                   
                   #Section I: Information on the active credits
                   #derive total variable on ID level for the given institution
                   Total_amount_all = sum(total_amount_approved, total_amount_approved_after, na.rm= T), # Total_amount_all = общо главница по активни кредити (за съответната институция)
                   Total_oustanding_all = sum(total_outstanding_performing_principal,total_outstanding_overdue_principal ,total_amount_approved_after,na.rm = T), # общо оставащо дължимо по активни кредити
                   Total_credits_all = sum(total_credits, total_credits_after, na.rm = T), # общ брой активни кредити в момента + новосъздадените  
                   #Instalment is not further aggregated since in the top part of the script we already have the maximum aggregation done (in the post last payment period there is not instalment info)
                   
                   #Section II: Information on overdue on active and closed credits
                   ## ever
                   worst_category = max(worst_category_FALSE, worst_category_TRUE, na.rm = T), # най-лош статус и по активни, и по приключени
                   max_outstanding_overdue_principal = max(max_outstanding_overdue_principal_FALSE, max_outstanding_overdue_principal_TRUE, na.rm = T), # макс стойност на просрочена главница и по активни, и по приключени
                   max_outstanding_overdue_interest_and_others = max(max_outstanding_overdue_interest_and_others_FALSE, max_outstanding_overdue_interest_and_others_TRUE, na.rm = T),
                   max_off_balance_sheet_dues = max(max_off_balance_sheet_dues_FALSE, max_off_balance_sheet_dues_TRUE, na.rm = T),
                   
                   ##last year
                   worst_category_L2 = max(worst_category_FALSE_L2, worst_category_TRUE_L2, na.rm = T),
                   max_outstanding_overdue_principal_L2 = max(max_outstanding_overdue_principal_FALSE_L2, max_outstanding_overdue_principal_TRUE_L2, na.rm = T),
                   max_outstanding_overdue_interest_and_others_L2 = max(max_outstanding_overdue_interest_and_others_FALSE_L2, max_outstanding_overdue_interest_and_others_TRUE_L2, na.rm = T),
                   max_off_balance_sheet_dues_L2 = max(max_off_balance_sheet_dues_FALSE_L2, max_off_balance_sheet_dues_TRUE_L2, na.rm = T),
                   
                   ##current year
                   worst_category_Curr = max(worst_category_FALSE_Curr, worst_category_TRUE_Curr, na.rm = T),
                   max_outstanding_overdue_principal_Curr = max(max_outstanding_overdue_principal_FALSE_Curr, max_outstanding_overdue_principal_TRUE_Curr, na.rm = T),
                   max_outstanding_overdue_interest_and_others_Curr = max(max_outstanding_overdue_interest_and_others_FALSE_Curr, max_outstanding_overdue_interest_and_others_TRUE_Curr, na.rm = T),
                   max_off_balance_sheet_dues_Curr = max(max_off_balance_sheet_dues_FALSE_Curr, max_off_balance_sheet_dues_TRUE_Curr, na.rm = T)
                   
)


all_transposed <- merge(all_transposed, group_vars,by ="CreditCode",all.x = T)
str(all_transposed)

# Inf values meaning - when only NA`s are compared, the output is "Inf"

################### Clean the objects from the environment which won`t be used in further analysis ################### 

rm(get_missing_columns_frame,group_vars,overdue_payment_period,overdue_payment_period_summ,overdues,overdues_summ_max,
   overdues_summ_max_Curr,overdues_summ_max_L2,overdues_summ_max_trans,overdues_summ_max_trans_Curr,overdues_summ_max_trans_L2,post_rep_per_summ,type_of_credit,
   type_of_credit_new,type_of_credit_summ,overdues_names)
