########### Script 2: Derive further variables from CKR ########### 

# 1) Derive "StatusLastToCurr"
#### The variable gives information on: the change in worst status from last to current year

all_transposed$StatusLastToCurr <- with(all_transposed,
                                        ifelse(is.infinite(worst_category_Curr) & is.infinite(worst_category_L2),"No Info",
                                               ifelse(is.infinite(worst_category_Curr) & !is.infinite(worst_category_L2),"No New Info",
                                                      ifelse(!is.infinite(worst_category_Curr) & is.infinite(worst_category_L2),"Only New Info",
                                                             ifelse(worst_category_Curr > worst_category_L2,"Worse",
                                                                    ifelse(worst_category_Curr < worst_category_L2,"Better", "No change"
                                                                    ))))))

table(all_transposed$StatusLastToCurr)
# Better     No change       No Info   No New Info   Only New Info         Worse 
# 68           266            709           263           130                75 
# 
# 2) percentage_paid_amount
#### Percentage paid principal from total outstanding

all_transposed$percentage_paid_amount <-(all_transposed$Total_amount_all - all_transposed$Total_oustanding_all)/all_transposed$Total_amount_all
table(round(all_transposed$percentage_paid_amount,2), useNA = "ifany")

sum(is.infinite(all_transposed$percentage_paid_amount)) # 0
sum(is.nan(all_transposed$percentage_paid_amount)) # 27

#x = filter(all_transposed, is.nan(percentage_paid_amount))
# x = filter(all_transposed, percentage_paid_amount < 0)

# all_transposed$percentage_paid_amount = with(all_transposed, ifelse(is.nan(percentage_paid_amount), -99,percentage_paid_amount ))
# table(round(all_transposed$percentage_paid_amount,2), useNA = "ifany") # -99 means "no information (missing)"

x = filter(all_transposed, percentage_paid_amount == 0)

# 3) PercOverduePrincipal (capped - value can`t be higher than 1)
all_transposed$PercOverduePrincipal = all_transposed$total_outstanding_overdue_principal/all_transposed$Total_amount_all

table(round(all_transposed$PercOverduePrincipal,2), useNA = "ifany") # 
x = filter(all_transposed, is.nan(PercOverduePrincipal)) # 0
rm(x)
x = filter(all_transposed, is.na(PercOverduePrincipal)) # 38

all_transposed$PercOverduePrincipal = with(all_transposed, ifelse(PercOverduePrincipal >1, 1,PercOverduePrincipal ))
#all_transposed$PercOverduePrincipal = with(all_transposed, ifelse(is.na(PercOverduePrincipal), -99 ,PercOverduePrincipal )) # -99 means "no info"
#table(round(all_transposed$PercOverduePrincipal,2), useNA = "ifany") # 

# 4) credits_ft_entities
all_transposed$credits_ft_entities = all_transposed$total_credits/all_transposed$total_entities
x = select(all_transposed,total_credits,total_entities,credits_ft_entities )
rm(x)
table(round(all_transposed$credits_ft_entities,2), useNA = "ifany")

#all_transposed$credits_ft_entities = with(all_transposed, ifelse(is.na(credits_ft_entities), -99, credits_ft_entities))
#table(round(all_transposed$credits_ft_entities,2), useNA = "ifany") # -99 means "no info"

# 5) PercOustanding
all_transposed$PercOutstanding = (all_transposed$total_outstanding_performing_principal + all_transposed$total_outstanding_overdue_principal)/all_transposed$Total_amount_all

table(round(all_transposed$PercOutstanding,2), useNA = "ifany") 
sum(is.nan(all_transposed$PercOutstanding)) # 0
sum(is.infinite(all_transposed$PercOutstanding)) # 0
x = select(all_transposed,total_outstanding_performing_principal,total_outstanding_overdue_principal,Total_amount_all,PercOutstanding )

#all_transposed$PercOutstanding = with(all_transposed, ifelse(is.na(PercOutstanding), -99, PercOutstanding )) # -99 means either no information, or only newly approved credit (for which at that stage, in CKR there is no info for oustanding sums) 
#table(round(all_transposed$PercOutstanding,2), useNA = "ifany") 

# 6) MaxNumMonthsOverdueStatus
all_transposed$MaxNumMonthsOverdueStatus = all_transposed$max_months_count_FALSE + all_transposed$max_months_count_TRUE
table(all_transposed$MaxNumMonthsOverdueStatus, useNA = "ifany")
# 2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23    24  <NA> 
# 14   17   29   25   32   25   21   35   30  18   17   36   30   15   15   24   14   12   10    8    9    4    29  1042 

# 7) MaxNumCreditsOverdueStatus
all_transposed$MaxNumCreditsOverdueStatus = all_transposed$max_max_cred_count_FALSE + all_transposed$max_max_cred_count_TRUE
table(all_transposed$MaxNumCreditsOverdueStatus, useNA = "ifany")
# 2    3    4    5    6    7    8    9   10   11   12 <NA> 
#114  127   93   62   31   24    7    6    2    2    1 1042 

# Import development sample for existing clients (including all the variables used in data modeling)
dev_sample_data = readRDS("E:/Projects/VIVA/Application Scorecards - Installment product - Reassessment Jul 2017/Data/Dev_sample_ex_cl.rds") # 5941 obs., 27 vars.

# Clean the dataset and add more vairables if possible
names(dev_sample_data)
sum(!duplicated(dev_sample_data$CreditCode)) # 5941
names(all_transposed)
sum(!duplicated(all_transposed$CreditCode)) # 1511

# Add all application variables from dev sample to "all_transposed" dataframe
all_transposed = merge(all_transposed, dev_sample_data, by = "CreditCode", all.x = T)
names(all_transposed)

# # check inf values
# x = filter(dev_sample_data, is.infinite(worst_category_Curr))
# table(x$worst_category_FALSE_Curr, useNA = "ifany") # only NA-s
# table(x$worst_category_TRUE_Curr, useNA = "ifany") # only NA-s
# rm(x)

## Derive features using application data and CKR both

# 8) SumaP as % from Total_amount_all (new principal as % from sum of principal of all active credits)
all_transposed$SumaP_asPerc = all_transposed$SumaP/all_transposed$Total_amount_all
table(round(all_transposed$SumaP_asPerc,2), useNA = "ifany")

# check special values
x = select(all_transposed, SumaP,Total_amount_all,SumaP_asPerc)
x = filter(x, is.infinite(SumaP_asPerc)) # 265obs - Inf -  these do not have other active credits
# sum(is.na(x$SumaP)) # 0
# sum(is.na(x$Total_amount_all)) # 0
# sum(x$Total_amount_all == 0) # 265

x = filter(all_transposed, is.na(SumaP_asPerc)) # 0
rm(x)

# # replace INF values with -999 category, meaning that these clients have zero "Total_amount_all" (the sum of principal on active credits is zero)
# all_transposed$SumaP_asPerc = with(all_transposed, ifelse(is.infinite(SumaP_asPerc), -999, SumaP_asPerc))
# table(round(all_transposed$SumaP_asPerc,2), useNA = "ifany")








