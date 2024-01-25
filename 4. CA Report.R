########### CA Reports generation ###########
# Import macro for CA report (located on the server)
# CA Reports will be generated on kid-s part of the development training sample

# Import kid-s from dev sample (separately for test and train datasets)
train_sample_kids = as.data.frame(readRDS("E:/Projects/VIVA/Application Scorecards - Installment product - Reassessment Jul 2017/Data/List_kid_Excl_train.rds"))
test_sample_kids = as.data.frame(readRDS("E:/Projects/VIVA/Application Scorecards - Installment product - Reassessment Jul 2017/Data/List_kid_Excl_test.rds"))

names(train_sample_kids)[1] = "CreditCode"
names(test_sample_kids)[1] = "CreditCode"

training_raw = all_transposed[all_transposed$CreditCode %in% train_sample_kids$CreditCode,] # 1064 obs.

# Place flag last (in order to apply macro for creating a CA report)

names(training_raw) # GB_flag - target
#training_raw <- training_raw[,c(1:93,95:ncol(training_raw),94)]

# Cut numeric features into % of population - use macro "Grouping variables % population with special values.R" located on the server
# Before applying the macro: Check number of special values within each column of the dataframe - the macro can`t split a vector containing only special values.

check_special_val = apply(training_raw, 2, function (x) (sum(is.na(x) | is.nan(x) | is.infinite(x) | as.integer(x) == 0 | x== "0" | x %in% c(401,402,403,404), na.rm = T)))
check_special_val = as.data.frame(check_special_val)
check_special_val$names<-rownames(check_special_val)

check_special_val = check_special_val[check_special_val$check == nrow(training_raw),]

# Create a vector containing the names of numeric features which will be split
num_vars_to_splt = colnames(training_raw[,sapply(training_raw, is.numeric)])

# Remove those features which contain only special values
num_vars_to_splt = num_vars_to_splt[!num_vars_to_splt %in% c(check_special_val$names, "worst_category_Curr","CreditCode","ClientEGN")]

# Cut into % of population, take care of special values
training_cut <- groupattr(training_raw, var = num_vars_to_splt , pct = 5, spec_values = c(NA,0,NaN,-Inf,Inf))

# Create "CA Report v1" (after new features addition)
# Create CA Report on training sample (training sample, all observations, the original split is retained)

m.CA.Report(training_cut,
             dir = "E:/Projects/VIVA/Application Scorecards - Installment product - Reassessment Jul 2017/Reassessment of VIVA Scorecard Existing Clients/Output tables/CA Reports/CSV v1/",exportToCSV = TRUE)



# # check test sample and for how many of the clients part of it we have CKR data 
# xx = dev_sample_data_CAreport_cut[dev_sample_data_CAreport_cut$CreditCode %in% test_sample_kids$CreditCode,]
# a = test_sample_kids
# a = merge(a, creditprogress[c("CreditCode", "ClientEGN")], by = "CreditCode", all.x = T)
# sum(!a$ClientEGN %in% id.list$code) # 135
# rm(a,xx,x)

