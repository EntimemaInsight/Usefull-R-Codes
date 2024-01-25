########### Script 3 - Data Audit ###########

library(stringr)

# Prepare the dataframe for exports - remove special characters from column names because otherwise features can`t be exported as separate csv files
# Fix column names (special characters - # \/:*?"<>|)

names(all_transposed)
for(i in 1:length(colnames(all_transposed))){
  
  colnames(all_transposed)[i]<- str_replace_all(colnames(all_transposed)[i], "([/])","-")

}

# Remove variables which won`t be used further
names(all_transposed)

all_transposed = all_transposed[,!colnames(all_transposed) %in% c("CreditBeginDate","current_year","last_year")] # 106 vars
all_transposed = all_transposed[,c(1,57,2:56,58:104,106,105)]
names(all_transposed)[2] = "ClientEGN"
names(all_transposed)

# Create Data Audit on "all_transposed" dataframe
# Import macro for data audit located on the server
str(all_transposed) # 106 features, 1511 clients for which we have information
m.data.audit(all_transposed, 
             dir = "E:/Projects/VIVA/Application Scorecards - Installment product - Reassessment Jul 2017/Reassessment of VIVA Scorecard New Clients/Output tables/Data Audit/After new features addition/CSV/")


