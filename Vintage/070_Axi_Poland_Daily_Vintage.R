# Load libraries
library(tidyverse)
library(caret)
library(xgboost)
library(readxl)
library(writexl)
library(lubridate)
library(data.table)

# Set date as of which to run the query 

if(day(Sys.Date()) == 1) {
  
  Start_Date = floor_date(floor_date(floor_date(floor_date(Sys.Date(), unit = "months")-1, unit = "months")-1, unit = "months")-1, unit = "months")
  End_Date = floor_date(floor_date(floor_date(Sys.Date(), unit = "months")-1, unit = "months")-1,unit = "months")-1
  
} else {
  
  Start_Date = floor_date(floor_date(floor_date(Sys.Date(), unit = "months")-1, unit = "months")-1,unit = "months")
  End_Date = floor_date(floor_date(Sys.Date(), unit = "months")-1, unit = "months")-1
  
}

# Load data sample for scoring from dwh
myc <- DBI::dbConnect(odbc::odbc()
                      , driver = "SQL Server"
                      , server = "Scorpio.smartitbg.int"
                      , database = "BIsmartWCPL"
                      , encoding = "Windows-1251"
                      
)

Query <- read_file('//hind.smartitbg.int/FileServer/Data Analyses/Analysis/1.Projects/110_1 WC All Businesses/15 Vintage Analysis/SQL Queries/Vintage_Query_Daily_Poland.sql'
                   , locale = locale(encoding = "Windows-1251")
)

# Substitute Year and Month Parameters in the SQL Query 
Query =   gsub("Start_Date_Sub", Start_Date, Query)
Query =   gsub("End_Date_Sub", End_Date, Query)

start_time <- Sys.time()
Vintage_Data <- DBI::dbFetch(DBI::dbSendQuery(myc, Query))
print(paste("Done in", round(difftime(Sys.time(), start_time,unit = "mins"),2), "minutes"))

# #Close the database connection
DBI::dbDisconnect(myc)

saveRDS(Vintage_Data, paste0("//hind.smartitbg.int/FileServer/Data Analyses/Analysis/1.Projects/110_1 WC All Businesses/15 Vintage Analysis/Output Data/Poland Daily/Axi_Poland_Vintage_Data_",Start_Date,"-",End_Date,".rds"))

# Load all rds files, bind and save into one csv
Vintage_RDS_Files = list.files("//hind.smartitbg.int/FileServer/Data Analyses/Analysis/1.Projects/110_1 WC All Businesses/15 Vintage Analysis/Output Data/Poland Daily", pattern = ".rds", full.names = T)
Vintage_RDS_Files_DF = do.call(bind_rows,lapply(Vintage_RDS_Files, readRDS))

Vintage_RDS_Files_DF_Wrangled = Vintage_RDS_Files_DF %>%
  mutate(NextObsMonth = floor_date(as.Date(NextObsPoint), unit = "months")) %>%
  group_by(NextObsMonth) %>%
  mutate(Max_Month_Date = max(DayOfMonth)) %>%
  ungroup() %>%
  mutate(Max_Day_Flag = ifelse(Max_Month_Date == DayOfMonth,1,0)
         , LatestMonth = ifelse(as.Date(MonthInserted) == floor_date(floor_date(floor_date(Sys.Date(),unit = "months")-1, unit = "months")-1, unit = "months"),1,0)
  ) %>%
  select(-NextObsMonth,-Max_Month_Date) 

fwrite(Vintage_RDS_Files_DF_Wrangled,"//hind.smartitbg.int/FileServer/Data Analyses/Analysis/1.Projects/110_1 WC All Businesses/15 Vintage Analysis/Output Data/Poland Daily/010_Poland_Daily_Vintage_Data.csv")





