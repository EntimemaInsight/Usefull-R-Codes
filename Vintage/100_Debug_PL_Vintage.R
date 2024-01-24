library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)

# Load Poland Vintage Data
PL_Vintage_2023_11 = readRDS("./Output Data/Poland Daily/Axi_Poland_Vintage_Data_2023-11-01-2023-11-30.rds")

# Load CIDs to check 
Poland_CIDs = read_xlsx("./Input Data/MDP Amount Calculated on Promotion Cases_PESEL.xlsx")

# Load Poland Scoring
PL_Scoring = readRDS("//hind.smartitbg.int/FileServer/Data Analyses/Analysis/1.Projects/113.WCard PL/7.Data/01 Automate Scoring Info Extraction from Provenir xmls/Output Data/020_WC_PL_Full_Scoring_Data.rds")

# Load data
myc <- DBI::dbConnect(odbc::odbc()
, driver = "SQL Server"
, server = "Scorpio.smartitbg.int"
, database = "BIsmartWCPL"
)

Query <- paste0("select * from dwh.DimOffers")

start_time <- Sys.time()
PL_DimOffers <- DBI::dbFetch(DBI::dbSendQuery(myc, Query))
print(paste("Done in", round(difftime(Sys.time(), start_time,unit = "mins"),2), "minutes"))

#Close the database connection
DBI::dbDisconnect(myc)

DF_For_Checks = PL_Vintage_2023_11 %>%
  filter(MOB == 2
         & Has_MAD_Due == 1
         & DayOfMonth == 10
         ) %>%
  left_join(PL_DimOffers %>% select(OfferID, ContractNumber), by = "OfferID") %>%
  left_join(Poland_CIDs %>% select(CID) %>% mutate(Included_With_MDP = 1), by = c("ContractNumber" = "CID")) %>%
  left_join(PL_Scoring %>% select(application_number, Crif_Score_Class), by = c("OfferID" = "application_number"))


sum(DF_For_Checks$Included_With_MDP, na.rm = T)

Included_With_MDP = DF_For_Checks %>%
  filter(Included_With_MDP == 1) %>%
  select(-Crif_Score_Class)

Missing_CRIF_Score_Class = DF_For_Checks %>%
  filter(is.na(Crif_Score_Class))

write_xlsx(Included_With_MDP,"./Archive/100_Included_With_MDP.xlsx")
write_xlsx(Missing_CRIF_Score_Class,"./Archive/100_Missing_CRIF_Score_Class.xlsx")

