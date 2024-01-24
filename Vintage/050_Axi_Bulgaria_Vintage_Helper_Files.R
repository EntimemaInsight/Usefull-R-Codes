# Load libraries
library(tidyverse)
library(lubridate)
library(stringr)

# Load AKPZ Portfolio Files 
akpz_files_2022 = Sys.glob("//hind.smartitbg.int/FileServer/Data Analyses/Analysis/3.Data/2022*/AKPZ/AKPZ*2022.csv")
akpz_files_2021 = Sys.glob("//hind.smartitbg.int/FileServer/Data Analyses/Analysis/3.Data/2021*/AKPZ/AKPZ*2021.csv")

# Define function to read AKPZ portfolio file 

read_akpz_pf = function(file) {
  
  library(lubridate)
  library(stringr)
  
  
  pf = read.csv(file, sep = ";")
  pf = pf[-1,]
  
  date = str_sub(file,-14,-5)
  
  pf$Date = dmy(date)
  
  return(pf)
  
}

akpz_2022_df = bind_rows(lapply(akpz_files_2022, read_akpz_pf))
akpz_2021_df = bind_rows(lapply(akpz_files_2021, read_akpz_pf))

akpz_df = akpz_2021_df %>%
  bind_rows(akpz_2022_df) %>%
  mutate(EGN = str_pad(EGN,width = 10,side = "left",pad = "0")
         , Package.Date = as.Date(Package.Date)
         ) %>%
  filter(`TYPE.2` == "Активен") %>%
  distinct(EGN,Date,.keep_all = T)

# Load data
myc <- DBI::dbConnect(odbc::odbc()
                      , driver = "SQL Server"
                      , server = "Scorpio.smartitbg.int"
                      , database = "BIsmartWCBG"
)

Query <- paste0("select DO.OfferSK
      , DO.OfferID
	    , DC.EGN
	    , cast(DateInserted as Date) as DateInserted
	    , FORMAT(DO.DateInserted, 'yyyy-MM-01') as MonthInserted
from dwh.DimOffers DO
left join dwh.DimClient DC on DO.ClientSK = DC.ClientSK")

start_time <- Sys.time()
WC_BG_DWH <- DBI::dbFetch(DBI::dbSendQuery(myc, Query))
print(paste("Done in", round(difftime(Sys.time(), start_time,unit = "mins"),2), "minutes"))

#Close the database connection
DBI::dbDisconnect(myc)

WC_BG_DWH = WC_BG_DWH %>%
  mutate(EGN = str_pad(EGN,width = 10,side = "left",pad = "0")
         , MonthInserted = as.Date(MonthInserted)
         )

# Create Helper Table with AKPZ at application Flag 
AKPZ_At_Application_Flag = WC_BG_DWH %>%
  filter(DateInserted >= as.Date("2021-02-01")) %>%
  left_join(akpz_df %>% select(EGN, Date, KID, `Package.Date`,`Employer...Centered`), by = c("EGN" = "EGN"
                            , "MonthInserted" = "Date"
                            )
            ) %>%
  mutate(At_AKPZ_Flag = ifelse(!is.na(KID),"Yes","No"))

write.csv(AKPZ_At_Application_Flag,"./Output Data/050_AKPZ_At_Application_Flag.csv")


















