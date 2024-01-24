library(tidyverse)
library(data.table)
library(lubridate)
library(readxl)

All_Parsed_CRIF_Files = list.files("//hind.smartitbg.int/FileServer/Data Analyses/Analysis/1.Projects/113.WCard PL/7.Data/03 Automate CRIF xml Parsing/Output Data"
                                   , full.names = T
                                   , pattern = ".rds"
                                   )

All_Parsed_CRIF_Files_DF = do.call(bind_rows,lapply(All_Parsed_CRIF_Files, readRDS))


  


