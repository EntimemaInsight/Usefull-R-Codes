library(data.table)
library(tidyverse)

Vintage_RDS_Files_DF_Wrangled = fread("//hind.smartitbg.int/FileServer/Data Analyses/Analysis/1.Projects/110_1 WC All Businesses/15 Vintage Analysis/Output Data/Romania Daily/010_Romania_Daily_Vintage_Data.csv")

prov = data.table::fread(paste0(
  "//hind.smartitbg.int/fileserver/data analyses/analysis/1.projects/112.wcard ro/3.reporting/",
  "regular/05 provenir data report/output data/010_parsed_provenir_data_final.csv"
))

ll <- list.files(
  '//hind.smartitbg.int/FileServer/Data Analyses/Analysis/1.Projects/112.WCard RO/7.Data/Provenir XMLs 2023_06_01_2023_06_30/',
  full.names = TRUE
)

# 
# data.frame(
#   fn = ll,
#   OfferID = stringr::str_extract(ll, "\\d+(?=\\.xml$)"),
#   dt = lubridate::as_datetime(
#     substr(stringr::str_extract(ll, "\\d{17}"), 1, 14),
#     format = "%Y%m%d%H%M%S"
#   )
# ) %>% 
#   mutate(date = lubridate::as_date(dt)) %>% 
#   select(date) %>% table()
#   # filter(as.Date(dt) == as.Date('2023-06-23')) %>% 
#   # filter(
#   #   OfferID == 439234
#   # ) %>% 
#   View()
# 
# system(paste0('open ', '"', grep(439234, ll, value = T)[[1]], '"'))
# system(paste0('open ', '"', grep(439234, ll, value = T)[[2]], '"'))
# 
# prov %>% 
#   filter(OfferID == 441515) %>% 
#   View()





Test_Missing_Scoring = Vintage_RDS_Files_DF_Wrangled %>%
  filter(MonthInserted == as.Date("2023-05-01")
         & Has_MAD_Due == 1
         & NextObsPoint == as.Date("2023-06-01")
         & MOB == 1
         ) %>%
  left_join(prov %>% select(OfferID, Scoring_Name, Scoring_Class), by = c("OfferID" = "OfferID"))

Test_Missing_Scoring %>% 
  filter(is.na(Scoring_Class)) %>% 
  View()


sum(is.na(Test_Missing_Scoring$Scoring_Name))

Missing_Score = Test_Missing_Scoring %>%
  filter(is.na(Scoring_Name))

Prov_Missing = prov %>%
  filter(OfferID %in% Missing_Score$OfferID)

Missing_Score %>% count(DateInserted)
# 1:   2023-07-03  8
# 2:   2023-07-14 37
# 3:   2023-07-21 31
# 4:   2023-07-22  5


Missing_Score %>% 
  filter(DateInserted == '2023-07-03') %>% 
  pull(OfferID)
# 3rd
# 441553 441527 441642 441633 441587 441625 441606 441619
# 14th
# 444250 444352 444371 444351 444192 444346 444229 444316 444258 444286 444315 444320 444252 444275 444303 444324 444138 444361 444270 444325 444311 444343
# 444298 444369 444317 444370 444384 444358 444143 444280 444205 444279 444279 444200 444264 444187 444182 
# 21st
# 446024 446031 446032 446046 446050 446056 446060
# 446063 446065 446077 446090 446093 446099 446105 446114 446128 446131 446133 446134 446144 446157 446172 446182 446198 446209 446221 446222 446245 446269
# 446282 446283
# 22nd
# 446332 446384 446392 446363 446366
Parsed_Provenir_Data <- data.table::fread(
  "//hind.smartitbg.int/FileServer/Data Analyses/Analysis/1.Projects/112.WCard RO/7.Data/05 Parse Main Provenir Data/Output Data/020_All_Data.csv")


444250 %in% Parsed_Provenir_Data$OfferID




ll = fs::dir_ls("C:/Users/martin.lazarov/Desktop/xmls", recurse = T, type = 'file')


df = data.frame(
  file_name = ll
) %>% 
  mutate(
    OfferID = str_extract(file_name, "\\d{6}(?=.xml)"),
    fn = str_extract(file_name, "\\d{17}.*")
  )

found_ids = df %>% 
  filter(OfferID %in% Missing_Score$OfferID) %>% 
  pull(OfferID)

found_files = df %>% 
  filter(OfferID %in% Missing_Score$OfferID) %>% 
  pull(file_name)

found_names = df %>% 
  filter(OfferID %in% Missing_Score$OfferID) %>% 
  pull(fn)

# 72 found
Missing_Score %>% 
  filter(!OfferID %in% found_ids) %>% 
  select(DateInserted) %>% 
  count()
length(unique(Missing_Score$OfferID))

fs::file_copy(
  found_files,
  '//hind.smartitbg.int/FileServer/Data Analyses/Analysis/1.Projects/112.WCard RO/7.Data/Provenir XMLs 2023_07_01_2023_07_31/'
)

jul <- list.files(
  '//hind.smartitbg.int/FileServer/Data Analyses/Analysis/1.Projects/112.WCard RO/7.Data/Provenir XMLs 2023_07_01_2023_07_31/',
  full.names = F
)

jul
sum(found_names %in% jul)

