
path_to_search <- "//10.254.1.4/Files/Analysis/3.Data/CCR Data/Easy Credit BG 20th/All files extracted"

###### Banks #####
file_list_banks <- list.files(path = path_to_search, pattern = "INF_B_F", all.files = FALSE,
                                 full.names = FALSE, recursive = T,
                                 ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

#corrections
file_list_banks_corrections <- list.files(path = path_to_search, pattern = "INF_BC_F", all.files = FALSE,
                                          full.names = FALSE, recursive = T,
                                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

##### Financial Institutions #####
file_list_fin <- list.files(path = path_to_search, pattern = "INF_F_F", all.files = FALSE,
                              full.names = FALSE, recursive = T,
                              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
#corrections
file_list_fin_corrections <- list.files(path = path_to_search, pattern = "INF_FC_F", all.files = FALSE,
                                          full.names = FALSE, recursive = T,
                                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

########
# Combine all data sets for Bank institutions

all_banks <- NULL

for (i in seq(1:length(file_list_banks))){
  
  f <- read.delim(file_list_banks[i], head=F, stringsAsFactors = F, quote = "")
  names(f) <- "x"
  f$INF_BORR <- substr(f$x, 1, 21)
  f$INF_NAME<-substr(f$x, 22, 82)
  f$INF_ADDR<-substr(f$x, 83, 143)
  f$INF_DATE<-substr(f$x, 144, 152)
  f$INF_BGL<-substr(f$x, 153, 165)
  f$INF_SUMA<-substr(f$x, 166, 178)
  f$INF_PRINC<-substr(f$x, 179, 191)
  f$INF_OVER<-substr(f$x, 192, 204)
  f$INF_OVER_INTER<-substr(f$x, 205, 217)
  f$INF_JUD_DUES<-substr(f$x, 218, 230)
  f$INF_BALLANS<-substr(f$x, 231, 243)
  f$INF_OFFBAL<-substr(f$x, 244, 256)
  f$INF_CODE_401<-substr(f$x, 257, 260)
  f$INF_401_BALL<-substr(f$x, 261, 273)
  f$INF_401_OFFB<-substr(f$x, 274, 286)
  f$INF_CODE_402<-substr(f$x, 287, 290)
  f$INF_402_BALL<-substr(f$x, 291, 303)
  f$INF_402_OFFB<-substr(f$x, 304, 316)
  f$INF_CODE_403<-substr(f$x, 317, 320)
  f$INF_403_BALL<-substr(f$x, 321, 333)
  f$INF_403_OFFB<-substr(f$x, 334, 346)
  f$INF_CODE_404<-substr(f$x, 347, 350)
  f$INF_404_BALL<-substr(f$x, 351, 363)
  f$INF_404_OFFB<-substr(f$x, 364, 376)
  f$INF_COND_BAL<-substr(f$x, 377, 389)
  f$INF_MONTH_SUMA<-substr(f$x, 390, 402)
  f$INF_CODE_70<-substr(f$x, 403, 406)
  f$INF_70_BALL<-substr(f$x, 407, 419)
  f$INF_70_OFFB<-substr(f$x, 420, 432)
  f$INF_CODE_71<-substr(f$x, 433, 436)
  f$INF_71_BALL<-substr(f$x, 437, 449)
  f$INF_71_OFFB<-substr(f$x, 450, 462)
  f$INF_CODE_72<-substr(f$x, 463, 466)
  f$INF_72_BALL<-substr(f$x, 467, 479)
  f$INF_72_OFFB<-substr(f$x, 480, 492)
  f$INF_CODE_73<-substr(f$x, 493, 496)
  f$INF_73_BALL<-substr(f$x, 497, 509)
  f$INF_73_OFFB<-substr(f$x, 510, 522)
  f$INF_CODE_74<-substr(f$x, 523, 526)
  f$INF_74_BALL<-substr(f$x, 527, 539)
  f$INF_74_OFFB<-substr(f$x, 540, 552)
  f$INF_CODE_75<-substr(f$x, 553, 556)
  f$INF_75_BALL<-substr(f$x, 557, 569)
  f$INF_75_OFFB<-substr(f$x, 570, 582)
  f$test <- substr(f$INF_BORR, 11, 21)
  
  # Махаме грешните редове
  
  f$INF_BORR=substr(f$x, 1, 10)
  f<-f[f$test=="           ",]
  f$x<-NULL
  f$test<- NULL
  all_banks <- rbind(all_banks, f)
  
}
# Combine all data sets for Financial institutions

all_fin <- NULL

for (i in seq(1:length(file_list_fin))){
  
  f <- read.delim(file_list_fin[i], head=F, stringsAsFactors = F, quote = "")
  names(f) <- "x"
  f$INF_BORR <- substr(f$x, 1, 21)
  f$INF_NAME<-substr(f$x, 22, 82)
  f$INF_ADDR<-substr(f$x, 83, 143)
  f$INF_DATE<-substr(f$x, 144, 152)
  f$INF_BGL<-substr(f$x, 153, 165)
  f$INF_SUMA<-substr(f$x, 166, 178)
  f$INF_PRINC<-substr(f$x, 179, 191)
  f$INF_OVER<-substr(f$x, 192, 204)
  f$INF_OVER_INTER<-substr(f$x, 205, 217)
  f$INF_JUD_DUES<-substr(f$x, 218, 230)
  f$INF_BALLANS<-substr(f$x, 231, 243)
  f$INF_OFFBAL<-substr(f$x, 244, 256)
  f$INF_CODE_401<-substr(f$x, 257, 260)
  f$INF_401_BALL<-substr(f$x, 261, 273)
  f$INF_401_OFFB<-substr(f$x, 274, 286)
  f$INF_CODE_402<-substr(f$x, 287, 290)
  f$INF_402_BALL<-substr(f$x, 291, 303)
  f$INF_402_OFFB<-substr(f$x, 304, 316)
  f$INF_CODE_403<-substr(f$x, 317, 320)
  f$INF_403_BALL<-substr(f$x, 321, 333)
  f$INF_403_OFFB<-substr(f$x, 334, 346)
  f$INF_CODE_404<-substr(f$x, 347, 350)
  f$INF_404_BALL<-substr(f$x, 351, 363)
  f$INF_404_OFFB<-substr(f$x, 364, 376)
  f$INF_COND_BAL<-substr(f$x, 377, 389)
  f$INF_MONTH_SUMA<-substr(f$x, 390, 402)
  f$INF_CODE_70<-substr(f$x, 403, 406)
  f$INF_70_BALL<-substr(f$x, 407, 419)
  f$INF_70_OFFB<-substr(f$x, 420, 432)
  f$INF_CODE_71<-substr(f$x, 433, 436)
  f$INF_71_BALL<-substr(f$x, 437, 449)
  f$INF_71_OFFB<-substr(f$x, 450, 462)
  f$INF_CODE_72<-substr(f$x, 463, 466)
  f$INF_72_BALL<-substr(f$x, 467, 479)
  f$INF_72_OFFB<-substr(f$x, 480, 492)
  f$INF_CODE_73<-substr(f$x, 493, 496)
  f$INF_73_BALL<-substr(f$x, 497, 509)
  f$INF_73_OFFB<-substr(f$x, 510, 522)
  f$INF_CODE_74<-substr(f$x, 523, 526)
  f$INF_74_BALL<-substr(f$x, 527, 539)
  f$INF_74_OFFB<-substr(f$x, 540, 552)
  f$INF_CODE_75<-substr(f$x, 553, 556)
  f$INF_75_BALL<-substr(f$x, 557, 569)
  f$INF_75_OFFB<-substr(f$x, 570, 582)
  f$test <- substr(f$INF_BORR, 11, 21)
  
  # Махаме грешните редове
  
  f$INF_BORR=substr(f$x, 1, 10)
  f<-f[f$test=="           ",]
  f$x<-NULL
  f$test<- NULL
  all_fin <- rbind(all_fin, f)
  
}

remove(f, f1, bla)
f<- all_banks
convert_vars_to_numeric <- function (f){
  f$INF_BGL=as.numeric(f$INF_BGL)
  f$INF_SUMA=as.numeric(f$INF_SUMA)
  f$INF_PRINC=as.numeric(f$INF_PRINC)
  f$INF_OVER=as.numeric(f$INF_OVER)
  f$INF_OVER_INTER=as.numeric(f$INF_OVER_INTER)
  f$INF_JUD_DUES=as.numeric(f$INF_JUD_DUES)
  f$INF_BALLANS=as.numeric(f$INF_BALLANS)
  f$INF_OFFBAL=as.numeric(f$INF_OFFBAL)
  f$INF_CODE_401=as.numeric(f$INF_CODE_401)
  f$INF_401_BALL=as.numeric(f$INF_401_BALL)
  f$INF_401_OFFB=as.numeric(f$INF_401_OFFB)
  f$INF_CODE_402=as.numeric(f$INF_CODE_402)
  f$INF_402_BALL=as.numeric(f$INF_402_BALL)
  f$INF_402_OFFB=as.numeric(f$INF_402_OFFB)
  f$INF_CODE_403=as.numeric(f$INF_CODE_403)
  f$INF_403_BALL=as.numeric(f$INF_403_BALL)
  f$INF_403_OFFB=as.numeric(f$INF_403_OFFB)
  f$INF_CODE_404=as.numeric(f$INF_CODE_404)
  f$INF_404_BALL=as.numeric(f$INF_404_BALL)
  f$INF_404_OFFB=as.numeric(f$INF_404_OFFB)
  f$INF_COND_BAL=as.numeric(f$INF_COND_BAL)
  f$INF_MONTH_SUMA=as.numeric(f$INF_MONTH_SUMA)
  f$INF_CODE_70=as.numeric(f$INF_CODE_70)
  f$INF_70_BALL=as.numeric(f$INF_70_BALL)
  f$INF_70_OFFB=as.numeric(f$INF_70_OFFB)
  f$INF_CODE_71=as.numeric(f$INF_CODE_71)
  f$INF_71_BALL=as.numeric(f$INF_71_BALL)
  f$INF_71_OFFB=as.numeric(f$INF_71_OFFB)
  f$INF_CODE_72=as.numeric(f$INF_CODE_72)
  f$INF_72_BALL=as.numeric(f$INF_72_BALL)
  f$INF_72_OFFB=as.numeric(f$INF_72_OFFB)
  f$INF_CODE_73=as.numeric(f$INF_CODE_73)
  f$INF_73_BALL=as.numeric(f$INF_73_BALL)
  f$INF_73_OFFB=as.numeric(f$INF_73_OFFB)
  f$INF_CODE_74=as.numeric(f$INF_CODE_74)
  f$INF_74_BALL=as.numeric(f$INF_74_BALL)
  f$INF_74_OFFB=as.numeric(f$INF_74_OFFB)
  f$INF_CODE_75=as.numeric(f$INF_CODE_75)
  f$INF_75_BALL=as.numeric(f$INF_75_BALL)
  f$INF_75_OFFB=as.numeric(f$INF_75_OFFB)
  
  return (f)
}

all_fin$source <- "F"

all_banks$source <- "B"

all_banks <- convert_vars_to_numeric(all_banks)
all_fin <- convert_vars_to_numeric(all_fin)

all_banks$INF_DATE <- as.Date(all_banks$INF_DATE, format = "%Y%m%d")
all_fin$INF_DATE <- as.Date(all_fin$INF_DATE, format = "%Y%m%d")

summary(all_banks$INF_DATE)
#str(all_banks$INF_DATE)
## Extract most recent observations for each credit


library(dplyr)

all_banks_cleaned <- all_banks %>% group_by(INF_BORR) %>% 
                                  mutate(LastPeriod = max(INF_BORR)
                                         , FilterDate = case_when(LastPeriod == INF_BORR ~ 1
                                                                  , TRUE ~ 0)) %>% 
                                  filter(FilterDate == 1)


all_fin_cleaned <- all_fin %>% group_by(INF_BORR) %>% 
                                mutate(LastPeriod = max(INF_BORR)
                                       , FilterDate = case_when(LastPeriod == INF_BORR ~ 1
                                                                , TRUE ~ 0)) %>% 
                                filter(FilterDate == 1)

setwd("//10.254.1.4/Files/Analysis/9. Personal folders/Ivelina Vasileva/1. Projects/010 Easy BG/Churn Easy/Data/CCR20 Data/DataSets")

saveRDS(all_banks_cleaned, "all_banks_cleaned.rds")
saveRDS(all_banks, "all_banks.rds")
saveRDS(all_fin_cleaned, "all_fin_cleaned.rds")
saveRDS(all_fin, "all_fin.rds")