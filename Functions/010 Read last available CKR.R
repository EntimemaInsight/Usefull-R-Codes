rm(list = ls())
library(gtools)

path_to_search <- "//10.254.1.4/Files/Analysis/3.Data/CCR Data/Easy Credit BG 20th/All files extracted"

###### Banks #####
file_list_banks <- list.files(path = path_to_search, pattern = "INF_B_F", all.files = FALSE,
                              full.names = FALSE, recursive = T,
                              ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
##### Financial Institutions #####
file_list_fin <- list.files(path = path_to_search, pattern = "INF_F_F", all.files = FALSE,
                            full.names = FALSE, recursive = T,
                            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

read_CKR_files =   defmacro(list_of_files, path_to_search, name_of_the_data, expr={
  
  f <- read.delim(paste0(path_to_search,"/", list_of_files[length(list_of_files)]), head=F, stringsAsFactors = F, quote = "")
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
  
  assign(name_of_the_data, f)
  
})

### read last CKR for banks
read_CKR_files(list_of_files = file_list_banks, path_to_search = path_to_search, name_of_the_data = "all_banks")

### read last CKR for financial institutions 
read_CKR_files(list_of_files = file_list_fin, path_to_search = path_to_search, name_of_the_data = "all_fin")


#### Combine the two datasets ####
#rename the bank dataset
test <- colnames(all_banks)

for (i in 1:length(test)){
  test[i] <- paste0(test[i], "_B")
  
}

colnames(all_banks) <- test

#rename the financial institutions dataset

test <- colnames(all_fin)

for (i in 1:length(test)){
  test[i] <- paste0(test[i], "_F")
  
}

colnames(all_fin) <- test

### combine the two together 

all_data <- all_fin %>% full_join(all_banks, by = c("INF_BORR_F" = "INF_BORR_B", "INF_DATE_F"="INF_DATE_B"))
