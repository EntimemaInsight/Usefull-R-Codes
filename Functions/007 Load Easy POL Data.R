library(lubridate)
library(dplyr)
library(tidyverse)

#### 01 Read most recent EasyCredit BG data ####

DateOfTheFile <- gsub("-", "",  floor_date(Sys.Date(), 'month'))
Previous_month <- floor_date(Sys.Date(), "month") -months(1)
DateOfTheFolder <- substr(gsub("-", ".", floor_date(Sys.Date(), "month") - months(1)), start = 1,stop = 7 )

Index <- "PL"
FolderIndex <- "Credit POL"


###02 Set the working paths for Bulgaria Easy #### 

Credit_progress_latest <- paste0('//10.254.1.4\\Files\\Analysis\\3.Data/' , DateOfTheFolder,
                                 '/Easy ', FolderIndex, '/E004_Credit_Progress_', Index,'_',DateOfTheFile ,'_v1.csv')

Harold_latest <-  paste0('//10.254.1.4\\Files\\Analysis\\3.Data/', DateOfTheFolder,
                         '/Easy ', FolderIndex,'/E006_Harold_',Index,'_',DateOfTheFile ,'_v1.csv')

Sold_latest <- paste0('//10.254.1.4\\Files\\Analysis\\3.Data/' , DateOfTheFolder,
                      '/Easy ', FolderIndex, '/E021A_Sold_Grouping_', Index, '_',  DateOfTheFile, '_v1.csv')

Sold_returned_latest <- paste0('//10.254.1.4\\Files\\Analysis\\3.Data/' , DateOfTheFolder,
                               '/Easy ', FolderIndex, '/E021B_Sold_Grouping_Returned_', Index, '_',  DateOfTheFile, '_v1.csv')

##### 02.1 Reading Data ####
CreProg_Latest <- read.csv(file = Credit_progress_latest, head=T, sep = ";", stringsAsFactors = F, na.strings = c("", " ", "NULL"), encoding = 'UTF-8', 
                           quote = "", colClasses=c("ClientEGN"= "character"))

Harold_Latest <- read.csv(file = Harold_latest, head = T, sep = ";", stringsAsFactors = F, na.strings = c("", " ", "NULL"), 
                          encoding = "UTF-8", quote = "", colClasses=c("ClientEGN"= "character"))

Sold_latest <- read.csv(file = Sold_latest, head=T, sep = ";", stringsAsFactors = F, na.strings = c("", " ", "NULL"), encoding = 'UTF-8', 
                        quote = "")

Sold_returned_latest <- read.csv(file = Sold_returned_latest, head=T, sep = ";", stringsAsFactors = F, na.strings = c("", " ", "NULL"), encoding = 'UTF-8', 
                                 quote = "") 
### Rename columns in Sold file###
colnames(Sold_latest)[1] <- "CreditCode"
colnames(Sold_latest)[3] <- "SoldDate"

colnames(Sold_returned_latest)[1] <- "CreditCode"


#### Remove tables if they are empty 

if (nrow(Sold_latest) == 0)
{
  rm(Sold_latest) 
} 

if (nrow(Sold_returned_latest) == 0){
  rm(Sold_returned_latest)
}