options(scipen=999)
rm(list = ls())
gc()
library("xlsx")
library("DBI")
library("odbc")
library("tidyverse")
library("lubridate")
library("ggthemes")




source('./Functions/F_FormatPercentage.R')
source('./Functions/F_DateDiffMonths.R')
source('./Functions/F_CreateDateGrid.R')
source('./Functions/F_InterpolateRecoveryRates.R')
source("./Functions/F_Elapsed_Months.R")

Sys.setlocale("LC_TIME", "English")


ReportDate <- as.Date("2019-10-01")
ReportDay = 1

substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}

### Load All File paths 
AllFiles <- data.frame(   FilePath = list.files('//10.254.1.4\\Files\\Analysis\\3.Data'
                                                , recursive = TRUE
                                                , full.names = TRUE)
                          , stringsAsFactors = FALSE
)

## Define Delinquency Groups Labelling: 
DPDGroups <- c('0 DPD', '1+ DPD', 'M: <= 30', 'M: [31;60]', 'M: [61;90]','M: [91;180]', 'M: >= 181' , 'Total:', 'Error!:')



```{r PrepareCreditProgress, include = FALSE}

### Initial credit progress is *.rds file with data as end of 5 sep 2017
### The remaining files are loaded from the relative data directory through the following routine

#setwd('..')

ConsolProgFilePath <- list.files('//10.254.1.4/Files/Analysis/9. Personal folders/Zhivko Kolev/R Projects/AutomatedReports/Data/002 WCBG Provisions')
ConsolProgFilePath <- ConsolProgFilePath[grepl("Card Progress Consolidated",ConsolProgFilePath)]

CreditProgress   <- readRDS(file.path('//10.254.1.4/Files/Analysis/9. Personal folders/Zhivko Kolev/R Projects/AutomatedReports/Data/002 WCBG Provisions', ConsolProgFilePath))
DateOfLastRecordCreProg <- max(CreditProgress$Exportdate)

CreditProgressNamePattern <- 'Export-33-Apostol Antonia-CardProgress_'
CreditProgressNamePattern_Exclude <- 'Export-33-Apostol Antonia-CardProgress_RO'
CreditProgressNamePattern_Exclude2 <- 'Export-33-Apostol Antonia-CardProgress_PL'


list(ReportDate = lubridate::floor_date(lubridate::as_date(Sys.Date()), 'month')

CrProgForLoad <- AllFiles %>% 
  filter( grepl(CreditProgressNamePattern        ,FilePath) # Includes filenames with this pattern 
          ,!grepl(CreditProgressNamePattern_Exclude,FilePath) # Excludes filenames with this pattern 
          ,!grepl(CreditProgressNamePattern_Exclude2,FilePath) # Excludes filenames with this pattern
  ) %>% 
  mutate(ExportDate = as.Date(substr(substrRight(FilePath, 14),1,10), format = '%d.%m.%Y')
         ,ExportDay = lubridate::day(ExportDate)
  ) %>% 
  ### Select Export as of 1st day of the month that are after the DateOFLastRecord ### 
  filter(floor_date(ExportDate, 'month') <=  floor_date(ReportDate, 'month')
         ,floor_date(ExportDate, 'month') >   floor_date(DateOfLastRecordCreProg, 'month')
         ,ExportDay == ReportDay
  )



if(nrow(CrProgForLoad) != 0) {
  
  CreditProgressColNames <- lapply(lapply(CrProgForLoad[[1]], read_lines, n_max = 1), strsplit,  split = ';')
  
  #### Define Fields Spec #### 
  
  CreProgColSpec <- cols(
    EasyClientNumber = col_integer(),
    AID = col_integer(),
    CardID = col_integer(),
    ClientID = col_integer(),
    CurrentLimit = col_double(),
    CurrentMPV = col_double(),
    DayDelay = col_double(),
    CurrentInterest = col_double(),
    CurrentStatus = col_double(),
    LastPayoffdate = col_date(),
    LastWithdrawnDate = col_date(),
    LastPayoffSum = col_double(),
    LastWithdrawnAmount = col_double(),
    PayedSum = col_double(),
    PromotionPrinc = col_double(),
    Princ = col_double(),
    Interest20 = col_double(),
    TotalInterest20 = col_double(),
    TotalPrinc20 = col_double(),
    RevolvingTax = col_double(),
    CollectorTax = col_double(),
    Interest = col_double(),
    AnnualFee = col_skip(),
    LeagalTax = col_double(),
    DelayTax = col_double(),
    PenaltyFee = col_skip(),
    OtherTaxes = col_skip(),
    ATMTransFee = col_skip(),
    Discount = col_double(),
    TotalDue = col_double(),
    MomentRate = col_double(),
    OperatorID = col_integer(),
    CollectorID = col_integer(),
    PreCollection = col_double(),
    Firm = col_integer(),
    TotalPromotionPrinc = col_double(),
    TotalPrinc = col_double(),
    TotalInterest = col_double(),
    TotalRevTax = col_double(),
    TotalCollectorTax = col_double(),
    TotalDelayTax = col_double(),
    TotalPenaltyFee = col_skip(),
    TotalLegalTax = col_double(),
    TotalOtherTaxes = col_skip(),
    TotalATMTransFee = col_skip(),
    TotalAnnualFee = col_skip(),
    Exportdate = col_datetime()
    
    
  )
  
  
  #
  
  NewData <- purrr::map2(  .x = as.list(CrProgForLoad[[1]])
                           , .y = CreditProgressColNames[[1]]
                           , .f =  ~ readr::read_delim(file = .x, delim = ';', na = c('','NULL'), quote = "" 
                                                       , col_names = .y
                                                       , col_types = CreProgColSpec
                                                       , skip = 2))
  
  TmpBuffer <- do.call(bind_rows, NewData)
  CreditProgress <- bind_rows(CreditProgress ,filter(TmpBuffer, !is.na(EasyClientNumber)) %>% mutate(LastWithdrawnDate = as_date(LastWithdrawnDate)
                                                                                                     ,LastPayoffdate = as_date(LastPayoffdate)
                                                                                                     # ,Exportdate = as_date(Exportdate)
  ))
  rm(TmpBuffer)
  
   saveRDS(
     CreditProgress

     , file.path(paste(gsub('-','',ReportDate), gsub('-','',Sys.Date()), 'Card Progress Consolidated.rds')
     )
  )
  
}

# CreditProgress <- readRDS("20191001 20191029 Card Progress Consolidated.rds")

### Initial Transactions  is *.rds file with data as end of 5 sep 2017
### The remaining files are loaded from the relative data directory through the following routine



ConsolTrFilePath <- list.files('//10.254.1.4/Files/Analysis/9. Personal folders/Zhivko Kolev/R Projects/AutomatedReports/Data/002 WCBG Provisions')
ConsolTrFilePath <- ConsolTrFilePath[grepl("Transactions",ConsolTrFilePath)]

Transactions   <- readRDS(file.path('//10.254.1.4/Files/Analysis/9. Personal folders/Zhivko Kolev/R Projects/AutomatedReports/Data/002 WCBG Provisions', ConsolTrFilePath))
DateOfLastRecordTransactions <- ceiling_date(max(Transactions$DocumentDate), 'month')

### Create List of filepaths to inport
## Filename patterns 

CreditProgressNamePatternTransactions <- 'Export-33-Apostol Antonia-ExtraExpenses_'
CreditProgressNamePatternTransactions_Exclude <- 'Export-33-Apostol Antonia-ExtraExpenses_RO'
CreditProgressNamePatternTransactions_Exclude2 <- 'Export-33-Apostol Antonia-ExtraExpenses_PL'

TransactionsForLoad <- AllFiles %>% 
  filter( grepl(CreditProgressNamePatternTransactions,FilePath) # Includes filenames with this pattern 
          ,!grepl(CreditProgressNamePatternTransactions_Exclude,FilePath) # Excludes filenames with this pattern
          ,!grepl(CreditProgressNamePatternTransactions_Exclude2,FilePath) # Excludes filenames with this pattern
  ) %>% 
  mutate(ExportDate = as.Date(substr(substrRight(FilePath, 14),1,10), format = '%d.%m.%Y')
         ,ExportDay = lubridate::day(ExportDate)
  ) %>% 
  ### Select Export as of 1st day of the month that are after the DateOFLastRecord ### 
  filter( floor_date(ExportDate, 'month') <=  floor_date(ReportDate, 'month')
          ,floor_date(ExportDate, 'month') >   floor_date(DateOfLastRecordTransactions, 'month')
          ,ExportDay ==  ReportDay
  )



if(nrow(TransactionsForLoad) != 0) {
  
  TransactionsSpec <- cols(
    EasyClientNumber = col_integer(),
    Amount = col_double(),
    DID = col_integer(),
    DocumentDate = col_datetime(),
    DocumentDate = col_character()
  )
  
  
  # Define utility function for transactions import # 
  
  ProcessTransactions <- function(x, colSpec = NULL, skipRows = 2) {
    
    
    NamesVector <<- sapply(lapply(x, read_lines, n_max = 1), strsplit,  split = ';')
    AppendTrans <- readr::read_delim( x
                                      , delim = ';'
                                      , col_names = NamesVector[[1]]
                                      , skip = skipRows, col_types = colSpec
                                      , na = c('','NULL')
                                      , quote = ""
    )
    
    
    AppendTrans$DID_name <- NULL
    AppendTrans <- AppendTrans %>% distinct() %>% 
      mutate(DocumentDate = as.Date(DocumentDate))
    
  }
  
  
  MyTmpList <- lapply(TransactionsForLoad[[1]], ProcessTransactions, colSpec = TransactionsSpec)
  MyTmpList <- do.call(bind_rows, MyTmpList)
  Transactions <- bind_rows(Transactions, MyTmpList)
  
  rm(MyTmpList)
  
  
  saveRDS(
    Transactions

    , file = file.path(paste(gsub('-','',ReportDate), gsub('-','',Sys.Date()), 'Transactions.rds')
    )
  )

  
}

# Transactions <- readRDS("20191001 20191029 Transactions.rds")


max(Transactions$DocumentDate)

# Sales 

DebtSaleList <- data.frame(FilePath = list.files("//10.254.1.4/Files/Analysis/9. Personal folders/Zhivko Kolev/R Projects/AutomatedReports/Data/002 WCBG Provisions/DebtSale", full.names = TRUE)
                           ,  stringsAsFactors = FALSE) %>% 
  ## create Date column ## 
  mutate(ReportDate = as.Date(paste0(substr(gsub(".*Sold_", "",FilePath ), 1,6),'01'), format = '%Y%m%d') #%>%
 # filter(as.Date(ReportDate) <= as.Date(floor_date(ReportDate, 'month') - days(1))) # do not take the deb sale in the current month

ImportDebtSale <- function(x,y) {
  require(dplyr)
  require(readr)
  
  specs <-  cols( EasyClientNumber = col_integer()
                  ,PrincipalDue = col_double()
                  ,Outstanding = col_double()
                  ,SalesPrice = col_double()
  )
  
  
  X <- mutate(read_delim(file = x, delim = ';'
                         ,col_types = specs
                         , col_names = c('EasyClientNumber', 'PrincipalDue', 'Outstanding', 'SalesPrice')
                         ,skip = 1)
              , ReportDate = as.Date(y))
  
  names(X) <- c('EasyClientNumber', 'PrincipalDue', 'Outstanding', 'SalesPrice','ReportDate')
  X
}

if (nrow(DebtSaleList) >0) {
  
  SoldDebt <-   purrr::map2(  .x = as.list(DebtSaleList$FilePath)
                              , .y = as.list(DebtSaleList$ReportDate)
                              , .f = ImportDebtSale)
  
  
  SoldDebt <- do.call(bind_rows, SoldDebt)
  
  saveRDS(
    SoldDebt

    , file = file.path(paste(gsub('-','',ReportDate), gsub('-','',Sys.Date()), 'DebtSales.rds')
    )
  )
  
}


# SoldDebt <- readRDS("20191001 20191029 DebtSales.rds")


#save.image("./Environment.rdata")


