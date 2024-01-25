### Automated vintage Function ###
#required packages 
# install.packages('dplyr')
# install.packages('zoo')
# install.packages('lazyeval')
# install.packages('dplyr')
# install.packages('ggplot2')
# install.packages('RODBC')
# install.packages('lubridate')


#### Function definition #### 

GetVintageData <- function(df
                           , ProdD, ProdForm = '%Y-%m-%d', ProdFormCondStart = '1901-01-01', ProdFormCondEnd = Sys.Date()
                           , DefD,  DefForm  = '%Y-%m-%d', DefFormCondStart  = '1901-01-01', DefFormCondEnd  = Sys.Date() 
                           , SplitVar = ''
                           , DisbursedField  = NULL, DisbursedValue = ''
                           
                           , Period = 'Monthly'  ## Period: Monthly;Quarterly;Annual 
                           , Aggregate = FALSE
                           , DataMode = 1 ## Return Data for display or for plot 
) {
  
  
  ## Select input supplied
  Sys.setlocale("LC_TIME", "English")
  require(dplyr)
  require(zoo)
  require(lazyeval)
  require(lubridate)
  
  
  ### help f
  
  SeqMData <- function(d1, d2) {

    
    Date1 <- as.Date(d1)
    Date2 <- as.Date(d2)
    
    seq.Date(floor_date(Date1, 'month'), Date2, by =  'month')
    
    
  }
  
  ###
  ProdDateMissings <- is.na(df[[ProdD]])
  DefDateMissings <- is.na(df[[DefD]])
  
  if (sum(is.na(df[[ProdD]])) > 0) {cat('Warning! There is missing Production dates!')}
  
  if (any(is.na(as.Date(as.character(df[[ProdD]][!ProdDateMissings]), format = ProdForm)))) {
    cat('The one or more values of the Production Date field does not match the value format!\n
        Please provide valid field or valid date format!')
    return(NULL)
  }
  
  if (any(is.na(as.Date(as.character(df[[DefD]][!DefDateMissings]), format = DefForm)))) {
    cat('The one or more values of the Default Date field does not match the value format!\n
        Please provide valid field or valid date format!')
    return(NULL)
  }
  
  ### FILTERS ### 
  
  ## Mini funct to extract year from date ## 
  if (SplitVar == '') {SplitVar <- NULL }
  
  as.YearOnly <- function(x,y) {as.numeric(format(as.Date(x, format = y),"%Y"))}
  
  if (is.null(DisbursedField)) {
    
    cat('Disbursed indication field not provided!\n
        No filtering has been applied!')
    
    DisbursedOnly <- TRUE
    
  }  else if (DisbursedValue == '') {
    
    cat('Disbursed indication condition not provided!\n
        No filtering has been applied!')
    
    DisbursedOnly <- TRUE
    
  } else {
    
    DisbursedOnly <- interp(~a == b, a = as.name(DisbursedField), b = DisbursedValue)
  }
  
  ## Time constrains
  ### Prod
  
  ProdFormCondStart <-  max(as.Date(ProdFormCondStart)
                            , min(as.Date(df[[ProdD]], format = ProdForm), na.rm = TRUE))
  
  ProdFormCondEnd <-  min(as.Date(ProdFormCondEnd)
                          , max(as.Date(df[[ProdD]], format = ProdForm), na.rm = TRUE))
  
  if (ProdFormCondStart > ProdFormCondEnd) {ProdFormCondEnd <- ProdFormCondStart}
  
  ###  Def
  
  DefFormCondStart <-  max(as.Date(DefFormCondStart)
                           , min(as.Date(df[[DefD]], format = DefForm), na.rm = TRUE))
  
  DefFormCondEnd <-  min(as.Date(DefFormCondEnd)
                         , max(as.Date(df[[DefD]], format = DefForm), na.rm = TRUE))
  
  if (DefFormCondStart > DefFormCondEnd) {DefFormCondEnd <- DefFormCondStart} 
  
  
  
  ### Function depending on month ### 
  
  per.fun <-   switch(Period, 
                      'Monthly'   = {list(fun = 'as.yearmon', per = 12)}
                      ,'Quarterly' = {list(fun = 'as.yearqtr', per = 4)}
                      ,'Annual'    = {list(fun = 'as.YearOnly', per = 1)}
  )
  
  ### 
  
  
  ### 
  
  ProdDf <-  df %>% 
    filter_(DisbursedOnly)  %>%
    filter_(interp(~!is.na(x), x = as.name(ProdD))) %>% 
    filter_(interp(~as.Date(x, format = ProdForm) >= ProdFormCondStart, x = as.name(ProdD))) %>%
    filter_(interp(~as.Date(x, format = ProdForm) <= ProdFormCondEnd, x = as.name(ProdD))) %>%
    select_(.dots = c(ProdD,SplitVar)) %>%
    mutate_(MyADate = interp(~f(x,ProdForm), f = as.name(per.fun[['fun']]),  x = as.name(ProdD))
            , MyADateHelp = interp(~f(x,ProdForm), f = as.name('as.yearmon'),  x = as.name(ProdD))
    ) %>%
    group_by_(.dots = c('MyADate', SplitVar)) %>%
    summarise(Production = n()
              , ObservationMonths = n_distinct(MyADateHelp)) %>% ungroup()
  
  ###
  
  DefaultDf <- df %>%
    filter_(interp(~!is.na(x), x = as.name(ProdD))) %>% 
    filter_(interp(~!is.na(x), x = as.name(DefD)))  %>% 
    filter_(interp(~as.Date(x, format = ProdForm) >= ProdFormCondStart, x = as.name(ProdD))) %>%
    filter_(interp(~as.Date(x, format = ProdForm) <= ProdFormCondEnd, x = as.name(ProdD))) %>%
    filter_(interp(~as.Date(x, format = DefForm) >= DefFormCondStart, x = as.name(DefD))) %>%
    filter_(interp(~as.Date(x, format = DefForm) <= DefFormCondEnd, x = as.name(DefD))) %>%
    select_(.dots = c(ProdD,DefD,SplitVar)) %>%
    mutate_(MyADate = interp(~f(x,ProdForm), f = as.name(per.fun[['fun']]),  x = as.name(ProdD))
            , MyDDate = interp(~f(x,DefForm ), f = as.name(per.fun[['fun']]),  x = as.name(DefD))) %>%
    group_by_(.dots = c('MyADate', 'MyDDate', SplitVar)) %>%
    summarise(Defaults = n()) %>% ungroup()
  
  
  
  
  SeqDates <- SeqMData(ProdFormCondStart, DefFormCondEnd)
  
  ### Silly but working ### 
  
  if (is.null(SplitVar)) {
    
    RefDates <- do.call('expand.grid', list(MyADate = unique(ProdDf[['MyADate']]),  MyDDate = unique(do.call(per.fun[['fun']],list(SeqDates))), stringsAsFactors = FALSE))
    
  } else {
    
    RefDates <- do.call('expand.grid', list(MyADate = unique(ProdDf[['MyADate']]),  MyDDate = unique(do.call(per.fun[['fun']],list(SeqDates))), SplV = unique(df[[SplitVar]]), stringsAsFactors = FALSE))
    names(RefDates)[3] <- SplitVar
  }
  
  
  RefDates <- RefDates %>% 
    filter(MyADate <= MyDDate) %>% 
    arrange(MyADate, MyADate) 
  # return(head(RefDates))
  
  
  PlotData <- RefDates %>%
    left_join(DefaultDf, by = c('MyDDate','MyADate', SplitVar )) %>%
    group_by_(.dots = c('MyADate', SplitVar)) %>%
    mutate(CumDefault = order_by(MyDDate, cumsum(as.numeric(ifelse(is.na(Defaults),0,Defaults))))) %>%
    left_join(ProdDf, by = c('MyADate', SplitVar)) %>% 
    ungroup() %>%
    mutate(`Cum Def %` = CumDefault / Production
           , MOB = round((MyDDate - MyADate)*12,2))
  
  
  if (Aggregate) {
    
    return( 
      PlotData %>% 
        group_by_(.dots = c('MOB', SplitVar)) %>%
        summarise(CumDefault = sum(CumDefault)
                  ,Production = sum(Production)
                  ,`Cum Def %` = CumDefault / Production
                  ,ObservationMontsh = sum(ObservationMonths)
        ) %>% ungroup()
    )
    
  }
  
  return(PlotData)
  
}

#### Ged SQL Data 

# Create connection # 

### END OF FUNCTION DEFINITION 
### 

# 
# myc <- odbcDriverConnect('Driver=SQL Server;Server=sql-risk-01;')
# 
# ## SQL QUERY ## 
# ## The vintage function requires 
# ## Disbursement date and the related date format
# ## Default Date  and the related date format 
# ## Default sql date format translated in r : 
# ## Optional split parameter
# 
# MyQuery <- " SELECT
# Data_otp Production
# , First_90dpd_NotFC DefDate
# , Case when mitigation is null then 'Non Mitigated' else 'Mitigated' end mitigation 
# from risk.[dbo].[ConsumerPaymentsDue_Feb17]
# where Data_otp > '2015-01-01'
# "
# 
# 
# dim(MyData)
# 
# MyData <- sqlQuery(myc, paste(MyQuery))
# 
# dim(MyData)
#   
# ##
# 
# VintageData <- GetVintageData(MyData, ProdD = 'Production'
#                                     , DefD = 'DefDate'
#                                     , Period = 'Monthly' # Period: Monthly;Quarterly;Annual
#                                    # , SplitVar = 'mitigation'
#                                     , Aggregate = FALSE
#                               )
# 
# ### With no Split Var ### 
# 
# library(ggplot2)
# 
# VintageData %>%
# ggplot(aes( x = MOB, y = `Cum Def %`))  + 
#   geom_line()
# 
# #With split Var 
# 
# VintageData %>%
#   ggplot(aes( x = MOB, y = `Cum Def %`, col = factor(MyADate)))  + 
#   geom_line() + 
#   facet_grid(mitigation ~ . , scales = "free_y" ) ## change split variable name 
# 
# 
# ?facet_grid
# 
# grid.arrange(gridExtra::tableGrob(VintageData))
# View(
# VintageData %>%
#   select(MyADate, MOB,`Cum Def %`) %>% 
#   tidyr::spread(MOB, `Cum Def %`)
# )