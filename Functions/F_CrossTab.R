### Things for further fixing 
# If there is cathegory Total !



CrossTab <- function(df,ind, ds
                        , ds.c = NULL
                        , cutv = NULL
                        , sam = NULL
                        , sam.cat = NULL
                        , rej = NULL
                        , rej.cat = NULL
                        , th = 10) {
  
  require(dplyr)
  require(tidyr)
  require(lazyeval)
  require(pROC)
  # source('Functions/F_Smart_Binning.R')
  
  results <- list()
  SummaryStat.df <- data.frame(
                              IV = double()
                            ,df =  integer()
                            , Chi_Sq = double()
                            , Sig = integer()
                            , CrammersV = double()
                            , AUC = double()
                            )
  # if(length(unique(df[,ds])) < 2) {stop('The ourcome has only one distinct value!')}
  
  if(is.numeric(sam)) {stop('The provided sample variable is numeric! Please provide character or factor variable !')}
  if(is.null(ds.c)) {ds.c = as.vector(unique(df[,ds])[2])}
  if(!is.null(sam)) {
    
    tmp.sam.cat <- as.vector(unique(sam.cat))
    
    if(is.null(sam.cat)| length(sam.cat) < 2) {
      tmp.sam.cat <- as.vector(unique(df[,sam]))
      warning(paste0('No target sample category has been provided!', 
                     tmp.sam.cat[1], ' has been used as default category for the development sample!'))
      sam.cat <- ifelse(any(sam.cat %in% tmp.sam.cat), sam.cat, tmp.sam.cat[1])
      
    } else if(!sam.cat[1] %in% tmp.sam.cat) {
      
      tmp.sam.cat <- as.vector(unique(df[,sam]))
      
      warning(paste0(sam.cat[1], 'has not been found as valid category! ', 
                     tmp.sam.cat[1], ' has been used as default category for the development sample!'))
      
      sam.cat <- as.vector(tmp.sam.cat[1])
    }
  }

  cols <- noquote(
    paste('~c(',
          paste0((c(ind,ds,sam, rej)), collapse = ",")
          ,')'
    )
  )
  
  wk.data <- select_(df, as.formula(cols))
  
  if(is.null(sam)) {wk.data$Sam.Def <- 'NoSampleSplit'} else {names(wk.data)[names(wk.data)==sam] <- 'Sam.Def'}
  if(is.null(rej)) {wk.data$Rejs <- 0} else {names(wk.data)[names(wk.data) == rej] <- 'Rejs'}
  
  if(is.numeric(df[,ind])) {
  
  SummaryStat.df[1, 'AUC']  <- wk.data %>% 
                                select_(var = ind, Ds = ds, 'Sam.Def', 'Rejs') %>%
                                filter(Sam.Def == ifelse(is.null(sam), 'NoSampleSplit', sam.cat)
                                       ,(!Rejs %in% rej.cat)) %>% 
                                mutate(Ds.num = ifelse(Ds %in% c(ds.c),1,0)) %>% 
                                pROC::roc(Ds.num  ~ var, data = .) %>% 
                                auc %>% 
                                as.vector()
  }
  ##########


  if(is.numeric(wk.data[[ind]]) & length(unique(wk.data[[ind]])) >= th ) {
    if(is.null(cutv)) {stop('Please provide cut values!')}
    cutv <- sort(cutv)
    
    wk.data <- wk.data %>% 
                select_(rVariable = ind,Ds = ds, 'Sam.Def', 'Rejs') %>% 
                mutate(bVariable = ifelse(rVariable < cutv[1], cutv[1], 
                                          ifelse(rVariable > cutv[length(cutv)],cutv[length(cutv)], rVariable )),
                       Variable = cut(bVariable, unique(cutv)
                                      , include.lowest = TRUE
                                      , ordered_result = TRUE
                                      , dig.lab= 5)) %>%
                group_by(Variable) %>% 
                mutate(Average = mean(bVariable, na.rm = T)) %>%
                select(-bVariable) %>% ungroup()
    

  } else {

    wk.data <- wk.data %>% 
      select_(Variable = ind,Ds = ds, 'Sam.Def', 'Rejs') %>% 
      mutate(Average = 1)
  }
  
  levels(wk.data$Variable) <- c(levels(wk.data$Variable), 'Missing', 'Total')
  wk.data$Variable[is.na(wk.data$Variable)] <- 'Missing'
  
  wk.data <- wk.data %>%
    group_by(Sam.Def) %>%
    mutate(SD.tot = n()
           ,sam.rej  = sum(ifelse(Rejs %in% c(rej.cat),1,0), na.rm = T)
           ,sam.good =  sum(ifelse(!Ds %in% c(ds.c),1,0), na.rm = T) - sam.rej
           ,sam.bad  =  sum(ifelse(Ds %in% c(ds.c) ,1,0), na.rm = T)) %>%
    ungroup() %>%
    group_by(Sam.Def, Variable) %>%
    summarise(
      Average = mean(Average)
      ,SD.tot = mean(SD.tot, na.rm = T)
      ,sam.rej = mean(sam.rej)
      ,sam.good = mean(sam.good)
      ,sam.bad = mean(sam.bad)
      , All.tot = n()
      , n.Rej = sum(ifelse(Rejs %in% c(rej.cat),1,0), na.rm = T)
      , Disb.tot = All.tot - n.Rej
      , n.Good = sum(ifelse(!Ds %in% c(ds.c),1,0), na.rm = T) - n.Rej
      , n.Bad  = sum(ifelse(Ds %in% c(ds.c) ,1,0), na.rm = T)
      , BR = n.Bad/ Disb.tot
      , BG_odds = n.Bad / n.Good
      , Log_odds = ifelse(BG_odds == 0, -12, log(BG_odds))
      , distr.Good = n.Good / (sam.good)
      , distr.Bad = n.Bad / sam.bad
      , distr.Total = Disb.tot/(sam.good + sam.bad)
    )%>%group_by(Sam.Def) %>%
    mutate(
      cum.Good = cumsum(distr.Good)
      , cum.Bad = cumsum(distr.Bad)
      , cum.Total = cumsum(distr.Total)
      , Woe = log(distr.Good / distr.Bad)
      , IV = (distr.Good - distr.Bad) * Woe ) %>% ungroup()
  
### Create the Summary row 'Total' ### 
  
  wk.data <- bind_rows(wk.data, 
                       wk.data %>% group_by(Sam.Def) %>%  
                         summarise(
                           Variable = factor('Total', levels = levels(wk.data$Variable))
                           ,Average = mean(Average, na.rm = T)
                           ,SD.tot = mean(SD.tot, na.rm = T)
                           ,sam.rej = mean(sam.rej)
                           ,sam.good = mean(sam.good)
                           ,sam.bad = mean(sam.bad)
                           ,All.tot = sum(All.tot, na.rm = T)
                           ,n.Rej = sum(n.Rej, na.rm = T)
                           ,Disb.tot = sum(Disb.tot, na.rm = T)
                           ,n.Good = sum(n.Good, na.rm = T)
                           ,n.Bad = sum(n.Bad, na.rm = T)
                           ,BR = n.Bad / Disb.tot
                           ,BG_odds = n.Bad / n.Good
                           ,Log_odds = ifelse(BG_odds == 0, -12, log(BG_odds))
                           ,distr.Good = sum(distr.Good, na.rm = T)
                           ,distr.Bad = sum(distr.Bad, na.rm = T)
                           ,distr.Total = sum(distr.Total, na.rm = T)
                           ,cum.Good = 1
                           ,cum.Bad = 1
                           ,cum.Total = 1
                           ,Woe = sum(Woe, na.rm = T)
                           ,IV = sum(IV, na.rm = T))
  ) %>% ungroup()
  
  summary.df <- wk.data %>% 
    filter(Sam.Def == ifelse(is.null(sam), 'NoSampleSplit', sam.cat)) %>%
    select(-Sam.Def) %>% 
    transmute( Variable
               ,Distr.Good = n.Good
               ,Perc.Good = scales::percent(distr.Good)
               ,Cum.Good = scales::percent(cum.Good)
               ,Distr.Bad = n.Bad
               ,Perc.Bad = scales::percent(distr.Bad)
               ,Cum.Bad = scales::percent(cum.Bad)
               ,Distr.Tot = Disb.tot
               ,Perc.Tot = scales::percent(distr.Total)
               ,Cum.Tot = scales::percent(cum.Total)
               ,Bad.Rate = scales::percent(BR)
               ,Woe = scales::comma(Woe, digits = 6)
               ,IV = scales::comma(IV, digits = 3))
  
  SummaryStat.df[1, 'IV'] <- summary.df[summary.df$Variable == 'Total', 'IV']
  
  ### Evaluate Chi-sq before renaming ###
  
  try(        
    Chisq <-chisq.test(
      summary.df%>% 
        filter(Variable != 'Total', Distr.Tot > 0 ) %>%
        select(Variable, Distr.Good, Distr.Bad) %>% 
        select(-1) %>% as.matrix() %>% as.table()
    )
  )
  
  ### Rename the summary DF ### 
  
  names(summary.df)[1] <- ind
  
  
  ### Create PSI summaries ### 
  
  if(!is.null(sam)) {
    
    PSI.df <- wk.data %>% 
      select(Sam.Def, Variable, distr.Total) %>%
      filter(Variable != 'Total') %>%
      spread(Sam.Def, distr.Total)
    
    for (i in tmp.sam.cat[-which(tmp.sam.cat == sam.cat[1])]) { 
      
      fla <- list(interp(
        ~ round(
                ifelse(x %in% c(0,-Inf, Inf, NaN, NA) | y %in% c(0,-Inf, Inf, NaN, NA) , 0, (x - y) * log(x / y))
                ,8), x = as.name(sam.cat), y = as.name(i)
        
        
        
        ))
      PSI.df <- PSI.df %>%
        mutate_(.dots = setNames(fla,  paste(i, 'PSI', sep = '.')))
    }
    
    PSI.df <- PSI.df %>%
      select(dplyr::contains('.PSI')) %>%
      summarise_each(funs(sum))
  }

  
 ### Fill the summary data ### 
  
if(exists('Chisq')) {
  
  SummaryStat.df[1,'Chi_Sq'] <- scales::comma(unlist(Chisq$statistic), digits = 3)
  SummaryStat.df[1,'Sig'] <- formatC(Chisq$p.value, format = 'f', digits =4 )
  SummaryStat.df[1,'df'] <- unlist(Chisq$parameter)
  SummaryStat.df[1,'CrammersV'] <- formatC(sqrt(unlist(Chisq$statistic)), format = 'f', digits = 4)
  
}
 
  # Merge the summaries together # 
  if (exists('PSI.df')) {SummaryStat.df <- bind_cols(SummaryStat.df, PSI.df)}
  
  # return(PSI.df)
  return(list(summary.df, SummaryStat.df, wk.data))
  # return(summary.df)
  
}
# 
#  Cross.tab.f(
#   All.data, 'MAXBAL_ROV_12', 'DefaultStatus'
#   , ds.c = NULL
#   , cutv = c(24, 30,40,50,60,90)
#   , sam = 'Samples'
#   , sam.cat = c('Training_Sample')
#   )
