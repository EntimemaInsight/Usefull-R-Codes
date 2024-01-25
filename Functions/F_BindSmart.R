#### Generate Non emty Bands #### 

BindSmart <- function(df, ind, dep = NULL, step = 0.05, cut.v = NULL
                        , Bad.Category = NULL
                        , smart_binning = 'yes'
                        , rng = NULL
                        , result = 'cuts') {
  require(dplyr)
  require(tidyr)
  
  # if(!is.numeric(df[[ind]])) {stop('The variable is not numeric!', call. = FALSE)}
  if(!is.numeric(rng)| length(rng) < 2) {rng <- range(df[[ind]], na.rm = TRUE)}
  if(!is.null(cut.v)) {cut.v <- unique(sort(as.numeric(unlist(strsplit(gsub('[[:space:]]','', cut.v), ',')))))}
  if(is.numeric(cut.v)){
    
    cut.final <- unique(as.vector(na.omit(c(cut.v, rng))))
  } else {
    
    
    cut.final <- unique(c(quantile(unlist(df[[ind]]), seq(0, 1, step), na.rm = T), rng))
    # rng <- range(MySample[['Age']])
    # unique(c(quantile(unlist(MySample[['Age']]), seq(0, 1, 0.05), na.rm = T), NULL))

  }


  
  ### Start Smart binning ### 
  
  
  if(!is.null(dep) & smart_binning == 'yes'& dep %in% names(df)) {
    
    wk.data <- df %>% select_(Ind = ind, DS = dep) %>% 
      mutate(Groups = cut(Ind, cut.final
                          ,include.lowest = T
                          ,dig.lab = 8
                          ,ordered_result = T)) %>%
      group_by(Groups) %>%
      mutate(Avg = mean(Ind, na.rm = T)) %>%
      ungroup() %>% 
      group_by(Groups, DS, Avg)  %>% 
      summarise(cnts = n()) %>%
      select(Groups, DS, cnts, Avg) %>%  
      spread(DS, cnts) %>% ungroup
    
    
    print('Check3')
    while( any(apply(wk.data,1, function(x) any(!is.na(x[1]) & (is.na(x) | x == 0  ) ))) == T) {
      mr <-c(1:nrow(wk.data))[apply(wk.data, 1, function(x) any(!is.na(x[1]) & (is.na(x) | x == 0 )))]
      mr <- mr[1]
      
      if(identical(cut.final, rng)) {
        
        wk.data <- df %>% select_(Ind = ind, DS = dep) %>% 
          mutate(Groups = cut(Ind, cut.final
                              ,include.lowest = T
                              ,dig.lab = 8
                              ,ordered_result = T)) %>%
          group_by(Groups) %>%
          mutate(Avg = mean(Ind, na.rm = T)) %>%
          ungroup() %>% 
          group_by(Groups, DS, Avg)  %>% 
          summarise(cnts = n()) %>%
          select(Groups, DS, cnts, Avg) %>%  
          spread(DS, cnts) %>% ungroup
        
        break
        
      }
      
      if(mr == 1) {
        
        cut.final <- cut.final[-2]
        
        wk.data <- df %>% select_(Ind = ind, DS = dep) %>% 
          mutate(Groups = cut(Ind, cut.final
                              ,include.lowest = T
                              ,dig.lab = 8
                              ,ordered_result = T)) %>%
          group_by(Groups) %>%
          mutate(Avg = mean(Ind, na.rm = T)) %>%
          ungroup() %>% 
          group_by(Groups, DS, Avg)  %>% 
          summarise(cnts = n()) %>%
          select(Groups, DS, cnts, Avg) %>%  
          spread(DS, cnts) %>% ungroup
        
        
      } else {
        
        cut.final <- cut.final[-mr]
        
        wk.data <- df %>% select_(Ind = ind, DS = dep) %>% 
          mutate(Groups = cut(Ind, cut.final
                              ,include.lowest = T
                              ,dig.lab = 8
                              ,ordered_result = T)) %>%
          group_by(Groups) %>%
          mutate(Avg = mean(Ind, na.rm = T)) %>%
          ungroup() %>% 
          group_by(Groups, DS, Avg)  %>% 
          summarise(cnts = n()) %>%
          select(Groups, DS, cnts, Avg) %>%  
          spread(DS, cnts) %>% ungroup
        
        
        
      }
      
    }
  }
  
  return(cut.final[!cut.final %in% rng])
  
}



#### Create cutted variable #### 

Cut.numeric <- function(x, cv, range.included = FALSE, Na.act = 'Missing', add.total = FALSE) {
  
  if(!is.numeric(cv)) {stop('The cut vector is not numeric!', call. = FALSE)}
  
  cv.s <- sort(as.vector(na.omit(cv)))
  cv.labs <- range(cv.s)
  
  # return(cv.s)
  
  rng <- range(x, na.rm = T)
  
  if(!range.included) {
    cv.s <- sort(unique(c(cv.s, rng)))  }
  
  
  if(Na.act == 'high') {x[is.na(x)] <- cv.s[1]
  } else if (Na.act == 'low') {x[is.na(x)] <- cv.s[length(cv.s)]}
  
  x.cutted <- cut(x, unique(cv.s), include.lowest = T, dig.lab = 5)
  lvl.list <- levels(x.cutted)
  lvl.list[c(1, length(lvl.list))] <- c(paste0('<= ', cv.labs[1])
                                      , paste0('> ', cv.labs[2]))
  
  levels(x.cutted) <- lvl.list
  
  if(Na.act == 'Missing') {
  
  levels(x.cutted) <- c(levels(x.cutted), 'Missing')
  x.cutted[is.na(x.cutted)] <- 'Missing'
  }
  
  if(add.total) {levels(x.cutted) <- c(levels(x.cutted), 'Total')}
  
  return(x.cutted)
  
}


# 
#   
# ### tests ###
# 
# asd <- c(20,30,40,50)
# 
#  All.data %>%
#   select(Age, Samples, Inferred_DS) %>% 
#   mutate_(nw  = lazyeval::interp(~Cut.numeric(x, cut.v,  Na.act = y), x = as.name('Age'), cut.v = asd, y = 'low' ))
#   
#    
#    as.vector() %>% table()
# 
# 
# Cut.numeric(c(1:100), c(2:10))
# 
# 
# Smart.Bands(All.data, 'AvgbaltoLInc', 'Inferred_DS', cut.v = c(0,1,4.5,8,24))
