KS_max_sep <- function(df, ind, dep, p.truncate=c(0.005, 0.995)) {
  
  require(dplyr)
  require(ggplot2)
  require(scales)
  
  lw.b <- quantile(df[,ind], p.truncate[1], na.rm = T )
  up.b <- quantile(df[,ind], ifelse(is.na(p.truncate[2]), 0, p.truncate[2]), na.rm = T )
  
  My.data <- df %>%select_(Ind = ind, Dep = dep) %>% 
                filter(Ind >= lw.b 
                       ,Ind <= up.b)
  
  
  en.cdf.1 <- ecdf( unlist(df %>% 
                             select_(Ind = ind, DS = dep) %>%
                             filter(DS == unique(df[,dep])[1])%>%
                             select(-DS)))


  en.cdf.2 <- ecdf( unlist(df %>% 
                             select_(Ind = ind, DS = dep) %>%
                             filter(DS == unique(df[,dep])[2])%>%
                             select(-DS)))
  
  
  labl1<-round(max(abs(en.cdf.1(df[,ind]) - en.cdf.2(df[,ind]))*100, na.rm = T),1)

  maxdif <- which.max(abs(en.cdf.1(df[,ind]) - en.cdf.2(df[,ind])))
  maxdif.x.coord <- (df[,ind])[maxdif]
  

  
  ### PsEUDO Data for graph representativness ### 
  
  
  p.en.cdf.1 <- ecdf(My.data[My.data$Dep == unique(My.data$Dep)[1],1])
  p.en.cdf.2 <- ecdf(My.data[My.data$Dep == unique(My.data$Dep)[2],1])
  
  p.maxdif <- which.max(abs(p.en.cdf.1(My.data[,1]) - p.en.cdf.2(My.data[,1])))
  
  p.maxdif.x.coord <- My.data[p.maxdif,1]
  
  p.maxdif.coord <- c(p.en.cdf.1(My.data[p.maxdif,1]),p.en.cdf.2(My.data[p.maxdif,1]))
  
  
#   return(paste0(' Max separation of'
#                 ,round(abs(p.en.cdf.1(My.data[,1]) - p.en.cdf.2(My.data[,1]))*100,1),'%'
#                 ,'(',round(abs(en.cdf.1(ind) - en.cdf.2(ind))*100,1),'%'
#                 
#   ))
  
  
  # return(ind[maxdif])
  
  My.data %>% 
    ggplot(aes(x = Ind, col = Dep)) + 
    stat_ecdf() +
    geom_segment(aes(x = p.maxdif.x.coord ,xend = p.maxdif.x.coord , y = p.maxdif.coord[1], yend =p.maxdif.coord[2])
                 , col = 'black', linetype = 'dashed') +
    scale_color_manual(values = c('#3399FF','#FF1111')) +
    ggtitle('K-S Max Separation') +
    scale_y_continuous(labels = percent) + 
    ylab('Cumulative Distribution (%) ') +
    xlab(ind)  + 
    annotate('text', x = p.maxdif.x.coord, y = p.maxdif.coord[1]
             
             
             , label= paste0('   Max separation of '
                             ,round(max(abs(p.en.cdf.1(My.data[,1]) - p.en.cdf.2(My.data[,1])))*100,1),'% '
                             ,'(',labl1,'%) \n  at ',ind, ' of '
                             ,round(p.maxdif.x.coord,0), ' (', round(maxdif.x.coord,0) ,')'
                             
                             ), hjust = 0 , vjust = 1, size = 3.5) +
    
    theme(legend.position = 'bottom'
        , legend.title = element_blank())

  
}

