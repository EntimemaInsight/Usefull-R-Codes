# require(lubridate)
# require(dplyr)
# require(tidyr)
# 
# 
# myDates = seq.Date(as_date('2015-01-01'), as_date('2017-09-01'), by = 'month' )
# myDates <- myDates-days(1)
# 
# 
# monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
#                        lt$year*12 + lt$mon } 
# 
# mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
# 
# mondf(as.Date("2017-08-31"), Sys.Date())
# 
# #util f
# set.seed(12113)
# MyF <- function(x) {exp(sort(seq(-7, -3, length.out = 33), decreasing = TRUE)[x+1])}
# 
# ## FinalData
# RRDF <- expand.grid(Validitydate = myDates, ValueDate =myDates) %>%
#   filter(Validitydate <= ValueDate) %>% 
#   mutate( datediff = mondf(Validitydate, ValueDate)
#         , chec = 1
#          ,RR = MyF(datediff) + rnorm(chec, 0.01, 0.01)
#          ) %>% 
#   group_by(Validitydate) %>% 
#   mutate(CumRR = order_by(ValueDate, cumsum(RR))) %>%
#   # ggplot(aes(x = datediff, y = CumRR, col = as.factor(Validitydate))) + geom_line()
#   
#   select(-ValueDate, - RR, -chec) %>% 
#   spread(datediff, CumRR)
# 
# 
# 
# RRDF %>% View()
# # rrbackup <-RRDF 
# 
# # RRDF<- rrbackup
# 
# RRDF <- as.data.frame(RRDF)

InterpolateRecoveryRates <- function(df) {
                                          RRDF <- as.data.frame(df)
                                          RRDF_Clone <- RRDF 
                                          
                                          for (i in 4:nrow(RRDF)) {
                                            
                                              for (j in 3:ncol(RRDF)) {
                                                                      
                                                                      if (!is.na(RRDF[i,j])) {
                                                                        
                                                                        RRDF_Clone[i,j] <- 0
                                                                      
                                                                        } else {
                                                                          
                                                                          xprev <- RRDF[i,j-1]
                                                                          x <- mean(RRDF[c(i-c(1:3)),j], na.rm =T)
                                                                          y <- mean(RRDF[i-c(1:3),j-1], na.rm =T)
                                                                          RRDF[i,j] <- (x/y)*xprev
                                                                          
                                                                          RRDF_Clone[i,j] <- 1
                                                                        }
                                              }
                                          }
                                          return(list(RRDF, RRDF_Clone))
}

# 
# x <- InterpolateRecoveryRates(df = RRDF)
# 
# View(x[[1]])
# 
# View(RRDF)
