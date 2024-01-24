DateDiffMonths <- function(d1, d2) {
  
  
  monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
                         lt$year*12 + lt$mon } 
  
  # compute a month difference as a difference between two monnb's
  
  mondf <- monnb(d2) - monnb(d1)
  return(mondf)
  
}

