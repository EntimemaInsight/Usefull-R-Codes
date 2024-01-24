FormatPercentage <- function(x, y = '%', dec.dig = 2, multiplier = 100) {
  
  if(is.nan(x)) {x <- 0}
  if(is.na(x)) {x <- 0}
  
  paste(
    formatC(x*multiplier, digits = dec.dig, format ='f' )
  ,y
  , sep = ''
  )
  
  
}
