replace_nan_with_na_in_df <-
function(x)
{
  for (i in 1:ncol(x)){
    x[,i][is.nan(x[,i])] = NA
  }
  return(x)
}
