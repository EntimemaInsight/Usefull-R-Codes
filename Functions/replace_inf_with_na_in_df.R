replace_inf_with_na_in_df <-
function(x)
{
  for (i in 1:ncol(x)){
    x[,i][is.infinite(x[,i])] = NA
  }
  return(x)
}
