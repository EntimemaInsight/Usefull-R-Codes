
#### 00 Load sme fucntions in advance ####
Calculating_Gini <- function(data, score, good_Bad4_var)
{
  
  Gini_relevant<- rcorr.cens(data[[score]] , data[[good_Bad4_var]] )[2]
  print(paste0("Gini for this sample: ", round(Gini_relevant, 4)))
  return (Gini_relevant)
  
}

Calculating_Gini(MySample_score,score = "PD_old", good_Bad4_var = "Bad4")