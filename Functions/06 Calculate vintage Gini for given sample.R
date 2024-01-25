
Calculating_Gini <- function(data, score, good_Bad4_var)
{
  
  Gini_relevant<- rcorr.cens(data[[score]] , data[[good_Bad4_var]] )[2]
  print(paste0("Gini for this sample: ", round(Gini_relevant, 4)))
  # print(confusionMatrix(data[[score]], data[[good_Bad4_var]]))
  # plot.roc(data$c_Target, data$Scoring)
  return (Gini_relevant)
  
}

Creating_Gini_table1 <- function (data = sample2, date_variable = "Short_Date", score2, good_Bad4_var2 ) {
  
  help_table <- data %>%
    group_by(Short_Date)%>%
    summarise(N_accounts = n()) 
  
  period <-help_table[[date_variable]]
  
  Gini_vector <- vector(mode = "integer", length = 0)
  
  for (i in 1:length(period)){
    
    subsample <- subset(data, data[[date_variable]] == period[i])
    print(paste("Period:", period[i]))
    Gini <- Calculating_Gini(subsample, score = score2, good_Bad4_var = good_Bad4_var2)
    Gini_vector[i] <- Gini
  }
  
  x <- period
  y<- Gini_vector
  
  x_name <- "Period"
  y_name <- "Gini"
  
  Gini_table <- data.frame(x,y)
  names(Gini_table) <- c(x_name,y_name)
  Gini_table <- tbl_df(Gini_table)
  return(Gini_table)
}

Creating_Gini_table1(MySample_score, date_variable = "Short_Date", score2 = "PD_old", good_Bad4_var2 = "Bad4" )