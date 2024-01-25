Sample_for_model <- Sample_for_model[,model_vars]

set.seed(123)

flds <- createFolds(Sample_for_model$c_Target, k = 5, list = TRUE, returnTrain = FALSE)

all_models <- NULL
all_auc_test <- NULL
all_auc_train <- NULL
all_class_train <- NULL 
all_class_test <- NULL

for(i in seq(1:5)){
  print(paste0("Results from ", i ,"iterarion"))
  train <- Sample_for_model[-flds[[i]], ]#Set the training set
  test <- Sample_for_model[flds[[i]], ]
  
  # 1. Run logistic regression
  model <- glm(c_Target ~.,family=binomial,data=train) 
  print("Results summary")
  print(summary(model))
  
  #2 Run multicolinearity test
  all_models[i]<- model
  print("Multicolinearity test")
  print(vif(model))
  
  # ANOVA test
  print("Run ANOVA test")
  print(anova(model, test="Chisq"))
  
  # Missclasification errors
  fitted_results <- predict(model,test,type='response')
  fitted_results2 <- ifelse(fitted_results > 0.5,1,0)
  misClasificError <- mean(fitted_results2 != test$c_Target)
  print(paste('Accuracy test',1-misClasificError))
  all_class_test[i] <- 1-misClasificError
  
  #Plot score / prediction distribution 
  all <- data.frame(fitted_results, fitted_results2)
  colnames(all) <- c("Score", "c_Target")
  
  mu <- ddply(all, "c_Target", summarise, grp.mean=mean(Score))
  # Histogram with density plot
  p1 <- ggplot(all, aes(x=Score)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666")+
    theme_classic() + 
    xlab(paste0("Sample ", i, " Score"))
  plot(p1)
  # Color by groups
  p2 <- ggplot(all, aes(x=Score, color=as.factor(c_Target), fill=as.factor(c_Target))) + 
    geom_histogram(aes(y=..density..), alpha=0.5, 
                   position="identity")+
    geom_density(alpha=.2) +
    geom_vline(data=mu, aes(xintercept=grp.mean, color=as.factor(c_Target)),
               linetype="dashed")+
    theme_classic()+ 
    xlab(paste0("Sample ", i, " Score"))
  
  plot(p2)
  
  fitted_results <- predict(model,train,type='response')
  fitted_results2 <- ifelse(fitted_results > 0.5,1,0)
  misClasificError <- mean(fitted_results2 != train$c_Target)
  print(paste('Accuracy train',1-misClasificError))
  all_class_train[i] <- 1-misClasificError
  
  # 6. Calculate model performance
  # Test sample
  print("Results for test sample")
  p <- predict(model, test, type="response")
  pr <- prediction(p, test$c_Target)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  plot(prf)
  
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  
  results <- data.frame(cbind(p, test$c_Target))
  Calculating_Gini(results,score = "p", good_Bad4_var = "V2")
  print(paste0("The Gini is ", round(2*auc - 1,2)))
  
  all_auc_test[i] <- auc
  
  # Train sample
  print("Results for train sample")
  p <- predict(model, train, type="response")
  pr <- prediction(p, train$c_Target)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  plot(prf)
  
  
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  print(paste0("The AUC value is ", round(auc,2)))
  
  results <- data.frame(cbind(p, train$c_Target))
  Calculating_Gini(results,score = "p", good_Bad4_var = "V2")
  print(paste0("The Gini is ", round(2*auc - 1,2)))
  
  # auc <- performance(pr, measure = "auc")
  #  auc <- auc@y.values[[1]]
  #  print(auc)
  all_auc_train[i] <- auc
  
}