
#### 1.1 Create table with missings for each variable
vec <- vector(length= ncol(ANAF))
vec_missing <- vector(length= ncol(ANAF))
vec_missing_p <- vector(length= ncol(ANAF))

names(vec) <- names(ANAF)
names(vec_missing) <- names(ANAF)
names(vec_missing_p) <- names(ANAF)

for (i in seq(1:ncol(ANAF))){
  
  type <- typeof(ANAF[,i])
  vec[i] <- type
  
  n_missings<- sum(is.na(ANAF[,i]))
  vec_missing[i] <- n_missings
  
  n_missings_p <-  n_missings / nrow(ANAF)
  vec_missing_p[i] <-n_missings_p
  
  i+1
}

combine <- as.data.frame(rbind(vec, vec_missing, vec_missing_p)) 

### Export the data ###
# write.csv(combine, "combine.csv", na= "")
# write.csv(table_content_D, "table_content_D.csv", na = "")

#Make it more readable#
combine <-as.data.frame(t(as.data.frame(rbind(vec, vec_missing, vec_missing_p))))  