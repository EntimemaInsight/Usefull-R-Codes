library(gtools)
woe_numeric_macro = defmacro(Sample_application_n, Sample_application_ext_n, VarName, minBinSize_perc, IV_table, WoE_table,
                             expr={
                               # Set the min size of the Bin 
                               minBinSize1 <- minBinSize_perc
                               
                               # minBinSize1 <- 0.10
                               # VarName <- "Mean_gross_salary"
                               # Sample_application_n <- Sample_application_numeric
                               # Sample_application_ext_n <- Sample_application_ext
                               
                               #### Rename the variable
                               colnames(Sample_application_n)[colnames(Sample_application_n) == VarName]  <- "Variable"
                               colnames(Sample_application_ext_n)[colnames(Sample_application_ext_n) == VarName]  <- "Variable"
                               
                               #### 02 Create the necessary data ####
                               Variable_sample <- Sample_application_n%>% 
                                 dplyr::select(CreditCode, Variable, c_Target) %>% 
                                 mutate(
                                   Obs = n(), 
                                   minBinSize = minBinSize1*Obs, 
                                   CountTargets = sum(c_Target), 
                                   CountNoTargets = Obs - sum(c_Target))%>%
                                 ### arange by the variable asc - missings will we at the bottom of the table ## 
                                 arrange(Variable)%>%
                                 ### Initial Bin ###
                                 mutate(Bin = case_when(is.na(Variable) ~ 0, TRUE ~ 1),
                                        Seq = seq_along(1:nrow(Sample_application_n)), 
                                        Used = 1,
                                        Percentile = round(Seq/Obs,2))
                               
                               ### Select only non missing data ###
                               Variable_sample1 <- data.matrix(Variable_sample %>% filter(!is.na(Variable))) 
                               count_obs <- nrow(Variable_sample1)
                               threshold <- 0.95*count_obs
                               #### 03 Create the bins ####
                               for (i in seq_along(1:nrow(Variable_sample1))){
                                 if(i==1){
                                   Variable_sample1[i, "Bin"]= Variable_sample1 [i, "Bin"] 
                                   
                                 }   else 
                                   
                                   if (Variable_sample1[i-1, "Used"]<=round(minBinSize1*nrow(Variable_sample),0) | 
                                       Variable_sample1[i, "Variable"] == Variable_sample1[i-1, "Variable"] |
                                       (Variable_sample1[i, "Seq"] >= threshold)
                                   ){
                                     
                                     Variable_sample1[i, "Used"] = Variable_sample1[i-1, "Used"]+1
                                     Variable_sample1[i, "Bin"] = Variable_sample1 [i-1, "Bin"]
                                   } else {
                                     
                                     Variable_sample1[i, "Bin"] = Variable_sample1 [i-1, "Bin"]+1 
                                     Variable_sample1[i, "Used"] = 0
                                   }
                               }
                               
                               #### 04 Create sepatate bin for missings #####
                               Variable_sample_missing <- Variable_sample%>% 
                                 filter(is.na(Variable)) %>% 
                                 
                                 group_by(Bin) %>% 
                                 summarise(BinSize = n()
                                           ,Min_boundary = NA
                                           ,Max_boundary = NA
                                           ,CountTargets = mean(CountTargets, na.rm = T)
                                           ,CountNoTargets = mean(CountNoTargets, na.rm = T)
                                           ,CountTargetsGroup = sum(c_Target)
                                           ,CountNoTargetsGroup = n() - sum(c_Target)
                                           ,c_Target = round(mean(c_Target, na.rm = T),2) 
                                           ,Population = round(BinSize/mean(Obs),2))
                                           
                               
                               Variable_sample2 <- as.data.frame(Variable_sample1) %>% 
                                 #mutate(Bin1 = Bin) %>%
                                 #mutate(Bin = Bin + 1)%>%
                                 group_by(Bin) %>% 
                                 summarise(BinSize = n()
                                           ,Min_boundary = min(Variable)
                                           ,Max_boundary = max(Variable)
                                           ,CountTargets = mean(CountTargets, na.rm = T)
                                           ,CountNoTargets = mean(CountNoTargets, na.rm = T)
                                           ,CountTargetsGroup = sum(c_Target)
                                           ,CountNoTargetsGroup = n() - sum(c_Target)
                                           ,c_Target = round(mean(c_Target, na.rm = T),2) 
                                           ,Population = round(BinSize/mean(Obs),2)) %>% 
                                 rbind(Variable_sample_missing) 
                               
                               
                               Variable_sample3 <- Variable_sample2 %>% 
                                 mutate(PercNoTargetsBin = (CountNoTargetsGroup / CountNoTargets)
                                        , PercTargetsBin = (CountTargetsGroup / CountTargets)
                                        , WoE_Bin = log(PercNoTargetsBin/PercTargetsBin)
                                        , IV_i = round(((PercNoTargetsBin - PercTargetsBin)*WoE_Bin),6)) %>% 
                                 mutate(Sum_IV = case_when(Bin == 0 ~ 0
                                                           , TRUE ~ 1))
                               
                               
                               
                               #### Credits with the respective Bin and WoE ####
                               
                               Variable_sample1_1 <- as.data.frame(Variable_sample1) %>% 
                                 dplyr::select(CreditCode, Bin) %>% 
                                 left_join(Variable_sample3 %>%  dplyr::select(Bin, WoE_Bin), by = "Bin")
                               
                               ###### Add Bin and woe to the master table ####
                               Sample_application_ext_n <- Sample_application_ext_n %>% left_join(Variable_sample1_1, by = "CreditCode")
                               
                               ## Name them appropriately ###
                               colnames(Sample_application_ext_n)[colnames(Sample_application_ext_n) == 'Bin']  <- paste0(VarName, "_bin")
                               colnames(Sample_application_ext_n)[colnames(Sample_application_ext_n) == 'WoE_Bin']  <- paste0(VarName, "_bin_woe")  
                               
                               #### Rename the Variable with its original name
                               colnames(Sample_application_ext_n)[colnames(Sample_application_ext_n) == "Variable"]  <- VarName
                               
                               #### Calculate the IV ###
                               
                               Variable_IV <- Variable_sample3 %>% filter(Sum_IV==1) %>% 
                                 summarise(IV = sum(IV_i))
                               ### Add Woe to the big table ####
                               vec <- as.data.frame(list(VarName, t((unique(Variable_sample3$WoE_Bin)))))
                               colnames(vec)[1] <- "VariableName"
                               WoE_table <-  merge(WoE_table,vec,intersect(names(vec), names(WoE_table)), all=TRUE)
                               
                               ##add IV
                               iv_vec<- as.data.frame(list(VarName, Variable_IV))
                               colnames(iv_vec)[1] <- "VariableName"
                               IV_table <-  merge(IV_table,iv_vec,intersect(names(iv_vec), names(IV_table)), all=TRUE)
                               
                               #### Rename back the column ####
                               colnames(Sample_application_n)[colnames(Sample_application_n) == "Variable"]  <- VarName
                               
                             })