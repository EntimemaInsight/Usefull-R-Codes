library(gtools)

#### 01 Macro for Binning, WOE and IV ####

## start ##
woe_character_macro = defmacro(Sample_application, Sample_application_ext, VarName, 
                               minBinSize_perc, WoE_table, IV_table, expr={

  minBinSize1 <- minBinSize_perc
 
  Sample_application[,VarName] <- as.character(Sample_application[,VarName])
  Sample_application_ext[,VarName] <- as.character(Sample_application[,VarName])
  
  colnames(Sample_application)[colnames(Sample_application) == VarName]  <- "Variable"
  colnames(Sample_application_ext)[colnames(Sample_application_ext) == VarName]  <- "Variable"
  Sample_application_ext <- Sample_application_ext%>%
    mutate(Variable = case_when(is.na(Variable) ~ "Missing", 
                                Variable == 'Неизвестно' | Variable == 'неизвестно' |
                                  Variable == 'неизвестна' |  Variable == 'Неизвестна' |
                                  Variable == 'неизвестен' |  Variable == 'Неизвестен'  ~ "Missing",
                                TRUE ~ Variable))
  Variable_sample3 <- Sample_application%>% 
    dplyr::select(CreditCode, Variable, c_Target) %>% 
    mutate(Variable = case_when(is.na(Variable) ~ "Missing", 
                                Variable == 'Неизвестно' | Variable == 'неизвестно' |
                                  Variable == 'неизвестна' |  Variable == 'Неизвестна' |
                                  Variable == 'неизвестен' |  Variable == 'Неизвестен'  ~ "Missing",
                                TRUE ~ Variable), 
           Obs = n(), 
           minBinSize = minBinSize1*Obs, 
           CountTargets = sum(c_Target), 
           CountNoTargets = Obs - sum(c_Target))%>%
    
    group_by(Variable) %>% 
    summarise(Total = n()
              ,CountTargets = mean(CountTargets, na.rm = T)
              ,CountNoTargets = mean(CountNoTargets, na.rm = T)
              ,CountTargetsGroup = sum(c_Target)
              ,CountNoTargetsGroup = n() - sum(c_Target)
              ,c_Target = round(mean(c_Target, na.rm = T),2) 
              ,Population = round(Total/mean(Obs),2)
              ,minBinSize = round(mean(minBinSize, na.rm = T),0)
    )%>% 
    arrange(c_Target) %>% mutate(Bin = 0
                                 ,CumSum = 0
                                 ,Rest = 1 - cumsum(Population))
  
  
  names <- as.matrix(Variable_sample3 [,1])
  
  Variable_sample4 <- data.matrix(Variable_sample3)[,-1]
  rownames(Variable_sample4) <-names
  
  Variable_sample <- Variable_sample4
  for (i in seq_along((1:nrow(Variable_sample)))){
    if (i == 1){
      Variable_sample [i, "Bin" ] = 1
      Variable_sample[i, "CumSum"] = Variable_sample[i, "Population"] 
      # Variable_sample[i, "Rest"] = 1 - Variable_sample[i, "CumSum"] 
    } else if (i != 1 & 1 - Variable_sample[i-1, "CumSum"] >= minBinSize1){
    
      Variable_sample [i, "Bin"] = as.numeric(Variable_sample [i-1, "Bin"]) + 1
      Variable_sample[i, "CumSum"] = Variable_sample[i - 1, "CumSum"] + Variable_sample[i, "Population"] 
      # Variable_sample[i, "Rest"] = 1 - Variable_sample[i, "CumSum"]
    } 
     else if(i != 1 & 1 - Variable_sample[i-1, "CumSum"] < minBinSize1){
      Variable_sample [i, "Bin"] = as.numeric(Variable_sample [i-1, "Bin"])
      Variable_sample[i, "CumSum"] = Variable_sample[i, "Population"] + Variable_sample[i-1, "CumSum"]
    }
    
  }
  
  Variable_sample1 <- as.data.frame(Variable_sample) %>% 
    mutate(Variable = as.vector(names)) %>%  dplyr::select (Variable, everything()) %>%
    group_by(Bin) %>% 
    mutate(CountTargetsBin = sum(CountTargetsGroup) 
           , CountNoTargetsBin = sum(CountNoTargetsGroup)
           , WoE_Bin = log((CountNoTargetsBin / mean(CountNoTargets))/ (CountTargetsBin/mean(CountTargets)))
           , IV_i = round(((CountNoTargetsBin / mean(CountNoTargets)) - ((CountTargetsBin/mean(CountTargets))))*WoE_Bin,6)
    )%>% ungroup()
  
  Variable_IV <- Variable_sample1 %>% group_by(Bin) %>% summarise(IV = mean(IV_i)) %>% summarise(IV = sum(IV))
  
  #### Append the woe and the bin to the dataset ####  

  Sample_application_ext <- Sample_application_ext %>% 
                                    left_join(Variable_sample1 %>%  
                                    dplyr::select(Variable, Bin, WoE_Bin), by = "Variable")
  
  colnames(Sample_application_ext)[colnames(Sample_application_ext) == "Variable"]  <- VarName
  
  ## Name them appropriately ###
  colnames(Sample_application_ext)[colnames(Sample_application_ext) == 'Bin']  <- paste0(VarName, "_bin")
  colnames(Sample_application_ext)[colnames(Sample_application_ext) == 'WoE_Bin']  <- paste0(VarName, "_bin_woe")
  
  ### Append the WOE and the IV to the respective table ###
  ##WoE
  vec <- as.data.frame(list(VarName, t(sort(unique(Variable_sample1$WoE_Bin)))))
  colnames(vec)[1] <- "VariableName"
  WoE_table <-  merge(WoE_table,vec,intersect(names(vec), names(WoE_table)), all=TRUE)
  
  ##IV
  iv_vec<- as.data.frame(list(VarName, Variable_IV))
  colnames(iv_vec)[1] <- "VariableName"
  IV_table <-  merge(IV_table,iv_vec,intersect(names(iv_vec), names(IV_table)), all=TRUE)
  
  #### Rename back the column ####
  colnames(Sample_application)[colnames(Sample_application) == "Variable"]  <- VarName
  
})