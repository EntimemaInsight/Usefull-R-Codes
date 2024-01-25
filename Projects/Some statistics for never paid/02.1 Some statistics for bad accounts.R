##### Score variable ########

ggplot(loans_approved3, aes(x=score, color=as.factor(Bad)
                            , fill=as.factor(Bad))) + 
  geom_histogram(aes(y=..density..), alpha=0.2,position="identity", bins = 20)+
  geom_density(alpha=.2) +
  theme_classic() + 
  xlab(paste0("Score distribution for all clients by Never paid flag"))

summary(loans_approved3$score)

ggplot(loans_approved3 %>% filter(product_id == 1), aes(x=score, color=as.factor(Bad)
                                                        , fill=as.factor(Bad))) + 
  geom_histogram(aes(y=..density..), alpha=0.2,position="identity", bins = 20)+
  geom_density(alpha=.2) +
  theme_classic() + 
  xlab(paste0("Score distribution for first clients by Never paid flag"))

ggplot(loans_approved3 %>% filter(product_id == 5), aes(x=score, color=as.factor(Bad)
                                                        , fill=as.factor(Bad))) + 
  geom_histogram(aes(y=..density..), alpha=0.2,position="identity", bins = 20)+
  geom_density(alpha=.2) +
  theme_classic() + 
  
  xlab(paste0("Score distribution for consequent clients by Never paid flag"))


#First time clients
summary(loans_approved3$score[loans_approved3$Bad == 1 & loans_approved3$product_id == 1])
summary(loans_approved3$score[loans_approved3$Bad == 0 & loans_approved3$product_id == 1])

#Consequent clients
summary(loans_approved3$score[loans_approved3$Bad == 0 & loans_approved3$product_id == 5])
summary(loans_approved3$score[loans_approved3$Bad == 1 & loans_approved3$product_id == 5])
##### DaysSinceReg ########

ggplot(loans_approved3, aes(x=DaysSinceReg, color=as.factor(Bad)
                            , fill=as.factor(Bad))) + 
  geom_histogram(aes(y=..density..), alpha=0.2,position="identity", bins = 20)+
  geom_density(alpha=.2) +
  theme_classic() + 
  
  scale_x_continuous(limits = c(0,10))+
  xlab(paste0("DaysSinceReg distribution for all clients by Never paid flag"))


ggplot(loans_approved3 %>% filter(product_id == 1), aes(x=DaysSinceReg, color=as.factor(Bad)
                                                        , fill=as.factor(Bad))) + 
  geom_histogram(aes(y=..density..), alpha=0.2,position="identity", bins = 20)+
  geom_density(alpha=.2) +
  theme_classic() + 
  scale_x_continuous(limits = c(0,5))+
  xlab(paste0("DaysSinceReg distribution for first clients by Never paid flag"))

ggplot(loans_approved3 %>% filter(product_id == 5), aes(x=DaysSinceReg, color=as.factor(Bad)
                                                        , fill=as.factor(Bad))) + 
  geom_histogram(aes(y=..density..), alpha=0.2,position="identity", bins = 20)+
  geom_density(alpha=.2) +
  theme_classic() + 
  scale_x_continuous(limits = c(0,10))+
  xlab(paste0("DaysSinceReg distribution for consequent clients by Never paid flag"))

#First time clients
summary(loans_approved3$DaysSinceReg[loans_approved3$Bad == 1 & loans_approved3$product_id == 1])
summary(loans_approved3$DaysSinceReg[loans_approved3$Bad == 0 & loans_approved3$product_id == 1])

#Consequent clients
summary(loans_approved3$DaysSinceReg[loans_approved3$Bad == 0 & loans_approved3$product_id == 5])
summary(loans_approved3$DaysSinceReg[loans_approved3$Bad == 1 & loans_approved3$product_id == 5])
##### DaysSinceReg ########

ggplot(loans_approved3, aes(x=DaysSinceReg2, color=as.factor(Bad)
                            , fill=as.factor(Bad))) + 
  geom_histogram(aes(y=..density..), alpha=0.2,position="identity", bins = 20)+
  geom_density(alpha=.2) +
  theme_classic() + 
  
  xlab(paste0("DaysSinceReg distribution for all clients by Never paid flag"))


ggplot(loans_approved3 %>% filter(product_id == 1), aes(x=DaysSinceReg2, color=as.factor(Bad)
                                                        , fill=as.factor(Bad))) + 
  geom_histogram(aes(y=..density..), alpha=0.2,position="identity", bins = 20)+
  geom_density(alpha=.2) +
  theme_classic() + 
  scale_x_continuous(limits = c(0,20))
  xlab(paste0("DaysSinceReg distribution for first clients by Never paid flag"))

ggplot(loans_approved3 %>% filter(product_id == 5), aes(x=DaysSinceReg2, color=as.factor(Bad)
                                                        , fill=as.factor(Bad))) + 
  geom_histogram(aes(y=..density..), alpha=0.2,position="identity", bins = 20)+
  geom_density(alpha=.2) +
  theme_classic() + 
  
  xlab(paste0("DaysSinceReg distribution for consequent clients by Never paid flag"))

#First time clients
summary(loans_approved3$DaysSinceReg2[loans_approved3$Bad == 1 & loans_approved3$product_id == 1])
summary(loans_approved3$DaysSinceReg2[loans_approved3$Bad == 0 & loans_approved3$product_id == 1])
# hist(loans_approved3$DaysSinceReg2[loans_approved3$Bad == 0 & loans_approved3$product_id == 1])
# hist(loans_approved3$DaysSinceReg2[loans_approved3$Bad == 1 & loans_approved3$product_id == 1])
#Consequent clients
summary(loans_approved3$DaysSinceReg2[loans_approved3$Bad == 0 & loans_approved3$product_id == 5])
summary(loans_approved3$DaysSinceReg2[loans_approved3$Bad == 1 & loans_approved3$product_id == 5])
# hist(loans_approved3$DaysSinceReg2[loans_approved3$Bad == 0 & loans_approved3$product_id == 5])
# hist(loans_approved3$DaysSinceReg2[loans_approved3$Bad == 1 & loans_approved3$product_id == 5])
# #####"Probability of default variable #####

ggplot(loans_approved3 %>% filter(product_id == 1), aes(x=probability_default, color=as.factor(Bad)
                                                        , fill=as.factor(Bad))) + 
  geom_histogram(aes(y=..density..), alpha=0.2,position="identity", bins = 20)+
  geom_density(alpha=.2) +
  theme_classic() + 
  xlab(paste0("Probability of default for first time clients"))

ggplot(loans_approved3 %>% filter(product_id == 5), aes(x=probability_default, color=as.factor(Bad)
                                                        , fill=as.factor(Bad))) + 
  geom_histogram(aes(y=..density..), alpha=0.2,position="identity", bins = 20)+
  geom_density(alpha=.2) +
  theme_classic() + 
  xlab(paste0("Probability of default for consequent time clients"))

#First time clients
# Never paid
summary(loans_approved3$probability_default[loans_approved3$Bad == 1 & loans_approved3$product_id == 1])
# paid someth
summary(loans_approved3$probability_default[loans_approved3$Bad == 0 & loans_approved3$product_id == 1])

#Consequent clients
# Paid someth
summary(loans_approved3$probability_default[loans_approved3$Bad == 0 & loans_approved3$product_id == 5])
# Never paid
summary(loans_approved3$probability_default[loans_approved3$Bad == 1 & loans_approved3$product_id == 5])


#### Cpnsequent cleints only ####
# Variables :
# Loan amount , SUm of loan amounts, number of closed credits

ggplot(loans_approved3 %>% filter(product_id == 5), aes(x=Loan_amount, color=as.factor(Bad)
                                                        , fill=as.factor(Bad))) + 
  geom_histogram(aes(y=..density..), alpha=0.2,position="identity", bins = 20)+
  geom_density(alpha=.2) +
  theme_classic() + 
  xlab(paste0("Loan_amount distribution by Bad paid flag for consequent clients"))


ggplot(loans_approved3 %>% filter(product_id == 5), aes(x=SumOfCredits, color=as.factor(Bad)
                                                        , fill=as.factor(Bad))) + 
  geom_histogram(aes(y=..density..), alpha=0.2,position="identity", bins = 20)+
  geom_density(alpha=.2) +
  theme_classic() + 
  xlab(paste0("SumOfCredits distribution by Never paid flag for consequent clients"))


ggplot(loans_approved3 %>% filter(product_id == 5), aes(x=Closed_credtits_count, color=as.factor(Bad)
                                                        , fill=as.factor(Bad))) + 
  geom_histogram(aes(y=..density..), alpha=0.2,position="identity", bins = 20)+
  geom_density(alpha=.2) +
  theme_classic() + 
  xlab(paste0("Closed_credtits_count distribution by Never paid flag for consequent clients"))

####### Categorical stuff
test <- CrossTable(loans_approved3$Bad, loans_approved3$risk_grade)
test[1]
test[3]


test <- CrossTable(loans_approved3$Bad, loans_approved3$reason_codes_1)
test[1]
test[3]


test <- CrossTable(loans_approved3$Bad, loans_approved3$reason_codes_2)
test[1]
test[3]

test <- CrossTable(loans_approved3$Bad, loans_approved3$reason_codes_3)
test[1]
test[3]


test <- loans_approved3 %>% 
  filter(product_id == 5) %>%
  group_by(reason_codes_1, risk_grade) %>% 
  summarise(AvgFraud = round(mean(Bad, na.rm = T), 2)) %>% 
  spread(key = risk_grade, value = AvgFraud)

test2 <- loans_approved3 %>% 
  filter(product_id == 5) %>%
  group_by(reason_codes_1, risk_grade) %>% 
  summarise(Total = n()) %>% 
  spread(key = risk_grade, value = Total)

test <- loans_approved3 %>% 
  filter(product_id == 1) %>%
  group_by(reason_codes_1, risk_grade) %>% 
  summarise(AvgFraud = round(mean(Bad, na.rm = T), 2)) %>% 
  spread(key = risk_grade, value = AvgFraud)

test2 <- loans_approved3 %>% 
  filter(product_id == 1) %>%
  group_by(reason_codes_1, risk_grade) %>% 
  summarise(Total = n()) %>% 
  spread(key = risk_grade, value = Total)


CrossTable(loans_approved3$Bad, loans_approved3$product_id)

CrossTable(loans_approved3$Bad, loans_approved3$Missing_score)

CrossTable(loans_approved3$Bad, loans_approved3$Facebook_missing)

CrossTable(loans_approved3$Bad, loans_approved3$module_sms_id)

CrossTable(loans_approved3$Bad, loans_approved3$Mail)
