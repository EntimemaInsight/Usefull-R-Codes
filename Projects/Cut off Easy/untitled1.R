qwe <- read.csv2(file = "Z:\\Analysis\\3.Data\\2020.01\\Easy BG\\E004_Credit_Progress_BG_20200201_v1.csv", 
                 stringsAsFactors = F, encoding = "UTF-8")




Harold_Easy_BG <- read.csv2(file = "Z:\\Analysis\\3.Data\\2020.01\\Easy BG\\E006_Harold_BG_20200201_v1.csv", 
                            stringsAsFactors = F, encoding = "UTF-8")

test<-Harold_Easy_BG[, c("X.U.FEFF.KID","MaxDaysPayedLater","CreditFirstPayment","CreditLastPayment","CompleteDate","Scoring","Sum","TotalPayed")]

test$Sum<-as.numeric(test$Sum)
test$TotalPayed<-as.numeric(test$TotalPayed)

test<-test%>%
  filter(test$Scoring!="NULL",test$CreditFirstPayment>as.Date("2018-01-01")& test$CreditLastPayment<as.Date("2019-02-01"))%>%
  mutate(DefaultPayment=as.factor(ifelse(TotalPayed/Sum>1.2,0,1)),
         DefaultDays=as.factor(ifelse(MaxDaysPayedLater>90,1,0)))


test<-na.omit(test)
