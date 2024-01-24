rm(list = ls())
options(scipen = 999)
##### Start Block: Load Libraries #####
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}

if(!require(future.apply)){
  install.packages("future.apply")
  library(future.apply)
}
##### End Block: Load Libraries #####

##### Start Block: Load Functions #####
source(".\\Functions\\F_Elapsed_Months.R")
##### End Block: Load Functions #####

##### Start Block: Input Parameters #####
number_of_cores <- 12
Reporting_Date <- as.Date("2019-09-01")
##### End Block: Input Parameters #####

##### Start Block: Load Input data #####

##### End Block: Load Input data #####

Day <- Sys.Date() - 12
#DECLARE_DATE <- paste0("'", Day, "'")
DECLARE_DATE <- "'2019-11-10'"

#### 01 Connect to the database ####
myc <- DBI::dbConnect(odbc::odbc()
                      , driver= "SQL Server"
                      , server = 'hidalgo'
                      , database = "BISmart")

#### R-duck scoring is scoring type 1001 ###
#extract all loans that have gone through R-duck scoring
all_loans7 <- DBI::dbGetQuery(myc, 'SELECT CodeContract, ProductName, Scoring, ScoringLimit
              	                       , Sum, Interest, ScoringModelDecision, CreditBeginDate, ClientSK
                                       , CompleteDate, DateRefused, DateDenied, DateApproved
                                       , RefinanceType, PreviousCreditsCount
                                       # 
                                       FROM  [dwh].[DimCredit] a LEFT JOIN [dwh].[DimProduct] b
                                       ON a.ProductSK = b.ProductSK 
                                       WHERE ScoringType = 1001 AND latest = 1
                                       ORDER BY  ProductName,Scoring, CreditBeginDate')


### Scoring for first credit is type 101 ###
#extract all loans that wnet through scoring 101
all_loans101 <- DBI::dbGetQuery(myc,'SELECT CodeContract, ProductName, Scoring, ScoringLimit
                                            , Sum, Interest, ScoringModelDecision, CreditBeginDate
                                            , CompleteDate, DateRefused, DateDenied, DateApproved
                                            , RefinanceType, PreviousCreditsCount
                                            
                                            FROM  [dwh].[DimCredit] a LEFT JOIN [dwh].[DimProduct] b
                                            ON a.ProductSK = b.ProductSK 
                                            WHERE ScoringType = 101 AND latest = 1
                                            ORDER BY  ProductName,Scoring, CreditBeginDate')

### Get credit products for all credits that went through R-duck scoring
Scoring7products <- DBI::dbGetQuery(myc, 'SELECT ProductName, count(*) AS number_of_credits
                                              FROM  [dwh].[DimCredit] a LEFT JOIN [dwh].[DimProduct] b
                                              ON a.ProductSK = b.ProductSK 
                                              WHERE ScoringType = 1001 AND latest = 1
                                              GROUP BY ProductName')

### Get credit products for all credits that went through scoring 101
Scoring101products <- DBI::dbGetQuery(myc, ' SELECT ProductName, count(*) AS number_of_credits
                                                 FROM  [dwh].[DimCredit] a LEFT JOIN [dwh].[DimProduct] b
                                                 ON a.ProductSK = b.ProductSK 
                                                 WHERE ScoringType = 101 AND latest = 1
                                                 GROUP BY ProductName')

### Extract information about R-duck scoring classes
ScoringClassesStats7 <- DBI::dbGetQuery(myc, ' SELECT ProductName, ScoringLimit
                                              , COUNT(*) AS number_of_credits
                                              , ROUND(AVG(Sum),0) AS avg_sum
                                              , ROUND(AVG(Interest),0) AS avg_interest
                                              FROM  [dwh].[DimCredit] a LEFT JOIN [dwh].[DimProduct] b
                                              ON a.ProductSK = b.ProductSK 
                                              WHERE ScoringType = 1001 AND latest = 1
                                              GROUP BY ScoringLimit, ProductName
                                              ORDER BY ProductName, ScoringLimit')

### Extract information about scoring classes 101
ScoringClassesStats101 <- DBI::dbGetQuery(myc, '  SELECT ProductName, ScoringLimit
                                          , COUNT(*) AS number_of_credits
                                          , ROUND(AVG(Sum),0) AS avg_sum
                                          , ROUND(AVG(Interest),0) AS avg_interest
                                          FROM  [dwh].[DimCredit] a LEFT JOIN [dwh].[DimProduct] b
                                          ON a.ProductSK = b.ProductSK 
                                          WHERE ScoringType = 101 AND latest = 1 AND ScoringLimit IS NOT NULL
                                          GROUP BY ScoringLimit, ProductName
                                          ORDER BY ProductName, ScoringLimit')

```


```{r Load CredProgr from BI, echo=FALSE, message=FALSE, warning=FALSE}
### Eactract credit progress information for credits which ere assessed by either R-duck scoring or 101 scoring

CreditProgress_Rduck_101 <- DBI::dbGetQuery(myc, 
                                            paste0("Declare @Date date = ", DECLARE_DATE, ";
with LastPayDate as
                                        (
                                        select c.CreditAccountSK, convert(date,convert(char(8),max(EffectiveDateSK))) as LastPayDate
                                        from dwh.FactCreditPayments as fcp 
                                        inner join dwh.DimDate as d on d.Date = @Date 
                                        inner join dwh.DimCredit as c on c.CreditAccountSK= fcp.CreditAccountSK
                                        where fcp.DateSK <= d.DateSK and fcp.FinOperationSK in (3,18)
                                        group by c.CreditAccountSK
                                        ),
                                        Credits as 
                                        (
                                        Select CreditAccountSK, (case when (CompleteDate is null or convert(date, CompleteDate )< @Date ) 
                                        then @Date 
                                        when CompleteDate is not null and (datediff(d, convert(date, CompleteDate ), @Date) >= 7) 
                                        then dateadd(d, 7, convert(date, CompleteDate )) 
                                        else dateadd(d, datediff(d, convert(date, CompleteDate ), @Date), convert(date, CompleteDate ))
                                        end ) as [Date]
                                        from dwh.DimCredit
                                        where Latest=1
                                        )
                                        select
                                        o.OfficeName as CurrentOfisName,
                                        o1.OfficeName as OfficeRegion,
                                        c.CodeContract as KID,
                                        cl.FirstName + ' ' + cl.MiddleName + ' ' + cl.LastName as ClientName,
                                        e.FirstName + ' ' + e.MiddleName + ' ' + e.LastName as CurrentLFSName, -- kredit konsyltant
                                        case when e1.Position not in ('Мениджър развитие', 'Мениджър обслужване на клиенти', 'ТОП Мениджър развитие') then e.FirstName + ' ' + e.MiddleName + ' ' + e.LastName else
                                        e1.FirstName + ' ' + e1.MiddleName + ' ' + e1.LastName end as CurrentMEName, -- manager razvitie 
                                        case when e2.Position not in ('Регионален мениджър', 'ТОП Регионален мениджър') then e1.FirstName + ' ' + e1.MiddleName + ' ' + e1.LastName else
                                        e2.FirstName + ' ' + e2.MiddleName + ' ' + e2.LastName end as CurrentRMName, -- regionalen manager
                                        case when e3.Position not in ('Ерия мениджър') then e2.FirstName + ' ' + e2.MiddleName + ' ' + e2.LastName else
                                        e3.FirstName + ' ' + e3.MiddleName + ' ' + e3.LastName end as CurrentAMName, -- area manager
                                        sp.productname as CreditProduct,
                                        cs.StatusBG as CreditStatus,
                                        cst.StateBG as CreditState,
                                        round(fcbc.CreditPrincipal,2) as SumaP,
                                        fcbc.CreditInterest as SumaI,
                                        round((fcbc.CreditPrincipal + fcbc.CreditInterest),2) as SumToGetBack,
                                        fcbc.CreditPaymentCount as Weeks,
                                        fcbc.PrincipalPaid as PayedPrinciple,
                                        fcbc.InterestPaid, 
                                        fcbc.PenaltyPaid,
                                        fcbc.InterestPaid + fcbc.PenaltyPaid as PayedInterest,
                                        round(fcbc.Overpaid,2) as OverPaid,
                                        fcbc.PrincipalPaid + fcbc.InterestPaid + fcbc.PenaltyPaid as PayedTotal,
                                        fcbc.CreditPrincipal - fcbc.PrincipalPaid as RestPrinciple,
                                        fcbc.CreditInterest - fcbc.InterestPaid - fcbc.PenaltyPaid as RestInterest,
                                        fcbc.CreditPrincipal + fcbc.CreditInterest - fcbc.PrincipalPaid - fcbc.InterestPaid - fcbc.PenaltyPaid as RestTotal,
                                        fcbc.CreditPrincipal + fcbc.CreditInterest - fcbc.PrincipalPaid - fcbc.InterestPaid - fcbc.PenaltyPaid - fcbc.Overpaid  as RestTotalOverPaid,
                                        (fcbc.CreditPrincipal - fcbc.PrincipalPaid) + (fcbc.CreditInterest - fcbc.InterestPaid - fcbc.PenaltyPaid) as RestTotalNoFilter,
                                        @Date as CalcDate,
                                        fcbc.CurrentDelay,
                                        fcbc.MaxDelay as MaxDelayP,
                                        fcbc.maxearly as MaxEarlyP,
                                        fcbc.PrincipalDue as MP,
                                        fcbc.RealInterestDue,
                                        fcbc.PenaltyInterestDue,
                                        fcbc.RealInterestDue + fcbc.PenaltyInterestDue  as MI,
                                        fcbc.PrincipalDue + fcbc.RealInterestDue + fcbc.PenaltyInterestDue as MaturityT,
                                        fcbc.CreditPrincipal - fcbc.principaldue as PLeftT,
                                        fcbc.CreditInterest - (fcbc.RealInterestDue + fcbc.PenaltyInterestDue ) as ILeftT,
                                        (fcbc.creditprincipal - fcbc.principaldue) + (fcbc.CreditInterest - fcbc.RealInterestDue) as LeftTotal,
                                        case when (fcbc.CurrentDelay > 3) then (fcbc.RealInterestDue + fcbc.PenaltyInterestDue + fcbc.PrincipalDue) - (fcbc.PrincipalPaid + fcbc.InterestPaid + fcbc.PenaltyPaid) else 0 END AS DelaysTotalNoFilter,
                                        lpd.LastPayDate,
                                        fcbc.DayDelayTotal,
                                        fcbc.DayDelayTotalMin,
                                        d.[Month],
                                        d.[Year],
                                        css.SubStatusBG as SubStatus,
                                        csst.SubStateBG as SubState,
                                        case when (fcbc.CurrentDelay > 90) then 'BAD' else 'GOOD' end as GoodOrBad,
                                        CASE WHEN ((fcbc.CurrentDelay > 0) AND (fcbc.CurrentDelay <= 3)) 
                                        THEN '0-3' 
                                        WHEN ((fcbc.CurrentDelay > 3) AND (fcbc.CurrentDelay <= 30)) 
                                        THEN '4-30' 
                                        WHEN ((fcbc.CurrentDelay > 30) AND (fcbc.CurrentDelay <= 60)) 
                                        THEN '31-60' 
                                        WHEN ((fcbc.CurrentDelay > 60) AND (fcbc.CurrentDelay <= 90)) 
                                        THEN '61-90' 
                                        WHEN ((fcbc.CurrentDelay > 90) AND (fcbc.CurrentDelay <= 180))
                                        THEN '91-180' 
                                        WHEN ((fcbc.CurrentDelay > 180) AND (fcbc.CurrentDelay <= 360)) 
                                        THEN '180-360' 
                                        WHEN ((fcbc.CurrentDelay >= 360)) 
                                        THEN '361+' 
                                        ELSE 'PrePayed' END AS PeriodsBG, 
                                        '1' AS BROJ,
                                        cast(floor((((fcbc.CreditPrincipal - fcbc.PrincipalPaid) + (fcbc.CreditInterest - fcbc.InterestPaid-fcbc.PenaltyPaid)) - 
                                        ((fcbc.PrincipalDue + fcbc.PenaltyInterestDue + fcbc.RealInterestDue) - (fcbc.PrincipalPaid + fcbc.InterestPaid + fcbc.PenaltyPaid))) / ((fcbc.CreditPrincipal + fcbc.CreditInterest) / fcbc.CreditPaymentCount))as int) as Ostavashti,
                                        
                                        fcbc.CreditPaymentCount - cast(floor((((fcbc.CreditPrincipal - fcbc.PrincipalPaid) + (fcbc.CreditInterest - fcbc.InterestPaid-fcbc.PenaltyPaid)) - 
                                        ((fcbc.PrincipalDue + fcbc.PenaltyInterestDue + fcbc.RealInterestDue) - (fcbc.PrincipalPaid + fcbc.InterestPaid + fcbc.PenaltyPaid))) / ((fcbc.CreditPrincipal + fcbc.CreditInterest) / fcbc.CreditPaymentCount))as int) as Padejirali,
                                        fcbc.CreditPayment as Vnoska,
                                        c.CreditBeginDate,
                                        c.CreditFirstPayment as CreditFirstDateToPay,
                                        (case when  CONVERT (DATE, fcbc.CompleteDate) < @Date then  CONVERT (DATE, fcbc.CompleteDate) else null end) AS CompleteDate,
                                        cl.EGN as ClientEGN,
                                        c.Scoring,
                                        c.ScoringType,
                                        c.ScoringLimit,
                                        c.IsPrePaid,
                                        c.Refinans,
                                        ic.Code as CreditChannel
                                        FROM [dwh].[FactCreditBalancesCurrent] as fcbc
                                        inner join dwh.DimDate as d on d.Date = @Date
                                        inner join Credits as cr on cr.CreditAccountSK = fcbc.CreditAccountSK
                                        inner join dwh.DimCredit as c on c.CreditAccountSK = cr.CreditAccountSK  and convert(date, c.CreditBeginDate ) <= d.Date and cr.Date between c.ValidFrom and c.ValidTo
                                        inner join dwh.DimOffice as o on o.OfficeSK=c.DepartmentSK
                                        inner join dwh.DimOffice as o1 on o1.OfficeSK=o.ParentOfficeSK
                                        inner join dwh.DimClient as cl on cl.ClientSK=c.ClientSK
                                        inner join dwh.DimEmployee as e on e.EmployeeSK= c.EmployeeSK
                                        inner join dwh.DimEmployee as e1 on e1.EmployeeSK = e.ParentEmployeeSK
                                        inner join dwh.DimEmployee as e2 on e2.EmployeeSK = e1.ParentEmployeeSK
                                        inner join dwh.DimEmployee as e3 on e3.EmployeeSK = e2.ParentEmployeeSK
                                        inner join dwh.DimSubProduct as sp on sp.SubProductSK =c.SubProductSK 
                                        inner join dwh.DimCreditStatus as cs on cs.CreditStatusSK = c.StatusSK
                                        INNER JOIN dwh.DimCreditState as cst on cst.CreditStateSK = c.StateSK
                                        inner join dwh.DimCreditSubStatus as css on css.CreditSubStatusSK=c.SubStatusSK and CreditSubStatusSK not in (17,18)
                                        inner join dwh.DimCreditSubState as csst on csst.CreditSubStateSK=c.SubStateSK
                                        inner join dwh.DimInputChannel as ic on ic.inputChannelSK=c.InputChannelSK
                                        left join LastPayDate as lpd on lpd.CreditAccountSK=fcbc.CreditAccountSK
                                        where fcbc.DateSK = d.DateSK and (c.ScoringType = 1001 or c.ScoringType = 101) and ScoringLimit != -1
                                        order by 1,4;"))


saveRDS(CreditProgress_Rduck_101, paste0("//10.254.1.4/Files/Analysis/9. Personal folders/Ivelina Vasileva/1. Projects/999 Others/9997 R duck/Stats/R-duck reporting/Credit Progress Data/",DECLARE_DATE, "CreditProgress_Rduck_101.rds"))
```

#### I R-duck scoring basic information

1. Number and percentage of utilized, refused and denied credits given at:
  
  - aggregated level 
- and separated by classes

2. Additional, the average credit amount is given for each group separated by scoring class (columns).

- where class 1 are clients with the best profile and class 5 are the worse clients;

- the rules are set such that credits are granted only for clients which are in classes between 1 and 4;

```{r Get some stats scoring 1001, echo=FALSE, message=FALSE, warning=FALSE}
### Set the status of the credit
all_loans7 <- all_loans7 %>% mutate(Status = case_when(!is.na(DateRefused) ~ "Refused"
                                                       , !is.na(DateDenied) ~ "Denied"
                                                       , !is.na(CreditBeginDate) ~ "Utilized"
                                                       , !is.na(DateApproved) ~ "Approved"
                                                       , TRUE ~ "Other"),
                                    ProductName = case_when(ProductName == "EasyMax24" ~ "EasyMax"
                                                            , TRUE ~ ProductName)) %>%
  filter(ProductName != "EasySpecial") %>%
  filter(ProductName != "EasyVIP25")
print("Number of credits and as percentage points by status /Rduck scoring/")
all_loans7 %>% mutate(Total = n()) %>%
  dplyr::group_by(Status) %>% 
  dplyr::summarise(Total_perc = round(n()/ mean(Total),2), Total2 = n())

print("Percentage of total credits: R-duck scoring vs class")
# print("Columns represent classes")
all_loans7 %>% mutate(Total = n()) %>%
  filter(Status != "Other") %>%
  group_by(Status, ScoringLimit) %>% 
  dplyr::summarise(Total_perc = round(n()/ mean(Total),2)) %>%
  tidyr::spread(ScoringLimit, Total_perc)

print("Number of credits: R-duck scoring")
all_loans7 %>% mutate(Total = n()) %>%
  filter(Status != "Other") %>%
  group_by(Status, ScoringLimit) %>% 
  dplyr::summarise(Total2 = n()) %>%
  tidyr::spread(ScoringLimit, Total2)



```
#### II Scoring for first credit (101) basic information

1. Number and percentage population of utilized, refused and denied credits given at:
  
  - aggregated level 
- and separated by classes

2. Additional, the average credit amount is given for each group separated by scoring class (columns).

- where class 1 are clients with the best profile and class 5 are the worse clients;

- the rules are set such that credits are granted only for clients which are in classes between 1 and 4;


!!! NB: Class 5 for both scorings are overrides
```{r Get some stats scoring 101, echo=FALSE}
all_loans101 <- all_loans101 %>% mutate(Status = case_when(!is.na(DateRefused) ~ "Refused"
                                                           , !is.na(DateDenied) ~ "Denied"
                                                           , !is.na(CreditBeginDate) ~ "Utilized"
                                                           , !is.na(DateApproved) ~ "Approved"
                                                           , TRUE ~ "Other")) %>%
  filter(ProductName != "EasyIncredo") %>%
  filter(ProductName != "EasyMax24") 

print("Number of credits and as percentage points by status /Rduck scoring/")
all_loans101 %>% mutate(Total = n()) %>%
  group_by(Status) %>% 
  summarise(Total_perc = round(n()/ mean(Total),2)
            , Total2 = n())

print("Percentage of total credits by class: 101 scoring")
# print("Columns represent classes")
all_loans101 %>% mutate(Total = n()) %>%
  filter(Status != "Other") %>%
  filter(!is.na(ScoringLimit)) %>%
  filter(ScoringLimit>0) %>%
  group_by(Status, ScoringLimit) %>% 
  dplyr::summarise(Total_perc = round(n()/ mean(Total),2)) %>%
  tidyr::spread(ScoringLimit, Total_perc)


print("Number of credits: 101 scoring")
all_loans101 %>% mutate(Total = n()) %>%
  filter(Status != "Other") %>%
  filter(!is.na(ScoringLimit)) %>%
  filter(ScoringLimit>0) %>%
  group_by(Status, ScoringLimit) %>% 
  dplyr::summarise(Total2 = n()) %>%
  tidyr::spread(ScoringLimit, Total2)

```

#### 3. Compare average amount granted from the two scorings

- where first table is for scoring 101 and the second gives information about the R-duck scoring

#### 3.1. Average amount for approved and utilized credits by product

```{r}
print("Compare average amount granted per product")
print("scoring 101")
all_loans101 %>% mutate(Total = n()) %>%
  filter(Status %in% c("Utilized", "Approved")) %>%
  group_by(ProductName) %>% 
  dplyr::summarise(Sum = round(mean(Sum, na.rm = T),2)
                   , Interest = round(mean(Interest, na.rm = T),2)
                   , Total_credits = n()
                   , Total_credits_perc = round(n() / mean(Total),2))
print("R-duck scoring")
all_loans7 %>% mutate(Total = n()) %>%
  filter(Status %in% c("Utilized", "Approved")) %>%
  group_by(ProductName) %>% 
  dplyr::summarise(Sum = round(mean(Sum, na.rm = T),2)
                   , Interest = round(mean(Interest, na.rm = T),2)
                   , Total_credits = n()
                   , Total_credits_perc = round(n() / mean(Total),2))
```


The results suggest that with the new scoring which is more precise, expecially in terms of classes, managed to increase the average credit amount grated. 

#### III Some Statistics about each scoring scoring type 

- Separation in number of credits by product type and scoring class

* First table for R-duck scoring

* Second - is for scoring 101


1. Some statistics about R-duck scoring products vs classes

```{r Statistcs about scoring type 1001 p2}
ScoringClassesStats7 %>%
  filter(ProductName != "EasyVIP25")%>%
  filter(ProductName != "EasySpecial")
```


2. Some statistics about Scoring for first clients 101: products vs classes

```{r Statistcs about scoring type 101 p2, echo=FALSE}
ScoringClassesStats101 %>% filter(ScoringLimit > 0)%>%
  filter(ProductName != "EasyMax24") %>%
  filter(ProductName != "EasyIncredo")
```

#### IV Credit Performance

1.R-duck scoring

The following graphs presents credit performance in terms of:
  
  - Total credits
- Mean credit amount 
- Delay across products and classes

The second set of tables gives infomation about the repayment behaviour by product and class in the following order:
  
  - Payed Total from MaturityT, SumaP and SumToGetBack

```{r Create performance indicators , echo=FALSE, warning=FALSE}
# print("R-duck scoring")

scoring7_progress <- CreditProgress_Rduck_101 %>% 
  mutate(CreditBeginDate = as.Date(CreditBeginDate)
         , Period = year(CreditBeginDate)*100 + month(CreditBeginDate)
         , RR_STGB = round(PayedTotal / SumToGetBack, 2)
         , RR_SumaP = round(PayedTotal / SumaP, 2)) %>%
  filter(ScoringType == 1001) %>%
  mutate( CreditProduct = case_when(CreditProduct == "EasyMax24" ~ "EasyMax"
                                    , TRUE ~ CreditProduct)) %>%
  filter(CreditProduct != "EasySpecial") %>%
  filter(CreditProduct != "EasyVIP25")

# Create some new variables
stats <- scoring7_progress %>% dplyr::group_by(CreditProduct, ScoringLimit) %>%
  dplyr::summarise(RR_STGB = round(sum(PayedTotal, na.rm =T) / sum(SumToGetBack, na.rm = T), 2)
                   , RR_SumaP = round(sum(PayedTotal, na.rm =T) / sum(SumaP, na.rm = T), 2)
                   , RR_MaturityT = round(sum(PayedTotal, na.rm =T) / sum(MaturityT, na.rm = T), 2))

print("Total credit and mean credit amount across products and classes")

scoring7_progress %>%
  dplyr::group_by(CreditProduct, ScoringLimit) %>%
  dplyr::summarise(Total = n()) %>% spread(ScoringLimit, Total)

scoring7_progress %>% dplyr::group_by(CreditProduct, ScoringLimit) %>%
  dplyr::summarise(SumaP = round(mean(SumaP, na.rm = T),2)) %>%
  tidyr::spread(ScoringLimit, SumaP)


```

1. Scoring for first time credits 101

The following graphs presents credit performance in terms of:
  
  - Total credits
- Mean credit amount 
- Delay across products and classes

The second set of tables gives infomation about the repayment behaviour by product and class in the following order:
  
  - Payed Total from MaturityT, SumaP and SumToGetBack

```{r stats 101, echo=FALSE, message=FALSE, warning=FALSE}

# print("Scoring 101 for first clients")
### Create some new variables
scoring101_progress <- CreditProgress_Rduck_101 %>% 
  dplyr::filter(!is.na(ScoringLimit)) %>%
  dplyr::mutate(CreditBeginDate = as.Date(CreditBeginDate)
                , Period = year(CreditBeginDate)*100 + month(CreditBeginDate)
                , RR_STGB = round(PayedTotal / SumToGetBack, 2)
                , RR_SumaP = round(PayedTotal / SumaP, 2)) %>%
  filter(ScoringType == 101) %>%
  
  filter(CreditProduct != "EasyMax24") %>%
  filter(CreditProduct != "EasyIncredo")


stats_101 <- scoring101_progress %>% 
  dplyr::group_by(CreditProduct, ScoringLimit) %>%
  dplyr::summarise(RR_STGB = round(sum(PayedTotal, na.rm =T) / sum(SumToGetBack, na.rm = T), 2)
                   , RR_SumaP = round(sum(PayedTotal, na.rm =T) / sum(SumaP, na.rm = T), 2)
                   , RR_MaturityT = round(sum(PayedTotal, na.rm =T) / sum(MaturityT, na.rm = T), 2))

print("Total credit and mean credit amount across products and classes")
scoring101_progress %>% 
  dplyr::group_by(CreditProduct, ScoringLimit) %>% 
  dplyr::summarise(Total = n()) %>%
  tidyr::spread(ScoringLimit, Total)

scoring101_progress %>%
  dplyr::group_by(CreditProduct, ScoringLimit) %>%
  summarise(SumaP = mean(SumaP, na.rm = T)) %>% 
  tidyr::spread(ScoringLimit, SumaP)


```

#### V Vitage performance only for scoring type 101

- Payed Total from MaturityT, from SumToGetBack and from SumaP

```{r echo=FALSE, message=FALSE, warning=FALSE}
print("Scoring 101 vintage statistics")
print("Vintage number of credits")
scoring101_progress %>%
  dplyr::group_by(CreditProduct, ScoringLimit, Period) %>%
  dplyr::summarise(Number_of_credits = n()) %>%
  dplyr::select(CreditProduct, ScoringLimit,Period, Number_of_credits)%>%
  tidyr::spread(Period, Number_of_credits)

print("Vintage number of DPD")
scoring101_progress %>%
  dplyr::group_by(CreditProduct, ScoringLimit, Period) %>%
  dplyr::summarise(MedianDelay = round(median(MaxDelayP, na.rm=T),0)) %>%
  dplyr::select(CreditProduct, ScoringLimit,Period, MedianDelay)%>%
  tidyr::spread(Period, MedianDelay)

print("Vintage portfolio within a class in BGN")
scoring101_progress %>%
  dplyr::group_by(CreditProduct, ScoringLimit, Period) %>%
  dplyr::summarise(Portfolio_amount = sum(SumaP, na.rm = T)) %>%
  dplyr::select(CreditProduct, ScoringLimit,Period, Portfolio_amount)%>%
  tidyr::spread(Period, Portfolio_amount)

print("Vintage Payed MaturityT")
scoring101_progress %>%
  dplyr::group_by(CreditProduct, ScoringLimit, Period) %>%
  dplyr::summarise(RR_STGB = round(sum(PayedTotal, na.rm =T) / sum(SumToGetBack, na.rm = T), 2)
                   , RR_SumaP = round(sum(PayedTotal, na.rm =T) / sum(SumaP, na.rm = T), 2)
                   , RR_MaturityT = round(sum(PayedTotal, na.rm =T) / sum(MaturityT, na.rm = T), 2)) %>%
  dplyr::select(CreditProduct, ScoringLimit,Period, RR_MaturityT)%>%
  tidyr::spread(Period, RR_MaturityT)

print("Vintage Payed SumToGetBack")
scoring101_progress %>% dplyr::group_by(CreditProduct, ScoringLimit, Period) %>%
  dplyr::summarise(RR_STGB = round(sum(PayedTotal, na.rm =T) / sum(SumToGetBack, na.rm = T), 2)) %>%
  dplyr::select(CreditProduct, ScoringLimit,Period, RR_STGB)%>%
  tidyr::spread(Period, RR_STGB)

print("Vintage Payed SumaP")
scoring101_progress %>% dplyr::group_by(CreditProduct, ScoringLimit, Period) %>%
  dplyr::summarise(RR_SumaP = round(sum(PayedTotal, na.rm =T) / sum(SumaP, na.rm = T), 2)) %>%
  dplyr::select(CreditProduct, ScoringLimit,Period, RR_SumaP)%>%
  tidyr::spread(Period, RR_SumaP)
```


```{r echo=FALSE, message=FALSE, warning=FALSE}
print("Scoring 7 vintage statistics")

print("Vintage number of credits")
scoring7_progress %>%
  dplyr::group_by(CreditProduct, ScoringLimit, Period) %>%
  dplyr::summarise(Number_of_credits = n()) %>%
  dplyr::select(CreditProduct, ScoringLimit,Period, Number_of_credits)%>%
  tidyr::spread(Period, Number_of_credits)

print("Vintage number of DPD")
scoring7_progress %>%
  dplyr::group_by(CreditProduct, ScoringLimit, Period) %>%
  dplyr::summarise(MedianDelay = round(median(MaxDelayP, na.rm=T),0)) %>%
  dplyr::select(CreditProduct, ScoringLimit,Period, MedianDelay)%>%
  tidyr::spread(Period, MedianDelay)

print("Vintage portfolio within a class")
scoring7_progress %>%
  dplyr::group_by(CreditProduct, ScoringLimit, Period) %>%
  dplyr::summarise(Portfolio_amount = sum(SumaP, na.rm = T)) %>%
  dplyr::select(CreditProduct, ScoringLimit,Period, Portfolio_amount)%>%
  tidyr::spread(Period, Portfolio_amount)

print("Vintage Payed MaturityT")
scoring7_progress %>%
  dplyr::group_by(CreditProduct, ScoringLimit, Period) %>%
  dplyr::summarise(RR_STGB = round(sum(PayedTotal, na.rm =T) / sum(SumToGetBack, na.rm = T), 2)
                   , RR_SumaP = round(sum(PayedTotal, na.rm =T) / sum(SumaP, na.rm = T), 2)
                   , RR_MaturityT = round(sum(PayedTotal, na.rm =T) / sum(MaturityT, na.rm = T), 2)) %>%
  dplyr::select(CreditProduct, ScoringLimit,Period, RR_MaturityT)%>%
  tidyr::spread(Period, RR_MaturityT)

print("Vintage Payed SumToGetBack")
scoring7_progress %>% dplyr::group_by(CreditProduct, ScoringLimit, Period) %>%
  dplyr::summarise(RR_STGB = round(sum(PayedTotal, na.rm =T) / sum(SumToGetBack, na.rm = T), 2)) %>%
  dplyr::select(CreditProduct, ScoringLimit,Period, RR_STGB)%>%
  tidyr::spread(Period, RR_STGB)

print("Vintage Payed SumaP")
scoring7_progress %>% dplyr::group_by(CreditProduct, ScoringLimit, Period) %>%
  dplyr::summarise(RR_SumaP = round(sum(PayedTotal, na.rm =T) / sum(SumaP, na.rm = T), 2)) %>%
  dplyr::select(CreditProduct, ScoringLimit,Period, RR_SumaP)%>%
  tidyr::spread(Period, RR_SumaP)
```

#### Explore scoring performance for scoring 101 with the following graphs:

Graphs are given for each product separtely. Additionally, the average granted loan amount within each bin is given.

The scoring shows good segmentation power across all product. 

*Points for further consideration:*
  
  * The loan limits can be more precise. For example, bigger loans can be given to clients with better scoring.

```{r message=FALSE, warning=FALSE, include=FALSE}
#### Some necessary fucntions for the plots

library(ggplot2)

stat_smooth_func <- function(mapping = NULL, data = NULL,
                             geom = "smooth", position = "identity",
                             ...,
                             method = "auto",
                             formula = y ~ x,
                             se = TRUE,
                             n = 80,
                             span = 0.75,
                             fullrange = FALSE,
                             level = 0.95,
                             method.args = list(),
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             xpos = NULL,
                             ypos = NULL) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmoothFunc,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      method.args = method.args,
      span = span,
      xpos = xpos,
      ypos = ypos,
      ...
    )
  )
}


StatSmoothFunc <- ggproto("StatSmooth", Stat,
                          
                          setup_params = function(data, params) {
                            # Figure out what type of smoothing to do: loess for small datasets,
                            # gam with a cubic regression basis for large data
                            # This is based on the size of the _largest_ group.
                            if (identical(params$method, "auto")) {
                              max_group <- max(table(data$group))
                              
                              if (max_group < 1000) {
                                params$method <- "loess"
                              } else {
                                params$method <- "gam"
                                params$formula <- y ~ s(x, bs = "cs")
                              }
                            }
                            if (identical(params$method, "gam")) {
                              params$method <- mgcv::gam
                            }
                            
                            params
                          },
                          
                          compute_group = function(data, scales, method = "auto", formula = y~x,
                                                   se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                                   xseq = NULL, level = 0.95, method.args = list(),
                                                   na.rm = FALSE, xpos=NULL, ypos=NULL) {
                            if (length(unique(data$x)) < 2) {
                              # Not enough data to perform fit
                              return(data.frame())
                            }
                            
                            if (is.null(data$weight)) data$weight <- 1
                            
                            if (is.null(xseq)) {
                              if (is.integer(data$x)) {
                                if (fullrange) {
                                  xseq <- scales$x$dimension()
                                } else {
                                  xseq <- sort(unique(data$x))
                                }
                              } else {
                                if (fullrange) {
                                  range <- scales$x$dimension()
                                } else {
                                  range <- range(data$x, na.rm = TRUE)
                                }
                                xseq <- seq(range[1], range[2], length.out = n)
                              }
                            }
                            # Special case span because it's the most commonly used model argument
                            if (identical(method, "loess")) {
                              method.args$span <- span
                            }
                            
                            if (is.character(method)) method <- match.fun(method)
                            
                            base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                            model <- do.call(method, c(base.args, method.args))
                            
                            m = model
                            eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                                             list(a = format(coef(m)[1], digits = 3), 
                                                  b = format(coef(m)[2], digits = 3), 
                                                  r2 = format(summary(m)$r.squared, digits = 3)))
                            func_string = as.character(as.expression(eq))
                            
                            if(is.null(xpos)) xpos = min(data$x)*0.9
                            if(is.null(ypos)) ypos = max(data$y)*0.9
                            data.frame(x=xpos, y=ypos, label=func_string)
                            
                          },
                          
                          required_aes = c("x", "y")
)
```


```{r message=FALSE, warning=FALSE, include=FALSE}
#### Creating graphs 

library(gridExtra)
library(gtools)

Day2 <- year(Day - 8*30) * 100 + month(Day - 8*30) 
Day3 <- year(Day ) * 100 + month(Day) 

### Products for scoring type 101 
products <- c("EasyCredit", "EasyMonth", "Pensioner")

### Products for scoring type 1001 
products2 <- c("EasyCredit", "EasyMonth", "EasyMax")


### Macro for creating graphs 
creating_graphs = defmacro(CreditProgress_Rduck_101 = CreditProgress_Rduck_101
                           , ScoringTypeOfInterest = 101
                           , mainDir = "//10.254.1.4/Files/Analysis/9. Personal folders/Ivelina Vasileva/1. Projects/999 Others/9997 R duck/Stats/R-duck reporting"
                           
                           ,expr = { 
                             
                             if(ScoringTypeOfInterest == 101)     {
                               CreditProgress <- 
                                 CreditProgress_Rduck_101 %>%
                                 filter(ScoringType == ScoringTypeOfInterest) %>%
                                 filter(CreditState %in% c("Нормално обслужван", "Обслужван от просрочени вземания")) %>%
                                 mutate(CreditBeginDate = as.Date(CreditBeginDate)
                                        , Period = year(CreditBeginDate)*100+ month(CreditBeginDate)) %>%
                                 filter(Period <= Day2) %>%
                                 mutate(RR_SumaP = round((PayedTotal + 0.01) / SumaP,2)
                                        , GoodOrBad_N = case_when(RR_SumaP >= 0.98 ~ 0, TRUE ~ 1)
                                        , c_Target = case_when(GoodOrBad_N == 1 ~ "Bad", TRUE ~ "Good")) %>% 
                                 filter(CreditProduct %in% products)
                               
                               sample <- CreditProgress %>% 
                                 select(KID,CreditBeginDate, CreditProduct,  GoodOrBad_N, c_Target, Scoring, SumaP) %>%
                                 filter(CreditProduct == products[i]) %>%
                                 mutate(month = month(CreditBeginDate), year = year(CreditBeginDate))%>%
                                 arrange(Scoring) %>% 
                                 mutate(Seq = seq(from = 1, to = n(), by = 1), 
                                        percentile = round(Seq/ n(),4)*100, 
                                        Bin = c( 1, 2, 3, 4, 5,6,7,8,9,10)[
                                          findInterval(percentile, c(-Inf, 10, 20, 30, 40, 50, 60, 70, 80, 90) ) ])
                               
                             } else {
                               
                               CreditProgress <- 
                                 CreditProgress_Rduck_101 %>%
                                 filter(ScoringType == ScoringTypeOfInterest) %>%
                                 filter(CreditState %in% c("Нормално обслужван", "Обслужван от просрочени вземания")) %>%
                                 mutate(CreditBeginDate = as.Date(CreditBeginDate)
                                        , Period = year(CreditBeginDate)*100+ month(CreditBeginDate)) %>%
                                 filter(Period <= Day3) %>%
                                 mutate(RR_MaturityT = round((PayedTotal + 0.01) / SumaP,2)
                                        , GoodOrBad_N = case_when(RR_MaturityT >= 0.98 ~ 0, TRUE ~ 1)
                                        , c_Target = case_when(GoodOrBad_N == 1 ~ "Bad", TRUE ~ "Good")) %>% 
                                 filter(CreditProduct %in% products2)
                               
                               sample <- CreditProgress %>% 
                                 select(KID,CreditBeginDate, CreditProduct,  GoodOrBad_N, c_Target, Scoring, SumaP) %>%
                                 filter(CreditProduct == products2[i]) %>%
                                 mutate(month = month(CreditBeginDate), year = year(CreditBeginDate))%>%
                                 arrange(Scoring) %>% 
                                 mutate(Seq = seq(from = 1, to = n(), by = 1), 
                                        percentile = round(Seq/ n(),4)*100, 
                                        Bin = c( 1, 2, 3, 4, 5,6,7,8,9,10)[
                                          findInterval(percentile, c(-Inf, 10, 20, 30, 40, 50, 60, 70, 80, 90) ) ])
                             }                   
                             
                             ### place to save graphs
                             subDir = paste0("Graphs Scoring Type ", ScoringTypeOfInterest, " ", Day3)
                             
                             
                             
                             #### Creating statistics becessary for the graphs #####
                             
                             sample4 <- sample %>% 
                               group_by(Bin) %>% 
                               summarise( AvgScorePerBin = mean(Scoring)
                                          , Bads =  sum(GoodOrBad_N)
                                          , Total = n()
                                          , Bad_rate = round(100*(sum(GoodOrBad_N)/ n()),0)
                                          , AvgSumaP = round(mean(SumaP),0))
                             
                             #### X scale for the graphs: The Scring left upper band for each interval ####
                             
                             scale <- sample %>%
                               group_by(Bin) %>% 
                               summarise( Scale_x = round(max(Scoring),0))
                             
                             vector_scale_c <- scale$Scale_x
                             
                             plot1 <- ggplot (sample4)+
                               stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE,  aes (x=AvgScorePerBin, y = Bad_rate))+
                               geom_smooth(method = lm, aes(x=AvgScorePerBin, y = Bad_rate))+
                               geom_point( aes (x=AvgScorePerBin, y = Bad_rate),  size = 3, alpha = 0.4)  + 
                               geom_text(aes(x=AvgScorePerBin, y = Bad_rate, label = Bad_rate),size = 3.5, hjust = 0, vjust = 0)+
                               scale_x_continuous(breaks = vector_scale_c) +
                               scale_y_continuous(breaks = c(0,10, 20, 30, 40, 50, 60,70,80,90, 100))+
                               ggtitle(paste0('% Лоши спрямо скор до ', Day2, ' ' , products[i]))+
                               xlab("Банд на скор (PD)") +
                               ylab("Процент лоши") +
                               theme(plot.title = element_text(color="blue", size=12, face="bold.italic"),
                                     axis.title.x = element_text(color="blue", size=8),
                                     axis.text = element_text(colour = "blue", size = 8),
                                     axis.title.y = element_text(color="blue", size=8),
                                     panel.background = element_blank()) 
                             
                             plot2 <- ggplot (sample4, aes(x = factor(round(AvgScorePerBin, 2))))+
                               geom_bar(aes (y = Bads, x = factor(round(AvgScorePerBin, 2))), fill="green", stat="identity",
                                        position=position_dodge(),
                                        size=.3, alpha = 0.3)+
                               geom_bar(aes (y = Total, x = factor(round(AvgScorePerBin, 2))), fill="blue", stat="identity",
                                        position=position_dodge(),
                                        size=.3, alpha = 0.3)+
                               
                               geom_text(aes(y = Total, label = Total),size = 2.5, hjust = 0, vjust = 0)+
                               geom_text(aes(y = Bads, label = Bads),size = 2.5, hjust = 0, vjust = 0)+
                               
                               ggtitle(paste0('Брой лоши спрямо скор' ))+
                               xlab("Среден скро за банд (PD)")+
                               ylab("Брой лоши vs всички кредити")+
                               theme(plot.title = element_text(color="blue", size=12, face="italic"),
                                     axis.title.x = element_text(color="blue", size=8),
                                     axis.title.y = element_text(color="blue", size=8),
                                     axis.text = element_text(colour = "blue", size = 8),
                                     panel.background = element_blank())
                             
                             plot3<- plot2 + 
                               scale_y_continuous(sec.axis = sec_axis(~./3, name = "Средна сума на кредит за банд (BGN)"))+
                               geom_point(aes(y = round(AvgSumaP)*3 ), fill="blue", stat="identity",
                                          size=4, alpha = 0.6, color = 'orange')+
                               geom_text(aes(y = AvgSumaP*3 ,label = AvgSumaP), size = 3.5, hjust = 0, vjust = 0)
                             # geom_text(aes(label = paste0('Exchange Rate:', exchange_rate, " Scoring Type: "
                             #                              , ScoringTypeOfInterest), x = 1.5, y = -3), color="#a0a0a0", size=3)
                             
                             
                             #### Plot the two graphs together one above the other
                             gp1<- ggplot_gtable(ggplot_build(plot1))
                             gp2<- ggplot_gtable(ggplot_build(plot3))
                             
                             #Put them together
                             gp3 <- grid.arrange(gp1, gp2)
                             
                             if (file.exists(subDir)){
                               setwd(file.path(mainDir, subDir))
                             } else {
                               dir.create(file.path(mainDir, subDir))
                               setwd(file.path(mainDir, subDir))
                             }
                             
                             ggsave(filename = paste0("Graph for ", products[i], " up to ", Day2 , ".jpeg"), plot = gp3, width = 10.24, height = 7.17, dpi = 100, units = "in", device='jpeg')
                             
                           })

```

```{r echo=FALSE, message=FALSE, warning=FALSE}
### Creating the graphs 
### For scoring 101 

for (i in seq(1:length(products))){
  print(products[i])
  creating_graphs() 
}

### For scoring 7
# for (i in seq(1:length(products2))){
# print(products2[i])
#   creating_graphs(ScoringTypeOfInterest = 7) 
# }

```


### R-duck BadRate Analysis

- The analysis will present bad rate across scoring clients and across products of the approved clients in different months.

- This will show how good the R-duck scoring is differentiating good and bad clients and whether or not the scoring classes are properly defined.

*Definition of Bad:*
  
  If the payed amount by the client is less than the loan amount => Bad client

!!! Note: Our business practice shows that a client need some time to repay the dept which depends also on the credit product specifiations.

Example: For Easy Credit product this time varies from 4 to 6 months at minimum.


Additionally, the fraud rate is analyzed the same way as the bad rate, where fraud means clients which up to now have paid 10 BGN or less.

#### The following tables show percentage of Bad and Fraud clients across scoring classes, product and period: 

```{r echo=FALSE, message=FALSE, warning=FALSE}
badrate_analysis <- CreditProgress_Rduck_101 %>% 
  # filter(ScoringType == 1001) %>%
  mutate(Period = year(CreditBeginDate)*100 + month(CreditBeginDate)
         , RR_SumaP = (PayedTotal + 0.001) / SumaP
         , Bad = case_when(RR_SumaP >= 1 ~ 0, TRUE ~ 1)
         , Fraud = case_when(PayedTotal>10 ~ 0, TRUE ~ 1)) %>%
  mutate(CreditProduct = case_when(CreditProduct == "EasyMax" ~ "EasyMax24", TRUE ~ CreditProduct)) %>%
  
  filter(CreditProduct != "EasyVIP25")%>%
  filter(CreditProduct != "EasySpecial") %>%
  filter(CreditProduct != "EasyIncredo")

print("Bad rate across classes from February 2019 untill today, with no other segmentation: ")

badrate_analysis %>%
  filter(ScoringType == 1001) %>%
  filter(CreditProduct %in% c("EasyCredit", "EasyMax24", "EasyMonth")) %>%
  group_by(ScoringLimit) %>%
  summarise(BadRate = round(mean(Bad, na.rm = T),2)) 

print("Vintage of the bad rate across classes from February 2019 untill today")
badrate_analysis %>%
  filter(ScoringType == 1001) %>%
  filter(CreditProduct %in% c("EasyCredit", "EasyMax24", "EasyMonth")) %>%
  group_by(Period, ScoringLimit) %>%
  summarise(BadRate = round(mean(Bad, na.rm = T),2)) %>%
  spread(Period, BadRate)

print("Vintage of the bad rate across classes and products from February 2019 untill today")

badrate_analysis %>%
  filter(ScoringType == 1001) %>%
  filter(CreditProduct %in% c("EasyCredit", "EasyMax24", "EasyMonth")) %>%
  group_by(Period,  CreditProduct, ScoringLimit) %>%
  summarise(BadRate = round(mean(Bad, na.rm = T),2)) %>%
  spread(Period, BadRate)

print("Fraud rate across classes from February 2019 untill today, with no other segmentation: ")

badrate_analysis %>%
  filter(ScoringType == 1001) %>%
  filter(CreditProduct %in% c("EasyCredit", "EasyMax24", "EasyMonth")) %>%
  group_by(Period, ScoringLimit) %>%
  summarise(FraudRate = round(mean(Fraud, na.rm = T),2)) %>%
  spread(Period, FraudRate)

print("Vintage of the fraud rate across classes and products from February 2019 untill today")

badrate_analysis %>%
  filter(ScoringType == 1001) %>%
  filter(CreditProduct %in% c("EasyCredit", "EasyMax24", "EasyMonth")) %>%
  group_by(ScoringLimit) %>%
  summarise(FraudRate = round(mean(Fraud, na.rm = T),2)) 

print("Vintage of the fraud rate across classes and products from February 2019 untill today")

badrate_analysis %>%
  filter(ScoringType == 1001) %>%
  filter(CreditProduct %in% c("EasyCredit", "EasyMax24", "EasyMonth")) %>%
  group_by(CreditProduct, Period, ScoringLimit) %>%
  summarise(FraudRate = round(mean(Fraud, na.rm = T),2)) %>%
  spread(Period, FraudRate)

```

#### Scoring 101

Same analysis was perform for Scoring 101 (alternative for R-duck)

#### The following tables show percentage of Bad and Fraud clients across scoring classes, product and period: 
```{r echo=FALSE, message=FALSE, warning=FALSE}
print("Bad rate across classes from February 2019 untill today, with no other segmentation: ")

badrate_analysis %>%
  filter(ScoringType == 101) %>%
  filter(CreditProduct %in% c("EasyCredit", "Pensioner", "EasyMonth")) %>%
  group_by(ScoringLimit) %>%
  summarise(BadRate = round(mean(Bad, na.rm = T),2)) 

print("Vintage of the bad rate across classes from February 2019 untill today")

badrate_analysis %>%
  filter(ScoringType == 101) %>%
  filter(CreditProduct %in% c("EasyCredit", "Pensioner", "EasyMonth")) %>%
  group_by(Period, ScoringLimit) %>%
  summarise(BadRate = round(mean(Bad, na.rm = T),2)) %>%
  spread(Period, BadRate)

print("Vintage of the bad rate across classes and products from February 2019 untill today")

badrate_analysis %>%
  filter(ScoringType == 101) %>%
  filter(CreditProduct %in% c("EasyCredit", "Pensioner", "EasyMonth")) %>%
  group_by(Period,  CreditProduct, ScoringLimit) %>%
  summarise(BadRate = round(mean(Bad, na.rm = T),2)) %>%
  spread(Period, BadRate)

print("Fraud rate across classes from February 2019 untill today, with no other segmentation: ")

badrate_analysis %>%
  filter(ScoringType == 101) %>%
  filter(CreditProduct %in% c("EasyCredit", "Pensioner", "EasyMonth")) %>%
  group_by(ScoringLimit) %>%
  summarise(FraudRate = round(mean(Fraud, na.rm = T),2)) 

print("Vintage of the fraud rate across classes and products from February 2019 untill today")

badrate_analysis %>%
  filter(ScoringType == 101) %>%
  filter(CreditProduct %in% c("EasyCredit", "Pensioner", "EasyMonth")) %>%
  group_by(Period, ScoringLimit) %>%
  summarise(FraudRate = round(mean(Fraud, na.rm = T),2)) %>%
  spread(Period, FraudRate)

print("Vintage of the fraud rate across classes and products from February 2019 untill today")

badrate_analysis %>%
  filter(ScoringType == 101) %>%
  filter(CreditProduct %in% c("EasyCredit", "Pensioner", "EasyMonth")) %>%
  group_by(CreditProduct, Period, ScoringLimit) %>%
  summarise(FraudRate = round(mean(Fraud, na.rm = T),2)) %>%
  spread(Period, FraudRate)


```

### R-duck segmentation and profile analysis 

We are analyzing data from  February 2019 till today.


```{r echo=FALSE, message=FALSE, warning=FALSE}
library(lubridate)
library(tidyverse)

#### 01 Connect to the database ####
myc1 <- DBI::dbConnect(odbc::odbc()
                       , driver= "SQL Server"
                       , server = 'eunomia'
                       , database = "RDUCK")

Rduck_results <-  DBI::dbGetQuery(myc1, paste0("SELECT a.Id, ScoringRequestId, ValueString, ValueFloat
                                                    , ModelResultId,  b.EGN, b.RequestDate

                                                 FROM [dbo].[ScoringRequestResults] a LEFT JOIN [dbo].[ScoringRequests] b
                                                 ON a.ScoringRequestId = b.Id"))


Rduck_results <- Rduck_results %>% 
  mutate(ValueString = case_when(ValueString == "bankov" ~ "банков",
                                 ValueString == "nebankov" ~ "небанков",
                                 ValueString == "high" ~ "висок",
                                 ValueString == "low" ~ "нисък",
                                 TRUE ~ ValueString)) 
Rduck_results1  <- Rduck_results %>% 
  # mutate(ValueString = case_when(ValueString == "bankov" ~ "банков",
  #                                ValueString == "nebankov" ~ "небанков",
  #                                ValueString == "high" ~ "висок",
  #                                ValueString == "low" ~ "нисък",
  #                                TRUE ~ ValueString)) %>%
  select( EGN, RequestDate) %>% 
  group_by(EGN) %>% 
  summarise(RequestDate = max(RequestDate)) %>% ungroup


Rduck_results2 <- Rduck_results1 %>% left_join (Rduck_results, by.x = c("RequestDate"), by.y= c("EGN"))

Rduck_results3<-  Rduck_results2 %>%
  filter(ModelResultId == 2)%>% 
  select(EGN, ValueString, RequestDate ) %>% rename(Segment = ValueString) %>%
  left_join(Rduck_results2 %>%
              filter(ModelResultId == 3)%>% 
              select(EGN, ValueString) %>% rename(Potential = ValueString), by = c("EGN")) %>% 
  left_join(Rduck_results2 %>%
              filter(ModelResultId == 1)%>% 
              select(EGN, ValueFloat) %>% rename(R_duck_scoring = ValueFloat), by = c("EGN")) %>% 
  mutate(RequestDate = as.Date(RequestDate))

print("Avg scoring by Segment and Potential")
Rduck_results3 %>% group_by(Segment,Potential ) %>%
  summarise(R_duck_scoring = round(mean(R_duck_scoring, na.rm = T),2)) %>% 
  spread(Potential, R_duck_scoring)

print("Number of Credits")
Rduck_results3 %>%
  group_by(Segment,Potential) %>%
  summarise(Total = n()) %>% 
  spread(Potential, Total)

print("Avg scoring by Segment")
Rduck_results3 %>% 
  group_by(Segment ) %>%
  summarise(R_duck_scoring = round(mean(R_duck_scoring, na.rm = T),2))

print("Avg scoring by Potential")
Rduck_results3 %>%
  group_by(Potential) %>%
  summarise(R_duck_scoring = round(mean(R_duck_scoring, na.rm = T),2))


```


```{r echo=FALSE, message=FALSE, warning=FALSE}
# CreditProgress_Rduck_101 <- readRDS("//10.254.1.4/Files/Analysis/9. Personal folders/Ivelina Vasileva/1. Projects/999 Others/9997 R duck/Stats/R-duck reporting/Credit Progress Data/'2019-06-24'CreditProgress_Rduck_101.rds")

#### 01 Scoring mapping with client credit ####

complete_table <- Rduck_results3 %>%
  left_join(CreditProgress_Rduck_101, by = c("EGN"="ClientEGN")) %>% 
  mutate(CreditBeginDate = as.Date(CreditBeginDate)
         , Diff = as.numeric(RequestDate - CreditBeginDate)) %>%
  filter(Diff >= 0) %>%
  group_by(EGN) %>%
  mutate(LastObs = max(RequestDate)) %>%
  filter(LastObs == RequestDate) %>%
  mutate (Period = year(CreditBeginDate)*100 + month(CreditBeginDate)) %>%
  filter(Period >= 201902)

```

#### Next table shows  population distribution of new credits across scoring classes by potential and segment

*Best class 1 and worse class is 5 (a.k.a overrides)*
  
  - On this table we can see that the business in concentrated in the following segments:
  
  ** clients with very good profile in class 1 with high potential and bank profile which represent 18% from the approved credits each month. 

** 29% are clients with bad profile with low potential and from the nonbank sector in class 4 and 5.

*Later in the analysis we will explore the repayment behaviour of these segments*
  
  ```{r echo=FALSE, message=FALSE, warning=FALSE}
complete_table %>%
  mutate(Total = nrow(complete_table)) %>%
  group_by(Potential, Segment, ScoringLimit) %>%
  summarise(Percentage = round(n() / mean(Total),2)) %>% 
  spread(ScoringLimit, Percentage)

```
#### Vintage table where the variable of interest is *Payed total / Loan Amount (%)* 

observations are devided in segment, potential and classes

We can see that class 4 and 5 come from riskier segments and are paying less than the other.

On the other hand clients with lower potential but from better classes have better repayment behavior.


```{r echo=FALSE, message=FALSE, warning=FALSE}

complete_table %>%
  mutate(Total = nrow(complete_table)) %>%
  group_by(Period, Potential, Segment, ScoringLimit) %>%
  summarise(RR_from_sumaP = round((sum(PayedTotal) + 0.0001) / sum(SumaP),2)) %>% 
  spread(ScoringLimit, RR_from_sumaP)
```

#### Vintage tables where the variable of interest is *Payed total / Loan Amount (%)* 

two tables are given - one for class from 1 to 4 and one for class 5

segmentation was done additionally on product type

On the table below we can see that in general, clients in same class but with either with higher potential are more profitable and better payers.

Clients from class 5 are paying far less than the rest of the clients.
```{r echo=FALSE, message=FALSE, warning=FALSE}

print("Vintage table with statistics for clients from class 1 to class 4")

complete_table %>% 
  filter(ScoringLimit<=4) %>%
  filter(CreditProduct %in% c("EasyMonth", "EasyCredit", "EasyMax24")) %>%
  group_by(CreditProduct, Period)%>%
  mutate(Total_product_period = n()) %>%
  ungroup()%>%
  group_by(CreditProduct, Period, Potential, Segment) %>%
  summarise(Observations = n()
            , Perc = round(Observations / mean(Total_product_period),2)
            , RR_SumaP = round((sum(PayedTotal) + 0.0001)/sum(SumaP),2)
            , AvgLoanAmount = round(mean(SumaP),0)
            , AverageScoring = round(mean(R_duck_scoring, na.rm = T),2)
            , RR_matured = round((sum(MaturityT) + 0.0001)/sum(SumaP),2)
            , DPD = round(mean(MaxDelayP, na.rm = T),2))

print("Vintage table with statistics for clients from class 5 (class overrides)")

complete_table %>% 
  filter(ScoringLimit==5) %>%
  filter(CreditProduct %in% c("EasyMonth", "EasyCredit", "EasyMax24")) %>%
  group_by(CreditProduct, Period)%>%
  mutate(Total_product_period = n()) %>%
  ungroup()%>%
  group_by(CreditProduct, Period, Potential, Segment) %>%
  summarise(Observations = n()
            , Perc = round(Observations / mean(Total_product_period),2)
            , RR_SumaP = round((sum(PayedTotal) + 0.0001)/sum(SumaP),2)
            , AvgLoanAmount = round(mean(SumaP),0)
            , AverageScoring = round(mean(R_duck_scoring, na.rm = T),2)
            
            , RR_matured = round((sum(MaturityT) + 0.0001)/sum(SumaP),2)
            , DPD = round(mean(MaxDelayP, na.rm = T),2))

```

In this table we can see that class 5 are paying far less than the othe class. 

Additionally, all clients from class 5 have low potential (which means that they have big amount of other credits / debt from financial and bank institutions) and they have difficulties with repaying his/her debt. 

```{r echo=FALSE, message=FALSE, warning=FALSE}

print("Vintage table with ecovery rates from loan amount for clients from class 1 to class 4")

complete_table %>% 
  filter(ScoringLimit<=4) %>%
  filter(CreditProduct %in% c("EasyMonth", "EasyCredit", "EasyMax24")) %>% 
  group_by(CreditProduct, Period)%>%
  mutate(Total_product_period = n()) %>%
  ungroup()%>%
  group_by(Period, CreditProduct, ScoringLimit, Potential, Segment ) %>%
  summarise(RR_SumaP = round((sum(PayedTotal) + 0.0001)/sum(SumaP),2)) %>% 
  spread(Period, RR_SumaP)

print("Vintage table with recovery rates from loan amount for clients from class 5 (class overrides)")

complete_table %>% 
  filter(ScoringLimit==5) %>%
  filter(CreditProduct %in% c("EasyMonth", "EasyCredit", "EasyMax24")) %>% 
  group_by(CreditProduct, Period)%>%
  mutate(Total_product_period = n()) %>%
  ungroup()%>%
  group_by(Period, CreditProduct, ScoringLimit, Potential, Segment ) %>%
  summarise(AvgLoanAmount = round((sum(PayedTotal) + 0.0001)/sum(SumaP),2)) %>% 
  spread(Period, AvgLoanAmount)


```


