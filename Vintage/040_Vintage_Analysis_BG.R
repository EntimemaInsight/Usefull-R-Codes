library(tidyverse)
library(readxl)
library(lubridate)

# Load data
myc <- DBI::dbConnect(odbc::odbc()
                      , driver = "SQL Server"
                      , server = "Scorpio.smartitbg.int"
                      , database = "BIsmartWCBG"
)

Query <- paste0("with WC_PL_Pmts as				
(				
SELECT OfferSK, cast(DocumentDate as Date) as DocumentDate,FORMAT(DocumentDate, 'yyyy-MM-01') as DocumentMonth, sum(Amount) as Amount, PenaltyInterestMarker, PP.DateSK		
FROM				
	(SELECT	FEE.OfferSK, 		
			FEE.DocumentDate, 	
			FEE.Amount, 	
			FEE.FinOperationSK,
			FEE.DateSK, 	
			PI.PenaltyInterestDate, 	
			CASE	
				WHEN FEE.DocumentDate < PI.PenaltyInterestDate THEN 1
				ELSE 0
			END as PenaltyInterestMarker	
	FROM dwh.FactFinanceOperations FEE
	LEFT JOIN dwh.DimFinOperation DFO on FEE.FinOperationSK = DFO.FinOperationSK			
LEFT JOIN 				
(SELECT min(FT.PenaltyInterestDate) as PenaltyInterestDate,				
	   format(FT.PenaltyInterestDate, 'yyyy-MM') as PenaltyInterestMonthMarker			
FROM				
  (SELECT min(DocumentDate) AS PenaltyInterestDate,				
          WDP.ClientsWithPenalty				
   FROM				
     (SELECT DP.DocumentDate AS DocumentDate,				
             DP.ClientsWithPenalty,				
             max(DP.ClientsWithPenalty) OVER (PARTITION BY format(DP.DocumentDate, 'yyyy-MM')) AS MaxClientsWithPenalty				
      FROM				
        (SELECT DocumentDate,				
                Count(*) AS ClientsWithPenalty				
         FROM dwh.FactExtraExpenses	EE
		 left join dwh.DimFinOperation DFO on EE.FinOperationSK = DFO.FinOperationSK			
         WHERE DFO.DID = 800				
		 AND day(DocumentDate) != 1		
         GROUP BY DocumentDate				
         HAVING DocumentDate >= '20170201'				
         ) AS DP) AS WDP				
   WHERE WDP.ClientsWithPenalty = WDP.MaxClientsWithPenalty				
   GROUP BY WDP.ClientsWithPenalty, DocumentDate) AS FT				
group by format(FT.PenaltyInterestDate, 'yyyy-MM')) as PI on format(FEE.DocumentDate, 'yyyy-MM') = PI.PenaltyInterestMonthMarker				
   WHERE DocumentDate >= '20180701' AND DID = 99 				
   ) as PP
   group by OfferSK, DocumentDate, PenaltyInterestMarker, PP.DateSK	  				
--WHERE PP.PenaltyInterestMarker = 1				
--GROUP BY OfferSK, format(PP.DocumentDate, 'yyyy-MM-01')				
),

PF as				
(			
select *				
from dwh.FactCardProgress2021			
union all 				
select *				
from dwh.FactCardProgressCurrent				
),	

Due_Sums as				
(				
select PF.OfferSK				
      , PF.CurrentMPV				
	  --, PF.MPVCurrentMonth			
	  --, PF.PaidMPVCurrentMonth
	  , PF.DateSK			
	  , cast(CDate as Date) CDate			
	  , CurrentLimit			
	  , TotalDue
	  --, D.DayOfMonth			
from PF				
left join dwh.DimDate D on PF.DateSK = D.DateSK				
where D.DayOfMonth = 1				
),

First_Date_Col as
(
select Date as FirstObsPoint
      , Month(Date) as CurrentMonth1
	  , Year(Date) as CurrentYear1
from dwh.DimDate
where Date >= '2021-01-01'
and Date <= (select EOMONTH(getdate()))
),

Second_Date_Col as
(
select Date as NextObsPoint
       , Month(Date) as CurrentMonth2
	   , Year(Date) as CurrentYear2
from dwh.DimDate
where Date >= '2021-01-01'
and Date <= (select EOMONTH(getdate()))
),

Helper_Dates_table as
(
select FirstObsPoint, NextObsPoint
from First_Date_Col, Second_Date_Col
where CurrentMonth1 = CurrentMonth2
and CurrentYear1 = CurrentYear2
and Day(FirstObsPoint) = 1
),

Last_MAD_Date as
(
SELECT min(FT.PenaltyInterestDate) as PenaltyInterestDate,	
           FORMAT(FT.PenaltyInterestDate, 'yyyy-MM-01') as PenaltyInterestMonth		 	
FROM				
  (SELECT min(DocumentDate) AS PenaltyInterestDate,				
          WDP.ClientsWithPenalty				
   FROM				
     (SELECT DP.DocumentDate AS DocumentDate,				
             DP.ClientsWithPenalty,				
             max(DP.ClientsWithPenalty) OVER (PARTITION BY format(DP.DocumentDate, 'yyyy-MM')) AS MaxClientsWithPenalty				
      FROM				
        (SELECT DocumentDate,				
                Count(*) AS ClientsWithPenalty				
         FROM dwh.FactExtraExpenses EE
		 left join dwh.DimFinOperation FO on EE.FinOperationSK = FO.FinOperationSK				
         WHERE FO.DID = 800				
		 AND day(DocumentDate) != 1			 
         GROUP BY DocumentDate				
         HAVING DocumentDate >= '20170201'				
         ) AS DP) AS WDP				
   WHERE WDP.ClientsWithPenalty = WDP.MaxClientsWithPenalty				
   GROUP BY WDP.ClientsWithPenalty, DocumentDate) AS FT				
group by FORMAT(FT.PenaltyInterestDate, 'yyyy-MM-01')
),

Last_MAD_Date_Formatted as 
(
select NextObsPoint
      , case when day(PenaltyInterestDate) > day(NextObsPoint) + 1
	  then 1
	  else 0
	  end as PenaltyInterestPeriodFlag
from Last_MAD_Date D
left join Helper_Dates_table H on D.PenaltyInterestMonth = H.FirstObsPoint
--order by FirstObsPoint
),

Combined as				
(				
select D.*	
      , HD.NextObsPoint
      , O.OfferID			
      , case 
	     when P.Amount	is null
		 then 0
		 when P.Amount < 0
		 then 0
		 else P.Amount
		 end as Payment	
		 , P.DocumentDate
		 , P.DocumentMonth
	  , cast(O.DateInserted	as Date) as DateInserted		
	  , FORMAT(O.DateInserted, 'yyyy-MM-01') as MonthInserted			
	  , datediff(month,FORMAT(O.DateInserted, 'yyyy-MM-01'), D.CDate) as MOB
	  , DD.DayOfMonth
	  , MDF.PenaltyInterestPeriodFlag	
from Due_Sums D	
left join Helper_Dates_table HD on D.CDate = HD.FirstObsPoint 
left join Last_MAD_Date_Formatted MDF on HD.NextObsPoint = MDF.NextObsPoint
left join dwh.DimDate DD on HD.NextObsPoint = DD.Date			
left join WC_PL_Pmts P on D.OfferSK = P.OfferSK and cast(HD.NextObsPoint as Date) = cast(P.DocumentDate as Date)				
left join dwh.DimOffers O on D.OfferSK = O.OfferSK	
where DateInserted >= '2021-01-01'				
)	
	

select C.*
       , P.Name as Product
	   , sum(Payment) OVER(PARTITION BY C.OfferSK, C.CDate order by C.NextObsPoint DESC ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) AS CumulativePayment
	   , max(C.NextObsPoint) OVER(PARTITION BY C.OfferSK, C.CDate, PenaltyInterestPeriodFlag) as MaxDate
	   , IIF(C.CurrentMPV > 0,1,0) as Has_MAD_Due
	   , IIF(sum(Payment) OVER(PARTITION BY C.OfferSK, C.CDate order by C.NextObsPoint DESC ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) >= CurrentMPV,1,0) as Paid_MAD
	   , IIF(max(C.NextObsPoint) OVER(PARTITION BY C.OfferSK, C.CDate, PenaltyInterestPeriodFlag) = C.NextObsPoint and PenaltyInterestPeriodFlag = 1,1,0) as Last_MDP_Date_Flag
from Combined C
left join dwh.DimOffers O on C.OfferSK = O.OfferSK
left join dwh.DimProduct P on O.ProductSK = P.ProductSK
where MOB <= 2
and NextObsPoint <= cast(getdate()-2 as Date)
--and C.OfferSK = 91399")

start_time <- Sys.time()
WC_BG_Vintage_Detailed <- DBI::dbFetch(DBI::dbSendQuery(myc, Query))
print(paste("Done in", round(difftime(Sys.time(), start_time,unit = "mins"),2), "minutes"))

#Close the database connection
DBI::dbDisconnect(myc)

myc <- DBI::dbConnect(odbc::odbc()
                      , driver = "SQL Server"
                      , server = "Scorpio.smartitbg.int"
                      , database = "BIsmartWCBG"
)

Query = paste0("Select * from dwh.DimOffers")

start_time <- Sys.time()
DimOffers <- DBI::dbFetch(DBI::dbSendQuery(myc, Query))
print(paste("Done in", round(difftime(Sys.time(), start_time,unit = "mins"),2), "minutes"))

#Close the database connection
DBI::dbDisconnect(myc)


Vintage_DWH_June_2022_WC = WC_BG_Vintage_Detailed %>%
  filter(as.Date(MonthInserted) == as.Date("2022-06-01")
         & MOB == 1
         & Has_MAD_Due == 1
         & DayOfMonth == 31
         & Product %in% c("Бяла Карта","Бяла Карта – само лихва")
         )


Vintage_BAR_June_2022_WC = read_xlsx("./Input Data/WC BG Vintage Joro June 2022 Cohort.xlsx")

Vintage_BAR_June_2022_WC_Plus_DimOffers = Vintage_BAR_June_2022_WC %>%
  left_join(DimOffers, by = c("CID" = "ContractNumber")) %>%
  mutate(MonthInserted = floor_date(as.Date(DateInserted), unit = "months")) %>%
  filter(MonthInserted == as.Date("2022-06-01"))

sum(Vintage_BAR_June_2022_WC_Plus_DimOffers$Paid_customers)/nrow(Vintage_BAR_June_2022_WC_Plus_DimOffers)




















