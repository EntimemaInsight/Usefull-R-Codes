-- Parameters Section ---
DECLARE @Sample_Start_Date_Param VARCHAR(20);
SET @Sample_Start_Date_Param = 'Start_Date_Sub' ; -- Minimum Activation Date of the Sample 

DECLARE @Sample_End_Date_Param VARCHAR(20);
SET @Sample_End_Date_Param = 'End_Date_Sub'; -- Minimum Activation Date of the Sample 

DECLARE @Offer_Param VARCHAR(20);
SET @Offer_Param = 'Offer_Sub'; -- OfferSYSID


--Declare variables that will be filled with the actual query as string
DECLARE @FCP_Query VARCHAR(5000) = '',
        @Final_Query VARCHAR(max)

--Construct query to SELECT and UNION ALL FactCardProgress tables
--CHAR(13) is line break and is not necessary
SELECT @FCP_Query = @FCP_Query + 'SELECT * ' + CHAR(13) + ' FROM ' + 'dwh.' + TABLE_NAME + CHAR(13) + ' UNION ALL ' + CHAR(13)
FROM INFORMATION_SCHEMA.TABLES
WHERE TABLE_NAME LIKE 'FactCardProgress%' AND TABLE_SCHEMA = 'dwh' 
--(Note that there are some tables that will match this regex, but they are in schema 'tmp')

-- Remove one extra UNION ALL at the end of the query
SELECT @FCP_Query = LEFT(@FCP_Query, LEN(@FCP_Query) - LEN(' UNION ALL ' + CHAR(13)))

SET @Final_Query = '

with WC_PL_Pmts as				
(				
SELECT offersysid, cast(DocumentDate as Date) as DocumentDate,FORMAT(DocumentDate, ''yyyy-MM-01'') as DocumentMonth, sum(Amount) as Amount, PenaltyInterestMarker		
FROM				
	(SELECT	FEE.offersysid, 		
			cast(FEE.DocumentDate as Date) as DocumentDate, 	
			FEE.Amount, 	
			FEE.FinOperationSK,
			--FEE.DateSK, 	
			PI.PenaltyInterestDate, 	
			CASE	
				WHEN FEE.DocumentDate < PI.PenaltyInterestDate THEN 1
				ELSE 0
			END as PenaltyInterestMarker	
	FROM dwh.FactFinanceOperations FEE
	LEFT JOIN dwh.DimFinOperation DFO on FEE.FinOperationSK = DFO.FinOperationSK			
LEFT JOIN 				
(SELECT min(FT.PenaltyInterestDate) as PenaltyInterestDate,				
	   format(FT.PenaltyInterestDate, ''yyyy-MM'') as PenaltyInterestMonthMarker			
FROM				
  (SELECT min(DocumentDate) AS PenaltyInterestDate,				
          WDP.ClientsWithPenalty				
   FROM				
     (SELECT DP.DocumentDate AS DocumentDate,				
             DP.ClientsWithPenalty,				
             max(DP.ClientsWithPenalty) OVER (PARTITION BY format(DP.DocumentDate, ''yyyy-MM'')) AS MaxClientsWithPenalty				
      FROM				
        (SELECT DocumentDate,				
                Count(*) AS ClientsWithPenalty				
         FROM dwh.FactExtraExpenses	EE
		 left join dwh.DimFinOperation DFO on EE.FinOperationSK = DFO.FinOperationSK			
         WHERE DFO.DID = 800				
		 AND day(DocumentDate) != 1		
         GROUP BY DocumentDate				
         HAVING DocumentDate >= ''20170201''				
         ) AS DP) AS WDP				
   WHERE WDP.ClientsWithPenalty = WDP.MaxClientsWithPenalty				
   GROUP BY WDP.ClientsWithPenalty, DocumentDate) AS FT				
group by format(FT.PenaltyInterestDate, ''yyyy-MM'')) as PI on format(FEE.DocumentDate, ''yyyy-MM'') = PI.PenaltyInterestMonthMarker				
   WHERE DocumentDate >= ''20180701'' AND DID = 99 				
   ) as PP
   group by offersysid, cast(DocumentDate as Date), PenaltyInterestMarker	  				
--WHERE PP.PenaltyInterestMarker = 1				
--GROUP BY OfferSK, format(PP.DocumentDate, ''yyyy-MM-01'')				
),

PF_Raw as				
('+@FCP_Query +'),	

PF as
(
select P.*
      , C.OfferSYSID
from PF_Raw P
left join dwh.DimCardsHist C on P.CardID = C.CardID
where C.Latest = 1
),


Due_Sums as				
(				
select PF.OfferSYSID				
      , PF.CurrentMPV				
	  --, PF.MPVCurrentMonth			
	  --, PF.PaidMPVCurrentMonth
	  --, PF.DateSK			
	  , cast(CDate as Date) CDate			
	  , CurrentLimit			
	  , TotalDue
	  --, D.DayOfMonth			
from PF				
left join BIsmartWCBG.dwh.DimDate D on cast(PF.CDATE as Date) = cast(D.Date as Date)			
where D.DayOfMonth = 1				
),

First_Date_Col as
(
select Date as FirstObsPoint
      , Month(Date) as CurrentMonth1
	  , Year(Date) as CurrentYear1
from BIsmartWCBG.dwh.DimDate
where Date >= ''2020-01-01''
and Date <= (select EOMONTH(getdate()))
),

Second_Date_Col as
(
select Date as NextObsPoint
       , Month(Date) as CurrentMonth2
	   , Year(Date) as CurrentYear2
from BIsmartWCBG.dwh.DimDate
where Date >= ''2020-01-01''
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
           FORMAT(FT.PenaltyInterestDate, ''yyyy-MM-01'') as PenaltyInterestMonth		 	
FROM				
  (SELECT min(DocumentDate) AS PenaltyInterestDate,				
          WDP.ClientsWithPenalty				
   FROM				
     (SELECT DP.DocumentDate AS DocumentDate,				
             DP.ClientsWithPenalty,				
             max(DP.ClientsWithPenalty) OVER (PARTITION BY format(DP.DocumentDate, ''yyyy-MM'')) AS MaxClientsWithPenalty				
      FROM				
        (SELECT DocumentDate,				
                Count(*) AS ClientsWithPenalty				
         FROM dwh.FactExtraExpenses EE
		 left join dwh.DimFinOperation FO on EE.FinOperationSK = FO.FinOperationSK				
         WHERE FO.DID = 800				
		 AND day(DocumentDate) != 1			 
         GROUP BY DocumentDate				
         HAVING DocumentDate >= ''20170201''				
         ) AS DP) AS WDP				
   WHERE WDP.ClientsWithPenalty = WDP.MaxClientsWithPenalty				
   GROUP BY WDP.ClientsWithPenalty, DocumentDate) AS FT				
group by FORMAT(FT.PenaltyInterestDate, ''yyyy-MM-01'')
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
	  , FORMAT(O.DateInserted, ''yyyy-MM-01'') as MonthInserted			
	  , datediff(month,FORMAT(O.DateInserted, ''yyyy-MM-01''), D.CDate) as MOB
	  , DD.DayOfMonth
	  , MDF.PenaltyInterestPeriodFlag	
from Due_Sums D	
left join Helper_Dates_table HD on D.CDate = HD.FirstObsPoint 
left join Last_MAD_Date_Formatted MDF on HD.NextObsPoint = MDF.NextObsPoint
left join BIsmartWCBG.dwh.DimDate DD on HD.NextObsPoint = DD.Date			
left join WC_PL_Pmts P on D.OfferSYSID = P.offersysid and HD.NextObsPoint = P.DocumentDate				
left join (select * from dwh.DimOfferHist where Latest = 1) O on D.OfferSYSID = O.OfferSYSID	
where DateInserted >= ''2020-01-01''				
)	
	

select C.*
       , P.Name as Product
	   , sum(Payment) OVER(PARTITION BY C.OfferSYSID, C.CDate order by C.NextObsPoint DESC ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) AS CumulativePayment
	   , max(C.NextObsPoint) OVER(PARTITION BY C.OfferSYSID, C.CDate, PenaltyInterestPeriodFlag) as MaxDate
	   , IIF(C.CurrentMPV > 0,1,0) as Has_MAD_Due
	   , IIF(sum(Payment) OVER(PARTITION BY C.OfferSYSID, C.CDate order by C.NextObsPoint DESC ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING) >= CurrentMPV,1,0) as Paid_MAD
	   , IIF(max(C.NextObsPoint) OVER(PARTITION BY C.OfferSYSID, C.CDate, PenaltyInterestPeriodFlag) = C.NextObsPoint and PenaltyInterestPeriodFlag = 1,1,0) as Last_MDP_Date_Flag
from Combined C
left join (select * from dwh.DimOfferHist where Latest = 1) O on C.OfferSYSID = O.OfferSYSID
left join dwh.DimProduct P on O.ProductSK = P.ProductSK
where MOB <= 1
and NextObsPoint <= cast(getdate()-1 as Date)
and C.DateInserted >= '''+ @Sample_Start_Date_Param +'''
and C.DateInserted <= '''+ @Sample_End_Date_Param +'''
and C.OfferSYSID = '+ @Offer_Param + '
'
EXEC (@Final_Query)