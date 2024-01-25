
DECLARE_DATE <- "'2019-04-06'"
Report_date <- 20190406
myc <- DBI::dbConnect(odbc::odbc()
                      , driver= "SQL Server"
                      , server = 'hidalgo'
                      , database = "BISmart")

credit_characteristics <- DBI::dbGetQuery(myc
                                          , paste0("DECLARE @val datetime 
                                                   SET @val =  ",DECLARE_DATE,";
                                                   
                                                   
                                                   DECLARE @val1  int
                                                   SET @val1 =  ",Report_date,";
                                                   
                                                   WITH CREDITPROGRESS AS
                                                   (SELECT DateSK, CreditAccountSK, CreditSK, CreditPayment, CreditAnnualInterest, CreditPrincipal, CreditInterest
                                                   , (CreditPrincipal + CreditInterest) AS SumToGetBack
                                                   , CreditPaymentCount, PrincipalPaid, InterestPaid, PenaltyPaid, Overpaid, CurrentDelay, MaxDelay
                                                   , (PrincipalPaid + InterestPaid + PenaltyPaid + Overpaid)  AS PaidTotal
                                                   , (PrincipalPaid + InterestPaid)  AS PaidTotal2
                                                   
                                                   FROM dwh.FactCreditBalancesCurrent 
                                                   WHERE DateSK = @val1 and CompleteDate is NULL
                                                   )
                                                   
                                                   
                                                   SELECT CodeContract
                                                   , e.EGN
                                                   , b.* 
                                                   , Scoring, ScoringType, ScoringLimit, DayDelay
                                                   , CreditBeginDate, Sum
                                                   , RefinanceType,CompleteDate 
                                                   , c.ProductName
                                                   , d.Name
                                                   , a.CreditLastDate
                                                   , st.StateBG, st.StateEN 
                                                   FROM dwh.DimCredit a LEFT JOIN CREDITPROGRESS b
                                                   ON a.CreditAccountSK = b.CreditAccountSK AND a.CreditSK = b.CreditSK
                                                   
                                                   LEFT JOIN dwh.DimProduct c 
                                                   ON c.ProductSK = a.ProductSK
                                                   
                                                   LEFT JOIN dwh.DimPeriod d
                                                   ON d.PeriodSK = a.PeriodSK
                                                   
                                                   LEFT JOIN dwh.DimClient e
                                                   ON e.ClientSK = a.ClientSK
                                                   
                                                   LEFT JOIN dwh.DimCreditState st
                                                   ON a.StateSK = st.CreditStateSK
                                                   
                                                   WHERE a.CompleteDate IS NULL AND a.Latest = 1 AND 
                                                   a.CreditBeginDate IS NOT NULL AND @val BETWEEN a.ValidFrom and a.ValidTo"))
