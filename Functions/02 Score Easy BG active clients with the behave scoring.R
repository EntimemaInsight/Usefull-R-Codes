
DECLARE_DATE <- "'2019-04-06'"
Report_date <- 20190406
myc <- DBI::dbConnect(odbc::odbc()
                      , driver= "SQL Server"
                      , server = 'hidalgo'
                      , database = "BISmart")

scored_active_credits <-  DBI::dbGetQuery(myc
                          , paste0("declare @val datetime 
                                     set @val =  ",DECLARE_DATE,"
                                     
                                  
                                     SELECT tab2.KID
                                     ,round(1000*(exp(   PaidTillObs_score 
                                     + Passed_Score 
                                     + MaxDelay_Score 
                                     + DPDChange_Score 
                                     + PrevCredits_Score 
                                     + PreviousDefaulted_Score 
                                     + Salaty_Score 
                                     + Gender_Score
                                     + Education_Score 
                                     + Job_score) 
                                     -------------------
                                     / (1 + exp( 
                                     + PaidTillObs_score 
                                     + Passed_Score 
                                     + MaxDelay_Score 
                                     + DPDChange_Score 
                                     + PrevCredits_Score 
                                     + PreviousDefaulted_Score 
                                     + Salaty_Score 
                                     + Gender_Score
                                     + Education_Score 
                                     + Job_score))
                                     )
                                     ,0) BehScore
                                     
                                     FROM (
                                     
                                     SELECT *
                                     ,Case when PaidTillObs in (-99,-999)  then -0.93467
                                     when PaidTillObs <= 0.67		then  0.00000
                                     when PaidTillObs <= 0.80		then -0.43370
                                     when PaidTillObs <= 0.89		then -0.93467
                                     when PaidTillObs <= 1.00		then -1.45906
                                     else								 -1.77958 end PaidTillObs_score
                                     --
                                     ,CASE WHEN Passed <= 0.29 then  0.00000  
                                     WHEN Passed <= 0.42 then -0.65949
                                     WHEN Passed <= 0.58 then -1.10634
                                     WHEN Passed <= 1.00 then -1.41085
                                     END  Passed_Score
                                     
                                     ,CASE WHEN MaxDelay <=  3 then 0.00000  
                                     WHEN MaxDelay <= 10 then 0.31081
                                     WHEN MaxDelay <= 17 then 0.51107
                                     ELSE					    1.06919 END  MaxDelay_Score
                                     
                                     ,CASE WHEN CurrentDelay_current - CurrentDelay_old <=  0 THEN 0.00000
                                     WHEN CurrentDelay_current - CurrentDelay_old <=  8 THEN 0.29203
                                     WHEN CurrentDelay_current - CurrentDelay_old <= 15 THEN 0.32646
                                     WHEN CurrentDelay_current - CurrentDelay_old <= 22 THEN 0.57872
                                     ELSE													   0.75941 END DPDChange_Score
                                     
                                     ,CASE WHEN PrevCreditsCount <= 0  THEN  0.30483
                                     WHEN PrevCreditsCount <= 1  THEN  0.00000
                                     WHEN PrevCreditsCount <= 2  THEN -0.12499
                                     WHEN PrevCreditsCount <= 3  THEN -0.31961
                                     WHEN PrevCreditsCount <= 4  THEN -0.45149
                                     WHEN PrevCreditsCount <= 6  THEN -0.57752
                                     WHEN PrevCreditsCount <= 7  THEN -0.80470
                                     WHEN PrevCreditsCount <= 8  THEN -0.71439
                                     WHEN PrevCreditsCount <= 13 THEN -0.80655
                                     ELSE								-1.02696 END PrevCredits_Score
                                     
                                     ,CASE WHEN PreviousDefaulted = 0  THEN - 0.5712 ELSE 0.0000 END PreviousDefaulted_Score
                                     ,CASE WHEN SalaryPaymentType = 'По сметка в банка' then -0.07856 else 0.00000 end Salaty_Score
                                     ,CASE WHEN Gender = 'Man' then 0.11793 else 0.00000 end Gender_Score
                                     
                                     ,CASE WHEN EducationBG in ( 'Полувисше / Специалист '
                                     ,'Висше магистър'
                                     ,'Висше Бакалавър'
                                     ,'Висше Доктор и др. академични степени'
                                     ,'Висше'
                                     ,'Полувисше/Специалист') THEN -0.65956
                                     WHEN EducationBG in ('Средно') then -0.38419
                                     ELSE								   0.00000 END Education_Score
                                     
                                     ,CASE WHEN TypeOfContract in ('Пенсионер по стаж и възраст неработещ'
                                     , 'Пенсионер по стаж и възраст работещ'
                                     , 'Пенсионер по болест неработещ'
                                     , 'Пенсионер по болест работещ'
                                     , 'Безсрочен трудов договор'
                                     , 'Договор за управление'
                                     , 'Допълнително споразумение' )	THEN -0.33832 
                                     ELSE														  0.00000 END Job_score
                                     
                                     
                                     
                                     FROM (
                                     
                                     select dc.CodeContract as KID
                                     , dc.CreditAccountSK
                                     --, floor(datediff(day, dcl.BirthDate, @val)/365.25) as Age
                                     , dcl.EducationBG
                                     , djd.SalaryPaymentType
                                     , djd.TypeOfContract
                                     , dcl.Gender
                                     , coalesce(inum.InstNum,0) as InstNum
                                     , case when mdp.MaxDelayP > 0 then 1 else 0 end PreviousDefaulted -- CountPreviousBads 
                                     , isnull(PrevCtCount,0) PrevCreditsCount -- count previous credit -- count only completed
                                     , np.MaxDelay  -- Max Delay P
                                     ---
                                     , case when  coalesce(inum.MaturedDebt,0) = 0 and (coalesce(np.InterestPaid,0) + coalesce(np.PrincipalPaid,0))  = 0 then -99 -- 0/0
                                     when  coalesce(inum.MaturedDebt,0) = 0 and (coalesce(np.InterestPaid,0) + coalesce(np.PrincipalPaid,0))  != 0 then -999 -- x/0
                                     else (coalesce(np.InterestPaid,0) + coalesce(np.PrincipalPaid,0)) / inum.MaturedDebt end PaidTillObs
                                     
                                     
                                     , coalesce(inum.InstNum,0) / np.CreditPaymentCount  as Passed
                                     , case when coalesce(np.CurrentDelay,0) <= 0 then 0 else np.CurrentDelay end   CurrentDelay_current -- For current delay fluctuation 
                                     , case when coalesce(op.CurrentDelay,0) <= 0 then 0 else op.CurrentDelay end   CurrentDelay_old -- For current delay fluctuation 
                                     
                                     
                                     from dwh.DimCredit dc
                                     join dwh.DimDate didn on didn.Date  = @val 
                                     join dwh.DimDate dido on dido.Date = dateadd(month, -1, @val)
                                     
                                     ---- Change the fact credit balances dates 
                                     join dwh.FactCreditBalancesCurrent/*CHANGE !!*/ np on dc.CreditSK = np.CreditSK and np.DateSK = didn.DateSK
                                     left join dwh.FactCreditBalances201804 /*CHANGE !!*/ op on dc.CreditSK = op.CreditSK and op.DateSK = dido.DateSK
                                     ----
                                     
                                     join dwh.DimClient dcl on dc.ClientSK = dcl.ClientSK and dcl.latest = 1	
                                     
                                     left join (select CreditAccountSK, SalaryPaymentType, TypeOfContract
                                     , ROW_NUMBER() OVER (PARTITION BY CreditAccountSK ORDER BY ClientJobDetailsID desc) rw_ix
                                     from dwh.DimClientJobDetails
                                     where  EmployerContactType is null
                                     
                                     /*срещу един CreditAccountSK има повече от 1 запис
                                     за да не дублираме редове ... 
                                     филтрираме записа с най-нов (най-висок) ClientJobDetailsID */
                                     
                                     ) djd on dc.CreditAccountSK = djd.CreditAccountSK and rw_ix = 1 --and djd.EmployerContactType is null
                                     left join 
                                     (
                                     select CreditAccountSK
                                     , max(CurrentNumber) InstNum  -- последна падежирала вноска
                                     , sum(coalesce(InterestAccured,0) + coalesce(PrincipalOFPeriod,0)) MaturedDebt
                                     from dwh.DimCreditPlan 
                                     where CurrentDate <= @val
                                     group by CreditAccountSK
                                     
                                     )  inum on dc.CreditAccountSK = inum.CreditAccountSK
                                     
                                     -- Count Previous bads and previous credits
                                     left join (
                                     
                                     SELECT dc.clientsk
                                     , sum(case when MaxDelay <= 90 then 0 else 1 end) MaxDelayP
                                     , count(distinct dc.CreditAccountSK) PrevCtCount
                                     from dwh.DimCredit dc
                                     join dwh.DimDate didn on didn.Date  = @val
                                     join dwh.FactCreditBalancesCurrent/*CHANGE !!*/ np on dc.CreditSK = np.CreditSK and np.DateSK = didn.DateSK
                                     where np.CompleteDate is not null -- only completed loans
                                     and dc.SubStatusSK not in (17,18) -- Rejected/ Denied
                                     group by dc.clientsk
                                     
                                     ) mdp on dc.ClientSK = mdp.ClientSK
                                     
                                     where  dc.CompleteDate is null
                                     and @val between dc.ValidFrom and dc.ValidTo
                                     and dc.SubStatusSK not in (17,18) -- Rejected/ Denied
                                     
                                     ) tab
                                     
                                     )Tab2"))

