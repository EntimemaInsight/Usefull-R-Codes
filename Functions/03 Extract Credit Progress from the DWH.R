DECLARE_DATE <- "'2019-05-21'"

#### 01 Connect to the database ####
myc <- DBI::dbConnect(odbc::odbc()
                      , driver= "SQL Server"
                      , server = 'hidalgo'
                      , database = "BISmart")



CreditProgress <- DBI::dbGetQuery(myc, 
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
                                                   where fcbc.DateSK = d.DateSK 
                                                    -- and (c.ScoringType = 7 or c.ScoringType = 101)
                                                   order by 1,4;"))
