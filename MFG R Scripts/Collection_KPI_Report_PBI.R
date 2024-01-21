library(readxl)
library(writexl)
library(lubridate)
library(openxlsx)
library(tidyr)
library(dplyr)
library(odbc)
library(DBI)

#Colletion BG
{
#WhiteCard
{
#SQL
{
  myc <- DBI::dbConnect(odbc::odbc()
                        , driver = "SQL Server"
                        , server = "scorpio.smartitbg.int"
                        , database = "BIsmartWCBG"
  )
  
  Payments_all_BG <- paste0(" 
declare @CurrentDate date = getdate();
declare @FirstOfMonth date = case when day(getdate())=1 then DATEADD(DAY,1,EOMONTH(@CurrentDate,-2)) else DATEADD(DAY,1,EOMONTH(@CurrentDate,-1)) end; 

		SELECT EasyClientNumber
		,CONVERT(DATE, CDATE) AS [Date]
		, Product
		, SUM(Amount) AS Amount
		, tr
		FROM (SELECT DISTINCT EasyClientNumber
		, CASE 
			WHEN p.Name = 'РРђРњ'
				THEN 'WhiteCard'
			WHEN p.Name = 'Visa Р‘СЏР»Р° РєР°СЂС‚Р°'
				THEN 'WhiteCard'
			WHEN p.Name = '180 РґРЅРё Visa Р‘СЏР»Р° РљР°СЂС‚Р°'
				THEN 'WhiteCard'
			WHEN p.Name = 'РђРІР°РЅСЃ Р‘Р“ РџРѕС‰Рё'
				THEN 'WhiteCard'
			WHEN p.Name = 'Visa Free ATM World'
				THEN 'WhiteCard'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° Test'
				THEN 'WhiteCard'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р°'
				THEN 'WhiteCard'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° FastCash'
				THEN 'WhiteCard'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° вЂ“ СЃР°РјРѕ Р»РёС…РІР°'
				THEN 'WhiteCard'
			WHEN p.Name = 'Visa Р‘СЏР»Р° РљР°СЂС‚Р° - SC'
				THEN 'WhiteCard'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° Gold - 10%'
				THEN 'WhiteCard-Gold'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° Gold - 5%'
				THEN 'WhiteCard-Gold'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° Gold-MFG-5%'
				THEN 'WhiteCard-Gold'
			WHEN p.Name = 'AXI 2'
				THEN 'WhiteCard-Gold'
			WHEN p.Name = 'AXI 2-500'
				THEN 'WhiteCard-Gold' 
			WHEN p.Name = 'Axi Card - test'
				THEN 'WhiteCard-Gold' 
			WHEN p.Name = 'A1 Credit 1'
				THEN 'A1 Credit'
			WHEN p.Name = 'A1 Credit 2'
				THEN 'A1 Credit'
			WHEN p.Name = 'Рђ1 РїРѕС‚СЂРµР±РёС‚РµР»СЃРєРё Р‘РµР· Р“Р”'
				THEN 'A1 Consumer Loan-Cheap'
			WHEN p.Name = 'Рђ1 РїРѕС‚СЂРµР±РёС‚РµР»СЃРєРё Р“Р”'
				THEN 'A1 Consumer Loan-Expensive'
			WHEN p.Name = 'Axi Credit 2'
				THEN 'Axi Credit'
			WHEN p.Name = 'Axi Credit 3'
				THEN 'Axi Credit'
			WHEN p.Name = 'Axi Credit 4'
				THEN 'Axi Credit'
			WHEN p.Name = 'Axi Credit 4 - paper'
				THEN 'Axi Credit'
			ELSE p.Name
		END AS Product
		, fo.CDATE
		, Amount
		, RANK() OVER(PARTITION BY EasyClientNumber ORDER BY ActivationDate ASC) AS tr
					FROM dwh.FactFinanceOperations fo
					JOIN dwh.DimCards dc ON dc.OfferSK = fo.OfferSK
				    JOIN dwh.DimOffers do ON do.OfferSK = dc.OfferSK
					JOIN dwh.DimProduct p ON p.ProductSK = do.ProductSK
					WHERE FinOperationSK = '73' 
						--AND CONVERT(DATE, fo.CDATE, 4) BETWEEN '2022-04-01' AND DATEADD(day, -1, CAST(GETDATE() AS date))
						AND CONVERT(DATE, fo.CDATE, 4) >= @FirstOfMonth 
						AND CONVERT(DATE, fo.CDATE, 4) <= @CurrentDate
						AND ActivationDate IS NOT NULL
						--AND EasyClientNumber = 555004
		) AS Result
		where tr = 1
		  GROUP BY Product
		  , CONVERT(DATE, CDATE)
		  , EasyClientNumber
		  , tr
		-- ORDER BY EasyClientNumber, Date

")
  start_time <- Sys.time()
  Payments_all_BG <- DBI::dbFetch(DBI::dbSendQuery(myc, Payments_all_BG))
  print(paste("Done in", round(difftime(Sys.time(), start_time,unit = "mins"),2), "minutes"))
  
  #Close the database connection
  DBI::dbDisconnect(myc)
}

#Date Table
{
FirstDateOfMonth = min(as.Date(Payments_all_BG$Date))

Yesterday = Sys.Date()-1

Dates_DF = data.frame(FirstDateOfMonth = as.Date(FirstDateOfMonth)
                      ,NextDate = seq(as.Date(FirstDateOfMonth)
                                      , ceiling_date(as.Date(FirstDateOfMonth)
                                                     , unit = "months")-1
                                      , by="days"))
}

#MaturedClients
{
  files_MaturedClients_BG <- list.files(path = "\\\\cow.easycredit.bg/FileServer/White Card/Reports/Week_report/KPI_Collection_PBI/R_Script_Colletion_Automation/Input Data/Matured_Clients/Matured_BG"
                                        , pattern = "*.xlsx"
                                        , full.names = T)
  
  MaturedClients_BG <- sapply(files_MaturedClients_BG, read_excel, simplify=FALSE) %>% 
    bind_rows() %>%
    select(Product, CID, Bucket = `Bucket NEW`, MaturedMDPs = MDPNew) %>%
    mutate(MaturedMDPs = as.numeric(MaturedMDPs)) %>%
    ungroup() %>%
    mutate(FirstDateOfMonth = FirstDateOfMonth)
  
}

#Base
{
  Base_Colletion = MaturedClients_BG %>%
    left_join(Dates_DF, by = "FirstDateOfMonth") %>%
    left_join(Payments_all_BG %>% mutate(Date = as.Date(Date)) 
              , by = c("CID" = "EasyClientNumber"
                       ,"NextDate" = "Date" )) %>%
    mutate(Product.x = ifelse(Product.x == "Visa Free ATM World", "WhiteCard"
                              ,ifelse(Product.x == "Visa Р‘СЏР»Р° РєР°СЂС‚Р°", "WhiteCard"
                                      ,ifelse(Product.x == "Р‘СЏР»Р° РљР°СЂС‚Р°", "WhiteCard"
                                              ,ifelse(Product.x == "РРђРњ", "WhiteCard"
                                                      ,ifelse(Product.x == "Р‘СЏР»Р° РљР°СЂС‚Р° вЂ“ СЃР°РјРѕ Р»РёС…РІР°", "WhiteCard-Only Intrest"
                                                              ,ifelse(Product.x == "AXI 2", "WhiteCard-Gold"
                                                                      ,ifelse(Product.x == "AXI 2-500", "WhiteCard-Gold"
                                                                              ,ifelse(Product.x == "Р‘СЏР»Р° РљР°СЂС‚Р° Gold - 10%", "WhiteCard-Gold"
                                                                                      ,ifelse(Product.x == "Р‘СЏР»Р° РљР°СЂС‚Р° Gold - 5%", "WhiteCard-Gold"
                                                                                              ,ifelse(Product.x == "Р‘СЏР»Р° РљР°СЂС‚Р° Gold-MFG-5%", "WhiteCard-Gold"
                                                                                                      ,Product.x))))))))))) %>%
    mutate(Amount = ifelse(is.na(Amount),0,Amount)) %>% 
    arrange(CID, NextDate) %>%
    group_by(CID) %>%
    mutate(cumulative = cumsum(Amount)) %>%
    ungroup() %>%
    mutate(Is_paid_Customer = ifelse(MaturedMDPs <= cumulative,1,0)
           , Is_paid_mdp = ifelse(Is_paid_Customer == 1, MaturedMDPs, 0)) %>%
    arrange(CID) %>%
    group_by(CID) %>%
    mutate(Double_payment_CHECK = cumsum(Is_paid_Customer)) %>%
    ungroup() 
}

#Colletion for PBI
{
  
  Collection_WhiteCard_PBIReport = Base_Colletion %>%
    filter(Product.x %in% c("WhiteCard", "WhiteCard-Only Intrest")
           ,NextDate >= FirstDateOfMonth & NextDate <= Yesterday) %>%
    mutate(Product.x = ifelse(Product.x == "WhiteCard-Only Intrest","WhiteCard", Product.x)) %>%
    mutate(MaturedClients = MaturedMDPs)%>%
    group_by(Product.x, NextDate, Bucket) %>%
    summarise(Is_paid_mdp = sum(Is_paid_mdp)
              ,Is_paid_Customer = sum(Is_paid_Customer)
              ,MaturedMDPs = sum(MaturedMDPs)
              ,MaturedClients = length(MaturedClients)) %>%
    select(Product = Product.x
           , Date = NextDate
           , Buckets = Bucket
           , `Count of CID` = MaturedClients
           , `Sum of Paid_customers` = Is_paid_Customer
           , `Sum of MDP` = MaturedMDPs
           , `Sum of Paid_MDP` = Is_paid_mdp) %>%
    mutate(Paid_customers = `Sum of Paid_customers` / `Count of CID`
           ,Paid_MDP = `Sum of Paid_MDP` / `Sum of MDP`) %>%
    arrange(Date)
  
  write_xlsx(Collection_WhiteCard_PBIReport, paste0("\\\\cow.easycredit.bg/FileServer/White Card/Reports/Week_report/KPI_Collection_PBI/R_Script_Colletion_Automation/Output Data/Colletion_Files_PBI/Colletion_WhiteCard/Collection_WhiteCard_PBIReport_", FirstDateOfMonth,".xlsx"))
  
}

files_Colletion_WhiteCard <- list.files(path = "\\\\cow.easycredit.bg/FileServer/White Card/Reports/Week_report/KPI_Collection_PBI/R_Script_Colletion_Automation/Output Data/Colletion_Files_PBI/Colletion_WhiteCard"
                                      , pattern = "*.xlsx"
                                      , full.names = T)

Colletion_PBI_WhiteCard<- sapply(files_Colletion_WhiteCard, read_excel, simplify=FALSE) %>%
  bind_rows()
}

#WhiteCard_Gold
{
  #SQL
  {
    myc <- DBI::dbConnect(odbc::odbc()
                          , driver = "SQL Server"
                          , server = "scorpio.smartitbg.int"
                          , database = "BIsmartWCBG"
    )
    
    Payments_all_BG <- paste0(" 
declare @CurrentDate date = getdate();
declare @FirstOfMonth date = case when day(getdate())=1 then DATEADD(DAY,1,EOMONTH(@CurrentDate,-2)) else DATEADD(DAY,1,EOMONTH(@CurrentDate,-1)) end; 

		SELECT EasyClientNumber
		,CONVERT(DATE, CDATE) AS [Date]
		, Product
		, SUM(Amount) AS Amount
		, tr
		FROM (SELECT DISTINCT EasyClientNumber
		, CASE 
			WHEN p.Name = 'РРђРњ'
				THEN 'WhiteCard'
			WHEN p.Name = 'Visa Р‘СЏР»Р° РєР°СЂС‚Р°'
				THEN 'WhiteCard'
			WHEN p.Name = '180 РґРЅРё Visa Р‘СЏР»Р° РљР°СЂС‚Р°'
				THEN 'WhiteCard'
			WHEN p.Name = 'РђРІР°РЅСЃ Р‘Р“ РџРѕС‰Рё'
				THEN 'WhiteCard'
			WHEN p.Name = 'Visa Free ATM World'
				THEN 'WhiteCard'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° Test'
				THEN 'WhiteCard'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р°'
				THEN 'WhiteCard'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° FastCash'
				THEN 'WhiteCard'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° вЂ“ СЃР°РјРѕ Р»РёС…РІР°'
				THEN 'WhiteCard'
			WHEN p.Name = 'Visa Р‘СЏР»Р° РљР°СЂС‚Р° - SC'
				THEN 'WhiteCard'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° Gold - 10%'
				THEN 'WhiteCard-Gold'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° Gold - 5%'
				THEN 'WhiteCard-Gold'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° Gold-MFG-5%'
				THEN 'WhiteCard-Gold'
			WHEN p.Name = 'AXI 2'
				THEN 'WhiteCard-Gold'
			WHEN p.Name = 'AXI 2-500'
				THEN 'WhiteCard-Gold' 
			WHEN p.Name = 'Axi Card - test'
				THEN 'WhiteCard-Gold' 
			WHEN p.Name = 'A1 Credit 1'
				THEN 'A1 Credit'
			WHEN p.Name = 'A1 Credit 2'
				THEN 'A1 Credit'
			WHEN p.Name = 'Рђ1 РїРѕС‚СЂРµР±РёС‚РµР»СЃРєРё Р‘РµР· Р“Р”'
				THEN 'A1 Consumer Loan-Cheap'
			WHEN p.Name = 'Рђ1 РїРѕС‚СЂРµР±РёС‚РµР»СЃРєРё Р“Р”'
				THEN 'A1 Consumer Loan-Expensive'
			WHEN p.Name = 'Axi Credit 2'
				THEN 'Axi Credit'
			WHEN p.Name = 'Axi Credit 3'
				THEN 'Axi Credit'
			WHEN p.Name = 'Axi Credit 4'
				THEN 'Axi Credit'
			WHEN p.Name = 'Axi Credit 4 - paper'
				THEN 'Axi Credit'
			ELSE p.Name
		END AS Product
		, fo.CDATE
		, Amount
		, RANK() OVER(PARTITION BY EasyClientNumber ORDER BY ActivationDate ASC) AS tr
					FROM dwh.FactFinanceOperations fo
					JOIN dwh.DimCards dc ON dc.OfferSK = fo.OfferSK
				    JOIN dwh.DimOffers do ON do.OfferSK = dc.OfferSK
					JOIN dwh.DimProduct p ON p.ProductSK = do.ProductSK
					WHERE FinOperationSK = '73' 
						--AND CONVERT(DATE, fo.CDATE, 4) BETWEEN '2022-04-01' AND DATEADD(day, -1, CAST(GETDATE() AS date))
						AND CONVERT(DATE, fo.CDATE, 4) >= @FirstOfMonth 
						AND CONVERT(DATE, fo.CDATE, 4) <= @CurrentDate
						AND ActivationDate IS NOT NULL
						--AND EasyClientNumber = 555004
		) AS Result
		where tr = 1
		  GROUP BY Product
		  , CONVERT(DATE, CDATE)
		  , EasyClientNumber
		  , tr
		-- ORDER BY EasyClientNumber, Date

")
    start_time <- Sys.time()
    Payments_all_BG <- DBI::dbFetch(DBI::dbSendQuery(myc, Payments_all_BG))
    print(paste("Done in", round(difftime(Sys.time(), start_time,unit = "mins"),2), "minutes"))
    
    #Close the database connection
    DBI::dbDisconnect(myc)
  }
  
  #Date Table
  {
    FirstDateOfMonth = min(as.Date(Payments_all_BG$Date))
    
    Yesterday = Sys.Date()-1
    
    Dates_DF = data.frame(FirstDateOfMonth = as.Date(FirstDateOfMonth)
                          ,NextDate = seq(as.Date(FirstDateOfMonth)
                                          , ceiling_date(as.Date(FirstDateOfMonth)
                                                         , unit = "months")-1
                                          , by="days"))
  }
  
  #MaturedClients
  {
    files_MaturedClients_BG <- list.files(path = "\\\\cow.easycredit.bg/FileServer/White Card/Reports/Week_report/KPI_Collection_PBI/R_Script_Colletion_Automation/Input Data/Matured_Clients/Matured_BG"
                                          , pattern = "*.xlsx"
                                          , full.names = T)
    
    MaturedClients_BG <- sapply(files_MaturedClients_BG, read_excel, simplify=FALSE) %>% 
      bind_rows() %>%
      select(Product, CID, Bucket = `Bucket NEW`, MaturedMDPs = MDPNew) %>%
      mutate(MaturedMDPs = as.numeric(MaturedMDPs)) %>%
      ungroup() %>%
      mutate(FirstDateOfMonth = FirstDateOfMonth)
    
  }
  
  #Base
  {
    Base_Colletion = MaturedClients_BG %>%
      left_join(Dates_DF, by = "FirstDateOfMonth") %>%
      left_join(Payments_all_BG %>% mutate(Date = as.Date(Date)) 
                , by = c("CID" = "EasyClientNumber"
                         ,"NextDate" = "Date" )) %>%
      mutate(Product.x = ifelse(Product.x == "Visa Free ATM World", "WhiteCard"
                                ,ifelse(Product.x == "Visa Р‘СЏР»Р° РєР°СЂС‚Р°", "WhiteCard"
                                        ,ifelse(Product.x == "Р‘СЏР»Р° РљР°СЂС‚Р°", "WhiteCard"
                                                ,ifelse(Product.x == "РРђРњ", "WhiteCard"
                                                        ,ifelse(Product.x == "Р‘СЏР»Р° РљР°СЂС‚Р° вЂ“ СЃР°РјРѕ Р»РёС…РІР°", "WhiteCard-Only Intrest"
                                                                ,ifelse(Product.x == "AXI 2", "WhiteCard-Gold"
                                                                        ,ifelse(Product.x == "AXI 2-500", "WhiteCard-Gold"
                                                                                ,ifelse(Product.x == "Р‘СЏР»Р° РљР°СЂС‚Р° Gold - 10%", "WhiteCard-Gold"
                                                                                        ,ifelse(Product.x == "Р‘СЏР»Р° РљР°СЂС‚Р° Gold - 5%", "WhiteCard-Gold"
                                                                                                ,ifelse(Product.x == "Р‘СЏР»Р° РљР°СЂС‚Р° Gold-MFG-5%", "WhiteCard-Gold"
                                                                                                        ,Product.x))))))))))) %>%
      mutate(Amount = ifelse(is.na(Amount),0,Amount)) %>% 
      arrange(CID, NextDate) %>%
      group_by(CID) %>%
      mutate(cumulative = cumsum(Amount)) %>%
      ungroup() %>%
      mutate(Is_paid_Customer = ifelse(MaturedMDPs <= cumulative,1,0)
             , Is_paid_mdp = ifelse(Is_paid_Customer == 1, MaturedMDPs, 0)) %>%
      arrange(CID) %>%
      group_by(CID) %>%
      mutate(Double_payment_CHECK = cumsum(Is_paid_Customer)) %>%
      ungroup() 
  }
  
  #Colletion for PBI
  {
    
    Collection_WhiteCard_Gold_PBIReport = Base_Colletion %>%
      filter(Product.x == "WhiteCard-Gold"
             ,NextDate >= FirstDateOfMonth & NextDate <= Yesterday) %>%
      mutate(MaturedClients = MaturedMDPs)%>%
      group_by(Product.x, NextDate, Bucket) %>%
      summarise(Is_paid_mdp = sum(Is_paid_mdp)
                ,Is_paid_Customer = sum(Is_paid_Customer)
                ,MaturedMDPs = sum(MaturedMDPs)
                ,MaturedClients = length(MaturedClients)) %>%
      select(Product = Product.x
             , Date = NextDate
             , Buckets = Bucket
             , `Count of CID` = MaturedClients
             , `Sum of Paid_customers` = Is_paid_Customer
             , `Sum of MDP` = MaturedMDPs
             , `Sum of Paid_MDP` = Is_paid_mdp) %>%
      mutate(Paid_customers = `Sum of Paid_customers` / `Count of CID`
             ,Paid_MDP = `Sum of Paid_MDP` / `Sum of MDP`) %>%
      arrange(Date)
    
    write_xlsx(Collection_WhiteCard_Gold_PBIReport, paste0("\\\\cow.easycredit.bg/FileServer/White Card/Reports/Week_report/KPI_Collection_PBI/R_Script_Colletion_Automation/Output Data/Colletion_Files_PBI/Colletion_WhiteCard_Gold/Collection_WhiteCard_Gold_PBIReport_", FirstDateOfMonth,".xlsx"))
    
  }
  
  files_Colletion_WhiteCard_Gold <- list.files(path = "\\\\cow.easycredit.bg/FileServer/White Card/Reports/Week_report/KPI_Collection_PBI/R_Script_Colletion_Automation/Output Data/Colletion_Files_PBI/Colletion_WhiteCard_Gold"
                                          , pattern = "*.xlsx"
                                          , full.names = T)
  
  Colletion_PBI_WhiteCard_Gold<- sapply(files_Colletion_WhiteCard_Gold, read_excel, simplify=FALSE) %>%
    bind_rows()
  
}

#A1
{
  #SQL
  {
    myc <- DBI::dbConnect(odbc::odbc()
                          , driver = "SQL Server"
                          , server = "scorpio.smartitbg.int"
                          , database = "BIsmartWCBG"
    )
    
    Payments_all_BG <- paste0(" 
declare @CurrentDate date = getdate();
declare @FirstOfMonth date = case when day(getdate())=1 then DATEADD(DAY,1,EOMONTH(@CurrentDate,-2)) else DATEADD(DAY,1,EOMONTH(@CurrentDate,-1)) end; 

		SELECT EasyClientNumber
		,CONVERT(DATE, CDATE) AS [Date]
		, Product
		, SUM(Amount) AS Amount
		, tr
		FROM (SELECT DISTINCT EasyClientNumber
		, CASE 
			WHEN p.Name = 'РРђРњ'
				THEN 'WhiteCard'
			WHEN p.Name = 'Visa Р‘СЏР»Р° РєР°СЂС‚Р°'
				THEN 'WhiteCard'
			WHEN p.Name = '180 РґРЅРё Visa Р‘СЏР»Р° РљР°СЂС‚Р°'
				THEN 'WhiteCard'
			WHEN p.Name = 'РђРІР°РЅСЃ Р‘Р“ РџРѕС‰Рё'
				THEN 'WhiteCard'
			WHEN p.Name = 'Visa Free ATM World'
				THEN 'WhiteCard'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° Test'
				THEN 'WhiteCard'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р°'
				THEN 'WhiteCard'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° FastCash'
				THEN 'WhiteCard'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° вЂ“ СЃР°РјРѕ Р»РёС…РІР°'
				THEN 'WhiteCard'
			WHEN p.Name = 'Visa Р‘СЏР»Р° РљР°СЂС‚Р° - SC'
				THEN 'WhiteCard'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° Gold - 10%'
				THEN 'WhiteCard-Gold'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° Gold - 5%'
				THEN 'WhiteCard-Gold'
			WHEN p.Name = 'Р‘СЏР»Р° РљР°СЂС‚Р° Gold-MFG-5%'
				THEN 'WhiteCard-Gold'
			WHEN p.Name = 'AXI 2'
				THEN 'WhiteCard-Gold'
			WHEN p.Name = 'AXI 2-500'
				THEN 'WhiteCard-Gold' 
			WHEN p.Name = 'Axi Card - test'
				THEN 'WhiteCard-Gold' 
			WHEN p.Name = 'A1 Credit 1'
				THEN 'A1 Credit'
			WHEN p.Name = 'A1 Credit 2'
				THEN 'A1 Credit'
			WHEN p.Name = 'Рђ1 РїРѕС‚СЂРµР±РёС‚РµР»СЃРєРё Р‘РµР· Р“Р”'
				THEN 'A1 Consumer Loan-Cheap'
			WHEN p.Name = 'Рђ1 РїРѕС‚СЂРµР±РёС‚РµР»СЃРєРё Р“Р”'
				THEN 'A1 Consumer Loan-Expensive'
			WHEN p.Name = 'Axi Credit 2'
				THEN 'Axi Credit'
			WHEN p.Name = 'Axi Credit 3'
				THEN 'Axi Credit'
			WHEN p.Name = 'Axi Credit 4'
				THEN 'Axi Credit'
			WHEN p.Name = 'Axi Credit 4 - paper'
				THEN 'Axi Credit'
			ELSE p.Name
		END AS Product
		, fo.CDATE
		, Amount
		, RANK() OVER(PARTITION BY EasyClientNumber ORDER BY ActivationDate ASC) AS tr
					FROM dwh.FactFinanceOperations fo
					JOIN dwh.DimCards dc ON dc.OfferSK = fo.OfferSK
				    JOIN dwh.DimOffers do ON do.OfferSK = dc.OfferSK
					JOIN dwh.DimProduct p ON p.ProductSK = do.ProductSK
					WHERE FinOperationSK = '73' 
						--AND CONVERT(DATE, fo.CDATE, 4) BETWEEN '2022-04-01' AND DATEADD(day, -1, CAST(GETDATE() AS date))
						AND CONVERT(DATE, fo.CDATE, 4) >= @FirstOfMonth 
						AND CONVERT(DATE, fo.CDATE, 4) <= @CurrentDate
						AND ActivationDate IS NOT NULL
						--AND EasyClientNumber = 555004
		) AS Result
		where tr = 1
		  GROUP BY Product
		  , CONVERT(DATE, CDATE)
		  , EasyClientNumber
		  , tr
		-- ORDER BY EasyClientNumber, Date

")
    start_time <- Sys.time()
    Payments_all_BG <- DBI::dbFetch(DBI::dbSendQuery(myc, Payments_all_BG))
    print(paste("Done in", round(difftime(Sys.time(), start_time,unit = "mins"),2), "minutes"))
    
    #Close the database connection
    DBI::dbDisconnect(myc)
  }
  
  #Date Table
  {
    FirstDateOfMonth = min(as.Date(Payments_all_BG$Date))
    
    Yesterday = Sys.Date()-1
    
    Dates_DF = data.frame(FirstDateOfMonth = as.Date(FirstDateOfMonth)
                          ,NextDate = seq(as.Date(FirstDateOfMonth)
                                          , ceiling_date(as.Date(FirstDateOfMonth)
                                                         , unit = "months")-1
                                          , by="days"))
  }
  
  #MaturedClients
  {
    files_MaturedClients_BG <- list.files(path = "\\\\cow.easycredit.bg/FileServer/White Card/Reports/Week_report/KPI_Collection_PBI/R_Script_Colletion_Automation/Input Data/Matured_Clients/Matured_BG"
                                          , pattern = "*.xlsx"
                                          , full.names = T)
    
    MaturedClients_BG <- sapply(files_MaturedClients_BG, read_excel, simplify=FALSE) %>% 
      bind_rows() %>%
      select(Product, CID, Bucket = `Bucket NEW`, MaturedMDPs = MDPNew) %>%
      mutate(MaturedMDPs = as.numeric(MaturedMDPs)) %>%
      ungroup() %>%
      mutate(FirstDateOfMonth = FirstDateOfMonth)
    
  }
  
  #Base
  {
    Base_Colletion = MaturedClients_BG %>%
      left_join(Dates_DF, by = "FirstDateOfMonth") %>%
      left_join(Payments_all_BG %>% mutate(Date = as.Date(Date)) 
                , by = c("CID" = "EasyClientNumber"
                         ,"NextDate" = "Date" )) %>%
      mutate(Product.x = ifelse(Product.x == "Visa Free ATM World", "WhiteCard"
                                ,ifelse(Product.x == "Visa Р‘СЏР»Р° РєР°СЂС‚Р°", "WhiteCard"
                                        ,ifelse(Product.x == "Р‘СЏР»Р° РљР°СЂС‚Р°", "WhiteCard"
                                                ,ifelse(Product.x == "РРђРњ", "WhiteCard"
                                                        ,ifelse(Product.x == "Р‘СЏР»Р° РљР°СЂС‚Р° вЂ“ СЃР°РјРѕ Р»РёС…РІР°", "WhiteCard-Only Intrest"
                                                                ,ifelse(Product.x == "AXI 2", "WhiteCard-Gold"
                                                                        ,ifelse(Product.x == "AXI 2-500", "WhiteCard-Gold"
                                                                                ,ifelse(Product.x == "Р‘СЏР»Р° РљР°СЂС‚Р° Gold - 10%", "WhiteCard-Gold"
                                                                                        ,ifelse(Product.x == "Р‘СЏР»Р° РљР°СЂС‚Р° Gold - 5%", "WhiteCard-Gold"
                                                                                                ,ifelse(Product.x == "Р‘СЏР»Р° РљР°СЂС‚Р° Gold-MFG-5%", "WhiteCard-Gold"
                                                                                                        ,Product.x))))))))))) %>%
      mutate(Amount = ifelse(is.na(Amount),0,Amount)) %>% 
      arrange(CID, NextDate) %>%
      group_by(CID) %>%
      mutate(cumulative = cumsum(Amount)) %>%
      ungroup() %>%
      mutate(Is_paid_Customer = ifelse(MaturedMDPs <= cumulative,1,0)
             , Is_paid_mdp = ifelse(Is_paid_Customer == 1, MaturedMDPs, 0)) %>%
      arrange(CID) %>%
      group_by(CID) %>%
      mutate(Double_payment_CHECK = cumsum(Is_paid_Customer)) %>%
      ungroup() 
  }
  
  #Colletion for PBI
  {
    
    Collection_A1_PBIReport = Base_Colletion %>%
      filter(Product.x == "A1"
             ,NextDate >= FirstDateOfMonth & NextDate <= Yesterday) %>%
      mutate(MaturedClients = MaturedMDPs)%>%
      group_by(Product.x, NextDate, Bucket) %>%
      summarise(Is_paid_mdp = sum(Is_paid_mdp)
                ,Is_paid_Customer = sum(Is_paid_Customer)
                ,MaturedMDPs = sum(MaturedMDPs)
                ,MaturedClients = length(MaturedClients)) %>%
      select(Product = Product.x
             , Date = NextDate
             , Buckets = Bucket
             , `Count of CID` = MaturedClients
             , `Sum of Paid_customers` = Is_paid_Customer
             , `Sum of MDP` = MaturedMDPs
             , `Sum of Paid_MDP` = Is_paid_mdp) %>%
      mutate(Paid_customers = `Sum of Paid_customers` / `Count of CID`
             ,Paid_MDP = `Sum of Paid_MDP` / `Sum of MDP`) %>%
      arrange(Date)
    
    write_xlsx(Collection_A1_PBIReport, paste0("\\\\cow.easycredit.bg/FileServer/White Card/Reports/Week_report/KPI_Collection_PBI/R_Script_Colletion_Automation/Output Data/Colletion_Files_PBI/Colletion_A1/Collection_A1_", FirstDateOfMonth,".xlsx"))
    
  }
  
  files_Colletion_A1 <- list.files(path = "\\\\cow.easycredit.bg/FileServer/White Card/Reports/Week_report/KPI_Collection_PBI/R_Script_Colletion_Automation/Output Data/Colletion_Files_PBI/Colletion_A1"
                                               , pattern = "*.xlsx"
                                               , full.names = T)
  
  Colletion_PBI_A1<- sapply(files_Colletion_A1, read_excel, simplify=FALSE) %>%
    bind_rows()
  
}
}

#Collection PL
{
  
  #SQL
  {
    myc <- DBI::dbConnect(odbc::odbc()
                          , driver = "SQL Server"
                          , server = "scorpio.smartitbg.int"
                          , database = "BIsmartWCPL"
    )
    
    Payments_all_PL <- paste0(" declare @CurrentDate date = getdate();
declare @FirstOfMonth date = case when day(getdate())=1 then DATEADD(DAY,1,EOMONTH(@CurrentDate,-2)) else DATEADD(DAY,1,EOMONTH(@CurrentDate,-1)) end; 


    SELECT EasyClientNumber
	, CONVERT(DATE, CDATE) AS [Date]
	, Product
	, SUM(Amount) AS Amount
	FROM (SELECT 
	EasyClientNumber,
	CASE 
	WHEN p.Name = 'Axi Card PL'
	THEN 'AXI PL'
ELSE p.Name
END AS Product, 
	Notes, 
	fo.CDATE, 
	Amount,
	RANK() OVER(PARTITION BY EasyClientNumber  
                                ORDER BY ActivationDate DESC) AS tr2
			FROM dwh.FactFinanceOperations fo
			JOIN dwh.DimCards dc ON dc.OfferSK = fo.OfferSK
			JOIN dwh.DimPaymentPartners pp ON pp.PaymentPartnersSK = fo.PaymentPartnersSK
			JOIN dwh.DimOffers do ON do.OfferSK = dc.OfferSK
			JOIN dwh.DimProduct p ON p.ProductSK = do.ProductSK
			WHERE FinOperationSK = '1' 
				AND CONVERT(DATE, fo.CDATE, 4) >= @FirstOfMonth 
				AND CONVERT(DATE, fo.CDATE, 4) <= @CurrentDate
				AND ActivationDate IS NOT NULL
			) AS Result
			where tr2 = 1 	
  GROUP BY Product
  ,CONVERT(DATE, CDATE)
  , EasyClientNumber
  ORDER BY Date
  , Product



")
    start_time <- Sys.time()
    Payments_all_PL <- DBI::dbFetch(DBI::dbSendQuery(myc, Payments_all_PL))
    print(paste("Done in", round(difftime(Sys.time(), start_time,unit = "mins"),2), "minutes"))
    
    #Close the database connection
    DBI::dbDisconnect(myc)
  }
  
  #Date Table
  {
    FirstDateOfMonth = min(as.Date(Payments_all_BG$Date))
    
    Yesterday = Sys.Date()-1
    
    Dates_DF = data.frame(FirstDateOfMonth = as.Date(FirstDateOfMonth)
                          ,NextDate = seq(as.Date(FirstDateOfMonth)
                                          , ceiling_date(as.Date(FirstDateOfMonth)
                                                         , unit = "months")-1
                                          , by="days"))
  }
  
  #MaturedClients
  {
    files_MaturedClients_PL <- list.files(path = "\\\\cow.easycredit.bg/FileServer/White Card/Reports/Week_report/KPI_Collection_PBI/R_Script_Colletion_Automation/Input Data/Matured_Clients/Matured_PL"
                                          , pattern = "*.xlsx"
                                          , full.names = T)
    
    MaturedClients_PL <- sapply(files_MaturedClients_PL, read_excel, simplify=FALSE) %>% 
      bind_rows() %>%
      select(Product, CID, Bucket = `Bucket NEW`, MaturedMDPs = MDPNew) %>%
      mutate(MaturedMDPs = as.numeric(MaturedMDPs)) %>%
      ungroup() %>%
      mutate(FirstDateOfMonth = FirstDateOfMonth)
    
  }
  
  #Base
  {
    Base_Colletion_PL = MaturedClients_PL %>%
      left_join(Dates_DF, by = "FirstDateOfMonth") %>%
      left_join(Payments_all_PL %>% mutate(Date = as.Date(Date)) 
                , by = c("CID" = "EasyClientNumber"
                         ,"NextDate" = "Date" )) %>%
      mutate(Amount = ifelse(is.na(Amount),0,Amount)) %>% 
      arrange(CID, NextDate) %>%
      group_by(CID) %>%
      mutate(cumulative = cumsum(Amount)) %>%
      ungroup() %>%
      mutate(Is_paid_Customer = ifelse(MaturedMDPs <= cumulative,1,0)
             , Is_paid_mdp = ifelse(Is_paid_Customer == 1, MaturedMDPs, 0)) %>%
      arrange(CID) %>%
      group_by(CID) %>%
      mutate(Double_payment_CHECK = cumsum(Is_paid_Customer)) %>%
      ungroup() 
  }
  
  #Colletion for PBI
  {
    
    Collection_PL_PBIReport = Base_Colletion_PL %>%
      filter(NextDate >= FirstDateOfMonth & NextDate <= Yesterday) %>%
      mutate(MaturedClients = MaturedMDPs)%>%
      group_by(Product.x, NextDate, Bucket) %>%
      summarise(Is_paid_mdp = sum(Is_paid_mdp)
                ,Is_paid_Customer = sum(Is_paid_Customer)
                ,MaturedMDPs = sum(MaturedMDPs)
                ,MaturedClients = length(MaturedClients)) %>%
      select(Product = Product.x
             , Date = NextDate
             , Buckets = Bucket
             , `Count of CID` = MaturedClients
             , `Sum of Paid_customers` = Is_paid_Customer
             , `Sum of MDP` = MaturedMDPs
             , `Sum of Paid_MDP` = Is_paid_mdp) %>%
      mutate(Paid_customers = `Sum of Paid_customers` / `Count of CID`
             ,Paid_MDP = `Sum of Paid_MDP` / `Sum of MDP`) %>%
      arrange(Date)
    
    write_xlsx(Collection_PL_PBIReport, paste0("\\\\cow.easycredit.bg/FileServer/White Card/Reports/Week_report/KPI_Collection_PBI/R_Script_Colletion_Automation/Output Data/Colletion_Files_PBI/Collection_PL/Collection_PL_PBIReport_", FirstDateOfMonth,".xlsx"))
    
  }
  
  files_Colletion_PL <- list.files(path = "\\\\cow.easycredit.bg/FileServer/White Card/Reports/Week_report/KPI_Collection_PBI/R_Script_Colletion_Automation/Output Data/Colletion_Files_PBI/Collection_PL"
                                          , pattern = "*.xlsx"
                                          , full.names = T)
  
  Colletion_PBI_PL<- sapply(files_Colletion_PL, read_excel, simplify=FALSE) %>%
    bind_rows()
  
}

#Collection RO
{
  
  #SQL
  {
    myc <- DBI::dbConnect(odbc::odbc()
                          , driver = "SQL Server"
                          , server = "scorpio.smartitbg.int"
                          , database = "BIsmartWCRO"
    )
    
    Payments_all_RO <- paste0(" declare @CurrentDate date = getdate();
declare @FirstOfMonth date = case when day(getdate())=1 then DATEADD(DAY,1,EOMONTH(@CurrentDate,-2)) else DATEADD(DAY,1,EOMONTH(@CurrentDate,-1)) end; 

SELECT EasyClientNumber
, CONVERT(DATE, CDATE) AS [Date]
, CASE 
    WHEN Product = 'AFR'
		THEN 'Axi RO'
    WHEN Product = 'AFR 2'
		THEN 'Axi RO'
    WHEN Product = 'AFR 3'
		THEN 'Axi RO'
	WHEN Product = 'AFR 2 - SC'
		THEN 'Axi RO'
	ELSE Product
END AS Product
, SUM(Amount) AS Amount
	FROM (SELECT DISTINCT EasyClientNumber
	, p.Name AS Product
	, Notes
	, fo.CDATE
	, Amount
			FROM dwh.FactFinanceOperations fo
			JOIN dwh.DimCards dc ON dc.OfferSK = fo.OfferSK
			JOIN dwh.DimPaymentPartners pp ON pp.PaymentPartnersSK = fo.PaymentPartnersSK
			JOIN dwh.DimOffers do ON do.OfferSK = dc.OfferSK
			JOIN dwh.DimProduct p ON p.ProductSK = do.ProductSK
			WHERE FinOperationSK = '1' 
			AND CONVERT(DATE, fo.CDATE, 4) >= @FirstOfMonth 
			AND CONVERT(DATE, fo.CDATE, 4) <= @CurrentDate
			AND ActivationDate IS NOT NULL) AS Result
  GROUP BY Notes
  , EasyClientNumber
  , Product
  ,CONVERT(DATE, CDATE)
  ORDER BY Date
  , Product

")
    start_time <- Sys.time()
    Payments_all_RO <- DBI::dbFetch(DBI::dbSendQuery(myc, Payments_all_RO))
    print(paste("Done in", round(difftime(Sys.time(), start_time,unit = "mins"),2), "minutes"))
    
    #Close the database connection
    DBI::dbDisconnect(myc)
  }
  
  #Date Table
  {
    FirstDateOfMonth = min(as.Date(Payments_all_BG$Date))
    
    Yesterday = Sys.Date()-1
    
    Dates_DF = data.frame(FirstDateOfMonth = as.Date(FirstDateOfMonth)
                          ,NextDate = seq(as.Date(FirstDateOfMonth)
                                          , ceiling_date(as.Date(FirstDateOfMonth)
                                                         , unit = "months")-1
                                          , by="days"))
  }
  
  #MaturedClients
  {
    files_MaturedClients_RO <- list.files(path = "\\\\cow.easycredit.bg/FileServer/White Card/Reports/Week_report/KPI_Collection_PBI/R_Script_Colletion_Automation/Input Data/Matured_Clients/Matured_RO"
                                          , pattern = "*.xlsx"
                                          , full.names = T)
    
    MaturedClients_RO <- sapply(files_MaturedClients_RO, read_excel, simplify=FALSE) %>% 
      bind_rows() %>%
      select(Product, CID, Bucket = `Bucket NEW`, MaturedMDPs = MDPNew) %>%
      mutate(MaturedMDPs = as.numeric(MaturedMDPs)) %>%
      ungroup() %>%
      mutate(FirstDateOfMonth = FirstDateOfMonth)
    
  }
  
  #Base
  {
    Base_Colletion_RO = MaturedClients_RO %>%
      left_join(Dates_DF, by = "FirstDateOfMonth") %>%
      left_join(Payments_all_RO %>% mutate(Date = as.Date(Date)) 
                , by = c("CID" = "EasyClientNumber"
                         ,"NextDate" = "Date" )) %>%
      mutate(Amount = ifelse(is.na(Amount),0,Amount)) %>% 
      arrange(CID, NextDate) %>%
      group_by(CID) %>%
      mutate(cumulative = cumsum(Amount)) %>%
      ungroup() %>%
      mutate(Is_paid_Customer = ifelse(MaturedMDPs <= cumulative,1,0)
             , Is_paid_mdp = ifelse(Is_paid_Customer == 1, MaturedMDPs, 0)) %>%
      arrange(CID) %>%
      group_by(CID) %>%
      mutate(Double_payment_CHECK = cumsum(Is_paid_Customer)) %>%
      ungroup() 
  }
  
  #Colletion for PBI
  {
    
    Collection_RO_PBIReport = Base_Colletion_RO %>%
      filter(NextDate >= FirstDateOfMonth & NextDate <= Yesterday) %>%
      mutate(MaturedClients = MaturedMDPs)%>%
      group_by(Product.x, NextDate, Bucket) %>%
      summarise(Is_paid_mdp = sum(Is_paid_mdp)
                ,Is_paid_Customer = sum(Is_paid_Customer)
                ,MaturedMDPs = sum(MaturedMDPs)
                ,MaturedClients = length(MaturedClients)) %>%
      select(Product = Product.x
             , Date = NextDate
             , Buckets = Bucket
             , `Count of CID` = MaturedClients
             , `Sum of Paid_customers` = Is_paid_Customer
             , `Sum of MDP` = MaturedMDPs
             , `Sum of Paid_MDP` = Is_paid_mdp) %>%
      mutate(Paid_customers = `Sum of Paid_customers` / `Count of CID`
             ,Paid_MDP = `Sum of Paid_MDP` / `Sum of MDP`) %>%
      arrange(Date)
    
    write_xlsx(Collection_RO_PBIReport, paste0("\\\\cow.easycredit.bg/FileServer/White Card/Reports/Week_report/KPI_Collection_PBI/R_Script_Colletion_Automation/Output Data/Colletion_Files_PBI/Collection_RO/Collection_RO_PBIReport_", FirstDateOfMonth,".xlsx"))
    
  }
  
  files_Colletion_RO <- list.files(path = "\\\\cow.easycredit.bg/FileServer/White Card/Reports/Week_report/KPI_Collection_PBI/R_Script_Colletion_Automation/Output Data/Colletion_Files_PBI/Collection_RO"
                                   , pattern = "*.xlsx"
                                   , full.names = T)
  
  Colletion_PBI_RO<- sapply(files_Colletion_RO, read_excel, simplify=FALSE) %>%
    bind_rows()
  
}
