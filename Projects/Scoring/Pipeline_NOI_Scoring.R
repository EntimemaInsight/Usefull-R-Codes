tryCatch(
  expr = {
    library(tidyverse)
    library(XML)
    library(lubridate)
    
    # Sourcing in functions
    if (Sys.info()[["user"]] == 'provenir') {
      source("f_extract_NOI_Data_Feature_Engineering.r")
      source("f_wrangle_NOI_2_Feature_Engineering.r")
      source("f_wrangle_NOI_7_Feature_Engineering.r")
      source("f_elapsed_months.r")
      source("f_age_calc.r")
      source("f_fill_cols.r")
      
      # Sourcing in list of fatures names
      source("Features_Names_NOI_2.r")
      source("Features_Names_NOI_7.r")
      
    } else {
      source("./Files For Provenir/f_extract_NOI_Data_Feature_Engineering.r")
      source("./Files For Provenir/f_wrangle_NOI_2_Feature_Engineering.r")
      source("./Files For Provenir/f_wrangle_NOI_7_Feature_Engineering.r")
      source("./Files For Provenir/f_elapsed_months.r")
      source("./Files For Provenir/f_age_calc.r")
      source("./Files For Provenir/f_fill_cols.r")
      
      # Sourcing in list of fatures names
      source("./Files For Provenir/Features_Names_NOI_2.r")
      source("./Files For Provenir/Features_Names_NOI_7.r")
      
    }
    
    
    # 1. Loading in NSSI data ----    
    if (Sys.info()[["user"]] == 'provenir') {
      input_data <-
        read.delim(file = "./input_data.txt",
                   na.strings = "NULL",
                   quote = "")
      
      Date = as.Date(input_data$Date, tryFormats = '%Y/%m/%d')
      EGN = as.character(input_data$EGN)
      CreditPayment = as.numeric(input_data$CreditPayment)
      # CreditPayment = input_data$CreditPayment
      CreditPeriod = as.integer(input_data$CreditPeriod)
      Product_ID = as.character(input_data$Product_ID)
      NOI_2 = as.character(input_data$NOI_2)
      NOI_7 = as.character(input_data$NOI_7)
      NOI_51 = as.character(input_data$NOI_51)
      
      ProductMapping <- readxl::read_excel('Viva_Product_Mapping.xlsx')
      
      LoanTerm_DF <- ProductMapping %>%
        filter(Nomenclature == Product_ID)
      
      LoanTerm <- LoanTerm_DF$Term
      LoanTermInDays <- LoanTerm * CreditPeriod
      
      # Correct EGN where neccessary
      
      if (nchar(EGN) < 10) {
        EGN <- paste0(paste(rep(0, 10 - nchar(EGN)), collapse = ""), EGN)
      }
      
      Unique_EGN_Date_Table <- data.frame(
        EGN = EGN,
        Date = Date,
        index = 1,
        NOI_2 = NOI_2,
        NOI_7 = NOI_7,
        NOI_51 = NOI_51
      )
      
    } else {
      input_data <-
        readxl::read_excel(
          './Files For Provenir/Test Files/Issues After Launch/xmls/noi_scoring_test_file.xlsx'
        )
      
      Date = as.Date(input_data$Date)
      EGN = as.character(input_data$EGN)
      CreditPayment = as.numeric(input_data$CreditPayment)
      #CreditPayment <- 100
      CreditPeriod = as.integer(input_data$CreditPeriod)
      Product_ID = as.character(input_data$Product_ID)
      NOI_2 = as.character(input_data$NOI_2)
      NOI_7 = as.character(input_data$NOI_7)
      NOI_51 = as.character(input_data$NOI_51)
      
      ProductMapping <-
        readxl::read_excel('./Files For Provenir/Viva_Product_Mapping.xlsx')
      
      LoanTerm_DF <- ProductMapping %>%
        filter(Nomenclature == Product_ID)
      
      LoanTerm <- LoanTerm_DF$Term
      LoanTermInDays <- LoanTerm * CreditPeriod
      
      
      # Correct EGN where neccessary
      
      if (nchar(EGN) < 10) {
        EGN <- paste0(paste(rep(0, 10 - nchar(EGN)), collapse = ""), EGN)
      }
      
      # Extracting a list of unique EGN and Dates for which we have data
      Unique_EGN_Date_Table <- data.frame(
        EGN = EGN,
        Date = Date,
        index = 1,
        NOI_2 = NOI_2,
        NOI_7 = NOI_7,
        NOI_51 = NOI_51
      )
      
    }
    
    # 2. Feature extraction ----    
    
    # Extract data for the EGN every EGN    
    Table_Results_NOI <- extract_NOI_Data_Feature_Engineering(1)
    
    Table_Results_NOI_FE <- Table_Results_NOI %>%
      bind_cols(data.frame(
        CreditPayment = as.numeric(CreditPayment),
        Loan_Term_In_Days = as.numeric(LoanTermInDays)
      ))
    
    # 3. Imputing missing values and feature engineering ---
    Dataset_NOI_7_Raw <- Table_Results_NOI_FE %>%
      select(
        EGN,
        date_of_report,
        Age,
        Sex,
        Warning_NOI_7,
        Number_Of_Employers_period_n_minus_2,
        Number_Of_Contracts_period_n_minus_2,
        Mean_Monthly_Salary_period_n_minus_2,
        # Latest_Salary_period_n_minus_2,
        # Latest_EGN_Function_Code_period_n_minus_2,
        # Latest_Economic_Activity_Code_period_n_minus_2,
        perc_time_one_active_contract_period_n_minus_2,
        perc_time_two_active_contracts_period_n_minus_2,
        perc_time_more_active_contracts_period_n_minus_2,
        perc_time_no_contract_period_n_minus_2,
        Function_Code_1_Digit_period_n_minus_2,
        # Days_Function_Code_1_Digit_period_n_minus_2,
        # Function_Code_2_Digit_period_n_minus_2,
        # Days_Function_Code_2_Digit_period_n_minus_2,
        Economic_Code_1_Digit_period_n_minus_2,
        # Days_Economic_Code_1_Digit_period_n_minus_2,
        # Economic_Code_2_Digit_period_n_minus_2,
        # Days_Economic_Code_2_Digit_period_n_minus_2,
        Number_Of_Employers_period_n_minus_1,
        Number_Of_Contracts_period_n_minus_1,
        Mean_Monthly_Salary_period_n_minus_1,
        # Latest_Salary_period_n_minus_1,
        # Latest_EGN_Function_Code_period_n_minus_1,
        # Latest_Economic_Activity_Code_period_n_minus_1,
        perc_time_one_active_contract_period_n_minus_1,
        perc_time_two_active_contracts_period_n_minus_1,
        perc_time_more_active_contracts_period_n_minus_1,
        perc_time_no_contract_period_n_minus_1,
        Function_Code_1_Digit_period_n_minus_1,
        # Days_Function_Code_1_Digit_period_n_minus_1,
        # Function_Code_2_Digit_period_n_minus_1,
        # Days_Function_Code_2_Digit_period_n_minus_1,
        Economic_Code_1_Digit_period_n_minus_1,
        # Days_Economic_Code_1_Digit_period_n_minus_1,
        # Economic_Code_2_Digit_period_n_minus_1,
        # Days_Economic_Code_2_Digit_period_n_minus_1,
        Number_Of_Employers_period_n_minus_0,
        Number_Of_Contracts_period_n_minus_0,
        Mean_Monthly_Salary_period_n_minus_0,
        Latest_Salary_period_n_minus_0,
        Latest_EGN_Function_Code_period_n_minus_0,
        Latest_Economic_Activity_Code_period_n_minus_0,
        perc_time_one_active_contract_period_n_minus_0,
        perc_time_two_active_contracts_period_n_minus_0,
        perc_time_more_active_contracts_period_n_minus_0,
        perc_time_no_contract_period_n_minus_0,
        Function_Code_1_Digit_period_n_minus_0,
        # Days_Function_Code_1_Digit_period_n_minus_0,
        # Function_Code_2_Digit_period_n_minus_0,
        # Days_Function_Code_2_Digit_period_n_minus_0,
        Economic_Code_1_Digit_period_n_minus_0,
        # Days_Economic_Code_1_Digit_period_n_minus_0,
        # Economic_Code_2_Digit_period_n_minus_0,
        # Days_Economic_Code_2_Digit_period_n_minus_0,
        Months_Work_Experience_Current_Employer,
        Total_Months_Work_Experience_Labor_Contracts,
        Total_Number_Of_Employers,
        CreditPayment,
        Loan_Term_In_Days
      ) %>%
      filter(is.na(Warning_NOI_7)) %>%
      select(-Warning_NOI_7)
    
    # Reading in summary for imputing missing values & correcting outliers
    
    if (Sys.info()[["user"]] == 'provenir') {
      summary_NOI_7 <- readRDS('Missing_Values_Outliers_NOI_7.rds')
      
    } else {
      summary_NOI_7 <-
        readRDS("./Files For Provenir/Missing_Values_Outliers_NOI_7.rds")
    }
    
    dataset_all_variables_processed <- Dataset_NOI_7_Raw %>%
      mutate(
        Sex = ifelse(Sex == "Male", 1, 0),
        
        # Period n minus 2
        Number_Of_Employers_period_n_minus_2 = case_when(
          is.na(Number_Of_Employers_period_n_minus_2) ~ 0,
          Number_Of_Employers_period_n_minus_2 < 0 ~ 0,
          Number_Of_Employers_period_n_minus_2 > as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Number_Of_Employers_period_n_minus_2"
          ), ]$X99th.Percentile) ~ as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Number_Of_Contracts_period_n_minus_2"
          ), ]$X99th.Percentile),
          TRUE ~ as.numeric(Number_Of_Employers_period_n_minus_2)
        ),
        
        Number_Of_Contracts_period_n_minus_2 = case_when(
          is.na(Number_Of_Contracts_period_n_minus_2) ~ 0,
          Number_Of_Contracts_period_n_minus_2 < 0 ~ 0,
          Number_Of_Contracts_period_n_minus_2 > as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Number_Of_Contracts_period_n_minus_2"
          ), ]$X99th.Percentile) ~ as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Number_Of_Contracts_period_n_minus_2"
          ), ]$X99th.Percentile),
          TRUE ~ as.numeric(Number_Of_Contracts_period_n_minus_2)
        ),
        
        Mean_Monthly_Salary_period_n_minus_2 = case_when(
          is.na(Mean_Monthly_Salary_period_n_minus_2) ~ 0,
          Mean_Monthly_Salary_period_n_minus_2 < 0 ~ 0,
          Mean_Monthly_Salary_period_n_minus_2 > as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Mean_Monthly_Salary_period_n_minus_2"
          ), ]$X99th.Percentile) ~ as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Mean_Monthly_Salary_period_n_minus_2"
          ), ]$X99th.Percentile),
          TRUE ~ as.numeric(Mean_Monthly_Salary_period_n_minus_2)
        ),
        
        
        perc_time_one_active_contract_period_n_minus_2 = ifelse(
          is.na(perc_time_one_active_contract_period_n_minus_2),
          0,
          perc_time_one_active_contract_period_n_minus_2
        ),
        perc_time_two_active_contracts_period_n_minus_2 = ifelse(
          is.na(perc_time_two_active_contracts_period_n_minus_2),
          0,
          perc_time_two_active_contracts_period_n_minus_2
        ),
        perc_time_more_active_contracts_period_n_minus_2 = ifelse(
          is.na(perc_time_more_active_contracts_period_n_minus_2),
          0,
          perc_time_more_active_contracts_period_n_minus_2
        ),
        perc_time_no_contract_period_n_minus_2 = ifelse(
          is.na(perc_time_no_contract_period_n_minus_2),
          0,
          perc_time_no_contract_period_n_minus_2
        ),
        
        perc_time_working_period_n_minus_2 =
          perc_time_one_active_contract_period_n_minus_2 +
          perc_time_two_active_contracts_period_n_minus_2 +
          perc_time_more_active_contracts_period_n_minus_2,
        
        Function_Code_1_Digit_period_n_minus_2 = ifelse(
          is.na(Function_Code_1_Digit_period_n_minus_2),
          10,
          Function_Code_1_Digit_period_n_minus_2
        ),
        
        Economic_Code_1_Digit_period_n_minus_2 = ifelse(
          is.na(Economic_Code_1_Digit_period_n_minus_2),
          10,
          Economic_Code_1_Digit_period_n_minus_2
        ),
        
        # Period n minus 1
        
        Number_Of_Employers_period_n_minus_1 = case_when(
          is.na(Number_Of_Employers_period_n_minus_1) ~ 0,
          Number_Of_Employers_period_n_minus_1 < 0 ~ 0,
          Number_Of_Employers_period_n_minus_1 > as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Number_Of_Employers_period_n_minus_1"
          ), ]$X99th.Percentile) ~ as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Number_Of_Contracts_period_n_minus_1"
          ), ]$X99th.Percentile),
          TRUE ~ as.numeric(Number_Of_Employers_period_n_minus_1)
        ),
        
        Number_Of_Contracts_period_n_minus_1 = case_when(
          is.na(Number_Of_Contracts_period_n_minus_1) ~ 0,
          Number_Of_Contracts_period_n_minus_1 < 0 ~ 0,
          Number_Of_Contracts_period_n_minus_1 > as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Number_Of_Contracts_period_n_minus_1"
          ), ]$X99th.Percentile) ~ as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Number_Of_Contracts_period_n_minus_1"
          ), ]$X99th.Percentile),
          TRUE ~ as.numeric(Number_Of_Contracts_period_n_minus_1)
        ),
        
        Mean_Monthly_Salary_period_n_minus_1 = case_when(
          is.na(Mean_Monthly_Salary_period_n_minus_1) ~ 0,
          Mean_Monthly_Salary_period_n_minus_1 < 0 ~ 0,
          Mean_Monthly_Salary_period_n_minus_1 > as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Mean_Monthly_Salary_period_n_minus_1"
          ), ]$X99th.Percentile) ~ as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Mean_Monthly_Salary_period_n_minus_1"
          ), ]$X99th.Percentile),
          TRUE ~ as.numeric(Mean_Monthly_Salary_period_n_minus_1)
        ),
        
        perc_time_one_active_contract_period_n_minus_1 = ifelse(
          is.na(perc_time_one_active_contract_period_n_minus_1),
          0,
          perc_time_one_active_contract_period_n_minus_1
        ),
        perc_time_two_active_contracts_period_n_minus_1 = ifelse(
          is.na(perc_time_two_active_contracts_period_n_minus_1),
          0,
          perc_time_two_active_contracts_period_n_minus_1
        ),
        perc_time_more_active_contracts_period_n_minus_1 = ifelse(
          is.na(perc_time_more_active_contracts_period_n_minus_1),
          0,
          perc_time_more_active_contracts_period_n_minus_1
        ),
        perc_time_no_contract_period_n_minus_1 = ifelse(
          is.na(perc_time_no_contract_period_n_minus_1),
          0,
          perc_time_no_contract_period_n_minus_1
        ),
        
        perc_time_working_period_n_minus_1 = perc_time_one_active_contract_period_n_minus_1 + perc_time_two_active_contracts_period_n_minus_1 + perc_time_more_active_contracts_period_n_minus_1,
        
        Function_Code_1_Digit_period_n_minus_1 = ifelse(
          is.na(Function_Code_1_Digit_period_n_minus_1),
          10,
          Function_Code_1_Digit_period_n_minus_1
        ),
        
        Economic_Code_1_Digit_period_n_minus_1 = ifelse(
          is.na(Economic_Code_1_Digit_period_n_minus_1),
          10,
          Economic_Code_1_Digit_period_n_minus_1
        ),
        
        # Period n minus 0
        Number_Of_Employers_period_n_minus_0 = case_when(
          is.na(Number_Of_Employers_period_n_minus_0) ~ 0,
          Number_Of_Employers_period_n_minus_0 < 0 ~ 0,
          Number_Of_Employers_period_n_minus_0 > as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Number_Of_Employers_period_n_minus_0"
          ), ]$X99th.Percentile) ~ as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Number_Of_Contracts_period_n_minus_0"
          ), ]$X99th.Percentile),
          TRUE ~ as.numeric(Number_Of_Employers_period_n_minus_0)
        ),
        
        Number_Of_Contracts_period_n_minus_0 = case_when(
          is.na(Number_Of_Contracts_period_n_minus_0) ~ 0,
          Number_Of_Contracts_period_n_minus_0 < 0 ~ 0,
          Number_Of_Contracts_period_n_minus_0 > as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Number_Of_Contracts_period_n_minus_0"
          ), ]$X99th.Percentile) ~ as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Number_Of_Contracts_period_n_minus_0"
          ), ]$X99th.Percentile),
          TRUE ~ as.numeric(Number_Of_Contracts_period_n_minus_0)
        ),
        
        Mean_Monthly_Salary_period_n_minus_0 = case_when(
          is.na(Mean_Monthly_Salary_period_n_minus_0) ~ 0,
          Mean_Monthly_Salary_period_n_minus_0 < 0 ~ 0,
          Mean_Monthly_Salary_period_n_minus_0 > as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Mean_Monthly_Salary_period_n_minus_0"
          ), ]$X99th.Percentile) ~ as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Mean_Monthly_Salary_period_n_minus_0"
          ), ]$X99th.Percentile),
          TRUE ~ as.numeric(Mean_Monthly_Salary_period_n_minus_0)
        ),
        
        
        perc_time_one_active_contract_period_n_minus_0 = ifelse(
          is.na(perc_time_one_active_contract_period_n_minus_0),
          0,
          perc_time_one_active_contract_period_n_minus_0
        ),
        perc_time_two_active_contracts_period_n_minus_0 = ifelse(
          is.na(perc_time_two_active_contracts_period_n_minus_0),
          0,
          perc_time_two_active_contracts_period_n_minus_0
        ),
        perc_time_more_active_contracts_period_n_minus_0 = ifelse(
          is.na(perc_time_more_active_contracts_period_n_minus_0),
          0,
          perc_time_more_active_contracts_period_n_minus_0
        ),
        perc_time_no_contract_period_n_minus_0 = ifelse(
          is.na(perc_time_no_contract_period_n_minus_0),
          0,
          perc_time_no_contract_period_n_minus_0
        ),
        
        perc_time_working_period_n_minus_0 = perc_time_one_active_contract_period_n_minus_0 + perc_time_two_active_contracts_period_n_minus_0 + perc_time_more_active_contracts_period_n_minus_0,
        
        Function_Code_1_Digit_period_n_minus_0 = ifelse(
          is.na(Function_Code_1_Digit_period_n_minus_0),
          10,
          Function_Code_1_Digit_period_n_minus_0
        ),
        
        Economic_Code_1_Digit_period_n_minus_0 = ifelse(
          is.na(Economic_Code_1_Digit_period_n_minus_0),
          10,
          Economic_Code_1_Digit_period_n_minus_0
        ),
        
        
        # Current situation        
        Latest_Salary_period_n_minus_0 = case_when(
          is.na(Latest_Salary_period_n_minus_0) ~ 0,
          Latest_Salary_period_n_minus_0 < 0 ~ 0,
          Latest_Salary_period_n_minus_0 > as.numeric(summary_NOI_7[which(summary_NOI_7$Names.of.Characteristics ==
                                                                            "Latest_Salary_period_n_minus_0"), ]$X99th.Percentile) ~ as.numeric(summary_NOI_7[which(summary_NOI_7$Names.of.Characteristics ==
                                                                                                                                                                      "Latest_Salary_period_n_minus_0"), ]$X99th.Percentile),
          TRUE ~ as.numeric(Latest_Salary_period_n_minus_0)
        ),
        
        Latest_EGN_Function_Code_1_Digit = substr(x = Latest_EGN_Function_Code_period_n_minus_0, start = 1, stop = 1),
        Latest_EGN_Function_Code_1_Digit = ifelse(
          is.na(Latest_EGN_Function_Code_1_Digit),
          10,
          Latest_EGN_Function_Code_1_Digit
        ),
        Latest_EGN_Function_Code_1_Digit = as.numeric(Latest_EGN_Function_Code_1_Digit),
        
        Latest_Economic_Activity_Code_1_Digit = substr(x = Latest_Economic_Activity_Code_period_n_minus_0, start = 1, stop = 1),
        Latest_Economic_Activity_Code_1_Digit = ifelse(
          is.na(Latest_Economic_Activity_Code_1_Digit),
          10,
          Latest_Economic_Activity_Code_1_Digit
        ),
        Latest_Economic_Activity_Code_1_Digit = as.numeric(Latest_Economic_Activity_Code_1_Digit),
        
        # Summary stats
        Months_Work_Experience_Current_Employer = case_when(
          is.na(Months_Work_Experience_Current_Employer) ~ 0,
          Months_Work_Experience_Current_Employer < 0 ~ 0,
          Months_Work_Experience_Current_Employer > as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Months_Work_Experience_Current_Employer"
          ), ]$X99th.Percentile) ~ as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Months_Work_Experience_Current_Employer"
          ), ]$X99th.Percentile),
          TRUE ~ as.numeric(Months_Work_Experience_Current_Employer)
        ),
        
        
        Total_Months_Work_Experience_Labor_Contracts = case_when(
          is.na(Total_Months_Work_Experience_Labor_Contracts) ~ 0,
          Total_Months_Work_Experience_Labor_Contracts < 0 ~ 0,
          Total_Months_Work_Experience_Labor_Contracts > as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Total_Months_Work_Experience_Labor_Contracts"
          ), ]$X99th.Percentile) ~ as.numeric(summary_NOI_7[which(
            summary_NOI_7$Names.of.Characteristics == "Total_Months_Work_Experience_Labor_Contracts"
          ), ]$X99th.Percentile),
          TRUE ~ as.numeric(Total_Months_Work_Experience_Labor_Contracts)
        ),
        
        
        Total_Number_Of_Employers = case_when(
          is.na(Total_Number_Of_Employers) ~ 0,
          Total_Number_Of_Employers < 0 ~ 0,
          Total_Number_Of_Employers > as.numeric(summary_NOI_7[which(summary_NOI_7$Names.of.Characteristics ==
                                                                       "Total_Number_Of_Employers"), ]$X99th.Percentile) ~ as.numeric(summary_NOI_7[which(summary_NOI_7$Names.of.Characteristics ==
                                                                                                                                                            "Total_Number_Of_Employers"), ]$X99th.Percentile),
          TRUE ~ as.numeric(Total_Number_Of_Employers)
        ),
        
        
        # And some Feature Engineering        
        Average_Months_Worked_Per_Year_After_18 = case_when(
          Age == 18 ~ Total_Months_Work_Experience_Labor_Contracts / (Age + 1 - 18),
          TRUE ~ Total_Months_Work_Experience_Labor_Contracts / (Age - 18)
        ),
        
        
        # Is Mean salary for previous periods higher than latest salary        
        Is_Mean_Monthly_Salary_period_n_minus_2_Higher_Than_Latest = ifelse(
          Mean_Monthly_Salary_period_n_minus_2 > Latest_Salary_period_n_minus_0,
          1,
          0
        ),
        Is_Mean_Monthly_Salary_period_n_minus_1_Higher_Than_Latest = ifelse(
          Mean_Monthly_Salary_period_n_minus_1 > Latest_Salary_period_n_minus_0,
          1,
          0
        ),
        Is_Mean_Monthly_Salary_period_n_minus_0_Higher_Than_Latest = ifelse(
          Mean_Monthly_Salary_period_n_minus_0 > Latest_Salary_period_n_minus_0,
          1,
          0
        ),
        
        # Is EGN Function Code for previous periods higher than latest salary (position change)
        Is_Function_Code_1_Digit_period_n_minus_2_Higher_Than_Latest = ifelse(
          Function_Code_1_Digit_period_n_minus_2 > Latest_EGN_Function_Code_1_Digit,
          1,
          0
        ),
        Is_Function_Code_1_Digit_period_n_minus_1_Higher_Than_Latest = ifelse(
          Function_Code_1_Digit_period_n_minus_1 > Latest_EGN_Function_Code_1_Digit,
          1,
          0
        ),
        Is_Function_Code_1_Digit_period_n_minus_0_Higher_Than_Latest = ifelse(
          Function_Code_1_Digit_period_n_minus_0 > Latest_EGN_Function_Code_1_Digit,
          1,
          0
        ),
        
        # Is Economic Activity Code for previous periods higher than latest salary (threshold change)
        Is_Economic_Code_1_Digit_period_n_minus_2_Higher_Than_Latest = ifelse(
          Economic_Code_1_Digit_period_n_minus_2 > Latest_Economic_Activity_Code_1_Digit,
          1,
          0
        ),
        Is_Economic_Code_1_Digit_period_n_minus_1_Higher_Than_Latest = ifelse(
          Economic_Code_1_Digit_period_n_minus_1 > Latest_Economic_Activity_Code_1_Digit,
          1,
          0
        ),
        Is_Economic_Code_1_Digit_period_n_minus_0_Higher_Than_Latest = ifelse(
          Economic_Code_1_Digit_period_n_minus_0 > Latest_Economic_Activity_Code_1_Digit,
          1,
          0
        ),
        
        # Difference between min and max EGN Function Code for the three periods and the latest one
        Max_Difference_EGN_Function_Code = pmax(
          Function_Code_1_Digit_period_n_minus_2,
          Function_Code_1_Digit_period_n_minus_1,
          Function_Code_1_Digit_period_n_minus_0,
          Latest_EGN_Function_Code_1_Digit
        ) - pmin(
          Function_Code_1_Digit_period_n_minus_2,
          Function_Code_1_Digit_period_n_minus_1,
          Function_Code_1_Digit_period_n_minus_0,
          Latest_EGN_Function_Code_1_Digit
        ),
        
        # Difference between min and max EGN Function Code for the three periods and the latest one
        Max_Difference_EGN_Function_Code = pmax(
          Economic_Code_1_Digit_period_n_minus_2,
          Economic_Code_1_Digit_period_n_minus_1,
          Economic_Code_1_Digit_period_n_minus_0,
          Latest_Economic_Activity_Code_1_Digit
        ) - pmin(
          Economic_Code_1_Digit_period_n_minus_2,
          Economic_Code_1_Digit_period_n_minus_1,
          Economic_Code_1_Digit_period_n_minus_0,
          Latest_Economic_Activity_Code_1_Digit
        ),
        
        # Difference between Latest Salary and Credit Payment
        Diff_Latest_Salary_Credit_Payment = Latest_Salary_period_n_minus_0 - CreditPayment,
        
        # Difference in salary
        Diff_Salary = pmax(
          Mean_Monthly_Salary_period_n_minus_2,
          Mean_Monthly_Salary_period_n_minus_1,
          Mean_Monthly_Salary_period_n_minus_0,
          Latest_Salary_period_n_minus_0
        ) - pmin(
          Mean_Monthly_Salary_period_n_minus_2,
          Mean_Monthly_Salary_period_n_minus_1,
          Mean_Monthly_Salary_period_n_minus_0,
          Latest_Salary_period_n_minus_0
        ),
        
        # Average_Months_Worked_Per_Employer after 18
        Average_Months_Worked_Per_Employer = Total_Months_Work_Experience_Labor_Contracts /
          Total_Number_Of_Employers,
        
        # Was downgraded
        Was_Downgraded = case_when(
          Latest_EGN_Function_Code_1_Digit > Function_Code_1_Digit_period_n_minus_2 |
            Latest_EGN_Function_Code_1_Digit > Function_Code_1_Digit_period_n_minus_1 |
            Latest_EGN_Function_Code_1_Digit > Function_Code_1_Digit_period_n_minus_0 ~ 1,
          TRUE ~ 0
        ),
        
        # Has promotion
        Has_Promotion =
          case_when(
            Latest_EGN_Function_Code_1_Digit < Function_Code_1_Digit_period_n_minus_2 |
              Latest_EGN_Function_Code_1_Digit < Function_Code_1_Digit_period_n_minus_1 |
              Latest_EGN_Function_Code_1_Digit < Function_Code_1_Digit_period_n_minus_0 ~ 1,
            TRUE ~ 0
          )
        
        
      ) %>%      
      select(
        -Latest_EGN_Function_Code_period_n_minus_0,
        -Latest_Economic_Activity_Code_period_n_minus_0,
        -perc_time_one_active_contract_period_n_minus_2,
        -perc_time_two_active_contracts_period_n_minus_2,
        -perc_time_more_active_contracts_period_n_minus_2,
        -perc_time_one_active_contract_period_n_minus_1,
        -perc_time_two_active_contracts_period_n_minus_1,
        -perc_time_more_active_contracts_period_n_minus_1,
        -perc_time_one_active_contract_period_n_minus_0,
        -perc_time_two_active_contracts_period_n_minus_0,
        -perc_time_more_active_contracts_period_n_minus_0
      )#,
    # -Number_Of_Contracts_period_n_minus_2, -Number_Of_Contracts_period_n_minus_1,
    # -Number_Of_Employers_period_n_minus_2, -Number_Of_Employers_period_n_minus_1,
    # -Function_Code_1_Digit_period_n_minus_2, -Function_Code_1_Digit_period_n_minus_1,
    # -Economic_Code_1_Digit_period_n_minus_2, -Economic_Code_1_Digit_period_n_minus_1)
    
    
    if (Sys.info()[["user"]] == 'provenir') {
      xgb_model_NOI_7 <- readRDS("064_NOI_7_model.rds")
      
    } else {
      xgb_model_NOI_7 <-
        readRDS("./Files For Provenir/064_NOI_7_model.rds")
      
    }
    
    
    # Prediction
    PD <-
      predict(xgb_model_NOI_7,
              as.matrix(dataset_all_variables_processed[, -c(1, 2)]))
    
    InputVariables = toString(paste0(
      names(dataset_all_variables_processed[, -c(1, 2)]),
      ": ",
      dataset_all_variables_processed[, -c(1, 2)][1, ]
    ))
    
  },
  
  error = function(e) {
    Error <<- as.character(e)
  }
)
