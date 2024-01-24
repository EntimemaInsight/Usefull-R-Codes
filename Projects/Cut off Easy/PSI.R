Harold$ProposalDate <- as.Date(Harold$ProposalDate)

library(zoo)
Harold$Period <- format(Harold$ProposalDate, "%Y-%m")
Harold$Quarter <- as.yearqtr(Harold$ProposalDate) 

library(eeptools)
Harold <- Harold %>%
  select(CreditCode, Period, Quarter, ClientEducation, TypeOfContract, StaffType, Automobiles, GetSalaryType, TotalLengthOfService, TypeOfHousing, ClientFamilyStatus, CreditPurpose, WorkExperienceCurrentWork, ClientSex, YearsOnCurrentAddress, ClientFamilySize, PartnerConnectionType, CountPreviousCredits, ClientBirtDate) %>%
  left_join(All_Loans %>% select(CreditBeginDate, CodeContract), by = c("CreditCode" = "CodeContract"))%>%
  filter(Quarter >= "2018 Q2")

Harold$CreditBeginDate <- as.Date(Harold$CreditBeginDate)  
Harold$ClientBirtDate <- as.Date(Harold$ClientBirtDate)

Harold <- Harold %>%
  filter(CreditBeginDate > ClientBirtDate) %>%
  mutate(Age = age_calc(ClientBirtDate, enddate = CreditBeginDate, units = "years", precise = TRUE))


Loop <- c("ClientEducation", "TypeOfContract", "StaffType", "Automobiles", "GetSalaryType", "TotalLengthOfService", "TypeOfHousing", "ClientFamilyStatus", "CreditPurpose", "WorkExperienceCurrentWork", "ClientSex", "YearsOnCurrentAddress", "ClientFamilySize", "PartnerConnectionType", "CountPreviousCredits", "Age") 

Loop.function <- function(arg1){
  output <- Harold[, c("Quarter", arg1)] %>%
    filter(Quarter >= "2018 Q2") %>%
    mutate_at(arg1, as.factor) %>%
    group_by(.dots = list(arg1)) %>%
    mutate(Total = n()) %>%
    group_by(Quarter) %>%
    mutate(Total = n()) %>%
    ungroup() %>%
    group_by(.dots = list("Quarter",arg1)) %>%
    summarize(Total = mean(Total),
              Broi = n()) %>%
    ungroup() %>%
    mutate(Percent = (Broi / Total) * 100,
           Quarter = as.character(Quarter)) %>%
    arrange_at(c("Quarter",arg1))
  
  return(output)
}

FinalPercentages <- lapply(Loop, Loop.function)


PSI.function <- function(arg1){
  nameColumn <- colnames(arg1)[2]
  output <- arg1 %>%
    filter(is.na(!!as.symbol(nameColumn)) == 0) %>%
    group_by(.dots = list(nameColumn)) %>%
    mutate(test = row_number(),
           Percent1 = Percent[1],
           Part1 = Percent - Percent[1],
           Part2 = log(Percent / Percent[1]),
           Part3 = Part1 * Part2) %>%
    ungroup() %>%
    group_by(Quarter) %>%
    summarise(PSI = sum(Part3) / 100)
  
  names(output) <- c('Quarter', paste0('PSI_', nameColumn))
  return(output)
}

FinalPSI <- lapply(FinalPercentages, PSI.function)
PSI_table <- do.call(bind_cols, FinalPSI)

library(openxlsx)
out <- createWorkbook()
addWorksheet(out, "PSI")
writeData(out, sheet = "PSI", x = PSI_table)
saveWorkbook(out, "PSI_results.xlsx")
