

DateUntill <-  as_date('2016-01-01')
DatesList  <- seq.Date(from = DateUntill, length.out = 33, by = '1 month') 
### Create Data Frame with directories and dates 
DirectoriesDF <-   data.frame( DirPath = list.dirs('//10.254.1.4\\Files\\Analysis\\3.Data', recursive = FALSE)
                               ,stringsAsFactors =  FALSE) %>% 
  mutate(ReportDate = as.Date(paste(gsub(".*./" ,"",DirPath), '01', sep = '.'), format = '%Y.%m.%d')) %>% 
  arrange(desc(ReportDate)) %>%
  filter(!is.na(ReportDate)
         ,ReportDate %in% DatesList
  ) %>%
  filter(row_number() != 1)

DirectoriesFilesList <- do.call(c,lapply(file.path(as.list(DirectoriesDF$DirPath),'Easy Credit UKR'), list.files, full.names = TRUE))

#test <- as.data.frame(DirectoriesFilesList)
substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}

Progresses_Location <- DirectoriesFilesList[grepl("E024_Export_Finished",DirectoriesFilesList)]

SelectColumns <- function(x) {
  
  require(dplyr)
  require(readr)
  
  data <- readr::read_delim(file = x, delim = ';', na = c('','NULL'), quote = "")
  
}

Progresses <- lapply(Progresses_Location, SelectColumns)

ProgressesCombined <- do.call(bind_rows, Progresses)
ProgressesCombined1 <- ProgressesCombined %>%
                            mutate(KID = CodeContract) %>% 
                            select(KID, RefinanceType)


###Select necessary columns from Harold

Harold_Latest_UKR_1 <- Harold_Latest_UKR %>% 
  
  select(KID, TypeOfContract , MoneyGrantedDateByCashDesk, Scoring, ScoringType, ClientEGN, RefinansNew) %>%
  left_join(CreProg_Latest_UKR_1, by = "KID") %>%
  arrange(ClientEGN, MoneyGrantedDateByCashDesk)%>%
  group_by(ClientEGN)%>%
  mutate(CreditNumber = seq(n()), 
         Fist_Credit = case_when(CreditNumber == 1 ~ 1, 
                                 TRUE ~ 2))
####Check Rule 3
Rule3 <- Harold_Latest_UKR_1 %>% 
            filter(is.na(RefinansNew))%>%
            arrange(ClientEGN, MoneyGrantedDateByCashDesk)%>%
            group_by(ClientEGN)%>%
            mutate(LastCreditCompleteDate = lag(CompleteDate), 
                   NextBeginDate = lead(CreditBeginDate), 
                   PreviousDelay = lag(Delay),
                   LastP = lag(SumaP),
                   NexDelay = lead(Delay), 
                   
                   NextSumaP = lead(SumaP), 
                #### Delay of the previous credit 
                   DelayGroup = case_when(PreviousDelay< 31 ~ "0-30 Days", 
                                                 PreviousDelay < 61 & PreviousDelay > 30 ~ "31-60 Days", 
                                                 PreviousDelay < 91 & PreviousDelay >60 ~ "61-90 Days", 
                                                 PreviousDelay >90 ~"Above 90")) %>%
            ungroup () %>% 
            mutate(DiffDays = CreditBeginDate - LastCreditCompleteDate, 
                   NextDiffDays = NextBeginDate - CompleteDate ) %>%
            filter(Year >= 2016)

Rule3.1 <- Rule3 %>% left_join(ProgressesCombined1, by = "KID")  %>%
                    arrange(ClientEGN, MoneyGrantedDateByCashDesk)%>%
                    group_by(ClientEGN) %>% 
                    mutate(PreviousRefinanceType = lag(RefinanceType)) %>%
                    ungroup()%>%
                      mutate(Closed = case_when(is.na(CompleteDate) ~ 0, TRUE ~ 1)) %>%
                    #  filter(Closed == 1) %>% 
                    #  filter(CreditNumber >1) %>% 
                    #  filter(!is.na(NextBeginDate))%>%
                      filter(PreviousRefinanceType <= 3) %>%
                      #filter(!is.na(PreviousRefinanceType)) %>%
                      mutate(Overrides = case_when( DiffDays <7 & PreviousRefinanceType == 1 ~ "Override Group 1", 
                                                    DiffDays <3 & PreviousRefinanceType == 2 ~ "Override Group 2",
                                                    DiffDays == 7 & PreviousRefinanceType == 1 ~ "Group 1 BEP", 
                                                    DiffDays == 3 & PreviousRefinanceType == 2 ~ "Group 2 BEP",
                                                    PreviousRefinanceType == 3 ~ "Normal Group 3", 
                                                    PreviousRefinanceType == 1 ~ "Normal Group 1", 
                                                    PreviousRefinanceType == 2 ~ "Normal Group 2", TRUE ~ "Else"
                                                  )) %>% 
                      mutate(Target = case_when(Delay <91 ~0, TRUE ~1)) %>%
                      arrange(PreviousRefinanceType, DiffDays)

summary(Rule3.1$Delay)

Rule3_stats <- Rule3.1 %>% filter(Period <= 201803)%>%
                      group_by(Overrides)%>% 
                      summarise(Total = n(), 
                                AvgSumaP = mean(SumaP, na.rm = T), 
                                AvgLastSumaP = mean(LastP, na.rm = T), 
                                RR_Total = round(sum(PayedTotal, na.rm = T)/ sum(SumToGetBack, na.rm = T),2), 
                                RR_SumaP = round(sum(PayedTotal, na.rm = T)/ sum(SumaP, na.rm = T), 2), 
                                AvgDaySiff = round(mean(DiffDays, na.rm = T), 2),
                                TermAvg = round(mean(Weeks),0),
                                DelayAvg = round(mean(Delay),2),
                                meanTarget = round(mean(Target),2),
                                MedianDelayAvg = round(median(Delay, na.rm = T),0),
                                AvgPreviousDelay = round(mean(PreviousDelay, na.rm = T),0),
                                ClosedAccounts = sum(!is.na(CompleteDate)), 
                                ClosedAccounts = round(sum(!is.na(CompleteDate))/ Total, 2))
  
  

WriteExcel(Rule3_stats)

#### Create vectors for wilcox test ####
