# Function to load data from dwh

# N.B.!!! - The name of the database should be in quatation marks; query should be a sql query loaded from file

#Query <- read_file('./SQL Queries/010_ActivationDates_BadFlag_CF.sql')


read_data_from_dwh_hidalgo = function(db, query) {
  
  myc <- DBI::dbConnect(odbc::odbc()
                        , driver = "SQL Server"
                        , server = "hidalgo.smartitbg.int"
                        , database = db
  )
  
  
  start_time <- Sys.time()
  Data_Table <- DBI::dbFetch(DBI::dbSendQuery(myc, query)) 
  print(paste("Done in", round(difftime(Sys.time(), start_time,unit = "secs"),0), "seconds"))
  
  return(Data_Table) 
}



#test_data = read_data_from_dwh_hidalgo(db = "BIsmartWCRO",query = Query)




