parseAnafXmlDecizie <-
function(x){
  
  #Load libraries the function is using
  library(XML)
  library(xml2)
  library(httr)
  library(dplyr)
  
  ### Check if the file is really an xml type file
  # Get the file extention in a astring using the file_ext function from the tools package
  fileExtention = tools::file_ext(x)
  
  # If the loaded file is an xml file - continue with the parsing
  if (fileExtention == "xml"){
    
    #Read xml file - into xml object using xml parse
    anaf = xmlParse(file = x,useInternalNodes = TRUE)
    
    ### Convert the xml object to list using xmlToList
    anaf_list = xmlToList(anaf)
    
    # If the xml is empty - xmlToList wil return a character object
    # If this is the case - return NULL as there is no info in the xml
    if (class(anaf_list) == "character"){
      return(NULL)
      
    }
    
    # If the xml is not empty - continue with the parsing
    else{
      # Extract DocumentDate - the date on which the ANAF document has been generated
      DocumentDate = data.frame(data = anaf_list[[".attrs"]]) %>%
        tibble::rownames_to_column()%>%
        filter(rowname == "Data_creare_mesaj")%>%
        mutate(data = substring(data,1,8))
      
      # Extract information from the list structured xml - all the info in persoana
      # On this step I am assuming that every xml will have all its information in persoana tag -
      # which is the case as of 24 August 2018
      fullXMLInfo = anaf_list[["persoana"]]
      
      #Extract only the attribute from the base element - there is the info about Name, CNP and Address
      nameCNP = data.frame(metaData = fullXMLInfo[".attrs"][[1]]) %>%
        tibble::rownames_to_column()%>% # keep rownames as a separate column
        filter(rowname == "CNP") # filter only the rowname with the CNP number
      
      # Extract here all the names of the lists in persoana - all tags with info
      persoanaNames = names(fullXMLInfo)
      
      #Check wether decizie tags are part of persoana - if they are - get and save their indexes - else return Null
      # VERY IMPORTANT!!! - Here I assume that most of the xml files will possess D112 tags
      if ("decizie" %in% persoanaNames) {
        
        #Get d112 indexes - positions of the D112 lists
        DecizieIndexes = which(persoanaNames == "decizie")
        
        
      }
      
      # Return NULL if the xml file doesn't contain D112 tags 
      else{
        return(NULL)
      }
      
      # Extract D112 lists using the indexes found on the previos step
      DecizieLists = fullXMLInfo[DecizieIndexes]
      
      # Extract D112 info from the full d112 list info - with for loop
      
      for (i in 1:length(DecizieLists)){
        if (i == 1){
          #Create the initial data frame on the first iteration; note that unlist unlists at one go a nested list
          DecizieDF <- data.frame(lapply(unlist(DecizieLists[i]), type.convert), stringsAsFactors=FALSE)
        } else {
          # Create a temporary data frame at each iteration - with info from each D112 list
          tempD112DF <- data.frame(lapply(unlist(DecizieLists[i]), type.convert), stringsAsFactors=FALSE)
          
          # Bind the temporary data frame to the initial data frame at each iteration
          #DecizieDF <-merge(DecizieDF, tempD112DF, all=T) # working binding functionallity with merge
          DecizieDF <-bind_rows(DecizieDF, tempD112DF) 
          
        }
      }
      
      #Add the CNP to the data frame to be able to associate the rows with the person to whom the information belongs
      DecizieDF$CNP = nameCNP$metaData
      
      DecizieDF$DocumentDate = DocumentDate$data
      
      #Return all the ANAF info scrapped from the xml
      return(DecizieDF)
      
      
    }
    
    
  }
  
  # Define return value - text warning - that one or more of the files loaded in the function is not an xml file
  else{
    return(print("You are trying to load a non-xml file. Please load an xml file!"))
  }
  
  
}
