CreateDateGrid <- function(x,y,z) {
  
                                        require(dplyr)
                                        
                                        Seq <- seq(as.Date(x),as.Date(y), by = z)
                                        
                                        DF <- expand.grid(RefDate = Seq, FollowingDate = Seq)
                                              filter(DF, FollowingDate >= RefDate) %>% 
                                                arrange(RefDate)
                                        
                                      }