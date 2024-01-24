univariateAnalysisTable_v2 <-
function(OutcomeVector,varBinnedVector,varContinuosVector){
    outputTable = data.frame(OutcomeVector,varBinnedVector,varContinuosVector)%>%
      mutate(total = n(), totalBads = sum(OutcomeVector), totalGoods = total - totalBads) %>%
      group_by(varBinnedVector) %>%
      summarise(Count = n()
                , Mean = mean(varContinuosVector)
                , Min = min(varContinuosVector)
                , Max = max(varContinuosVector)
                , total = mean(total)
                , totalGoods = mean(totalGoods)
                , totalBads = mean(totalBads)
                , GroupPercentage = Count/total
                , Bads = sum(OutcomeVector)
                , Goods = Count - Bads
                , GoodsPercentage = Goods/totalGoods
                , BadsPercentage = Bads/totalBads
                , GR = Goods/ Count
                , BR = Bads/Count
                , GoodBadOdds = Goods/Bads
                , LogOdds = log(GoodBadOdds)
                , WOE = log(GoodsPercentage/BadsPercentage)
                , IV = (sum(GoodsPercentage - BadsPercentage))*WOE
                
                
      ) %>% ungroup() %>%
      mutate(CumGood = cumsum(GoodsPercentage), CumBad = cumsum(BadsPercentage))
    
    outputTable = outputTable %>%
      #mutate(varBinnedVector = as.numeric(varBinnedVector)) %>%
      #arrange(varBinnedVector) %>%
      #mutate(varBinnedVector = as.character(varBinnedVector)) %>%
      bind_rows( 
        outputTable %>% 
          summarise(varBinnedVector = " Total"
                    , Count = sum(Count)
                    , Mean = NA
                    , Min = NA
                    , Max = NA
                    , total = NA
                    , totalGoods = NA
                    , totalBads = NA
                    , GroupPercentage = sum(GroupPercentage)
                    , Goods = sum(Goods)
                    , GoodsPercentage = sum(GoodsPercentage)
                    , Bads = sum(Bads)
                    , BadsPercentage = sum(BadsPercentage)
                    , GR = Goods/Count
                    , BR = Bads/Count
                    , GoodBadOdds = NA
                    , LogOdds = NA
                    , WOE = NA
                    , IV = sum(IV)
                    , CumGood = 1
                    , CumBad = 1
                    
          )
        
      )
    
    outputTable = outputTable[, -c(6,7,8)]
    return(outputTable)
  }
