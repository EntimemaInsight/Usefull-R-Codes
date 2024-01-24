univariateAnalysisTableDiscrete <-
function(OutcomeVector,varBinnedVector){
  outputTable = data.frame(OutcomeVector,varBinnedVector)%>%
    mutate(total = n(), totalBads = sum(OutcomeVector), totalGoods = total - totalBads) %>%
    group_by(varBinnedVector) %>%
    summarise( Count = n()
               , total = mean(total)
               , totalGoods = mean(totalGoods)
               , totalBads = mean(totalBads)
               , GroupPercentage = Count/total
               , Bads = sum(OutcomeVector)
               , BadsPercentage = Bads/totalBads
               , Goods = Count - Bads
               , GoodsPercentage = Goods/totalGoods
               , GR = Goods/Count
               , BR = Bads/Count
               , GoodBadOdds = Goods/Bads
               , LogOdds = log(GoodBadOdds)
               , WOE = log(GoodsPercentage/BadsPercentage)
               , IV = (sum(GoodsPercentage - BadsPercentage))*WOE
               
               
    ) %>% ungroup() %>%
    mutate( CumGood = cumsum(GoodsPercentage), CumBad = cumsum(BadsPercentage))
  
  outputTable = outputTable %>%
    bind_rows( 
      outputTable %>% 
        summarise(varBinnedVector = " Total"
                  , Count = sum(Count)
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
  
  outputTable = outputTable[, -c(3,4,5)]
  return(outputTable)
}
