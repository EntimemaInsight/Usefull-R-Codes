# apply like #
library(furrr)

plan(multisession, workers = 2)
future_map(c("hello", "world"), ~.x)
plan(multisession, workers = 2)




# Loop like #
library(foreach)
library(doParallel)


# Make cluster
cl = makeCluster(10)
registerDoParallel(cl) # register the parallel backend with the foreach package
# Set locale for each node in order to deal with bulgarian - did not solve problem
# clusterEvalQ(cl, Sys.setlocale("LC_ALL", "Bulgarian_Bulgaria.1251"))

system.time({
  wc_bg_models_df = foreach(
    i = 1:length(Provenir_Paths),
    .packages = c("XML", "tidyverse", "lubridate"),
    .combine = rbind
  ) %dopar% {
    extract_wc_bg_models_input_data(i, Provenir_Paths)
  }
})
stopCluster(cl)