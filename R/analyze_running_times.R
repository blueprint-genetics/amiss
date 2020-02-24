library(purrr)
library(magrittr)

rf_imputers <- readRDS(file = "output/rf_imputers.rds")
lr_imputers <- readRDS(file = "output/lr_imputers.rds", refhook = function(x) .GlobalEnv)

rf_times <- map(.x = rf_imputers, function(x) attr(x, "timing"))
lr_times <- map(.x = rf_imputers, function(x) attr(x, "timing"))

rf_times_df <- do.call(rbind, rf_times) %>% data.frame
lr_times_df <- do.call(rbind, lr_times) %>% data.frame

rf_times_df$method <- row.names(rf_times_df)
lr_times_df$method <- row.names(lr_times_df)

rf_times_df$classifier <- "rf"
lr_times_df$classifier <- "lr"

times_df <- rbind(rf_times_df, lr_times_df) %>% extract(c("method", "classifier", "elapsed"))
row.names(times_df) <- NULL

# Stochastic methods are timed over the repetitions, so need to be divided by the number of repetitions (10)
times_df[times_df$method %in% c("pmm", "norm.predict", "norm", "rf", "knnImpute", "missForest"), "elapsed"] %<>% `/`(10)
