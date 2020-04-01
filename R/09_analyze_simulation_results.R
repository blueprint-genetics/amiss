library(magrittr)
library(dplyr)
library(stringr)
library(here)

sim_data_paths <- read.csv(file = here("sim", "successfully_simulated_file_list.csv"), as.is = TRUE)[,2]
sim_result_paths <- paste0(sim_data_paths, "_output")

rf_rmses <- file.path(sim_result_paths, "rf_rmse.csv")
lr_rmses <- file.path(sim_result_paths, "lr_rmse.csv")
rf_perfs <- file.path(sim_result_paths, "rf_performance.csv")
lr_perfs <- file.path(sim_result_paths, "lr_performance.csv")

rbind_csvs <- function(paths) {
  csvs <- lapply(paths, . %>% read.csv(as.is = TRUE))
  paths_components <- data.frame(str_match(paths, pattern = "repeat_([0-9]+).*(0.[1-9])_([MNCAR]+)_(LEFT|RIGHT|MID|TAIL)")[,-1])
  colnames(paths_components)[1:4] <- c("repeat", "pct", "mech", "orientation")
  csvs <- mapply(function(path_ix, csv) {
    path <- paths_components[rep(path_ix, NROW(csv)),, drop = FALSE]
    cbind(path, csv)
  }, path_ix = 1:NROW(paths_components), csv = csvs, SIMPLIFY = FALSE)
  csv_df <- dplyr::bind_rows(csvs)
  return(csv_df)
}
rf_rmse_df <- rbind_csvs(rf_rmses)
colnames(rf_rmse_df)[colnames(rf_rmse_df) == "X"] <- "method"
lr_rmse_df <- rbind_csvs(lr_rmses)
colnames(lr_rmse_df)[colnames(lr_rmse_df) == "X"] <- "method"

rf_perf_df <- rbind_csvs(rf_perfs)
lr_perf_df <- rbind_csvs(lr_perfs)

rf_df <- merge(rf_perf_df, rf_rmse_df, by = c("repeat", "pct", "mech", "orientation", "method"), all = TRUE)
lr_df <- merge(lr_perf_df, lr_rmse_df, by = c("repeat", "pct", "mech", "orientation", "method"), all = TRUE)

write.csv(x = rf_df, here("sim", "simulated_rf_results.csv"))
write.csv(x = lr_df, here("sim", "simulated_lr_results.csv"))
