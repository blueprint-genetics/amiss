library(magrittr)
library(dplyr)
library(stringr)
library(here)

source(here("R", "constants.R"))

sim_data_paths <- read.csv(file = here("output", "sim", FILE_SUCCESSFULLY_SIMULATED_FILE_LIST_CSV), as.is = TRUE)[,2]
sim_result_paths <- paste0(sim_data_paths, "_output")

rf_rmses <- file.path(sim_result_paths, FILE_RF_RMSE_CSV)
lr_rmses <- file.path(sim_result_paths, FILE_LR_RMSE_CSV)
rf_perfs <- file.path(sim_result_paths, FILE_RF_PERFORMANCE_CSV)
lr_perfs <- file.path(sim_result_paths, FILE_LR_PERFORMANCE_CSV)

rf_times <- file.path(sim_result_paths, "rf_run_times.csv")
lr_times <- file.path(sim_result_paths, "lr_run_times.csv")

rbind_csvs <- function(paths) {
  csvs <- lapply(paths, . %>% read.csv(as.is = TRUE))
  paths_components <- data.frame(str_match(paths, pattern = "repeat_([0-9]+).*(0.[1-9])_([MNCAR]+)")[,-1])
  colnames(paths_components)[1:3] <- c("repeat", "pct", "mech")
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

rf_df <- merge(rf_perf_df, rf_rmse_df, by = c("repeat", "pct", "mech", "method"), all = TRUE)
lr_df <- merge(lr_perf_df, lr_rmse_df, by = c("repeat", "pct", "mech", "method"), all = TRUE)

write.csv(x = rf_df, here("output", "sim", FILE_SIMULATED_RF_RESULTS_CSV))
write.csv(x = lr_df, here("output", "sim", FILE_SIMULATED_LR_RESULTS_CSV))

rf_times_df <- rbind_csvs(rf_times)
rf_times_df$X <- NULL # Row names
rf_times_df$classifier <- "RF"
lr_times_df <- rbind_csvs(lr_times)
lr_times_df$X <- NULL # Row names
lr_times_df$classifier <- "LR"
times_df <- rbind(rf_times_df, lr_times_df)

write.csv(x = times_df, here("output", "sim", "times.csv"))

write(capture.output(sessionInfo()), here("output", "09_analyze_simulation_results_sessioninfo.txt"))
