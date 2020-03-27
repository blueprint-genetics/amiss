library(stringr)
library(futile.logger)

source("R/recursive_application.R")

flog.appender(appender.tee("10_describe_simulated_data.log"), name = "desc_sim_logger")
flog.threshold(DEBUG, name = "desc_sim_logger")

orig_data <- read.csv("contracted_training_data.csv", row.names = 1)
orig_data <- scale(orig_data, center= TRUE, scale = TRUE)
sim_data_paths <- read.csv(file = "simulated_file_list.csv", as.is = TRUE)[,2]
names(sim_data_paths) <- seq_along(sim_data_paths)

paths_components <- data.frame(str_match(sim_data_paths, pattern = "repeat_([0-9]+).*(0.[1-9])_([MNCAR]+)_(LEFT|RIGHT|MID|TAIL)")[,-1])
colnames(paths_components)[1:4] <- c("repeat", "pct", "mech", "orientation")

summaries <- list()
for (path in enumerate(sim_data_paths)) {
  sim_data <- read.csv(path$value, row.names = 1)
  sm <- list()
  for(col in enumerate(sim_data)) {
    orig_data_miss <- is.na(orig_data[,col$name])
    simu_data_miss <- is.na(col$value)
    sm[[col$name]] <- summary(orig_data[!orig_data_miss & simu_data_miss, col$name])
    sm[[col$name]] <- rbind(as.matrix(sm[[col$name]]), na_add = sum(!orig_data_miss & simu_data_miss))
    sm[[col$name]] <- t(sm[[col$name]])
    row.names(sm[[col$name]]) <- col$name
  }
  sm <- data.frame(do.call(rbind, sm))
  sm$variable <- row.names(sm)
  sm <- cbind(sm, paths_components[path$name,])
  summaries[[path$name]] <- sm
}
 
summaries_df <- do.call(rbind, summaries)

write.csv(summaries_df, file = "simulated_data_summaries.csv")

write(capture.output(sessionInfo()), "10_describe_simulated_data_sessioninfo.txt")
