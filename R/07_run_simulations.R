
source("R/compute_rmse.R")
source("R/impute_and_train.R")
source("R/predict_on_test_set.R")

flog.appender(appender.tee("07_run_simulations.log"), name = "simulation_logger")
flog.threshold(DEBUG, name = "simulation_logger")

seed <- 42
cores <- 24
registerDoParallel(cores)

sim_data_paths <- read.csv(file = "simulated_file_list.csv", as.is = TRUE)[,2]

successes <- foreach(sim_data_path = sim_data_paths, .options.RNG = seed) %dorng% {
  
  success <- tryCatch({
    flog.pid.info("Imputing and training on %s", sim_data_path, name = "simulation_logger")
    output_path <- paste0(sim_data_path, "_output")
    
    flog.pid.info("Parameters:", name = "simulation_logger")
    iat_params <- list(training_path = sim_data_path,
                       outcome_path = "training_outcomes.csv",
                       output_path = output_path,
                       cores = 1,
                       seed = seed,
                       lean = TRUE)
    flog.pid.info(paste0(names(iat_params), " = ", iat_params), name = "simulation_logger")
    
    do.call(impute_and_train, iat_params)
    
    flog.pid.info("Producing performance statistics on %s", sim_data_path, name = "simulation_logger")
    test_params <- list(
      test_path = "preprocessed_test_data.csv",
      outcome_path = "test_outcomes.csv",
      tr_output_path = output_path,
      results_dir_path = output_path,
      lean = TRUE,
      cores = 1,
      seed = seed
    )
    flog.pid.info(paste0(names(test_params), " = ", test_params), name = "simulation_logger")
    do.call(predict_on_test_set, test_params)

    flog.pid.info("Computing RMSE values on %s", sim_data_path, name = "simulation_logger")

    flog.pid.info("Parameters:", name = "simulation_logger")
    rf_rmse_params <- list(
      imputer_path = file.path(output_path, "rf_classifiers.rds"),
      orig_data_path = "preprocessed_training_data.csv",
      simu_data_path = sim_data_path,
      output_filename = file.path(output_path, "rf_rmse.csv")
    )
    flog.pid.info(paste0(names(rf_rmse_params), " = ", rf_rmse_params), name = "simulation_logger")
    do.call(compute_rmse, rf_rmse_params)

    lr_rmse_params <- list(
      imputer_path = file.path(output_path, "lr_classifiers.rds"),
      orig_data_path = "preprocessed_training_data.csv",
      simu_data_path = sim_data_path,
      output_filename = file.path(output_path, "lr_rmse.csv")
    )
    flog.pid.info(paste0(names(lr_rmse_params), " = ", lr_rmse_params), name = "simulation_logger")
    do.call(compute_rmse, lr_rmse_params)

    flog.pid.info("Completed analysis of %s", sim_data_path, name = "simulation_logger")

    return(TRUE)

  }, error = function(e) {
    flog.pid.debug(e, name = "simulation_logger")
    return(FALSE)
  })

  return(success)
  
}
successes <- unlist(successes)

if (any(!successes)) {
  flog.pid.info(paste0("Simulation ", sim_data_paths[!successes], " failed"), name = "simulation_logger")
}

flog.pid.info("%d / %d simulations performed successfully", sum(successes), length(sim_data_paths), name = "simulation_logger")

write.csv(sim_data_paths[successes], file = "successfully_simulated_file_list.csv")
saveRDS(object=.Random.seed, file = "simulations_last_seed.RDS")

write(capture.output(sessionInfo()), "07_run_simulations_sessioninfo.txt")
