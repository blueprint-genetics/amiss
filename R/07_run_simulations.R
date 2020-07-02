library(here)

source(here("R", "constants.R"))
source(here("R", "compute_rmse.R"))
source(here("R", "impute_and_train.R"))
source(here("R", "predict_on_test_set.R"))
source(here("R", "utils.R"))
source(here("R", "imputation_definitions.R"))

other_hyperparameter_grids$missForest <- NULL # missForest takes so long that it is not worth running in simulations

flog.appender(appender.tee(here("output", "07_run_simulations.log")), name = "simulation_logger")
flog.threshold(DEBUG, name = "simulation_logger")

seed <- 10
cores <- get_env_cores()

registerDoParallel(cores)

sim_data_paths <- read.csv(file = here("output", "sim", FILE_SIMULATED_FILE_LIST_CSV), as.is = TRUE)[,2]

successes <- foreach(sim_data_path = sim_data_paths, .options.RNG = seed) %dorng% {
  
  success <- tryCatch({
    flog.pid.info("Imputing and training on %s", sim_data_path, name = "simulation_logger")
    output_path <- paste0(sim_data_path, "_output")
    
    flog.pid.info("Parameters:", name = "simulation_logger")
    iat_params <- list(training_path = sim_data_path,
                       outcome_path = here("output", "data", FILE_TRAINING_OUTCOMES_CSV),
                       output_path = output_path,
                       mice_hyperparameter_grids = mice_hyperparameter_grids,
                       other_hyperparameter_grids = other_hyperparameter_grids,
                       single_value_imputation_hyperparameter_grids = single_value_imputation_hyperparameter_grids,
                       cores = 1,
                       seed = seed,
                       lean = TRUE)
    flog.pid.info(paste0(names(iat_params), " = ", iat_params), name = "simulation_logger")
    
    do.call(impute_and_train, iat_params)
    
    flog.pid.info("Producing performance statistics on %s", sim_data_path, name = "simulation_logger")
    test_params <- list(
      test_path = here("output", "data", FILE_PREPROCESSED_TEST_DATA_CSV),
      outcome_path = here("output", "data", FILE_TEST_OUTCOMES_CSV),
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
      imputer_path = file.path(output_path, FILE_RF_CLASSIFIERS_RDS),
      orig_data_path = here("output", "data", FILE_PREPROCESSED_TRAINING_DATA_CSV),
      simu_data_path = sim_data_path,
      output_filename = file.path(output_path, FILE_RF_RMSE_CSV)
    )
    flog.pid.info(paste0(names(rf_rmse_params), " = ", rf_rmse_params), name = "simulation_logger")
    do.call(compute_rmse, rf_rmse_params)

    lr_rmse_params <- list(
      imputer_path = file.path(output_path, FILE_LR_CLASSIFIERS_RDS),
      orig_data_path = here("output", "data", FILE_PREPROCESSED_TRAINING_DATA_CSV),
      simu_data_path = sim_data_path,
      output_filename = file.path(output_path, FILE_LR_RMSE_CSV)
    )
    flog.pid.info(paste0(names(lr_rmse_params), " = ", lr_rmse_params), name = "simulation_logger")
    do.call(compute_rmse, lr_rmse_params)

    flog.pid.info("Completed analysis of %s", sim_data_path, name = "simulation_logger")

    TRUE

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

write.csv(sim_data_paths[successes], file = here("output", "sim", FILE_SUCCESSFULLY_SIMULATED_FILE_LIST_CSV))
saveRDS(object=.Random.seed, file = here("output", "sim", "simulations_last_seed.RDS"))

write(capture.output(sessionInfo()), here("output", "07_run_simulations_sessioninfo.txt"))
