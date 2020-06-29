library(here)

source(here("R", "constants.R"))
source(here("R", "impute_and_train.R"))
source(here("R", "utils.R"))
source(here("R", "imputation_definitions.R"))

cores <- get_env_cores()

impute_and_train(training_path = here("output", "data", FILE_PREPROCESSED_TRAINING_DATA_CSV),
                 outcome_path = here("output", "data", FILE_TRAINING_OUTCOMES_CSV),
                 output_path = here("output"),
                 mice_hyperparameter_grids = mice_hyperparameter_grids,
                 other_hyperparameter_grids = other_hyperparameter_grids,
                 single_value_imputation_hyperparameter_grids = single_value_imputation_hyperparameter_grids,
                 cores = cores, seed = 42, lean = FALSE)

write(capture.output(sessionInfo()), here("output", "04_run_impute_and_train_sessioninfo.txt"))
