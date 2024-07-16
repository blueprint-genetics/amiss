library(here)
library(magrittr)
library(amiss)

source(here("R", "constants.R"))
source(here("R", "imputation_definitions.R"))

impute_and_train(training_path = here("output", FILE_PREPROCESSED_TRAINING_DATA_CSV),
                 outcome_path = here("output", FILE_TRAINING_OUTCOMES_CSV),
                 output_path = here("output"),
                 mice_hyperparameter_grids = list(),
                 other_hyperparameter_grids = list(),
                 single_value_imputation_hyperparameter_grids = single_value_imputation_hyperparameter_grids,
                 parameter_list=rjson::fromJSON(file = "combination_orig.json"),
                 cores = 1, seed = 10, lean = TRUE)

write(capture.output(sessionInfo()), here("output", "04_run_impute_and_train_sessioninfo.txt"))
