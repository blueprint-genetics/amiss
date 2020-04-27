library(here)

source(here("R", "constants.R"))
source(here("R", "predict_on_test_set.R"))
source(here("R", "utils.R"))

cores <- get_env_cores()

predict_on_test_set(test_path = here("output", "data", FILE_PREPROCESSED_TEST_DATA_CSV),
                    outcome_path = here("output", "data", FILE_TEST_OUTCOMES_CSV),
                    tr_output_path = here("output"),
                    results_dir_path = here("output", "results"),
                    lean = FALSE, cores = cores, seed = 42)

write(capture.output(sessionInfo()), here("output", "05_run_prediction_sessioninfo.txt"))
