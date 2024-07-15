library(here)
library(amiss)

source(here("R", "constants.R"))

predict_on_test_set(test_path = here("output", FILE_PREPROCESSED_TEST_DATA_CSV),
                    outcome_path = here("output", FILE_TEST_OUTCOMES_CSV),
                    tr_output_path = here("output"),
                    results_dir_path = here("output", "results"),
                    seed = 10)

write(capture.output(sessionInfo()), here("output", "05_run_prediction_sessioninfo.txt"))
