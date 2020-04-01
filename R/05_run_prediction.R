library(here)

source(here("R", "predict_on_test_set.R"))

predict_on_test_set(test_path = here("data", "preprocessed_test_data.csv"), outcome_path = here("data", "test_outcomes.csv"), tr_output_path = here("output"), results_dir_path = here("output", "results"), lean = FALSE, cores=16, seed = 42)

write(capture.output(sessionInfo()), here("05_run_prediction_sessioninfo.txt"))
