
source("R/05_test_prediction.R")

test_prediction(test_path="contracted_test_data.csv", outcome_path="test_outcomes.csv", tr_output_path="output", results_dir_path="output/results", lean = FALSE, cores=16, seed = 42) 

write(capture.output(sessionInfo()), "05_run_prediction_sessioninfo.txt")