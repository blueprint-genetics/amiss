
source("R/04_impute_and_train.R")

impute_and_train(training_path="contracted_training_data.csv", outcome_path="training_outcomes.csv", output_path="output_20_3_2020", cores=24, seed = 42, lean = FALSE)

write(capture.output(sessionInfo()), "04_run_impute_and_train_sessioninfo.txt")
