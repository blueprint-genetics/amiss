library(here)

source(here("R", "impute_and_train.R"))

impute_and_train(training_path = here("data", "preprocessed_training_data.csv"), outcome_path = here("data", "training_outcomes.csv"), output_path = here("output"), cores=24, seed = 42, lean = FALSE)

write(capture.output(sessionInfo()), here("04_run_impute_and_train_sessioninfo.txt"))
