library(here)

source(here("R", "visualizations.R"))
source(here("R", "constants.R"))
source(here("R", "impute_and_train.R"))
source(here("R", "predict_on_test_set.R"))
source(here("R", "utils.R"))
source(here("R", "imputation_definitions.R"))

cores <- get_env_cores()

seed <- 42
set.seed(seed)

training_data <- read.csv(here("output", "data", FILE_PREPROCESSED_TRAINING_DATA_CSV), row.names = 1, as.is = TRUE)
outcomes <- read.csv(here("output", "data", FILE_TRAINING_OUTCOMES_CSV), row.names = 1, as.is = TRUE)[[1]]

n_folds <- 5
rows <- NROW(training_data)

# Reorder rows
reordering <- sample(1:rows, rows, replace = FALSE)
training_data <- training_data[reordering,]
outcomes <- outcomes[reordering]

folds <- replicate(10, sample(1:rows, 0.7*rows, replace = TRUE), simplify = FALSE)

fold_tr_datas <- lapply(folds, function(fold) training_data[-fold, ])
fold_tr_outcomes <- lapply(folds, function(fold) outcomes[-fold])
fold_te_datas <- lapply(folds, function(fold) training_data[fold, ])
fold_te_outcomes <- lapply(folds, function(fold) outcomes[fold])

create_dir(here("output", "cv"))

rf_results <- list()
lr_results <- list()
rf_pc_results <- list()
lr_pc_results <- list()
for (i in 1:length(folds)) {
  
  dir_path <- here("output", "cv", paste0("fold_", i))
  create_dir(dir_path)
   
  tr_data_path <- file.path(dir_path, paste0(FILE_CROSSVALIDATION_TRAINING_DATA, "_", i, ".csv"))
  tr_outcome_path <- file.path(dir_path, paste0(FILE_CROSSVALIDATION_TRAINING_OUTCOMES, "_", i, ".csv"))
  te_data_path <- file.path(dir_path, paste0(FILE_CROSSVALIDATION_TEST_DATA, "_", i, ".csv"))
  te_outcome_path <- file.path(dir_path, paste0(FILE_CROSSVALIDATION_TEST_OUTCOMES, "_", i, ".csv") )
   
  write.csv(fold_tr_datas[[i]], tr_data_path)
  write.csv(fold_tr_outcomes[[i]],  tr_outcome_path)
  write.csv(fold_te_datas[[i]], te_data_path)
  write.csv(fold_te_outcomes[[i]], te_outcome_path)
  
  impute_and_train(training_path = tr_data_path, outcome_path = tr_outcome_path, output_path = dir_path,
                   mice_hyperparameter_grids = mice_hyperparameter_grids, other_hyperparameter_grids = other_hyperparameter_grids, single_value_imputation_hyperparameter_grids = single_value_imputation_hyperparameter_grids,
                   cores = 12, seed = 42, lean = TRUE)
  predict_on_test_set(test_path = te_data_path, outcome_path = te_outcome_path, tr_output_path = dir_path, results_dir_path = file.path(dir_path, "results"), cores = cores, seed = 42, lean = TRUE)
  
  rf_results[[i]] <- read.csv(file.path(dir_path, "results", FILE_RF_PERFORMANCE_CSV))
  rf_results[[i]]$fold <- i
  lr_results[[i]] <- read.csv(file.path(dir_path, "results", FILE_LR_PERFORMANCE_CSV))
  lr_results[[i]]$fold <- i
}
rf_results <- do.call(rbind, rf_results)
lr_results <- do.call(rbind, lr_results)

rf_results <- rename_methods(rf_results)
rf_results$method <- reorder(rf_results$method, rf_results$MCC, mean)
lr_results <- rename_methods(lr_results)

write.csv(rf_results, here("output", "cv", FILE_RF_CROSSVALIDATION_RESULTS_CSV))
write.csv(lr_results, here("output", "cv", FILE_LR_CROSSVALIDATION_RESULTS_CSV))

ggsave(filename = here("output", "cv", "si_cv_double_boxplots.pdf"), 
       plot = doubleboxplot("MCC", rf_results, lr_results, FALSE),
       device = "pdf", width = 170, height = 180, units = "mm")

