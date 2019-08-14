library(magrittr)
library(futile.logger)

source("R/process_data.R")

flog.threshold(DEBUG)

set.seed(20)

training_set <- read.csv("training_data.csv", as.is = TRUE)

numeric_features <- make.names(c(
  "SIFT_score", 
  "LRT_score", 
  "LRT_Omega", 
  "MutationTaster_score", 
  "MutationAssessor_score", 
  "FATHMM_score", 
  "PROVEAN_score", 
  "M-CAP_score", 
  "MutPred_score", 
  "fathmm-MKL_coding_score", 
  "Eigen-raw", 
  "Eigen-PC-raw", 
  "GenoCanyon_score", 
  "integrated_fitCons_score", 
  "GERP++_NR", 
  "GERP++_RS", 
  "phyloP100way_vertebrate", 
  "phyloP20way_mammalian", 
  "phastCons100way_vertebrate", 
  "phastCons20way_mammalian", 
  "SiPhy_29way_logOdds", 
  "gnomAD_exomes_AF"
))
categorical_features <- make.names(c(
  "LRT_pred"
))

training_data <- training_set[, c(numeric_features, categorical_features)]
training_data$outcome <- compute_numeric_labels(training_set$CLNSIG)

training_data <- cbind(
  training_data[, numeric_features, drop = FALSE], 
  dummify_categoricals(training_data[, categorical_features, drop = FALSE]))

correlations <- training_data[,numeric_features] %>% cor
missingness_correlations <- is.na(training_data) %>% data.frame
missingness_correlations <- missingness_correlations[, sapply(missingness_correlations, any), drop=FALSE]
missingness_correlations[,] <- missingness_correlations %>% sapply(as.integer)
missingness_correlations %<>% cor
