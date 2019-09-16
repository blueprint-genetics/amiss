library(magrittr)
library(futile.logger)

source("R/preprocessing.R")

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
  dummify_categoricals(training_data[, categorical_features, drop = FALSE]),
  training_data$outcome)

write.csv(training_data, "preprocessed_training_data.csv", row.names = FALSE)
