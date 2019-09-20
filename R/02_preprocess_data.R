library(magrittr)
library(futile.logger)
library(ggcorrplot)
library(gridExtra)

source("R/preprocessing.R")

flog.threshold(DEBUG)

set.seed(20)

training_set <- read.csv("training_data.csv", as.is = TRUE)

numeric_features <- make.names(c(
  # From dbNSFP
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
  "gnomAD_exomes_AF",
  
  # From CADD annotations
  "Length",
  "GC",
  "CpG",
  "motifECount",
  "motifEHIPos",
  "motifEScoreChng",
  # "(Intron)",
  # "(Exon)",
  "relcDNApos",
  "relCDSpos",
  "relProtPos",
  "Dst2Splice",
  "minDistTSS",
  "minDistTSE",
  # "targetScan",
  "mirSVR-Score",
  "mirSVR-E",
  "mirSVR-Aln",
  "tOverlapMotifs",
  "motifDist",
  "EncodeH3K4me1-sum",
  "EncodeH3K4me1-max",
  "EncodeH3K4me2-sum",
  "EncodeH3K4me2-max",
  "EncodeH3K4me3-sum",
  "EncodeH3K4me3-max",
  "EncodeH3K9ac-sum",
  "EncodeH3K9ac-max",
  "EncodeH3K9me3-sum",
  "EncodeH3K9me3-max",
  "EncodeH3K27ac-sum",
  "EncodeH3K27ac-max",
  "EncodeH3K27me3-sum",
  "EncodeH3K27me3-max",
  "EncodeH3K36me3-sum",
  "EncodeH3K36me3-max",
  "EncodeH3K79me2-sum",
  "EncodeH3K79me2-max",
  "EncodeH4K20me1-sum",
  "EncodeH4K20me1-max",
  "EncodeH2AFZ-sum",
  "EncodeH2AFZ-max",
  "EncodeDNase-sum",
  "EncodeDNase-max",
  "EncodetotalRNA-sum",
  "EncodetotalRNA-max",
  # "Grantham",
  "RemapOverlapTF",
  "RemapOverlapCL"
))
categorical_features <- make.names(c(
  # From dbNSFP
  "LRT_pred",
  
  # From CADD annotations
  "Type",
  "Dst2SplType"
))

features <- c(numeric_features, categorical_features)
training_data <- training_set[, features, drop = FALSE]
training_data$outcome <- compute_numeric_labels(training_set$CLNSIG)

dummy_categoricals <- dummify_categoricals(training_data[, categorical_features, drop = FALSE])
processed_features <- c(numeric_features, colnames(dummy_categoricals))

training_data <- cbind(
  training_data[, numeric_features, drop = FALSE], 
  dummy_categoricals,
  outcome = training_data$outcome
)

plot_missingness_correlations <- function(data, features, title) {
  miss_data <- is.na(data)
  missingness_corr <- cor(miss_data[, features])
  ggcorrplot(corr = missingness_corr, type = "lower", title = title)
}
plot_observed_correlations <- function(data, features, title) {
  corr <- cor(data[, features], use = "pairwise.complete.obs")
  # This makes hierarchical clustering work, and produces a gray square in the plot as desired
  corr[is.na(corr)] <- -2.0
  ggcorrplot(corr = corr, type = "lower", title = title)
}
plot_missingness_vs_observed_correlations <- function(data, features, title) {
  miss_data <- is.na(data)
  colnames(miss_data) <- paste0("miss_", colnames(miss_data))
  missingness_vs_value_corr <- cor(data, miss_data, use = "pairwise.complete.obs")
  # This makes hierarchical clustering work, and produces a gray square in the plot as desired
  missingness_vs_value_corr[is.na(missingness_vs_value_corr)] <- -2.0
  ggcorrplot(missingness_vs_value_corr, title = title)
}

positive_data <- training_data[training_data$outcome == 1.0, ]
negative_data <- training_data[training_data$outcome == 0.0, ]

# Missingness indicator correlations
grid.arrange(
  ncol = 3,
  plot_missingness_correlations(training_data, processed_features, "Missingness indicator correlations"),
  plot_missingness_correlations(positive_data, processed_features, "Missingness indicator correlations (positive-labeled)"),
  plot_missingness_correlations(negative_data, processed_features, "Missingness indicator correlations (negative-labeled)")
)

# Observed value correlations
grid.arrange(
  ncol = 3,
  plot_observed_correlations(training_data, processed_features, "Correlations of observed values"),
  plot_observed_correlations(positive_data, processed_features, "Correlations of observed values (positive-labeled)"),
  plot_observed_correlations(negative_data, processed_features, "Correlations of observed values (negative-labeled)") 
)

# Missingness vs. observed correlations
grid.arrange(
  ncol = 3,
  plot_missingness_vs_observed_correlations(training_data, processed_features, "Missingness correlations vs. observed values"),
  plot_missingness_vs_observed_correlations(positive_data, processed_features, "Missingness correlations vs. observed values (positive-labeled)"),
  plot_missingness_vs_observed_correlations(negative_data, processed_features, "Missingness correlations vs. observed values (negative-labeled)")
)

# Write preprocessed file
write.csv(training_data, "preprocessed_training_data.csv", row.names = FALSE)
