library(magrittr)
library(futile.logger)

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

training_data <- training_set[, c(numeric_features, categorical_features)]
training_data$outcome <- compute_numeric_labels(training_set$CLNSIG)

training_data <- cbind(
  training_data[, numeric_features, drop = FALSE], 
  dummify_categoricals(training_data[, categorical_features, drop = FALSE]),
  outcome = training_data$outcome
)

write.csv(training_data, "preprocessed_training_data.csv", row.names = FALSE)
