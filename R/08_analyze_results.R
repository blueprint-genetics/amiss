library(futile.logger)
library(gridExtra)
library(ggplot2)
library(here)
library(magrittr)

source(here("R/visualizations.R"))
source(here("R/constants.R"))
source(here("R/utils.R"))

rf_perf_path <- here("output", "results", FILE_RF_PERFORMANCE_CSV)
lr_perf_path <- here("output", "results", FILE_LR_PERFORMANCE_CSV)
rf_perf_pc_path <- here("output", "results", FILE_RF_PERFORMANCE_PER_CONSEQUENCE_CSV)
lr_perf_pc_path <- here("output", "results", FILE_LR_PERFORMANCE_PER_CONSEQUENCE_CSV)
results_path <- here("output", "results")

rf_perf_table <- read.csv(rf_perf_path, as.is = TRUE)
lr_perf_table <- read.csv(lr_perf_path, as.is = TRUE)
rf_perf_pc_table <- read.csv(rf_perf_pc_path, as.is = TRUE)
lr_perf_pc_table <- read.csv(lr_perf_pc_path, as.is = TRUE)

flog.pid.info("Creating output directory")
if (!dir.exists(results_path)) {
  dir_creation_success <- dir.create(results_path, showWarnings = TRUE)
  if (!dir_creation_success) {
    stop("Failed to create directory for saving results.")
  }
}

flog.pid.info("Averaging performance tables")
aggregate_over_perf_table <- function(perf_table) {

  perf_stats <- perf_table[, !colnames(perf_table) %in% c(METHOD_COLUMN, MODEL_INDEX_COLUMN, TEST_COMPLETION_INDEX_COLUMN)]

  return(list(
    model_mean = aggregate(perf_stats, perf_table[METHOD_COLUMN], mean),
    model_sd = aggregate(perf_stats, perf_table[METHOD_COLUMN], sd),
    over_test_mean = aggregate(perf_stats, perf_table[c(METHOD_COLUMN, MODEL_INDEX_COLUMN)], mean),
    over_test_sd = aggregate(perf_stats, perf_table[c(METHOD_COLUMN, MODEL_INDEX_COLUMN)], sd),
    over_train_mean = aggregate(perf_stats, perf_table[c(METHOD_COLUMN, TEST_COMPLETION_INDEX_COLUMN)], mean),
    over_train_sd = aggregate(perf_stats, perf_table[c(METHOD_COLUMN, TEST_COMPLETION_INDEX_COLUMN)], sd)
  ))

}

rf_perf_aggregations <- aggregate_over_perf_table(rf_perf_table)
lr_perf_aggregations <- aggregate_over_perf_table(lr_perf_table)

rf_perf_pc_aggregations <- aggregate_over_perf_table(rf_perf_pc_table)
lr_perf_pc_aggregations <- aggregate_over_perf_table(lr_perf_pc_table)

flog.pid.info("Writing averaged performance tables")
for (name in names(rf_perf_aggregations)) {
  write.csv(x = rf_perf_aggregations[[name]],
            file = file.path(results_path, paste0("rf_", name, ".csv")),
            row.names = FALSE)
}
for (name in names(lr_perf_aggregations)) {
  write.csv(x = lr_perf_aggregations[[name]],
            file = file.path(results_path, paste0("lr_", name, ".csv")),
            row.names = FALSE)
}
flog.pid.info("Writing averaged performance tables")
for (name in names(rf_perf_pc_aggregations)) {
  write.csv(x = rf_perf_pc_aggregations[[name]],
            file = file.path(results_path, paste0("rf_", name, "_per_consequence.csv")),
            row.names = FALSE)
}
for (name in names(lr_perf_pc_aggregations)) {
  write.csv(x = lr_perf_pc_aggregations[[name]],
            file = file.path(results_path, paste0("lr_", name, "_per_consequence.csv")),
            row.names = FALSE)
}


rename_methods <- function(perf) {
  perf$method <- factor(perf$method,
                        c("missForest", "bpca", "norm.predict", "pmm", "norm", "rf", "outlier_imp", "max_imp", "min_imp", "zero_imp", "knnImputation", "median_imp", "missingness_indicators", "mean_imp"),
                        c("missForest", "BPCA", "MICE Regr.", "MICE PMM", "MICE Bayes r.", "MICE RF", "Outlier", "Maximum", "Minimum", "Zero", "k-NN", "Median", "Missingness ind.", "Mean"))
  return(perf)
}
rename_consequences <- function(perf) {
  perf$consequence <- factor(perf$consequence,
                             perf$consequence %>% unique,
                             gsub(CONSEQUENCE_COLUMN %>% paste0("."), "", perf$consequence %>% unique))
  return(perf)
}

rf_perf <- rename_methods(rf_perf_table)
rf_perf$method <- reorder(rf_perf$method, rf_perf$MCC, mean)
lr_perf <- rename_methods(lr_perf_table)

rf_perf_pc <- rename_methods(rf_perf_pc_table)
rf_perf_pc$method <- reorder(rf_perf_pc$method, rf_perf_pc$MCC, mean)
lr_perf_pc <- rename_methods(lr_perf_pc_table)
rf_perf_pc %<>% rename_consequences
lr_perf_pc %<>% rename_consequences

for (metric in c("TP", "FP", "FN", "TN", "Brier", "Accuracy", "MCC", "AUC", "Sensitivity", "Specificity", "F1", "Precision")) {
  double_boxplots <- doubleboxplot(metric, rf_perf, lr_perf, FALSE)
  ggsave(filename = here("output", "results", paste0(metric, "_double_boxplots.pdf")), plot = double_boxplots, device = "pdf", width = 170, height = 180, units = "mm")
}
# One with both MCC and AUC
double_boxplots_MCC_AUC <- doubleboxplot(c("MCC", "AUC"), rf_perf, lr_perf, FALSE)
ggsave(filename = here("output", "results", "MCC_AUC_double_boxplots.pdf"), plot = double_boxplots_MCC_AUC, device = "pdf", width = 340, height = 180, units = "mm")

for (metric in c("TP", "FP", "FN", "TN", "Brier", "Accuracy", "MCC", "AUC", "Sensitivity", "Specificity", "F1", "Precision")) {
  double_boxplots <- doubleboxplot(metric, rf_perf_pc, lr_perf_pc, TRUE)
  ggsave(filename = here("output", "results", paste0(metric, "_double_boxplots_perconseq.pdf")), plot = double_boxplots, device = "pdf", width = 340, height = 220, units = "mm")
}

flog.pid.info("Plotting and writing performance boxplots")
# RF MCC
rf_mcc_boxplots <- arrangeGrob(
  ggplot(rf_perf_table, aes(x = method, y = MCC)) + geom_boxplot() + ggtitle("MCC of random forest classifier") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(rf_perf_aggregations$over_test_mean, aes(x = method, y = MCC)) + geom_boxplot() + ggtitle("MCC of random forest classifier", subtitle = "Aggregated over test set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(rf_perf_aggregations$over_train_mean, aes(x = method, y = MCC)) + geom_boxplot() + ggtitle("MCC of random forest classifier", subtitle = "Aggregated over training set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
ggsave(filename =  "rf_mcc_boxplots.pdf", plot = rf_mcc_boxplots, device = "pdf", path = results_path, width = 170, height = 180, units = "mm")

# RF AUC-ROC
rf_roc_boxplots <- arrangeGrob(
  ggplot(rf_perf_table, aes(x = method, y = AUC)) + geom_boxplot() + ggtitle("AUC-ROC of random forest classifier") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(rf_perf_aggregations$over_test_mean, aes(x = method, y = AUC)) + geom_boxplot() + ggtitle("AUC-ROC of random forest classifier", subtitle = "Aggregated over test set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(rf_perf_aggregations$over_train_mean, aes(x = method, y = AUC)) + geom_boxplot() + ggtitle("AUC-ROC of random forest classifier", subtitle = "Aggregated over training set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
ggsave(filename =  "rf_roc_boxplots.pdf", plot = rf_roc_boxplots, device = "pdf", path = results_path, width = 170, height = 180, units = "mm")

# LR MCC
lr_mcc_boxplots <- arrangeGrob(
  ggplot(lr_perf_table, aes(x = method, y = MCC)) + geom_boxplot() + ggtitle("MCC of logistic regression classifier") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(lr_perf_aggregations$over_test_mean, aes(x = method, y = MCC)) + geom_boxplot() + ggtitle("MCC of logistic regression classifier", subtitle = "Aggregated over test set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(lr_perf_aggregations$over_train_mean, aes(x = method, y = MCC)) + geom_boxplot() + ggtitle("MCC of logistic regression classifier", subtitle = "Aggregated over training set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
ggsave(filename =  "lr_mcc_boxplots.pdf", plot = lr_mcc_boxplots, device = "pdf", path = results_path, width = 170, height = 180, units = "mm")

# LR AUC-ROC
lr_roc_boxplots <- arrangeGrob(
  ggplot(lr_perf_table, aes(x = method, y = AUC)) + geom_boxplot() + ggtitle("AUC-ROC of logistic regression classifier") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(lr_perf_aggregations$over_test_mean, aes(x = method, y = AUC)) + geom_boxplot() + ggtitle("AUC-ROC of logistic regression classifier", subtitle = "Aggregated over test set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(lr_perf_aggregations$over_train_mean, aes(x = method, y = AUC)) + geom_boxplot() + ggtitle("AUC-ROC of logistic regression classifier", subtitle = "Aggregated over training set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
ggsave(filename =  "lr_roc_boxplots.pdf", plot = lr_roc_boxplots, device = "pdf", path = results_path, width = 170, height = 180, units = "mm")

write(capture.output(sessionInfo()), here("output", "08_analyze_results_sessioninfo.txt"))
