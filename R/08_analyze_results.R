
cmdargs <- commandArgs(trailingOnly = TRUE)
if (!(length(cmdargs) == 1)) {
  stop("Must have the following arguments: 
       1. path to rf performance csv
       2. path to lr performance csv
       3. path to directory to write in", call. = FALSE)
}

rf_perf_path <- cmdargs[1]
lr_perf_path <- cmdargs[2]
results_path <- cmdargs[3]

rf_perf_table <- read.csv(rf_perf_path, as.is = TRUE)
lr_perf_table <- read.csv(lr_perf_path, as.is = TRUE)

flog.pid.info("Creating output directory")
if (!dir.exists(results_path)) {
  dir_creation_success <- dir.create(results_path, showWarnings = TRUE)
  if (!dir_creation_success) {
    stop("Failed to create directory for saving results.")
  }
}

flog.pid.info("Averaging performance tables")
aggregate_over_perf_table <- function(perf_table) {

  perf_stats <- perf_table[, !colnames(perf_table) %in% c("method", "model_index", "test_realization")]

  return(list(
    model_mean = aggregate(perf_stats, perf_table["method"], mean),
    model_sd = aggregate(perf_stats, perf_table["method"], sd),
    over_test_mean = aggregate(perf_stats, perf_table[c("method", "model_index")], mean),
    over_test_sd = aggregate(perf_stats, perf_table[c("method", "model_index")], sd),
    over_train_mean = aggregate(perf_stats, perf_table[c("method", "test_realization")], mean),
    over_train_sd = aggregate(perf_stats, perf_table[c("method", "test_realization")], sd)
  ))

}

rf_perf_aggregations <- aggregate_over_perf_table(rf_perf_table)
lr_perf_aggregations <- aggregate_over_perf_table(lr_perf_table)

flog.pid.info("Writing averaged performance tables")
for (name in names(rf_perf_aggregations)) {
  write.csv(x = rf_perf_aggregations[[name]],
            file = paste0(results_path, "rf_", name, ".csv"),
            row.names = FALSE)
}
for (name in names(lr_perf_aggregations)) {
  write.csv(x = lr_perf_aggregations[[name]],
            file = paste0(results_path, "lr_", name, ".csv"),
            row.names = FALSE)
}

flog.pid.info("Plotting and writing performance boxplots")
# RF MCC
rf_mcc_boxplots <- arrangeGrob(
  ggplot(rf_perf_table, aes(x = method, y = mcc)) + geom_boxplot() + ggtitle("MCC of random forest classifier") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(rf_perf_aggregations$over_test_mean, aes(x = method, y = mcc)) + geom_boxplot() + ggtitle("MCC of random forest classifier", subtitle = "Aggregated over test set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(rf_perf_aggregations$over_train_mean, aes(x = method, y = mcc)) + geom_boxplot() + ggtitle("MCC of random forest classifier", subtitle = "Aggregated over training set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
ggsave(filename =  "rf_mcc_boxplots.pdf", plot = rf_mcc_boxplots, device = "pdf", path = results_path, width = 210, height = 297, units = "mm")

# RF AUC-ROC
rf_roc_boxplots <- arrangeGrob(
  ggplot(rf_perf_table, aes(x = method, y = auc)) + geom_boxplot() + ggtitle("AUC-ROC of random forest classifier") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(rf_perf_aggregations$over_test_mean, aes(x = method, y = auc)) + geom_boxplot() + ggtitle("AUC-ROC of random forest classifier", subtitle = "Aggregated over test set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(rf_perf_aggregations$over_train_mean, aes(x = method, y = auc)) + geom_boxplot() + ggtitle("AUC-ROC of random forest classifier", subtitle = "Aggregated over training set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
ggsave(filename =  "rf_roc_boxplots.pdf", plot = rf_roc_boxplots, device = "pdf", path = results_path, width = 210, height = 297, units = "mm")

# LR MCC
lr_mcc_boxplots <- arrangeGrob(
  ggplot(lr_perf_table, aes(x = method, y = mcc)) + geom_boxplot() + ggtitle("MCC of logistic regression classifier") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(lr_perf_aggregations$over_test_mean, aes(x = method, y = mcc)) + geom_boxplot() + ggtitle("MCC of logistic regression classifier", subtitle = "Aggregated over test set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(lr_perf_aggregations$over_train_mean, aes(x = method, y = mcc)) + geom_boxplot() + ggtitle("MCC of logistic regression classifier", subtitle = "Aggregated over training set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
ggsave(filename =  "lr_mcc_boxplots.pdf", plot = lr_mcc_boxplots, device = "pdf", path = results_path, width = 210, height = 297, units = "mm")

# LR AUC-ROC
lr_roc_boxplots <- arrangeGrob(
  ggplot(lr_perf_table, aes(x = method, y = auc)) + geom_boxplot() + ggtitle("AUC-ROC of logistic regression classifier") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(lr_perf_aggregations$over_test_mean, aes(x = method, y = auc)) + geom_boxplot() + ggtitle("AUC-ROC of logistic regression classifier", subtitle = "Aggregated over test set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
  ggplot(lr_perf_aggregations$over_train_mean, aes(x = method, y = auc)) + geom_boxplot() + ggtitle("AUC-ROC of logistic regression classifier", subtitle = "Aggregated over training set realizations") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
ggsave(filename =  "lr_roc_boxplots.pdf", plot = lr_roc_boxplots, device = "pdf", path = results_path, width = 210, height = 297, units = "mm")
