library(here)
library(ggplot2)
library(gridExtra)
library(magrittr)

source(here("R/visualizations.R"))
source(here("R/constants.R"))

rf_perfs <- read.csv(here("output", "sim", FILE_SIMULATED_RF_RESULTS_CSV))
lr_perfs <- read.csv(here("output", "sim", FILE_SIMULATED_LR_RESULTS_CSV))
rf_perfs_pc <- read.csv(here("output", "sim", FILE_SIMULATED_RF_RESULTS_PER_CONSEQUENCE_CSV))
lr_perfs_pc <- read.csv(here("output", "sim", FILE_SIMULATED_LR_RESULTS_PER_CONSEQUENCE_CSV))

times <- read.csv(here("output", "sim", "times.csv"))

rename_methods <- function(perf) {
  perf$method <- factor(perf$method,
                        c("missForest", "bpca", "norm.predict", "pmm", "norm", "rf", "outlier_imp", "max_imp", "min_imp", "zero_imp", "knnImputation", "median_imp", "missingness_indicators", "mean_imp"),
                        c("missForest", "BPCA", "MICE Regr.", "MICE PMM", "MICE Bayes r.", "MICE RF", "Outlier", "Maximum", "Minimum", "Zero", "k-NN", "Median", "Missingness ind.", "Mean"))
  return(perf)
}

rf_perfs <- rename_methods(rf_perfs)
rf_perfs$method <- reorder(rf_perfs$method, rf_perfs$MCC, mean)
lr_perfs <- rename_methods(lr_perfs)
lr_perfs$method <- factor(lr_perfs$method, levels = levels(rf_perfs$method))

# MCC vs. pct

dir.create(here("output", "sim", "plots"))

rf_mcc_vs_pct <- ggplot(subset(rf_perfs, method != "k-NN")) + 
  geom_boxplot(aes(x = factor(pct), y = MCC)) + 
  xlab("Intended missingness percentage") + 
  ylab("MCC") + 
  facet_wrap(vars(method))
ggsave(filename = "rf_mcc_versus_miss_pct.pdf", plot = rf_mcc_vs_pct, device = "pdf", path = here("output", "sim", "plots"), width = 170, height = 200, units = "mm")

rf_mcc_vs_obs_pct <- ggplot(subset(rf_perfs, method != "k-NN")) + 
  geom_point(aes(x = na_prop, y = MCC), alpha = 0.3) + 
  geom_smooth(aes(x = na_prop, y = MCC), color = "red", method = "loess") + 
  xlab("Observed missingness percentage") + 
  ylab("MCC") + 
  facet_wrap(vars(method))
ggsave(filename = "rf_mcc_versus_miss_pct_observed.pdf", plot = rf_mcc_vs_obs_pct, device = "pdf", path = here("output", "sim", "plots"), width = 170, height = 200, units = "mm")

lr_mcc_vs_pct <- ggplot(subset(lr_perfs, method != "k-NN")) + 
  geom_boxplot(aes(x = factor(pct), y = MCC)) + 
  xlab("Intended missingness percentage") + 
  ylab("MCC") + 
  facet_wrap(vars(method))
ggsave(filename = "lr_mcc_versus_miss_pct.pdf", plot = lr_mcc_vs_pct, device = "pdf", path = here("output", "sim", "plots"), width = 170, height = 200, units = "mm")

lr_mcc_vs_obs_pct <- ggplot(subset(lr_perfs, method != "k-NN")) + 
  geom_point(aes(x = na_prop, y = MCC), alpha = 0.3)  + 
  geom_smooth(aes(x = na_prop, y = MCC), method = "loess") + 
  xlab("Observed missingness percentage") + 
  ylab("MCC") + 
  facet_wrap(vars(method))
ggsave(filename = "lr_mcc_versus_miss_pct_observed.pdf", plot = lr_mcc_vs_obs_pct, device = "pdf", path = here("output", "sim", "plots"), width = 170, height = 200, units = "mm")

dir.create(here("output", "sim", "plots", "rmse", "fixed_scales"), recursive = TRUE)
dir.create(here("output", "sim", "plots", "rmse", "free_x_scale"), recursive = TRUE)

form_and_save_rmse_plots <- function(data, prefix, path, x_scale=NULL, y_scale = c(0.4, 0.85), drop_methods=c("Missingness indicators", "missForest")) {
  
  non_feature_cols <- c("X", "repeat.", "pct", "mech", "orientation", "method", "model_ix", "test_completion_ix", "TP", "FP", "FN", "TN", "MCC", "AUC", "Accuracy", "Sensitivity", "Specificity", "F1", "Precision", "na_prop", "Brier")
  feature_cols <- colnames(data)[!colnames(data) %in% non_feature_cols]
  
  mean_rmse <- data[, feature_cols] %>% rowMeans(na.rm = TRUE)
  rmse_perf <- cbind(data[, c("method", "MCC")], RMSE = mean_rmse)
  
  # Overall
  plots <- lapply(setdiff(levels(data$method), drop_methods), function(m) { 
    rmses <- ggplot(subset(rmse_perf, method == m)) + 
      geom_point(aes(x = RMSE, y = MCC)) +
      theme_bw() +
      xlab('RMSE') +
      ylab('MCC') +
      ggtitle(m) + 
      #coord_flip() +
      theme(legend.position='none') + theme(text = element_text(size = 10)) + ylim(y_scale)
    if(!is.null(x_scale))
      rmses <- rmses + xlim(x_scale)
    return(rmses)
  })
  plots <- do.call(arrangeGrob, plots)
  ggsave(filename = paste0(prefix, "_rmses.pdf"), plot = plots, device = "pdf", path = path, width = 170, height = 170, units = "mm")
  
  # Per feature
  for (f in feature_cols) {
    plots <- lapply(setdiff(levels(data$method), drop_methods), function(m) { 
      f_var <- rlang::sym(f)
      rmses <- ggplot(subset(data, method == m)) + 
        geom_point(aes(x = !!f_var, y = MCC)) +
        theme_bw() +
        xlab('RMSE') +
        ylab('MCC') +
        ggtitle(m) + 
        #coord_flip() +
        theme(legend.position='none') + theme(text = element_text(size = 10)) + ylim(y_scale)
      if(!is.null(x_scale))
        rmses <- rmses + xlim(x_scale)
      return(rmses)
    })
    plots <- do.call(arrangeGrob, plots)
    ggsave(filename = paste0(prefix, "_", f, "_rmse.pdf"), plot = plots, device = "pdf", path = path, width = 170, height = 170, units = "mm")
  }
}

form_and_save_rmse_plots(rf_perfs, "rf", here("output", "sim", "plots", "rmse", "fixed_scales"), x_scale = c(0,8), y_scale = c(0.4, 0.85), drop_methods = c("Outlier", "Missingness indicators", "missForest"))
form_and_save_rmse_plots(lr_perfs, "lr", here("output", "sim", "plots", "rmse", "fixed_scales"), x_scale = c(0,8), y_scale = c(-0.25, 0.75), drop_methods = c("Outlier", "Missingness indicators", "missForest"))
<<<<<<< HEAD

form_and_save_rmse_plots(rf_perfs, "rf", here("output", "sim", "plots", "rmse", "free_x_scale"), y_scale = c(0.4, 0.85))
form_and_save_rmse_plots(lr_perfs, "lr", here("output", "sim", "plots", "rmse", "free_x_scale"), y_scale = c(-0.25, 0.75))

for (metric in c("TP", "FP", "FN", "TN", "Brier", "Accuracy", "MCC", "AUC", "Sensitivity", "Specificity", "F1", "Precision")) {
  double_boxplots <- doubleboxplot(metric, rf_perfs, lr_perfs, FALSE)
  ggsave(filename = here("output", "sim", "plots", paste0(metric, "_double_boxplots.pdf")), plot = double_boxplots, device = "pdf", width = 170, height = 180, units = "mm")
}
for (metric in c("TP", "FP", "FN", "TN", "Brier", "Accuracy", "MCC", "AUC", "Sensitivity", "Specificity", "F1", "Precision")) {
  double_boxplots <- doubleboxplot(metric, rf_perfs_pc, lr_perfs_pc, TRUE)
  ggsave(filename = here("output", "sim", "plots", paste0(metric, "_double_boxplots_perconseq.pdf")), plot = double_boxplots, device = "pdf", width = 340, height = 220, units = "mm")
}
=======

form_and_save_rmse_plots(rf_perfs, "rf", here("output", "sim", "plots", "rmse", "free_x_scale"), y_scale = c(0.4, 0.85))
form_and_save_rmse_plots(lr_perfs, "lr", here("output", "sim", "plots", "rmse", "free_x_scale"), y_scale = c(-0.25, 0.75))
>>>>>>> master
