library(here)
library(ggplot2)
library(gridExtra)
library(magrittr)

rf_perfs <- read.csv(here("sim", "simulated_rf_results.csv"))
lr_perfs <- read.csv(here("sim", "simulated_lr_results.csv"))
times <- read.csv(here("sim", "times.csv"))

rename_methods <- function(perf) {
  perf$method <- factor(perf$method,
                        c("missForest", "bpca", "norm.predict", "pmm", "norm", "rf", "outlier_imp", "max_imp", "min_imp", "zero_imp", "knnImputation", "median_imp", "missingness_indicators", "mean_imp"),
                        c("missForest", "BPCA", "MICE Regr.", "MICE PMM", "MICE Bayes r.", "MICE RF", "Outlier", "Maximum", "Minimum", "Zero", "k-NN", "Median", "Missingness ind.", "Mean"))
  return(perf)
}

rf_perfs <- rename_methods(rf_perfs)
rf_perfs$method <- reorder(rf_perfs$method, rf_perfs$mcc, mean)
lr_perfs <- rename_methods(lr_perfs)
lr_perfs$method <- factor(lr_perfs$method, levels = levels(rf_perfs$method))

# MCC vs. pct

dir.create(here("sim", "plots"))

rf_mcc_vs_pct <- ggplot(subset(rf_perfs, method != "k-NN")) + 
  geom_boxplot(aes(x = factor(pct), y = mcc)) + 
  xlab("Intended missingness percentage") + 
  ylab("MCC") + 
  facet_wrap(vars(method))
ggsave(filename = "rf_mcc_versus_miss_pct.pdf", plot = rf_mcc_vs_pct, device = "pdf", path = here("sim", "plots"), width = 170, height = 200, units = "mm")

rf_mcc_vs_obs_pct <- ggplot(subset(rf_perfs, method != "k-NN")) + 
  geom_point(aes(x = na_prop, y = mcc), alpha = 0.3) + 
  geom_smooth(aes(x = na_prop, y = mcc), method = "loess") + 
  xlab("Observed missingness percentage") + 
  ylab("MCC") + 
  facet_wrap(vars(method))
ggsave(filename = "rf_mcc_versus_miss_pct_observed.pdf", plot = rf_mcc_vs_obs_pct, device = "pdf", path = here("sim", "plots"), width = 170, height = 200, units = "mm")

lr_mcc_vs_pct <- ggplot(subset(lr_perfs, method != "k-NN")) + 
  geom_boxplot(aes(x = factor(pct), y = mcc)) + 
  xlab("Intended missingness percentage") + 
  ylab("MCC") + 
  facet_wrap(vars(method))
ggsave(filename = "lr_mcc_versus_miss_pct.pdf", plot = lr_mcc_vs_pct, device = "pdf", path = here("sim", "plots"), width = 170, height = 200, units = "mm")

lr_mcc_vs_obs_pct <- ggplot(subset(lr_perfs, method != "k-NN")) + 
  geom_point(aes(x = na_prop, y = mcc), alpha = 0.3)  + 
  geom_smooth(aes(x = na_prop, y = mcc), method = "loess") + 
  xlab("Observed missingness percentage") + 
  ylab("MCC") + 
  facet_wrap(vars(method))
ggsave(filename = "lr_mcc_versus_miss_pct_observed.pdf", plot = lr_mcc_vs_obs_pct, device = "pdf", path = here("sim", "plots"), width = 170, height = 200, units = "mm")

# RMSE

dir.create(here("sim", "plots", "rmse", "fixed_scales"), recursive = TRUE)
dir.create(here("sim", "plots", "rmse", "free_x_scale"), recursive = TRUE)

form_and_save_rmse_plots <- function(data, prefix, path, x_scale=NULL, y_scale = c(0.4, 0.85), drop_methods=c("Missingness indicators", "missForest")) {
  
  non_feature_cols <- c("X", "repeat.", "pct", "mech", "orientation", "method", "model_ix", "test_completion_ix", "tp", "fp", "fn", "tn", "mcc", "auc", "sensitivity", "specificity", "f1", "precision", "recall", "na_prop", "brier")
  feature_cols <- colnames(data)[!colnames(data) %in% non_feature_cols]
  
  mean_rmse <- data[, feature_cols] %>% rowMeans(na.rm = TRUE)
  rmse_perf <- cbind(data[, c("method", "mcc")], RMSE = mean_rmse)
  
  # Overall
  plots <- lapply(setdiff(levels(data$method), drop_methods), function(m) { 
    rmses <- ggplot(subset(rmse_perf, method == m)) + 
      geom_point(aes(x = RMSE, y = mcc)) +
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
        geom_point(aes(x = !!f_var, y = mcc)) +
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

form_and_save_rmse_plots(rf_perfs, "rf", here("sim", "plots", "rmse", "fixed_scales"), x_scale = c(0,8), y_scale = c(0.4, 0.85), drop_methods = c("Outlier", "Missingness indicators", "missForest"))
form_and_save_rmse_plots(lr_perfs, "lr", here("sim", "plots", "rmse", "fixed_scales"), x_scale = c(0,8), y_scale = c(-0.25, 0.75), drop_methods = c("Outlier", "Missingness indicators", "missForest"))

form_and_save_rmse_plots(rf_perfs, "rf", here("sim", "plots", "rmse", "free_x_scale"), y_scale = c(0.4, 0.85))
form_and_save_rmse_plots(lr_perfs, "lr", here("sim", "plots", "rmse", "free_x_scale"), y_scale = c(-0.25, 0.75))
