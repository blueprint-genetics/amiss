library(magrittr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggcorrplot)
library(here)
library(futile.logger)

source(here("R", "utils.R"))
source(here("R", "constants.R"))
source(here("R", "visualizations.R"))
source(here("R", "feature_definitions.R"))

flog.appender(appender.tee(here("output", "03_descriptive_stats.log")))
flog.info("03_descriptive_stats.R")

seed <- 10
flog.pid.info("Creating output directory")
if (!dir.exists(here("output", "stats"))) {
  dir_creation_success <- dir.create(here("output", "stats"), showWarnings = TRUE)
  if (!dir_creation_success) {
    stop("Failed to create directory for saving results.")
  }
}

flog.pid.info("Reading data")
training_set <- read.csv(here("output", "data", FILE_TRAINING_DATA_FOR_STATS_CSV), row.names = 1, as.is = TRUE)
tr_outcome <- read.csv(here("output", "data", FILE_TRAINING_OUTCOMES_CSV), row.names = 1)[,1]

stopifnot(row.names(training_set) == row.names(tr_outcome))

features <- colnames(training_set)

## Correlations

flog.pid.info("Plotting correlations")
# Plot correlation matrices of missingness indicators against missingness indicators, observed values against observed values, and missingness indicators against observed values.
positive_data <- training_set[tr_outcome == "positive", ]
negative_data <- training_set[tr_outcome == "negative", ]

# Missingness indicator correlations
ggsave(filename = here("output", "stats", "MI_correlations.pdf"),
       plot = plot_missingness_correlations(training_set, numeric_features, "Missingness indicator correlations"),
       device = "pdf", width = 340, height = 180, units = "mm")
ggsave(filename = here("output", "stats", "MI_correlations_positive_labels.pdf"),
       plot = plot_missingness_correlations(positive_data, numeric_features, "Missingness indicator correlations (positive-labeled)"),
       device = "pdf", width = 340, height = 180, units = "mm")
ggsave(filename = here("output", "stats", "MI_correlations_negative_labels.pdf"),
       plot = plot_missingness_correlations(negative_data, numeric_features, "Missingness indicator correlations (negative-labeled)"),
       device = "pdf", width = 340, height = 180, units = "mm")

# Observed value correlations
ggsave(filename = here("output", "stats", "value_correlations.pdf"),
       plot = plot_observed_correlations(training_set, numeric_features, "Correlations of observed values"),
       device = "pdf", width = 340, height = 180, units = "mm")
ggsave(filename = here("output", "stats", "value_correlations_positive_labels.pdf"),
       plot = plot_observed_correlations(positive_data, numeric_features, "Correlations of observed values (positive-labeled)"),
       device = "pdf", width = 340, height = 180, units = "mm")
ggsave(filename = here("output", "stats", "value_correlations_negative_labels.pdf"),
       plot = plot_observed_correlations(negative_data, numeric_features, "Correlations of observed values (negative-labeled)"),
       device = "pdf", width = 340, height = 180, units = "mm")

# Missingness vs. observed correlations
ggsave(filename = here("output", "stats", "MI_vs_value_correlations.pdf"),
       plot = plot_missingness_vs_observed_correlations(training_set, numeric_features, "Missingness correlations vs. observed values"),
       device = "pdf", width = 340, height = 180, units = "mm")
ggsave(filename = here("output", "stats", "MI_vs_value_correlations_negative_labels.pdf"),
       plot = plot_missingness_vs_observed_correlations(positive_data, numeric_features, "Missingness correlations vs. observed values (positive-labeled)"),
       device = "pdf", width = 340, height = 180, units = "mm")
ggsave(filename = here("output", "stats", "MI_vs_value_correlations_positive_labels.pdf"),
       plot_missingness_vs_observed_correlations(negative_data, numeric_features, "Missingness correlations vs. observed values (negative-labeled)"),
       device = "pdf", width = 340, height = 180, units = "mm")

## Feature value distributions

flog.pid.info("Plotting feature histograms")
feature_distribution_plots <- lapply(numeric_features,
                                     function(column) {
                                       ggplot2::quickplot(
                                         na.omit(training_set[,column]),
                                         main = column,
                                         xlab = "",
                                         bins = 30
                                       )
                                     })

ggsave(filename = here("output", "stats", "feature_histograms.pdf"),
       marrangeGrob(
         ncol = 2, nrow = 3,
         grobs = feature_distribution_plots
       ),
       device = "pdf", width = 340, height = 340, units = "mm")

## Correlations with outcome indicator

feature_to_outcome_correlations <- sapply(numeric_features, function(feature) cor(training_set[[feature]], tr_outcome == "positive", use = "pairwise.complete.obs"))
feature_to_outcome_correlations <- data.frame(Feature = numeric_features, Correlation = feature_to_outcome_correlations)
ggsave(filename = here("output", "stats", "feature_to_outcome_correlation.pdf"),
       ggplot(feature_to_outcome_correlations) +
         geom_col(aes(x=Feature, y = Correlation)) +
         theme_bw() +
         theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
               legend.text.align = 1),
       device = "pdf", width = 280, height = 140, units = "mm")
miss_ind_to_outcome_correlations <- sapply(c(numeric_features, categorical_features), function(feature) cor(is.na(training_set[[feature]]), tr_outcome == "positive", use = "pairwise.complete.obs"))
miss_ind_to_outcome_correlations <- data.frame(Feature = c(numeric_features, categorical_features), Correlation = miss_ind_to_outcome_correlations)
ggsave(filename = here("output", "stats", "feature_missingness_to_outcome_correlation.pdf"),
       ggplot(miss_ind_to_outcome_correlations) +
         geom_col(aes(x=Feature, y = Correlation)) +
         theme_bw() +
         theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
               legend.text.align = 1),
       device = "pdf", width = 280, height = 140, units = "mm")

## Categorical level occurence counts

flog.pid.info("Categorical variable level occurrences:")
# Print (one-dimensional) contingency tables, i.e. occurence counts of each level of categorical variables.
for (cat_feat in categorical_features) {
  flog.pid.info(capture.output({
  table(training_set[, cat_feat, drop = FALSE], dnn = cat_feat, useNA = "always") %>% as.data.frame %>% print
  cat("\n")
  }
  ))
}
# Looking at `Consequence.x` is redundant as it was already displayed earlier, but looking at `LRT_pred` we can see a troublingly low number of observations of level `U`.

## Heatmap of feature missingness against consequence

# It is likely that missing values are more or less common in some variables depending on the predicted consequence. This can be visualized by a heatmap:
missing_value_sum_per_consequence <- aggregate(is.na(training_set[, numeric_features]),
                                               by = list(consequence = training_set$Consequence.x),
                                               function(x) sum(x)/length(x) * 100)
heatmap <- function(long_df, log) {
  cols <- colnames(long_df)
  return(ggplot(long_df) +
           (if (log)
             scale_fill_distiller("Proportion (%)",
                                  palette="YlGnBu",
                                  direction = 1,
                                  trans="log1p",
                                  breaks = c(0, 2, 9, 30, 100),
                                  guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"))
           else
             scale_fill_distiller("Proportion (%)",
                                  palette="YlGnBu",
                                  direction = 1,
                                  breaks = c(0, 25, 50, 75, 100),
                                  guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"))) +
           geom_tile(aes(x = x, y = y, fill = z)) +
           theme_bw() +
           theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
                 text = element_text(size=21),
                 legend.text.align = 1, legend.key = element_rect(fill = "white", colour = "black"))
  )
}
cluster <- function(data) {

  if (is.null(colnames(data))) {
    stop("Input must have column names")
  }
  data <- data[order(row.names(data)), order(colnames(data))]
  hx <- stats::hclust(stats::dist(data[, colnames(data) != "consequence"]))
  hy <- stats::hclust(stats::dist(data[, colnames(data) != "consequence"] %>% t))
  d <- gather(data, "name", "value", -consequence, factor_key = TRUE)
  d <- d[order(d$consequence),]
  data$consequence <- NULL
  d$consequence <- factor(d$consequence, unique(d$consequence)[hx$order])
  d$name <- factor(d$name, d$name[hy$order])

  return(d)
}

missing_values_longer <- cluster(data = missing_value_sum_per_consequence)
dd <- data.frame(x = missing_values_longer$consequence, y = missing_values_longer$name, z = missing_values_longer$value)
ggsave(filename = here("output", "stats", "missingness_heatmap.pdf"),
  plot = heatmap(dd, log = FALSE) + coord_flip() + xlab("") + ylab(""),
  device = "pdf", width = 340, height = 190, units = "mm")
ggsave(filename = here("output", "stats", "missingness_heatmap_log1p.pdf"),
  plot = heatmap(dd, log = TRUE) + coord_flip() + xlab("") + ylab(""),
  device = "pdf", width = 340, height = 190, units = "mm")

## Basic stats on preprocessed data

training_set <- read.csv(here("output", "data", FILE_PREPROCESSED_TRAINING_DATA_CSV), as.is = TRUE)
test_set <- read.csv(here("output", "data", FILE_PREPROCESSED_TEST_DATA_CSV), as.is = TRUE)
te_outcome <- read.csv(here("output", "data", FILE_TEST_OUTCOMES_CSV), row.names = 1)[,1]
flog.info("Number of training set variants: %d", nrow(training_set))
flog.info("Number of test set variants: %d", nrow(test_set))
flog.info("Number of positive training set variants: %d", sum(tr_outcome == "positive"))
flog.info("Number of negative training set variants: %d", sum(tr_outcome == "negative"))
flog.info("Number of positive test set variants: %d", sum(te_outcome == "positive"))
flog.info("Number of negative test set variants: %d", sum(te_outcome == "negative"))
flog.info("Number of complete training set cases: %d", sum(complete.cases(training_set)))
flog.info("Number of complete test set cases: %d", sum(complete.cases(test_set)))
flog.info("Missingness percentage in training data: %f", sum(is.na(training_set))/NCOL(training_set)/NROW(training_set))


## Basic stats on preprocessed data

training_set <- read.csv(here("output", "data", FILE_PREPROCESSED_TRAINING_DATA_CSV), as.is = TRUE)
test_set <- read.csv(here("output", "data", FILE_PREPROCESSED_TEST_DATA_CSV), as.is = TRUE)
te_outcome <- read.csv(here("output", "data", FILE_TEST_OUTCOMES_CSV), row.names = 1)[,1]
flog.info("Number of training set variants: %d", nrow(training_set))
flog.info("Number of test set variants: %d", nrow(test_set))
flog.info("Number of positive training set variants: %d", sum(tr_outcome == "positive"))
flog.info("Number of negative training set variants: %d", sum(tr_outcome == "negative"))
flog.info("Number of positive test set variants: %d", sum(te_outcome == "positive"))
flog.info("Number of negative test set variants: %d", sum(te_outcome == "negative"))
flog.info("Number of complete training set cases: %d", sum(complete.cases(training_set)))
flog.info("Number of complete test set cases: %d", sum(complete.cases(test_set)))
flog.info("Missingness percentage in training data: %f", sum(is.na(training_set))/NCOL(training_set)/NROW(training_set))


write(capture.output(sessionInfo()), here("output", "03_descriptive_stats_sessioninfo.txt"))
