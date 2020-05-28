library(magrittr)
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
outcome <- read.csv(here("output", "data", FILE_TRAINING_OUTCOMES_CSV), row.names = 1)[,1]

stopifnot(row.names(training_set) == row.names(outcome))

features <- colnames(training_set)

## Correlations

flog.pid.info("Plotting correlations") 
# Plot correlation matrices of missingness indicators against missingness indicators, observed values against observed values, and missingness indicators against observed values.
positive_data <- training_set[outcome == "positive", ]
negative_data <- training_set[outcome == "negative", ]

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
# Next, plot distributions of each feature. Are they normal or linear?
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
# They are not, and thus it might be worth considering data transformations. In the case of random forest, however, monotone transformations should have no effect.

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
                                               sum)
heatmap <- function(long_df, log) {
  cols <- colnames(long_df)
  return(ggplot(long_df) +
           (if (log)
             scale_fill_gradient("Count", low = "white", high = "red", trans = "log1p", breaks = c(0, 10, 60, 400, 775), na.value = "white")
           else
             scale_fill_gradient("Count", low = "white", high = "red", breaks = c(0, 200, 400, 600, 775), na.value = "white")) +
           geom_tile(aes(x = x, y = y, fill = z)) +
           theme_bw() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1),text = element_text(size=21))
  )
}
cluster <- function(data) {
  
  if (is.null(colnames(data))) {
    stop("Input must have column names")
  }
  hx <- stats::hclust(stats::dist(data[, colnames(data) != "consequence"]))
  hy <- stats::hclust(stats::dist(data[, colnames(data) != "consequence"] %>% t))
  d <-  pivot_longer(data, cols = -consequence)
  data$consequence <- NULL
  #d$x <- colnames(data)[d$x]
  d$consequence <- factor(d$consequence, unique(d$consequence)[hx$order])
  #d$y <- colnames(data)[d$y]
  d$name <- factor(d$name, d$name[hy$order])
  
  return(d)
}

missing_values_longer <- cluster(data = missing_value_sum_per_consequence)
#missing_values_longer$value <- cut(missing_values_longer$value, c(0, 150, 300, 450, 600, 750, 800), right = FALSE)
dd <- data.frame(x = missing_values_longer$consequence, y = missing_values_longer$name, z = missing_values_longer$value)
ggsave(filename = here("output", "stats", "missingness_heatmap.pdf"), 
  plot = heatmap(dd, log = FALSE) + coord_flip() + xlab("") + ylab(""),
  device = "pdf", width = 340, height = 300, units = "mm")
ggsave(filename = here("output", "stats", "missingness_heatmap_log1p.pdf"), 
  plot = heatmap(dd, log = TRUE) + coord_flip() + xlab("") + ylab(""),
  device = "pdf", width = 340, height = 300, units = "mm")

# Stop-gained and non-synonymous variants have much less missingness in certain variables (as expected), and missingness rates are somewhat constant over different consequences in epigenetics variables.

write(capture.output(sessionInfo()), here("output", "03_descriptive_stats_sessioninfo.txt"))
