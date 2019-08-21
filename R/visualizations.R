library(ggplot2)
library(tidyr)
library(cluster)

#' Form a heatmap plot
#'
#' @param data Tidy data.frame with `x`, `y` and `value` columns
#'
#' @return A ggplot2 object representing a heatmap
heatmap <- function(data) {
  
  ggplot(data = data, aes(x = x, y = y, fill = value)) + 
    geom_raster() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_gradient2(limits=c(-1, 1))
}
