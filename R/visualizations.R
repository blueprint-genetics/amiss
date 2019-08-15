library(ggplot2)
library(tidyr)
library(cluster)

to_heatmap_format <- function(data) {
  
  variables <- factor(colnames(data), levels=colnames(data))
  observations <- factor(rownames(data), levels=rownames(data))
  n_variables <- length(variables)
  n_observations <- length(observations)
  
  data_long <- data.frame(
    x = rep(observations, times = n_variables), 
    y = rep(variables, each = n_observations),  
    stringsAsFactors = FALSE)
  
  data_long$value <- mapply(FUN = function(x,y) data[x,y], 
                            x = data_long$x, 
                            y = data_long$y)
  
  return(data_long)
}

heatmap <- function(data) {
  
  ggplot(data = data, aes(x = x, y = y, fill = value)) + 
    geom_raster() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_gradient2(limits=c(-1, 1))
}
