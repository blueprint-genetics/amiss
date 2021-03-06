install_or_fail <- function(packages) {
	install.packages(packages)
	for(p in packages) library(p, character.only = TRUE)
}
install_or_fail("vcfR")
install_or_fail("futile.logger")
install_or_fail("tidyr")
install_or_fail("here")
install_or_fail("magrittr")
install_or_fail("ggcorrplot")
install_or_fail("mice")
install_or_fail("foreach")
install_or_fail("doParallel")
install_or_fail("ggplot2")
install_or_fail("iterators")
install_or_fail("missForest")
install_or_fail("DMwR")
install_or_fail("doRNG")
install_or_fail("rngtools")
install_or_fail("lattice")
install_or_fail("itertools")
install_or_fail("randomForest")
install_or_fail("ModelMetrics")
install_or_fail("stringr")
install_or_fail("gridExtra")
install_or_fail("digest")
install_or_fail("purrr")
install_or_fail("caret")
install_or_fail("testthat")
install_or_fail("e1071")

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("pcaMethods")
library(pcaMethods)
