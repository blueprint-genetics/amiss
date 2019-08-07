library(vcfR)
library(futile.logger)

flog.threshold(DEBUG)

source("R/load_data.R")
source("R/filters.R")

vcf <- vcfR::read.vcfR("../amiss_data/clinvar_20190624.vep.vcf")

vcf_df <- vcf_object_to_dataframe(vcf, num_batches = 100, info_filters = c(clingen), vep_filters = c(missense, canonical))

