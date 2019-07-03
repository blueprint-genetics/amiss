library(vcfR)

source("R/load_data.R")

vcf_df <- vcf_object_to_dataframe(vcfR::read.vcfR("../amiss_data/clinvar_20190624.vep.vcf"), row_limit = 100)

