library(vcfR)
library(futile.logger)

flog.threshold(DEBUG)

source("R/data_parsing.R")
source("R/filters.R")
source("R/process_data.R")

set.seed(10)

vcf <- vcfR::read.vcfR("../amiss_data/clinvar_20190624.vep.vcf")

vcf_df <- vcf_object_to_dataframe(vcf, num_batches = 100, info_filters = c(clingen), vep_filters = c(canonical))

# Drop mitochondrial variants


write.csv(vcf_df, "full_clingen.csv")

cadd_snv_data <- read.delim("../amiss_data/CADD_clingen.tsv", skip = 1, as.is = TRUE)
cadd_indel_data <- read.delim("../amiss_data/CADD_clingen_indel.tsv", skip = 1, as.is = TRUE)

stopifnot(colnames(cadd_snv_data) == colnames(cadd_indel_data))

cadd_data <- rbind(cadd_snv_data, cadd_indel_data)

merged_data <- merge(x = cadd_data,
                     y = vcf_df,
                     all = FALSE,
                     by.x = c("X.Chrom", "Pos", "Ref", "Alt", "FeatureID"),
                     by.y = c("CHROM", "POS", "REF", "ALT", "Feature"))

data_split <- split_train_test(merged_data, 0.7)

training <- data_split$training_set
test <- data_split$test_set
