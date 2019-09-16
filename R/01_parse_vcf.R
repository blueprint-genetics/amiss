library(vcfR)
library(futile.logger)

flog.threshold(DEBUG)

source("R/data_parsing.R")
source("R/filters.R")
source("R/process_data.R")

set.seed(10)

vcf <- vcfR::read.vcfR("../amiss_data/clinvar_20190624.vep.vcf")

vcf_df <- vcf_object_to_dataframe(vcf, num_batches = 100, info_filters = c(clingen), vep_filters = c(missense, canonical))
vcf_df <- vcf_df[vcf_df$CLNSIG != "drug_response", ]

stopifnot(all(vcf_df$Feature == vcf_df$Ensembl_transcriptid))

data_split <- split_train_test(vcf_df, 0.7)

training <- data_split$training_set
test <- data_split$test_set

write.csv(file = "training_data.csv", x = training, row.names = FALSE)
write.csv(file = "test_data.csv", x = test, row.names = FALSE)
