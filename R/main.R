library(vcfR)

source("R/load_data.R")
source("R/filters.R")

vcf <- vcfR::read.vcfR("../amiss_data/clinvar_20190624.vep.vcf")

# Divide row indices
divs <- 100
rows <- nrow(vcf@fix)
division <- seq(1, rows, length.out=divs)
row_grp <- findInterval(1:rows, division)
vcf_dfs <- vector("list", divs)
for (r in 1:divs) {
  print(paste0("r = ", r))
  vcf_dfs[[r]] <- vcf_object_to_dataframe(vcf, row_indices = (1:rows)[row_grp == r], info_filters = c(clingen), vep_filters = c(missense))
}
