library(tidyr)
library(stringr)

vcf_data <- vcfR::read.vcfR("../amiss_data/clinvar_20190624.vep.vcf")

idfiers <- type.convert(data.frame(vcf_data@fix[1:100, c("CHROM", "POS", "REF", "ALT")], stringsAsFactors = FALSE), as.is = TRUE)
info <- vcf_data@fix[1:100,"INFO", drop = TRUE] 
# TODO: once code seems to work, remove row restriction

info_split <- stringr::str_split(string = info, pattern = fixed(";"))

info_key_value_pair_lists <- lapply(X = info_split, 
                                    FUN = function(x) stringr::str_split_fixed(string = x, pattern = fixed('='), n = 2))

info_key_value_pair_lists <- lapply(info_key_value_pair_lists,
                                        function(x) {
                                          x <- data.frame(x, stringsAsFactors = FALSE)
                                          colnames(x) <- c("key", "value")
                                          return(x)
                                        }
                                    )

keys <- lapply(info_key_value_pair_lists,
               function(x) unique(x[, "key"]))

keys <- unique(unlist(keys))

info_key_value_pair_lists <- lapply(info_key_value_pair_lists,
                                    function(x)
                                      tidyr::spread(
                                        data.frame(x),
                                        key = key,
                                        value = value,
                                        convert = TRUE
                                      ))

row_idx = seq_along(info_key_value_pair_lists)

# Are all just one observation, as they should be?
is_all_one_obs <- all(sapply(info_key_value_pair_lists, function(x) nrow(x) == 1))

info_w_ids <- lapply(row_idx,
                     function(x) {
                       cbind(
                         idfiers[x, , drop = FALSE],
                         info_key_value_pair_lists[[x]]
                       )
                     })

info_w_ids <- lapply(info_w_ids,
                     function(x) 
                     {
                       missing_cols <- setdiff(x = c("CHROM", "POS", "ALT", "REF", keys), y = colnames(x))
                       x[, missing_cols] <- NA
                       x
                     })

variant_data <- do.call(rbind, info_w_ids)

# TODO: check for existence of multiple ALT alleles

# Split by transcript
vep_data <- lapply(variant_data$CSQ, 
                   function(x) str_split(string = x, pattern = ',', n = Inf) %>% unlist)
variant_data$CSQ <- NULL

transcript_nums <- lapply(vep_data, . %>% length)

# Check that transcripts are unique
vep_data <- lapply(vep_data, unique)
unique_transcript_nums <- lapply(vep_data, . %>% length)
stopifnot(mapply(FUN = function(x,y) x == y, transcript_nums, unique_transcript_nums))

# Replicate rows 
variant_data <- variant_data[rep(1:nrow(variant_data), times = unlist(transcript_nums)), ]

# Get CSQ keys from metadata
csq_keys <- vcfR::queryMETA(vcf_data, 'CSQ', nice = TRUE)[[1]][4]
csq_keys <- stringr::str_split(csq_keys, fixed("Format: "))[[1]][-1]
csq_keys <- stringr::str_split(csq_keys, fixed("|"))[[1]]

vep_data <- stringr::str_split_fixed(vep_data %>% unlist, fixed("|"), n = length(csq_keys))
colnames(vep_data) <- csq_keys

variant_data <- data.frame(variant_data, vep_data)
variant_data <- type.convert(x = variant_data, as.is = TRUE, numerals = "allow.loss") # TODO: can the precision loss be avoided?


