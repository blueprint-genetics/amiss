
ampute_params <- list(
  prop = 1:9 * 0.1,
  mech = c("MCAR")
) %>% expand.grid(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)

repeats <- 100
