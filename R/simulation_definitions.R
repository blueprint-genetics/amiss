
# For MCAR, the type argument makes no difference
mcar_ampute_params <- list(
  prop = 1:4 * 0.2,
  mech = c("MCAR"),
  type = c("LEFT")
) %>% expand.grid(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)

non_mcar_ampute_params <- list(
  prop = 1:4 * 0.2,
  mech = c("MAR", "MNAR"),
  type = c("LEFT", "TAIL", "RIGHT", "MID")
  ) %>% expand.grid(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)

ampute_params <- rbind(mcar_ampute_params, non_mcar_ampute_params)

repeats <- 400
