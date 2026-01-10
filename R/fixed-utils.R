beezdemand_fixed_param_specs <- function(results) {
  param_specs <- list(
    Q0 = list(estimate = "Q0d", se = "Q0se"),
    alpha = list(estimate = "Alpha", se = "Alphase"),
    alpha_star = list(estimate = "alpha_star", se = "alpha_star_se"),
    k = list(estimate = "K", se = NA_character_),
    L = list(estimate = "L", se = "Lse"),
    b = list(estimate = "b", se = "bse"),
    a = list(estimate = "a", se = "ase")
  )

  Filter(function(spec) spec$estimate %in% names(results), param_specs)
}

beezdemand_fixed_id_values <- function(results) {
  if ("id" %in% names(results)) {
    as.character(results$id)
  } else {
    rep(NA_character_, nrow(results))
  }
}
