# Public parameter definitions extracted from the source analysis.
# Parameter means and SD formulae are intentionally unchanged.

beta_params <- function(mean, sd) {
  var <- sd^2
  alpha <- mean * ((mean * (1 - mean) / var) - 1)
  beta <- (1 - mean) * ((mean * (1 - mean) / var) - 1)
  c(alpha = alpha, beta = beta)
}
gamma_params <- function(mean, sd) {
  shape <- mean^2 / sd^2
  rate <- mean / sd^2
  c(shape = shape, rate = rate)
}

psa_inputs <- tibble::tibble(
  parameter = c(
    "qaly_dec_nonhosp", "qaly_dec_hosp", "cost_laiv_vaccine", "cost_im_vaccine",
    "cost_program_school", "cost_program_other", "cost_under5_extra", "cost_case", "cost_hosp"
  ),
  mean = c(0.009, 0.031, 25.00, 10.00, 12.77, 26.01, 6.00, 43.90, 10385.40),
  sd = c(
    (0.011 - 0.007) / 3.92,
    (0.037 - 0.025) / 3.92,
    (49.99 - 21.83) / 3.92,
    (35.00 - 5.00) / 3.92,
    (26.01 - 3.95) / 3.92,
    (43.90 - 20.05) / 3.92,
    0.0001,
    (84.90 - 20.05) / 3.92,
    (14153.10 - 6241.88) / 3.92
  ),
  dist = c("beta", "beta", "gamma", "gamma", "gamma", "gamma", "gamma", "gamma", "gamma")
)
base_pars <- as.list(stats::setNames(psa_inputs$mean, psa_inputs$parameter))
laiv_price_range <- c(21.83, 49.99)
iiv_price_range <- c(5, 35)

psa_inputs_par <- psa_inputs |>
  dplyr::mutate(par1 = NA_real_, par2 = NA_real_)
for (i in seq_len(nrow(psa_inputs_par))) {
  if (psa_inputs_par$dist[i] == "beta") {
    temp <- beta_params(psa_inputs_par$mean[i], psa_inputs_par$sd[i])
  } else if (psa_inputs_par$dist[i] == "gamma") {
    temp <- gamma_params(psa_inputs_par$mean[i], psa_inputs_par$sd[i])
  } else {
    stop("Unknown PSA distribution")
  }
  psa_inputs_par$par1[i] <- temp[1]
  psa_inputs_par$par2[i] <- temp[2]
}
stopifnot(all(is.finite(psa_inputs_par$par1)), all(psa_inputs_par$par1 > 0),
          all(is.finite(psa_inputs_par$par2)), all(psa_inputs_par$par2 > 0))

# Mirrors source code: one draw per parameter, shared across model sources,
# strategies and transmission iterations within each PSA iteration.
sample_psa_params <- function(input_table) {
  draws <- purrr::map2_dbl(input_table$dist, seq_len(nrow(input_table)), function(d, i) {
    if (d == "beta") {
      stats::rbeta(1, shape1 = input_table$par1[i], shape2 = input_table$par2[i])
    } else if (d == "gamma") {
      stats::rgamma(1, shape = input_table$par1[i], rate = input_table$par2[i])
    } else stop(paste("Unsupported distribution:", d))
  })
  names(draws) <- input_table$parameter
  as.list(draws)
}
