# Core deterministic and probabilistic analyses.
# Original source seed and draw sharing are preserved.

make_incrementals <- function(results, full = FALSE, comparator = "Baseline") {
  keys <- if (full) c("model_source", "simulation_index", "psa_iter") else c("model_source", "psa_iter")
  baseline <- results %>%
    dplyr::filter(scenario == comparator) %>%
    dplyr::select(dplyr::all_of(keys), base_cost = total_cost, base_qaly_loss = total_qaly_loss)
  if (anyDuplicated(baseline[keys])) stop("Baseline rows are not unique for pairing keys.")
  result <- results %>%
    dplyr::filter(scenario != comparator) %>%
    dplyr::left_join(baseline, by = keys) %>%
    dplyr::mutate(
      inc_cost = total_cost - base_cost,
      inc_qalys = base_qaly_loss - total_qaly_loss,
      icer = inc_cost / inc_qalys,
      nmb_50k = inc_qalys * 50000 - inc_cost,
      cost_effective_50k = nmb_50k > 0
    )
  if (anyNA(result$base_cost) || anyNA(result$base_qaly_loss)) {
    stop("Some Baseline rows did not match intervention iterations. Verify simulation_index alignment.")
  }
  result
}

run_core_analysis <- function(df_cea, df_mean_epi, n_psa_economic = 10000L,
                              n_psa_full = 1000L, seed = 12345) {
  # Deterministic cost and QALY calculations use the original two functions.
  base_results_full <- run_cea_once(df_cea, base_pars)
  base_results_mean_epi <- run_cea_once_mean_epi(df_mean_epi, base_pars)

  set.seed(seed)
  psa_results_mean_epi <- dplyr::bind_rows(lapply(seq_len(n_psa_economic), function(i) {
    pars <- sample_psa_params(psa_inputs_par)
    run_cea_once_mean_epi(df_mean_epi, pars) %>% dplyr::mutate(psa_iter = i)
  }))
  incremental_mean_epi <- make_incrementals(psa_results_mean_epi, full = FALSE)

  set.seed(seed)
  psa_results_full <- dplyr::bind_rows(lapply(seq_len(n_psa_full), function(i) {
    pars <- sample_psa_params(psa_inputs_par)
    run_cea_once(df_cea, pars) %>% dplyr::mutate(psa_iter = i)
  }))
  incremental_full <- make_incrementals(psa_results_full, full = TRUE)

  list(
    base_results_full = base_results_full,
    base_results_mean_epi = base_results_mean_epi,
    psa_results_mean_epi = psa_results_mean_epi,
    incremental_mean_epi = incremental_mean_epi,
    psa_results_full = psa_results_full,
    incremental_full = incremental_full
  )
}
