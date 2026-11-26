# Tables are derived outputs: review before sharing to avoid disclosing restricted simulations.

make_summary_table <- function(results, comparator = "Baseline", full = FALSE) {
  by_cols <- c("model_source", "scenario")
  point_est <- results %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by_cols))) %>%
    dplyr::summarise(
      mean_total_cost = mean(total_cost, na.rm = TRUE),
      mean_total_qaly_loss = mean(total_qaly_loss, na.rm = TRUE),
      mean_disease_costs = mean(disease_costs, na.rm = TRUE),
      mean_vaccine_costs = mean(vaccine_costs, na.rm = TRUE),
      mean_non_hosp_qaly_loss = mean(non_hosp_qaly_loss, na.rm = TRUE),
      mean_hosp_qaly_loss = mean(hosp_qaly_loss, na.rm = TRUE),
      mean_death_qaly_loss = mean(death_qaly_loss, na.rm = TRUE),
      .groups = "drop"
    )
  base <- point_est %>%
    dplyr::filter(scenario == comparator) %>%
    dplyr::select(model_source, base_cost = mean_total_cost,
                  base_qaly_loss = mean_total_qaly_loss)
  point_est %>%
    dplyr::left_join(base, by = "model_source") %>%
    dplyr::mutate(
      incremental_cost = mean_total_cost - base_cost,
      incremental_qalys_gained = base_qaly_loss - mean_total_qaly_loss,
      icer_from_means = ifelse(scenario == comparator, NA_real_,
                              incremental_cost / incremental_qalys_gained)
    ) %>%
    dplyr::arrange(model_source, scenario)
}

make_probabilistic_summary <- function(incremental) {
  incremental %>%
    dplyr::group_by(model_source, scenario) %>%
    dplyr::summarise(
      mean_inc_cost = mean(inc_cost, na.rm = TRUE),
      inc_cost_p025 = stats::quantile(inc_cost, .025, na.rm = TRUE),
      inc_cost_p975 = stats::quantile(inc_cost, .975, na.rm = TRUE),
      mean_inc_qalys = mean(inc_qalys, na.rm = TRUE),
      inc_qalys_p025 = stats::quantile(inc_qalys, .025, na.rm = TRUE),
      inc_qalys_p975 = stats::quantile(inc_qalys, .975, na.rm = TRUE),
      mean_nmb_50k = mean(nmb_50k, na.rm = TRUE),
      prob_ce_50k = mean(cost_effective_50k, na.rm = TRUE),
      prob_cost_saving = mean(inc_cost < 0, na.rm = TRUE),
      prob_more_effective = mean(inc_qalys > 0, na.rm = TRUE),
      n = dplyr::n(), .groups = "drop"
    ) %>%
    dplyr::mutate(icer_from_means = mean_inc_cost / mean_inc_qalys) %>%
    dplyr::arrange(model_source, scenario)
}

write_analysis_tables <- function(results, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  tables <- list(
    mean_epi_base = make_summary_table(results$base_results_mean_epi),
    full_base = make_summary_table(results$base_results_full, full = TRUE),
    economic_psa = make_probabilistic_summary(results$incremental_mean_epi),
    full_psa = make_probabilistic_summary(results$incremental_full)
  )
  for (nm in names(tables)) {
    readr::write_csv(tables[[nm]], file.path(output_dir, paste0(nm, ".csv")))
  }
  invisible(tables)
}
