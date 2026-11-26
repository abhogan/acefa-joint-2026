# Memory-safe CEAC computations. Each WTP is processed separately, avoiding
# crossing(PSA results, WTP) and large temporary allocations.

calc_pairwise_ceac <- function(incremental, wtp_values = analysis_wtp,
                               scenarios = central_5) {
  dt <- data.table::as.data.table(incremental %>%
    dplyr::filter(scenario %in% scenarios) %>%
    dplyr::select(model_source, scenario, inc_cost, inc_qalys))
  if (nrow(dt) == 0) stop("No strategies found in incremental results.")
  data.table::rbindlist(lapply(wtp_values, function(w) {
    res <- dt[, .(probability = mean(inc_qalys * w - inc_cost > 0, na.rm = TRUE)),
              by = .(model_source, scenario)]
    res[, wtp := w]
    res
  })) %>% tibble::as_tibble()
}

calc_competing_ceac <- function(psa, wtp_values = analysis_wtp,
                                scenarios = central_plus_baseline,
                                full = TRUE) {
  keys <- if (full) c("model_source", "simulation_index", "psa_iter") else c("model_source", "psa_iter")
  dt <- data.table::as.data.table(psa %>%
    dplyr::filter(scenario %in% scenarios) %>%
    dplyr::select(dplyr::all_of(keys), scenario, total_cost, total_qaly_loss))
  if (!nrow(dt)) stop("No competing scenarios found in PSA results.")
  if (anyDuplicated(dt[c(keys, "scenario")])) stop("Duplicate strategies within a simulation/PSA draw.")
  if (anyNA(dt$total_cost) || anyNA(dt$total_qaly_loss)) stop("NA in competing strategy totals.")
  res <- data.table::rbindlist(lapply(wtp_values, function(w) {
    # No data expansion: compute the maximising NMB row per model/PSA iteration.
    best_indices <- dt[, .I[which.max(-total_cost - w * total_qaly_loss)], by = keys]$V1
    winners <- dt[best_indices, .(model_source, scenario)]
    counts <- winners[, .N, by = .(model_source, scenario)]
    counts[, probability := N / sum(N), by = model_source]
    counts[, wtp := w]
    counts[, .(model_source, scenario, wtp, probability)]
  })) %>% tibble::as_tibble()
  res %>%
    tidyr::complete(
      model_source = unique(dt$model_source), scenario = scenarios,
      wtp = wtp_values, fill = list(probability = 0)
    )
}

# Original analysis' central 50% definition, based on the average incremental
# symptomatic cases averted over the five central interventions (per model run).
select_iqr_impact <- function(psa_full) {
  disease <- psa_full %>%
    dplyr::filter(scenario %in% central_plus_baseline) %>%
    dplyr::distinct(model_source, simulation_index, scenario, total_disease)
  base <- disease %>%
    dplyr::filter(scenario == "Baseline") %>%
    dplyr::select(model_source, simulation_index, baseline_total_disease = total_disease)
  impact <- disease %>%
    dplyr::filter(scenario %in% central_5) %>%
    dplyr::left_join(base, by = c("model_source", "simulation_index")) %>%
    dplyr::mutate(disease_averted = baseline_total_disease - total_disease) %>%
    dplyr::group_by(model_source, simulation_index) %>%
    dplyr::summarise(mean_disease_averted = mean(disease_averted, na.rm = TRUE), .groups = "drop")
  central_sims <- impact %>%
    dplyr::group_by(model_source) %>%
    dplyr::mutate(q25 = stats::quantile(mean_disease_averted, .25, na.rm = TRUE),
                  q75 = stats::quantile(mean_disease_averted, .75, na.rm = TRUE)) %>%
    dplyr::filter(mean_disease_averted >= q25, mean_disease_averted <= q75) %>%
    dplyr::ungroup()
  psa_full %>%
    dplyr::filter(scenario %in% central_plus_baseline) %>%
    dplyr::semi_join(central_sims, by = c("model_source", "simulation_index"))
}

make_4panel_ceac <- function(pairwise, competing, output_file = NULL) {
  df <- dplyr::bind_rows(
    pairwise %>% dplyr::mutate(analysis_type = "Pairwise versus Baseline"),
    competing %>% dplyr::mutate(analysis_type = "Competing strategies")
  ) %>%
    dplyr::mutate(
      analysis_type = factor(analysis_type, levels = c("Pairwise versus Baseline", "Competing strategies")),
      model_source = factor(model_source, levels = c("UNSW", "UoM")),
      strategy_label = factor(unname(scenario_labels_central[scenario]),
                              levels = unname(scenario_labels_central))
    )
  labels <- tidyr::expand_grid(
    analysis_type = factor(c("Pairwise versus Baseline", "Competing strategies"),
                           levels = c("Pairwise versus Baseline", "Competing strategies")),
    model_source = factor(c("UNSW", "UoM"), levels = c("UNSW", "UoM"))
  ) %>% dplyr::mutate(label = paste0(LETTERS[dplyr::row_number()], ")"), x = 0, y = 1)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = wtp, y = probability,
                                        colour = strategy_label, group = strategy_label)) +
    ggplot2::geom_vline(xintercept = 15000, linetype = "dotted", linewidth = .5) +
    ggplot2::geom_vline(xintercept = 50000, linetype = "solid", linewidth = .5) +
    ggplot2::geom_line(linewidth = .8) +
    ggplot2::geom_text(data = labels, ggplot2::aes(x = x, y = y, label = label),
                       inherit.aes = FALSE, hjust = -.25, vjust = -.25, size = 4.2) +
    ggplot2::facet_grid(analysis_type ~ model_source) +
    ggplot2::scale_colour_manual(name = "Strategy", values = scenario_palette_ceac,
                                 breaks = unname(scenario_labels_central), drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = c(0, 50000, 100000),
                                labels = c("$0k", "$50k", "$100k"), limits = c(0, 100000)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                breaks = seq(0, 1, .25), limits = c(0, 1)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = "Willingness-to-pay threshold per QALY gained",
                  y = "Probability of cost-effectiveness") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = 13, face = "bold"),
      strip.text.y = ggplot2::element_blank(), strip.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black", linewidth = .5),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right", legend.title = ggplot2::element_text(size = 10),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, p, width = 8.27, height = 6.8, dpi = 600, bg = "white")
  }
  p
}
