# Central-strategy cost-effectiveness plane. No titles; thresholds in $/QALY.
make_full_ce_plane <- function(incremental_full, output_file = NULL) {
  points <- incremental_full %>% dplyr::filter(scenario %in% central_5) %>%
    dplyr::mutate(strategy_label = factor(unname(scenario_labels_central[scenario]),
                                           levels = unname(scenario_labels_central[central_5])))
  means <- points %>% dplyr::group_by(model_source, strategy_label) %>%
    dplyr::summarise(inc_cost = mean(inc_cost, na.rm = TRUE),
                     inc_qalys = mean(inc_qalys, na.rm = TRUE), .groups = "drop")
  p <- ggplot2::ggplot(points, ggplot2::aes(x = inc_qalys, y = inc_cost, colour = strategy_label)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = .35) +
    ggplot2::geom_vline(xintercept = 0, linewidth = .35) +
    ggplot2::geom_abline(slope = 15000, intercept = 0, linewidth = .45, linetype = "dashed") +
    ggplot2::geom_abline(slope = 50000, intercept = 0, linewidth = .45, linetype = "solid") +
    ggplot2::geom_point(alpha = .07, size = .35) +
    ggplot2::geom_point(data = means, mapping = ggplot2::aes(fill = strategy_label),
                        size = 2.8, shape = 21, stroke = .65, colour = "black") +
    ggplot2::facet_wrap(~model_source, nrow = 1) +
    ggplot2::scale_colour_manual(values = scenario_palette_ceac, name = "Strategy") +
    ggplot2::scale_fill_manual(values = scenario_palette_ceac, guide = "none") +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::scale_y_continuous(labels = scales::label_dollar(scale = 1/1000, suffix = "k")) +
    ggplot2::labs(x = "Incremental QALYs gained vs Baseline",
                  y = "Incremental costs vs Baseline") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 13),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"))
  if (!is.null(output_file)) ggplot2::ggsave(output_file, p, width = 8.27, height = 4.6,
                                             dpi = 600, bg = "white")
  p
}
