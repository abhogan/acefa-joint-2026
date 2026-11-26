# Optional figure code preserved from the final source block (not independently executed here).
# Requires run_analysis.R to have created the analysis objects.
# Outputs remain git-ignored until approved.

# 4-panel tornado diagram
# ICER approach
# Central LAIV 5–<12 and 5–<18 strategies versus Baseline
# UNSW left, UoM right
#
# Updates:
# - Under-5 additional delivery cost removed
# - IM vaccine cost relabelled as IIV vaccine cost
# - X-axis labelled "Incremental cost-effectiveness ratio"
# - X-axis breaks in $20k increments
# - UNSW/UoM facet label font matched to other figures
# - Legend labels: low/high parameter value
# - No parameter values shown in y-axis labels or figure body
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(grid)

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

wtp <- 50000

# ------------------------------------------------------------
# Helper: calculate ICER by model source
# ------------------------------------------------------------

calc_icer_by_model <- function(
    results_df,
    strategy_name,
    baseline_name = "Baseline"
) {
  
  baseline <- results_df %>%
    filter(scenario == baseline_name) %>%
    select(
      model_source,
      base_cost = total_cost,
      base_qaly_loss = total_qaly_loss
    )
  
  results_df %>%
    filter(scenario == strategy_name) %>%
    left_join(
      baseline,
      by = "model_source"
    ) %>%
    mutate(
      inc_cost = total_cost - base_cost,
      inc_qalys = base_qaly_loss - total_qaly_loss,
      icer = inc_cost / inc_qalys
    ) %>%
    select(
      model_source,
      inc_cost,
      inc_qalys,
      icer
    )
}

# ------------------------------------------------------------
# OWSA parameter ranges and labels
# Under-5 additional delivery cost removed
# IM re-labelled as IIV
# ------------------------------------------------------------

# ------------------------------------------------------------
# Helper function: calculate ICER by model source
# ------------------------------------------------------------

calc_icer_by_model <- function(
    results_df,
    strategy_name,
    comparator_name = "Baseline"
) {
  
  required_vars <- c(
    "model_source",
    "scenario",
    "total_cost",
    "total_qaly_loss"
  )
  
  missing_vars <- setdiff(
    required_vars,
    names(results_df)
  )
  
  if (length(missing_vars) > 0) {
    stop(
      paste(
        "The following required variables are missing from results_df:",
        paste(missing_vars, collapse = ", ")
      )
    )
  }
  
  baseline <- results_df %>%
    filter(
      scenario == comparator_name
    ) %>%
    select(
      model_source,
      base_cost = total_cost,
      base_qaly_loss = total_qaly_loss
    )
  
  results_df %>%
    filter(
      scenario == strategy_name
    ) %>%
    left_join(
      baseline,
      by = "model_source"
    ) %>%
    mutate(
      inc_cost = total_cost - base_cost,
      inc_qalys = base_qaly_loss - total_qaly_loss,
      icer = inc_cost / inc_qalys
    ) %>%
    select(
      model_source,
      inc_cost,
      inc_qalys,
      icer
    )
}





owsa_ranges <- tibble::tribble(
  ~parameter,              ~parameter_label,                         ~low,       ~high,
  "qaly_dec_nonhosp",      "Non-hospitalised QALY decrement",         0.007,      0.011,
  "qaly_dec_hosp",         "Hospitalised QALY decrement",             0.025,      0.037,
  "cost_laiv_vaccine",     "LAIV vaccine cost",                      21.83,      49.99,
  "cost_im_vaccine",       "IIV vaccine cost",                        5.00,      35.00,
  "cost_program_school",   "School delivery cost",                    3.95,      26.01,
  "cost_program_other",    "Other delivery cost",                    20.05,      43.90,
  "cost_under5_extra",     "Additional under-5 delivery cost",         0.00,      12.00,
  "cost_case",             "Non-hospitalised case cost",             20.05,      84.90,
  "cost_hosp",             "Hospitalisation cost",                 6241.88,   14153.10
) %>%
  mutate(
    parameter_axis = parameter_label
  )


wtp <- 50000
wtp_low <- 15000



# ------------------------------------------------------------
# Comparisons for the 4 panels
# ------------------------------------------------------------

comparisons <- tibble::tribble(
  ~strategy,                ~comparison_label,
  "Central_LAIV_5-12yo",    "LAIV 5–<12",
  "Central_LAIV_5-18yo",    "LAIV 5–<18"
)

# ------------------------------------------------------------
# Base-case ICERs
# ------------------------------------------------------------

base_results_owsa <- run_cea_once_mean_epi(
  data = df_mean_epi,
  pars = base_pars
)

base_icers <- map_dfr(seq_len(nrow(comparisons)), function(i) {
  
  strategy_i <- comparisons$strategy[i]
  comparison_i <- comparisons$comparison_label[i]
  
  calc_icer_by_model(
    results_df = base_results_owsa,
    strategy_name = strategy_i
  ) %>%
    mutate(
      strategy = strategy_i,
      comparison_label = comparison_i
    )
})

# ------------------------------------------------------------
# Run OWSA
# ------------------------------------------------------------

owsa_results_all <- map_dfr(seq_len(nrow(comparisons)), function(j) {
  
  strategy_j <- comparisons$strategy[j]
  comparison_j <- comparisons$comparison_label[j]
  
  map_dfr(seq_len(nrow(owsa_ranges)), function(i) {
    
    param_i <- owsa_ranges$parameter[i]
    label_i <- owsa_ranges$parameter_label[i]
    
    # Low parameter value
    pars_low <- base_pars
    pars_low[[param_i]] <- owsa_ranges$low[i]
    
    low_results <- run_cea_once_mean_epi(
      data = df_mean_epi,
      pars = pars_low
    )
    
    low_icer <- calc_icer_by_model(
      results_df = low_results,
      strategy_name = strategy_j
    ) %>%
      mutate(
        strategy = strategy_j,
        comparison_label = comparison_j,
        parameter = param_i,
        parameter_label = label_i,
        value_type = "Low parameter value"
      )
    
    # High parameter value
    pars_high <- base_pars
    pars_high[[param_i]] <- owsa_ranges$high[i]
    
    high_results <- run_cea_once_mean_epi(
      data = df_mean_epi,
      pars = pars_high
    )
    
    high_icer <- calc_icer_by_model(
      results_df = high_results,
      strategy_name = strategy_j
    ) %>%
      mutate(
        strategy = strategy_j,
        comparison_label = comparison_j,
        parameter = param_i,
        parameter_label = label_i,
        value_type = "High parameter value"
      )
    
    bind_rows(
      low_icer,
      high_icer
    )
  })
})

# ------------------------------------------------------------
# Prepare plotting data
# ------------------------------------------------------------

tornado_plot_data <- owsa_results_all %>%
  left_join(
    base_icers %>%
      select(
        model_source,
        strategy,
        comparison_label,
        base_icer = icer
      ),
    by = c("model_source", "strategy", "comparison_label")
  ) %>%
  left_join(
    owsa_ranges %>%
      select(
        parameter,
        parameter_axis
      ),
    by = "parameter"
  ) %>%
  mutate(
    model_source = factor(
      model_source,
      levels = c("UNSW", "UoM")
    ),
    comparison_label = factor(
      comparison_label,
      levels = c("LAIV 5–<12", "LAIV 5–<18")
    ),
    value_type = factor(
      value_type,
      levels = c("Low parameter value", "High parameter value")
    )
  )

# ------------------------------------------------------------
# Order parameters by average absolute impact
# ------------------------------------------------------------

parameter_order <- tornado_plot_data %>%
  mutate(
    abs_change = abs(icer - base_icer)
  ) %>%
  group_by(
    parameter_axis
  ) %>%
  summarise(
    mean_abs_change = mean(abs_change, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    mean_abs_change
  ) %>%
  pull(
    parameter_axis
  )

tornado_plot_data <- tornado_plot_data %>%
  mutate(
    parameter_axis = factor(
      parameter_axis,
      levels = parameter_order
    )
  )

base_line_data <- base_icers %>%
  mutate(
    model_source = factor(
      model_source,
      levels = c("UNSW", "UoM")
    ),
    comparison_label = factor(
      comparison_label,
      levels = c("LAIV 5–<12", "LAIV 5–<18")
    )
  )

# ------------------------------------------------------------
# Theme
# ------------------------------------------------------------

ACEFA_plot_theme_tornado <- theme_bw() +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    
    axis.title = element_text(size = 13),
    axis.text.x = element_text(size = 11, colour = "black"),
    axis.text.y = element_text(size = 10.5, colour = "black"),
    
    strip.text = element_text(size = 12),
    strip.text.x = element_text(
      size = 15,
      face = "bold",
      margin = margin(t = 0, r = 0, b = 10, l = 0)
    ),
    strip.text.y = element_text(
      size = 12
    ),
    strip.background = element_blank(),
    
    legend.position = "bottom",
    
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.8),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    panel.spacing.x = unit(1.0, "lines"),
    panel.spacing.y = unit(1.0, "lines"),
    
    plot.margin = margin(
      t = 8,
      r = 20,
      b = 8,
      l = 8
    )
  )

# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

tornado_icer_4panel_updated <- ggplot(
  tornado_plot_data,
  aes(
    y = parameter_axis
  )
) +
  geom_vline(
    data = base_line_data,
    aes(
      xintercept = icer
    ),
    inherit.aes = FALSE,
    linewidth = 0.7,
    colour = "black"
  ) +
  geom_vline(
    xintercept = wtp_low,
    linetype = "dotted",
    linewidth = 0.7,
    colour = "black"
  ) +
  geom_vline(
    xintercept = wtp,
    linetype = "dashed",
    linewidth = 0.7,
    colour = "black"
  ) +
  geom_segment(
    aes(
      x = base_icer,
      xend = icer,
      yend = parameter_axis,
      colour = value_type
    ),
    linewidth = 5,
    lineend = "butt"
  ) +
  facet_grid(
    comparison_label ~ model_source,
    scales = "free_y",
    space = "free_y"
  ) +
  scale_colour_manual(
    name = NULL,
    values = c(
      "Low parameter value" = "#262261",
      "High parameter value" = "#eb008b"
    ),
    breaks = c(
      "Low parameter value",
      "High parameter value"
    )
  ) +
  scale_x_continuous(
    labels = label_dollar(
      scale = 1 / 1000,
      suffix = "k",
      prefix = "$"
    ),
    breaks = function(x) {
      seq(
        from = floor(x[1] / 20000) * 20000,
        to = ceiling(x[2] / 20000) * 20000,
        by = 20000
      )
    },
    expand = expansion(mult = c(0.08, 0.12))
  ) +
  labs(
    x = "Incremental cost-effectiveness ratio",
    y = NULL
  ) +
  ACEFA_plot_theme_tornado +
  guides(
    colour = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        linewidth = 4
      )
    )
  )

tornado_icer_4panel_updated

# ------------------------------------------------------------
# Save as PNG
# ------------------------------------------------------------

ggsave(
  filename = file.path("outputs", "figures", "tornado_icer_4panel_updated.png"),
  plot = tornado_icer_4panel_updated,
  width = 8.27,
  height = 7.2,
  units = "in",
  dpi = 600,
  bg = "white"
)
