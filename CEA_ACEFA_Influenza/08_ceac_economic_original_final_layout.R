# Original manuscript CEAC layout, provided for visual parity.
# Numeric results derived from the same PSA objects.
# Requires run_analysis.R objects; NOT run automatically.

# Economic-only CEAC figure
#
# Four panels:
# Row 1 = Pairwise CEAC versus Baseline
# Row 2 = Competing-strategy CEAC
# Columns = UNSW and UoM
#
# WTP vertical threshold lines:
# - $15,000/QALY = dotted
# - $50,000/QALY = solid
#
# X-axis labels:
# - $0k, $50k, $100k
# - no $15k axis label
#
# Uses:
# - psa_results_mean_epi
#
# Does NOT rerun the PSA
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(grid)

# ------------------------------------------------------------
# Check required objects exist
# ------------------------------------------------------------

if (!exists("psa_results_mean_epi")) {
  stop("psa_results_mean_epi is not in your R environment.")
}

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

output_dir <- file.path("outputs", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

wtp_values <- seq(
  from = 0,
  to = 100000,
  by = 1000
)

wtp_threshold_lines <- tibble(
  threshold = c(
    15000,
    50000
  ),
  threshold_label = factor(
    c("$15,000/QALY", "$50,000/QALY"),
    levels = c("$15,000/QALY", "$50,000/QALY")
  )
)

central_5 <- c(
  "Central_LAIV_2-5yo",
  "Central_LAIV_2-12yo",
  "Central_LAIV_2-18yo",
  "Central_LAIV_5-12yo",
  "Central_LAIV_5-18yo"
)

central_plus_baseline <- c(
  "Baseline",
  central_5
)

strategy_levels <- c(
  "LAIV 2–<5",
  "LAIV 2–<12",
  "LAIV 2–<18",
  "LAIV 5–<12",
  "LAIV 5–<18"
)

strategy_levels_ceac <- c(
  "Baseline",
  strategy_levels
)

scenario_labels_central <- c(
  "Baseline"              = "Baseline",
  "Central_LAIV_2-5yo"    = "LAIV 2–<5",
  "Central_LAIV_2-12yo"   = "LAIV 2–<12",
  "Central_LAIV_2-18yo"   = "LAIV 2–<18",
  "Central_LAIV_5-12yo"   = "LAIV 5–<12",
  "Central_LAIV_5-18yo"   = "LAIV 5–<18"
)

scenario_palette_ceac <- c(
  "Baseline"   = "#4D4D4D",
  "LAIV 2–<5"  = "#B2182B",
  "LAIV 2–<12" = "#eb008b",
  "LAIV 2–<18" = "#0072B2",
  "LAIV 5–<12" = "#009E73",
  "LAIV 5–<18" = "#D55E00"
)

analysis_levels <- c(
  "Pairwise versus Baseline",
  "Competing strategies"
)

# ============================================================
# Step 1: Prepare economic-only PSA results
# ============================================================

psa_results_mean_epi_central <- psa_results_mean_epi %>%
  filter(
    scenario %in% central_plus_baseline
  ) %>%
  mutate(
    strategy_label = recode(
      scenario,
      !!!scenario_labels_central
    ),
    strategy_label = factor(
      strategy_label,
      levels = strategy_levels_ceac
    ),
    model_source = factor(
      model_source,
      levels = c("UNSW", "UoM")
    )
  )

# ============================================================
# Step 2: Pairwise CEAC versus Baseline
# ============================================================

baseline_mean_epi <- psa_results_mean_epi_central %>%
  filter(
    scenario == "Baseline"
  ) %>%
  select(
    model_source,
    psa_iter,
    base_cost = total_cost,
    base_qaly_loss = total_qaly_loss
  )

pairwise_input_mean_epi <- psa_results_mean_epi_central %>%
  filter(
    scenario %in% central_5
  ) %>%
  left_join(
    baseline_mean_epi,
    by = c(
      "model_source",
      "psa_iter"
    )
  ) %>%
  mutate(
    inc_cost = total_cost - base_cost,
    inc_qalys = base_qaly_loss - total_qaly_loss,
    strategy_label = factor(
      strategy_label,
      levels = strategy_levels
    )
  ) %>%
  select(
    model_source,
    psa_iter,
    strategy_label,
    inc_cost,
    inc_qalys
  )

pairwise_ceac_mean_epi <- map_dfr(
  wtp_values,
  function(wtp_i) {
    
    pairwise_input_mean_epi %>%
      mutate(
        cost_effective = inc_qalys * wtp_i - inc_cost > 0
      ) %>%
      group_by(
        model_source,
        strategy_label
      ) %>%
      summarise(
        probability = mean(
          cost_effective,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      mutate(
        wtp = wtp_i,
        analysis_label = "Pairwise versus Baseline"
      )
  }
) %>%
  mutate(
    strategy_label = factor(
      strategy_label,
      levels = strategy_levels_ceac
    ),
    analysis_label = factor(
      analysis_label,
      levels = analysis_levels
    ),
    model_source = factor(
      model_source,
      levels = c("UNSW", "UoM")
    )
  )

# ============================================================
# Step 3: Competing-strategy CEAC
# ============================================================

competing_input_mean_epi <- psa_results_mean_epi_central %>%
  select(
    model_source,
    psa_iter,
    scenario,
    strategy_label,
    total_cost,
    total_qaly_loss
  )

competing_ceac_mean_epi <- map_dfr(
  wtp_values,
  function(wtp_i) {
    
    competing_input_mean_epi %>%
      mutate(
        nmb = -total_cost - total_qaly_loss * wtp_i
      ) %>%
      group_by(
        model_source,
        psa_iter
      ) %>%
      slice_max(
        order_by = nmb,
        n = 1,
        with_ties = FALSE
      ) %>%
      ungroup() %>%
      count(
        model_source,
        strategy_label,
        name = "n_best"
      ) %>%
      group_by(
        model_source
      ) %>%
      mutate(
        probability = n_best / sum(n_best)
      ) %>%
      ungroup() %>%
      mutate(
        wtp = wtp_i,
        analysis_label = "Competing strategies"
      )
  }
) %>%
  complete(
    model_source = factor(
      c("UNSW", "UoM"),
      levels = c("UNSW", "UoM")
    ),
    strategy_label = factor(
      strategy_levels_ceac,
      levels = strategy_levels_ceac
    ),
    wtp = wtp_values,
    fill = list(
      n_best = 0,
      probability = 0,
      analysis_label = "Competing strategies"
    )
  ) %>%
  mutate(
    strategy_label = factor(
      strategy_label,
      levels = strategy_levels_ceac
    ),
    analysis_label = factor(
      analysis_label,
      levels = analysis_levels
    ),
    model_source = factor(
      model_source,
      levels = c("UNSW", "UoM")
    )
  )

# ============================================================
# Step 4: Combine CEAC data
# ============================================================

ceac_4panel_economic_only <- bind_rows(
  pairwise_ceac_mean_epi,
  competing_ceac_mean_epi
) %>%
  mutate(
    strategy_label = factor(
      strategy_label,
      levels = strategy_levels_ceac
    ),
    analysis_label = factor(
      analysis_label,
      levels = analysis_levels
    ),
    model_source = factor(
      model_source,
      levels = c("UNSW", "UoM")
    )
  )

# ============================================================
# Step 5: Theme
# ============================================================

ACEFA_plot_theme_ceac_4panel <- theme_bw() +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, colour = "black"),
    
    strip.text.x = element_text(
      size = 15,
      face = "bold",
      margin = margin(t = 0, r = 0, b = 10, l = 0)
    ),
    strip.text.y = element_blank(),
    strip.background = element_blank(),
    
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical",
    
    panel.border = element_blank(),
    axis.line = element_line(
      color = "black",
      linewidth = 0.7
    ),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.spacing.x = unit(2.2, "lines"),
    panel.spacing.y = unit(1.1, "lines"),
    
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 10,
      l = 10
    )
  )

# ============================================================
# Step 6: Plot
# ============================================================

ceac_4panel_economic_only_main_15k_line_no_15k_axis_label <- ggplot(
  ceac_4panel_economic_only,
  aes(
    x = wtp,
    y = probability,
    colour = strategy_label
  )
) +
  geom_vline(
    data = wtp_threshold_lines,
    aes(
      xintercept = threshold,
      linetype = threshold_label
    ),
    colour = "black",
    linewidth = 0.55,
    inherit.aes = FALSE
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  facet_grid(
    analysis_label ~ model_source
  ) +
  scale_colour_manual(
    name = "Strategy",
    values = scenario_palette_ceac,
    breaks = strategy_levels_ceac,
    limits = strategy_levels_ceac,
    drop = FALSE
  ) +
  scale_linetype_manual(
    name = "WTP threshold",
    values = c(
      "$15,000/QALY" = "dotted",
      "$50,000/QALY" = "solid"
    ),
    breaks = c(
      "$15,000/QALY",
      "$50,000/QALY"
    ),
    drop = FALSE
  ) +
  scale_x_continuous(
    labels = c(
      "$0k",
      "$50k",
      "$100k"
    ),
    breaks = c(
      0,
      50000,
      100000
    ),
    limits = c(
      0,
      100000
    ),
    expand = expansion(
      mult = c(0, 0.01)
    )
  ) +
  scale_y_continuous(
    labels = percent_format(
      accuracy = 1
    ),
    breaks = c(
      0,
      0.25,
      0.50,
      0.75,
      1.00
    ),
    limits = c(
      0,
      1
    ),
    expand = expansion(
      mult = c(0, 0.02)
    )
  ) +
  labs(
    x = "Willingness-to-pay threshold per QALY gained",
    y = "Probability of cost-effectiveness"
  ) +
  ACEFA_plot_theme_ceac_4panel +
  guides(
    colour = guide_legend(
      order = 1,
      ncol = 1,
      byrow = TRUE
    ),
    linetype = guide_legend(
      order = 2,
      ncol = 1,
      byrow = TRUE
    )
  )

ceac_4panel_economic_only_main_15k_line_no_15k_axis_label

# ============================================================
# Step 7: Save figure
# ============================================================

ggsave(
  filename = file.path(
    output_dir,
    "ceac_4panel_economic_only_main_15k_line_no_15k_axis_label_15k_dotted_50k_solid.png"
  ),
  plot = ceac_4panel_economic_only_main_15k_line_no_15k_axis_label,
  width = 8.27,
  height = 6.6,
  units = "in",
  dpi = 600,
  bg = "white"
)









# ============================================================
