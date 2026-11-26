# Original manuscript CEAC layout, provided for visual parity.
# Numeric results derived from the same PSA objects.
# Requires run_analysis.R objects; NOT run automatically.

# Main manuscript figure:
# 4-panel CEAC using IQR of transmission-model outputs
#
# IQR is defined using INCREMENTAL transmission-model impact:
# mean symptomatic cases averted versus Baseline across the
# five central LAIV strategies.
#
# Row 1 = Pairwise CEAC versus Baseline
# Row 2 = Competing-strategy CEAC
# Columns = UNSW and UoM
#
# Includes WTP threshold lines:
# - $15,000/QALY gained = dotted
# - $50,000/QALY gained = solid
#
# X-axis labels:
# - $0k, $50k, $100k
# - no $15k axis label
#
# This uses existing psa_results_full.
# It does NOT rerun the PSA.
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)
library(grid)

# ------------------------------------------------------------
# Check required objects exist
# ------------------------------------------------------------

if (!exists("psa_results_full")) {
  stop("psa_results_full is not in your R environment.")
}

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

output_dir <- file.path("outputs", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

wtp_low <- 15000
wtp_high <- 50000

wtp_values <- seq(
  from = 0,
  to = 100000,
  by = 5000
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

strategy_levels_ceac <- c(
  "Baseline",
  "LAIV 2–<5",
  "LAIV 2–<12",
  "LAIV 2–<18",
  "LAIV 5–<12",
  "LAIV 5–<18"
)

strategy_levels_pairwise <- c(
  "LAIV 2–<5",
  "LAIV 2–<12",
  "LAIV 2–<18",
  "LAIV 5–<12",
  "LAIV 5–<18"
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

# ============================================================
# Step 1: Define IQR transmission-model simulations
# using incremental symptomatic cases averted
# ============================================================

# Keep one copy of each transmission-model output.
# psa_results_full contains repeated economic PSA draws, so distinct()
# prevents the same transmission simulation being counted repeatedly.

model_disease_outputs <- psa_results_full %>%
  filter(
    scenario %in% central_plus_baseline
  ) %>%
  select(
    model_source,
    simulation_index,
    scenario,
    total_disease
  ) %>%
  distinct()

baseline_disease_outputs <- model_disease_outputs %>%
  filter(
    scenario == "Baseline"
  ) %>%
  select(
    model_source,
    simulation_index,
    baseline_total_disease = total_disease
  )

incremental_model_impact <- model_disease_outputs %>%
  filter(
    scenario %in% central_5
  ) %>%
  left_join(
    baseline_disease_outputs,
    by = c(
      "model_source",
      "simulation_index"
    )
  ) %>%
  mutate(
    disease_averted = baseline_total_disease - total_disease
  ) %>%
  group_by(
    model_source,
    simulation_index
  ) %>%
  summarise(
    mean_disease_averted = mean(
      disease_averted,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

central_model_sims_incremental_impact <- incremental_model_impact %>%
  group_by(
    model_source
  ) %>%
  mutate(
    q25 = quantile(
      mean_disease_averted,
      probs = 0.25,
      na.rm = TRUE
    ),
    q75 = quantile(
      mean_disease_averted,
      probs = 0.75,
      na.rm = TRUE
    ),
    keep_iqr = mean_disease_averted >= q25 &
      mean_disease_averted <= q75
  ) %>%
  filter(
    keep_iqr
  ) %>%
  ungroup() %>%
  select(
    model_source,
    simulation_index,
    mean_disease_averted,
    q25,
    q75
  )

# Optional diagnostic
central_model_sims_incremental_impact %>%
  count(
    model_source,
    name = "n_iqr_model_simulations"
  )

# ============================================================
# Step 2: Subset existing full PSA results to IQR model runs
# ============================================================

psa_results_iqr_impact <- psa_results_full %>%
  filter(
    scenario %in% central_plus_baseline
  ) %>%
  semi_join(
    central_model_sims_incremental_impact,
    by = c(
      "model_source",
      "simulation_index"
    )
  )

# Optional diagnostic
psa_results_iqr_impact %>%
  count(
    model_source,
    scenario,
    name = "n_rows"
  )

# ============================================================
# Step 3: Create incremental results versus Baseline
# ============================================================

baseline_iqr_impact <- psa_results_iqr_impact %>%
  filter(
    scenario == "Baseline"
  ) %>%
  select(
    model_source,
    simulation_index,
    psa_iter,
    base_cost = total_cost,
    base_qaly_loss = total_qaly_loss
  )

incremental_iqr_impact <- psa_results_iqr_impact %>%
  filter(
    scenario %in% central_5
  ) %>%
  left_join(
    baseline_iqr_impact,
    by = c(
      "model_source",
      "simulation_index",
      "psa_iter"
    )
  ) %>%
  mutate(
    inc_cost = total_cost - base_cost,
    inc_qalys = base_qaly_loss - total_qaly_loss,
    strategy_label = recode(
      scenario,
      !!!scenario_labels_central
    ),
    strategy_label = factor(
      strategy_label,
      levels = strategy_levels_pairwise
    ),
    model_source = factor(
      model_source,
      levels = c("UNSW", "UoM")
    )
  ) %>%
  filter(
    !is.na(inc_cost),
    !is.na(inc_qalys),
    !is.na(strategy_label)
  )

# ============================================================
# Step 4: Pairwise CEAC versus Baseline
# ============================================================

pairwise_ceac_iqr_impact <- crossing(
  incremental_iqr_impact,
  wtp = wtp_values
) %>%
  mutate(
    nmb = inc_qalys * wtp - inc_cost,
    cost_effective = nmb > 0
  ) %>%
  group_by(
    model_source,
    strategy_label,
    wtp
  ) %>%
  summarise(
    probability = mean(
      cost_effective,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# ============================================================
# Step 5: Competing-strategy CEAC
# ============================================================

competing_nmb_iqr_impact <- psa_results_iqr_impact %>%
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
  ) %>%
  crossing(
    wtp = wtp_values
  ) %>%
  mutate(
    nmb = -total_cost - total_qaly_loss * wtp
  )

competing_best_iqr_impact <- competing_nmb_iqr_impact %>%
  group_by(
    model_source,
    simulation_index,
    psa_iter,
    wtp
  ) %>%
  slice_max(
    order_by = nmb,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

competing_ceac_iqr_impact <- competing_best_iqr_impact %>%
  count(
    model_source,
    strategy_label,
    wtp,
    name = "n_best"
  ) %>%
  group_by(
    model_source,
    wtp
  ) %>%
  mutate(
    probability = n_best / sum(n_best)
  ) %>%
  ungroup() %>%
  complete(
    model_source,
    strategy_label = factor(
      strategy_levels_ceac,
      levels = strategy_levels_ceac
    ),
    wtp = wtp_values,
    fill = list(
      n_best = 0,
      probability = 0
    )
  ) %>%
  mutate(
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
# Step 6: Optional threshold summaries
# ============================================================

pairwise_ceac_iqr_impact %>%
  filter(
    wtp %in% c(wtp_low, wtp_high)
  ) %>%
  mutate(
    probability_percent = percent(
      probability,
      accuracy = 0.1
    )
  ) %>%
  arrange(
    wtp,
    model_source,
    strategy_label
  ) %>%
  print(n = Inf)

competing_ceac_iqr_impact %>%
  filter(
    wtp %in% c(wtp_low, wtp_high)
  ) %>%
  mutate(
    probability_percent = percent(
      probability,
      accuracy = 0.1
    )
  ) %>%
  arrange(
    wtp,
    model_source,
    strategy_label
  ) %>%
  print(n = Inf)

# ============================================================
# Step 7: Extract legend helper
# ============================================================

extract_legend <- function(plot) {
  
  plot_grob <- ggplotGrob(plot)
  
  guide_index <- which(
    grepl(
      "guide-box",
      plot_grob$layout$name
    )
  )
  
  if (length(guide_index) == 0) {
    stop("No legend found in plot.")
  }
  
  guide_classes <- sapply(
    plot_grob$grobs[guide_index],
    function(x) paste(
      class(x),
      collapse = " "
    )
  )
  
  real_guide_index <- guide_index[
    which(
      !grepl(
        "zeroGrob",
        guide_classes
      )
    )[1]
  ]
  
  if (is.na(real_guide_index)) {
    real_guide_index <- guide_index[1]
  }
  
  plot_grob$grobs[[real_guide_index]]
}

# ============================================================
# Step 8: Theme
# ============================================================

ACEFA_plot_theme_ceac_main <- theme_bw() +
  theme(
    legend.text = element_text(size = 9.2),
    legend.title = element_text(size = 9.2),
    
    axis.title = element_text(size = 10.5),
    axis.text = element_text(size = 9.2, colour = "black"),
    
    strip.text.x = element_text(
      size = 15,
      face = "bold",
      margin = margin(t = 0, r = 0, b = 10, l = 0)
    ),
    strip.text.y = element_blank(),
    strip.background = element_blank(),
    
    legend.position = "none",
    
    panel.border = element_blank(),
    axis.line = element_line(
      color = "black",
      linewidth = 0.7
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    panel.spacing.x = unit(1.15, "lines"),
    panel.spacing.y = unit(1.1, "lines"),
    
    plot.margin = margin(
      t = 8,
      r = 10,
      b = 8,
      l = 8
    )
  )

# ============================================================
# Step 9: Row labels
# ============================================================

pairwise_labels_iqr <- tibble(
  model_source = factor(
    c("UNSW", "UoM"),
    levels = c("UNSW", "UoM")
  ),
  label = c("A)", "B)"),
  x = 0,
  y = 1
)

competing_labels_iqr <- tibble(
  model_source = factor(
    c("UNSW", "UoM"),
    levels = c("UNSW", "UoM")
  ),
  label = c("C)", "D)"),
  x = 0,
  y = 1
)

# ============================================================
# Step 10: Pairwise CEAC plot
# ============================================================

p_pairwise_iqr_main <- ggplot(
  pairwise_ceac_iqr_impact,
  aes(
    x = wtp,
    y = probability,
    colour = strategy_label
  )
) +
  geom_vline(
    xintercept = wtp_low,
    linetype = "dotted",
    linewidth = 0.55,
    colour = "black"
  ) +
  geom_vline(
    xintercept = wtp_high,
    linetype = "solid",
    linewidth = 0.55,
    colour = "black"
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_text(
    data = pairwise_labels_iqr,
    aes(
      x = x,
      y = y,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = -0.25,
    vjust = -0.35,
    size = 4.2
  ) +
  facet_grid(
    . ~ model_source
  ) +
  scale_colour_manual(
    name = "Strategy",
    values = scenario_palette_ceac,
    breaks = strategy_levels_pairwise,
    limits = strategy_levels_pairwise,
    drop = FALSE
  ) +
  scale_x_continuous(
    labels = label_dollar(
      scale = 1 / 1000,
      suffix = "k",
      prefix = "$"
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
      mult = c(0.02, 0.03)
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
      mult = c(0.02, 0.04)
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    x = NULL,
    y = "Probability of cost-effectiveness"
  ) +
  ACEFA_plot_theme_ceac_main

# ============================================================
# Step 11: Competing CEAC plot
# ============================================================

p_competing_iqr_main <- ggplot(
  competing_ceac_iqr_impact,
  aes(
    x = wtp,
    y = probability,
    colour = strategy_label
  )
) +
  geom_vline(
    xintercept = wtp_low,
    linetype = "dotted",
    linewidth = 0.55,
    colour = "black"
  ) +
  geom_vline(
    xintercept = wtp_high,
    linetype = "solid",
    linewidth = 0.55,
    colour = "black"
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_text(
    data = competing_labels_iqr,
    aes(
      x = x,
      y = y,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = -0.25,
    vjust = -0.35,
    size = 4.2
  ) +
  facet_grid(
    . ~ model_source
  ) +
  scale_colour_manual(
    name = "Strategy",
    values = scenario_palette_ceac,
    breaks = strategy_levels_ceac,
    limits = strategy_levels_ceac,
    drop = FALSE
  ) +
  scale_x_continuous(
    labels = label_dollar(
      scale = 1 / 1000,
      suffix = "k",
      prefix = "$"
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
      mult = c(0.02, 0.03)
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
      mult = c(0.02, 0.04)
    )
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  labs(
    x = "Willingness-to-pay threshold per QALY gained",
    y = "Probability of cost-effectiveness"
  ) +
  ACEFA_plot_theme_ceac_main +
  theme(
    strip.text.x = element_blank()
  )

# ============================================================
# Step 12: Legend
# ============================================================

legend_source_iqr <- ggplot(
  competing_ceac_iqr_impact,
  aes(
    x = wtp,
    y = probability,
    colour = strategy_label
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  scale_colour_manual(
    name = "Strategy",
    values = scenario_palette_ceac,
    breaks = strategy_levels_ceac,
    limits = strategy_levels_ceac,
    drop = FALSE
  ) +
  guides(
    colour = guide_legend(
      ncol = 1,
      byrow = TRUE,
      override.aes = list(
        linewidth = 0.9
      )
    )
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.key.width = unit(0.55, "cm"),
    legend.key.height = unit(0.38, "cm"),
    legend.spacing.y = unit(0.08, "cm"),
    legend.text = element_text(size = 9.2),
    legend.title = element_text(size = 9.2)
  )

legend_grob_iqr <- extract_legend(
  legend_source_iqr
)

legend_only_iqr <- wrap_elements(
  full = legend_grob_iqr
)

legend_column_iqr <- (
  plot_spacer() /
    legend_only_iqr /
    plot_spacer()
) +
  plot_layout(
    heights = c(
      1,
      0.9,
      1
    )
  )

# ============================================================
# Step 13: Combine figure
# ============================================================

ceac_4panel_iqr_main <- (
  (
    p_pairwise_iqr_main /
      p_competing_iqr_main
  ) |
    legend_column_iqr
) +
  plot_layout(
    widths = c(
      1,
      0.17
    ),
    heights = c(
      1,
      1
    )
  ) &
  theme(
    plot.margin = margin(
      t = 12,
      r = 12,
      b = 8,
      l = 34
    )
  )

ceac_4panel_iqr_main

# ============================================================
# Step 14: Save figure
# ============================================================

ggsave(
  filename = file.path(
    output_dir,
    "ceac_4panel_iqr_incremental_transmission_model_main_15k_dotted_50k_solid_no_15k_axis_label.png"
  ),
  plot = ceac_4panel_iqr_main,
  width = 8.27,
  height = 6.8,
  units = "in",
  dpi = 600,
  bg = "white"
)




# ============================================================
