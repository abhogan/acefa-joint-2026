# Original manuscript CEAC layout, provided for visual parity.
# Numeric results derived from the same PSA objects.
# Requires run_analysis.R objects; NOT run automatically.

# Main manuscript-style figure:
# 4-panel CEAC using FULL model simulations
#
# Memory-safe version:
# - avoids large crossing() objects
# - calculates each WTP threshold one at a time
#
# Uses existing psa_results_full
# Does NOT rerun the PSA
#
# Row 1 = Pairwise CEAC versus Baseline
# Row 2 = Competing-strategy CEAC
# Columns = UNSW and UoM
#
# Includes WTP threshold lines:
# - $15,000/QALY gained = dotted vertical line
# - $50,000/QALY gained = solid vertical line
#
# $15k line is included but not shown as an x-axis tick
#
# Single Strategy legend placed middle-right
# Panels labelled A) to D)
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)
library(grid)
library(data.table)

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
# Step 1: Subset full PSA results
# ============================================================

psa_results_full_central <- psa_results_full %>%
  filter(
    scenario %in% central_plus_baseline
  ) %>%
  select(
    model_source,
    simulation_index,
    psa_iter,
    scenario,
    total_cost,
    total_qaly_loss
  )

# Optional diagnostic
psa_results_full_central %>%
  count(
    model_source,
    scenario,
    name = "n_rows"
  )

# ============================================================
# Step 2: Create incremental results versus Baseline
# ============================================================

baseline_full <- psa_results_full_central %>%
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

incremental_full <- psa_results_full_central %>%
  filter(
    scenario %in% central_5
  ) %>%
  left_join(
    baseline_full,
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
  ) %>%
  select(
    model_source,
    simulation_index,
    psa_iter,
    strategy_label,
    inc_cost,
    inc_qalys
  )

# ============================================================
# Step 3: Pairwise CEAC versus Baseline
# Memory-safe: no crossing()
# ============================================================

incremental_full_dt <- as.data.table(
  incremental_full %>%
    mutate(
      model_source = as.character(model_source),
      strategy_label = as.character(strategy_label)
    )
)

pairwise_ceac_full <- rbindlist(
  lapply(
    wtp_values,
    function(wtp_i) {
      
      incremental_full_dt[
        ,
        .(
          probability = mean(
            inc_qalys * wtp_i - inc_cost > 0,
            na.rm = TRUE
          )
        ),
        by = .(
          model_source,
          strategy_label
        )
      ][
        ,
        wtp := wtp_i
      ]
    }
  ),
  fill = TRUE
) %>%
  as_tibble() %>%
  mutate(
    strategy_label = factor(
      strategy_label,
      levels = strategy_levels_pairwise
    ),
    model_source = factor(
      model_source,
      levels = c("UNSW", "UoM")
    )
  )

rm(incremental_full_dt)
gc()

# ============================================================
# Step 4: Competing-strategy CEAC
# Memory-safe: no crossing()
# ============================================================

competing_input_full_dt <- as.data.table(
  psa_results_full_central %>%
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
    select(
      model_source,
      simulation_index,
      psa_iter,
      scenario,
      strategy_label,
      total_cost,
      total_qaly_loss
    ) %>%
    mutate(
      model_source = as.character(model_source),
      strategy_label = as.character(strategy_label)
    )
)

competing_ceac_full <- rbindlist(
  lapply(
    wtp_values,
    function(wtp_i) {
      
      competing_input_full_dt[
        ,
        nmb := -total_cost - total_qaly_loss * wtp_i
      ]
      
      setorder(
        competing_input_full_dt,
        model_source,
        simulation_index,
        psa_iter,
        -nmb
      )
      
      best_temp <- competing_input_full_dt[
        !duplicated(
          competing_input_full_dt,
          by = c(
            "model_source",
            "simulation_index",
            "psa_iter"
          )
        )
      ]
      
      best_temp[
        ,
        .N,
        by = .(
          model_source,
          strategy_label
        )
      ][
        ,
        probability := N / sum(N),
        by = model_source
      ][
        ,
        wtp := wtp_i
      ]
    }
  ),
  fill = TRUE
) %>%
  as_tibble() %>%
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
      N = 0,
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

rm(competing_input_full_dt)
gc()

# ============================================================
# Step 5: Optional threshold summaries
# ============================================================

pairwise_ceac_full %>%
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

competing_ceac_full %>%
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
# Step 6: Extract legend helper
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
# Step 7: Theme
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
# Step 8: Row labels
# ============================================================

pairwise_labels_full <- tibble(
  model_source = factor(
    c("UNSW", "UoM"),
    levels = c("UNSW", "UoM")
  ),
  label = c("A)", "B)"),
  x = 0,
  y = 1
)

competing_labels_full <- tibble(
  model_source = factor(
    c("UNSW", "UoM"),
    levels = c("UNSW", "UoM")
  ),
  label = c("C)", "D)"),
  x = 0,
  y = 1
)

# ============================================================
# Step 9: Pairwise CEAC plot
# ============================================================

p_pairwise_full_main <- ggplot(
  pairwise_ceac_full,
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
    data = pairwise_labels_full,
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
# Step 10: Competing CEAC plot
# ============================================================

p_competing_full_main <- ggplot(
  competing_ceac_full,
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
    data = competing_labels_full,
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
# Step 11: Legend
# ============================================================

legend_source_full <- ggplot(
  competing_ceac_full,
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

legend_grob_full <- extract_legend(
  legend_source_full
)

legend_only_full <- wrap_elements(
  full = legend_grob_full
)

legend_column_full <- (
  plot_spacer() /
    legend_only_full /
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
# Step 12: Combine figure
# ============================================================

ceac_4panel_full_main <- (
  (
    p_pairwise_full_main /
      p_competing_full_main
  ) |
    legend_column_full
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

ceac_4panel_full_main

# ============================================================
# Step 13: Save figure
# ============================================================

ggsave(
  filename = file.path(
    output_dir,
    "ceac_4panel_full_model_main_memory_safe_15k_dotted_50k_solid_no_15k_axis_label.png"
  ),
  plot = ceac_4panel_full_main,
  width = 8.27,
  height = 6.8,
  units = "in",
  dpi = 600,
  bg = "white"
)






