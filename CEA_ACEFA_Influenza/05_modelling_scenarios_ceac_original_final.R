# Optional source-derived analysis. Requires prior core objects.
# Original numerical analysis preserved, paths made portable.

# CEAC figure:
# Pessimistic, Pessimistic-Central, Central, and Optimistic
#
# Pairwise versus Baseline only
#
# Panels:
# Rows = LAIV age group strategy
# Columns = UNSW and UoM
#
# Lines = scenario assumption:
# - Pessimistic
# - Pessimistic-Central
# - Central
# - Optimistic
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
# - psa_results_mean_epi by default
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
# Source object
# ------------------------------------------------------------

ceac_source_data <- psa_results_mean_epi

if (!exists("ceac_source_data")) {
  stop("ceac_source_data is not in your R environment.")
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

# ------------------------------------------------------------
# Scenario mapping
# Includes common possible names for Pessimistic-Central
# ------------------------------------------------------------

scenario_map <- tibble(
  scenario = c(
    "Pessimistic_LAIV_5-12yo",
    "Pessimistic_LAIV_5-18yo",
    
    "CentralLow_LAIV_5-12yo",
    "CentralLow_LAIV_5-18yo",
    "Central_Low_LAIV_5-12yo",
    "Central_Low_LAIV_5-18yo",
    "PessimisticCentral_LAIV_5-12yo",
    "PessimisticCentral_LAIV_5-18yo",
    "Pessimistic_Central_LAIV_5-12yo",
    "Pessimistic_Central_LAIV_5-18yo",
    
    "Central_LAIV_5-12yo",
    "Central_LAIV_5-18yo",
    
    "Optimistic_LAIV_5-12yo",
    "Optimistic_LAIV_5-18yo"
  ),
  scenario_assumption = c(
    "Pessimistic",
    "Pessimistic",
    
    "Pessimistic-Central",
    "Pessimistic-Central",
    "Pessimistic-Central",
    "Pessimistic-Central",
    "Pessimistic-Central",
    "Pessimistic-Central",
    "Pessimistic-Central",
    "Pessimistic-Central",
    
    "Central",
    "Central",
    
    "Optimistic",
    "Optimistic"
  ),
  age_group_strategy = c(
    "LAIV 5–<12",
    "LAIV 5–<18",
    
    "LAIV 5–<12",
    "LAIV 5–<18",
    "LAIV 5–<12",
    "LAIV 5–<18",
    "LAIV 5–<12",
    "LAIV 5–<18",
    "LAIV 5–<12",
    "LAIV 5–<18",
    
    "LAIV 5–<12",
    "LAIV 5–<18",
    
    "LAIV 5–<12",
    "LAIV 5–<18"
  )
)

scenario_assumption_levels <- c(
  "Pessimistic",
  "Pessimistic-Central",
  "Central",
  "Optimistic"
)

age_group_strategy_levels <- c(
  "LAIV 5–<12",
  "LAIV 5–<18"
)

# Publication palette for the four assumption scenarios.
# The original combined script defined this under a conflicting name
# in an earlier block; explicitly declare it here to make this standalone.
scenario_palette_ceac <- c(
  "Pessimistic" = "#08306B",
  "Pessimistic-Central" = "#56B4E9",
  "Central" = "#009E73",
  "Optimistic" = "#D55E00"
)

scenario_palette_modelling <- c(
  "Pessimistic" = "#262261",
  "Low central" = "#0072B2",
  "Central"     = "#009E73",
  "Optimistic"  = "#D55E00"
)

analysis_scenarios <- scenario_map %>%
  filter(
    scenario %in% unique(ceac_source_data$scenario)
  )

cat("\nScenarios used:\n")
print(analysis_scenarios)

missing_scenarios <- scenario_map %>%
  filter(
    !scenario %in% unique(ceac_source_data$scenario)
  )

cat("\nMapped scenarios not found in the data:\n")
print(missing_scenarios)

if (!"Baseline" %in% unique(ceac_source_data$scenario)) {
  stop("Baseline was not found in the source CEAC data.")
}

if (nrow(analysis_scenarios) == 0) {
  stop("None of the mapped Pessimistic/Pessimistic-Central/Central/Optimistic scenarios were found.")
}

join_vars <- c(
  "model_source",
  "psa_iter"
)

if ("simulation_index" %in% names(ceac_source_data)) {
  join_vars <- c(
    "model_source",
    "simulation_index",
    "psa_iter"
  )
}

# ============================================================
# Step 1: Prepare Baseline results
# ============================================================

baseline_ceac <- ceac_source_data %>%
  filter(
    scenario == "Baseline"
  ) %>%
  select(
    all_of(join_vars),
    base_cost = total_cost,
    base_qaly_loss = total_qaly_loss
  )

# ============================================================
# Step 2: Prepare pairwise input versus Baseline
# ============================================================

pairwise_input_scenarios <- ceac_source_data %>%
  filter(
    scenario %in% analysis_scenarios$scenario
  ) %>%
  left_join(
    scenario_map,
    by = "scenario"
  ) %>%
  left_join(
    baseline_ceac,
    by = join_vars
  ) %>%
  mutate(
    inc_cost = total_cost - base_cost,
    inc_qalys = base_qaly_loss - total_qaly_loss,
    scenario_assumption = factor(
      scenario_assumption,
      levels = scenario_assumption_levels
    ),
    age_group_strategy = factor(
      age_group_strategy,
      levels = age_group_strategy_levels
    ),
    model_source = factor(
      model_source,
      levels = c("UNSW", "UoM")
    )
  ) %>%
  filter(
    !is.na(inc_cost),
    !is.na(inc_qalys),
    !is.na(scenario_assumption),
    !is.na(age_group_strategy),
    !is.na(model_source)
  ) %>%
  select(
    all_of(join_vars),
    scenario,
    scenario_assumption,
    age_group_strategy,
    inc_cost,
    inc_qalys
  )

# ============================================================
# Step 3: Pairwise CEAC versus Baseline
# ============================================================

ceac_pairwise_pess_to_opt <- map_dfr(
  wtp_values,
  function(wtp_i) {
    
    pairwise_input_scenarios %>%
      mutate(
        cost_effective = inc_qalys * wtp_i - inc_cost > 0
      ) %>%
      group_by(
        model_source,
        age_group_strategy,
        scenario_assumption
      ) %>%
      summarise(
        probability = mean(
          cost_effective,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      mutate(
        wtp = wtp_i
      )
  }
) %>%
  mutate(
    scenario_assumption = factor(
      scenario_assumption,
      levels = scenario_assumption_levels
    ),
    age_group_strategy = factor(
      age_group_strategy,
      levels = age_group_strategy_levels
    ),
    model_source = factor(
      model_source,
      levels = c("UNSW", "UoM")
    )
  )

# ============================================================
# Step 4: Theme
# ============================================================

ACEFA_plot_theme_ceac_pairwise <- theme_bw() +
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
    strip.text.y = element_text(
      size = 12,
      face = "bold",
      margin = margin(t = 0, r = 10, b = 0, l = 0)
    ),
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
# Step 5: Plot
# ============================================================

ceac_pairwise_pessimistic_to_optimistic_by_age_group <- ggplot(
  ceac_pairwise_pess_to_opt,
  aes(
    x = wtp,
    y = probability,
    colour = scenario_assumption
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
    age_group_strategy ~ model_source
  ) +
  scale_colour_manual(
    name = "Scenario",
    values = scenario_palette_ceac,
    breaks = scenario_assumption_levels,
    limits = scenario_assumption_levels,
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
  ACEFA_plot_theme_ceac_pairwise +
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

ceac_pairwise_pessimistic_to_optimistic_by_age_group

# ============================================================
# Step 6: Save figure
# ============================================================

ggsave(
  filename = file.path(
    output_dir,
    "ceac_pairwise_vs_baseline_pessimistic_pesscentral_central_optimistic_by_age_group_15k_dotted_50k_solid.png"
  ),
  plot = ceac_pairwise_pessimistic_to_optimistic_by_age_group,
  width = 8.27,
  height = 6.6,
  units = "in",
  dpi = 600,
  bg = "white"
)
