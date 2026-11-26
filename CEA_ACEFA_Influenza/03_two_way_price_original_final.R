# Optional figure code preserved from the final source block (not independently executed here).
# Requires run_analysis.R to have created the analysis objects.
# Outputs remain git-ignored until approved.

# Two-way sensitivity analysis:
# LAIV vaccine price and IIV vaccine price
#
# Four panels:
# A) LAIV 5–<12, UNSW
# B) LAIV 5–<12, UoM
# C) LAIV 5–<18, UNSW
# D) LAIV 5–<18, UoM
#
# Outcome = ICER versus Baseline
#
# ICER categories:
# - $0–<$15k
# - $15k–<$25k
# - $25k–$50k
# - >$50k
#
# Updated assumptions:
# - LAIV base case = $25
# - LAIV range shown = $10 to $50
# - IIV base case = $10
# - IIV range = $5 to $35
#
# Price grid increment = $0.05
#
# Uses full model simulations summarised to mean costs and QALYs.
# Does NOT rerun the PSA.
#
# Uses geom_raster() to avoid visible tile/gap lines.
# Includes base-case dot legend.
# Panel labels A) to D) are bold and placed above panels.
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(grid)

# ------------------------------------------------------------
# Check required objects exist
# ------------------------------------------------------------

if (!exists("df_cea")) {
  stop("df_cea is not in your R environment.")
}

if (!exists("base_pars")) {
  stop("base_pars is not in your R environment.")
}

if (!exists("run_cea_once")) {
  stop("run_cea_once() is not in your R environment.")
}

required_df_vars <- c(
  "model_source",
  "simulation_index",
  "scenario",
  "age_group_cea",
  "method",
  "doses"
)

missing_df_vars <- setdiff(
  required_df_vars,
  names(df_cea)
)

if (length(missing_df_vars) > 0) {
  stop(
    paste(
      "df_cea is missing:",
      paste(missing_df_vars, collapse = ", ")
    )
  )
}

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

output_dir <- file.path("outputs", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

base_pars_two_way <- base_pars

base_pars_two_way$cost_laiv_vaccine <- 25
base_pars_two_way$cost_im_vaccine <- 10

base_laiv_price <- base_pars_two_way$cost_laiv_vaccine
base_iiv_price <- base_pars_two_way$cost_im_vaccine

laiv_price_min <- 10
laiv_price_max <- 50

iiv_price_min <- 5
iiv_price_max <- 35

price_increment <- 0.05

laiv_price_values <- seq(
  from = laiv_price_min,
  to = laiv_price_max,
  by = price_increment
)

iiv_price_values <- seq(
  from = iiv_price_min,
  to = iiv_price_max,
  by = price_increment
)

target_strategies <- c(
  "Central_LAIV_5-12yo",
  "Central_LAIV_5-18yo"
)

central_plus_baseline <- c(
  "Baseline",
  target_strategies
)

strategy_levels <- c(
  "LAIV 5–<12",
  "LAIV 5–<18"
)

strategy_labels <- c(
  "Central_LAIV_5-12yo" = "LAIV 5–<12",
  "Central_LAIV_5-18yo" = "LAIV 5–<18"
)

icer_category_levels <- c(
  "$0–<$15k",
  "$15k–<$25k",
  "$25k–$50k",
  ">$50k"
)

# Pink → purple → navy palette
# light to dark = lower to higher ICER
icer_category_palette <- c(
  "$0–<$15k"   = "#F7D9E8",
  "$15k–<$25k" = "#D890BD",
  "$25k–$50k"  = "#8E5EA2",
  ">$50k"      = "#262261"
)

# ============================================================
# Step 1: Run base deterministic model once
# ============================================================

base_results_two_way <- run_cea_once(
  data = df_cea,
  pars = base_pars_two_way
) %>%
  filter(
    scenario %in% central_plus_baseline
  )

# ============================================================
# Step 2: Calculate LAIV and IIV doses by model simulation
# ============================================================

dose_components_two_way <- df_cea %>%
  filter(
    scenario %in% central_plus_baseline
  ) %>%
  mutate(
    method_clean = toupper(
      as.character(method)
    ),
    doses = replace_na(
      as.numeric(doses),
      0
    )
  ) %>%
  distinct(
    model_source,
    simulation_index,
    scenario,
    age_group_cea,
    method_clean,
    doses
  ) %>%
  group_by(
    model_source,
    simulation_index,
    scenario
  ) %>%
  summarise(
    laiv_doses = sum(
      if_else(
        method_clean == "LAIV",
        doses,
        0
      ),
      na.rm = TRUE
    ),
    iiv_doses = sum(
      if_else(
        method_clean %in% c("IM", "IIV"),
        doses,
        0
      ),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# ============================================================
# Step 3: Remove vaccine product costs from base total cost
# ============================================================

cost_components_two_way <- base_results_two_way %>%
  left_join(
    dose_components_two_way,
    by = c(
      "model_source",
      "simulation_index",
      "scenario"
    )
  ) %>%
  mutate(
    laiv_doses = replace_na(
      laiv_doses,
      0
    ),
    iiv_doses = replace_na(
      iiv_doses,
      0
    ),
    fixed_cost_excluding_vaccine_products =
      total_cost -
      laiv_doses * base_laiv_price -
      iiv_doses * base_iiv_price
  )

# ============================================================
# Step 4: Summarise across full model simulations
# ============================================================

mean_cost_components_two_way <- cost_components_two_way %>%
  group_by(
    model_source,
    scenario
  ) %>%
  summarise(
    fixed_cost_excluding_vaccine_products = mean(
      fixed_cost_excluding_vaccine_products,
      na.rm = TRUE
    ),
    total_qaly_loss = mean(
      total_qaly_loss,
      na.rm = TRUE
    ),
    laiv_doses = mean(
      laiv_doses,
      na.rm = TRUE
    ),
    iiv_doses = mean(
      iiv_doses,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

baseline_components_two_way <- mean_cost_components_two_way %>%
  filter(
    scenario == "Baseline"
  ) %>%
  select(
    model_source,
    base_fixed_cost_excluding_vaccine_products =
      fixed_cost_excluding_vaccine_products,
    base_total_qaly_loss = total_qaly_loss,
    base_laiv_doses = laiv_doses,
    base_iiv_doses = iiv_doses
  )

# ============================================================
# Step 5: Create two-way price grid
# ============================================================

price_grid_two_way <- expand_grid(
  laiv_price = laiv_price_values,
  iiv_price = iiv_price_values
)

two_way_price_results_icer <- mean_cost_components_two_way %>%
  filter(
    scenario %in% target_strategies
  ) %>%
  left_join(
    baseline_components_two_way,
    by = "model_source"
  ) %>%
  crossing(
    price_grid_two_way
  ) %>%
  mutate(
    strategy_label = recode(
      scenario,
      !!!strategy_labels
    ),
    strategy_label = factor(
      strategy_label,
      levels = strategy_levels
    ),
    model_source = factor(
      model_source,
      levels = c("UNSW", "UoM")
    ),
    
    total_cost_at_price =
      fixed_cost_excluding_vaccine_products +
      laiv_doses * laiv_price +
      iiv_doses * iiv_price,
    
    baseline_cost_at_price =
      base_fixed_cost_excluding_vaccine_products +
      base_laiv_doses * laiv_price +
      base_iiv_doses * iiv_price,
    
    inc_cost = total_cost_at_price - baseline_cost_at_price,
    inc_qalys = base_total_qaly_loss - total_qaly_loss,
    
    icer = ifelse(
      inc_qalys > 0,
      inc_cost / inc_qalys,
      NA_real_
    ),
    
    icer_category = case_when(
      !is.finite(icer) ~ NA_character_,
      icer < 15000 ~ "$0–<$15k",
      icer >= 15000 & icer < 25000 ~ "$15k–<$25k",
      icer >= 25000 & icer <= 50000 ~ "$25k–$50k",
      icer > 50000 ~ ">$50k"
    ),
    icer_category = factor(
      icer_category,
      levels = icer_category_levels
    )
  ) %>%
  filter(
    !is.na(icer_category)
  )

# ============================================================
# Step 6: Base-case points
# ============================================================

base_case_points_two_way <- expand_grid(
  model_source = factor(
    c("UNSW", "UoM"),
    levels = c("UNSW", "UoM")
  ),
  strategy_label = factor(
    strategy_levels,
    levels = strategy_levels
  )
) %>%
  mutate(
    laiv_price = base_laiv_price,
    iiv_price = base_iiv_price
  )

# ============================================================
# Step 7: Panel labels
# Bold A) to D), positioned above each panel
# ============================================================

panel_labels_two_way <- expand_grid(
  strategy_label = factor(
    strategy_levels,
    levels = strategy_levels
  ),
  model_source = factor(
    c("UNSW", "UoM"),
    levels = c("UNSW", "UoM")
  )
) %>%
  arrange(
    strategy_label,
    model_source
  ) %>%
  mutate(
    label = paste0(
      LETTERS[row_number()],
      ")"
    ),
    laiv_price = laiv_price_min,
    iiv_price = iiv_price_max + 1.2
  )

# ============================================================
# Step 8: Theme
# ============================================================

ACEFA_plot_theme_two_way_icer <- theme_bw() +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, colour = "black"),
    
    strip.text.x = element_text(
      size = 15,
      face = "bold",
      margin = margin(t = 0, r = 0, b = 8, l = 0)
    ),
    strip.text.y.right = element_text(
      size = 12,
      face = "bold",
      margin = margin(t = 0, r = 10, b = 0, l = 14)
    ),
    strip.background = element_blank(),
    
    legend.position = "right",
    
    panel.border = element_blank(),
    axis.line = element_line(
      color = "black",
      linewidth = 0.7
    ),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.spacing.x = unit(1.2, "lines"),
    panel.spacing.y = unit(1.8, "lines"),
    
    plot.margin = margin(
      t = 18,
      r = 12,
      b = 8,
      l = 8
    )
  )

# ============================================================
# Step 9: Plot
# ============================================================

two_way_price_icer_4panel <- ggplot(
  two_way_price_results_icer,
  aes(
    x = laiv_price,
    y = iiv_price,
    fill = icer_category
  )
) +
  geom_raster(
    interpolate = FALSE
  ) +
  geom_contour(
    aes(
      z = icer
    ),
    breaks = 15000,
    colour = "black",
    linewidth = 0.45,
    linetype = "dashed"
  ) +
  geom_contour(
    aes(
      z = icer
    ),
    breaks = 50000,
    colour = "black",
    linewidth = 0.45,
    linetype = "solid"
  ) +
  geom_point(
    data = base_case_points_two_way,
    aes(
      x = laiv_price,
      y = iiv_price,
      shape = "Base case"
    ),
    inherit.aes = FALSE,
    fill = "white",
    colour = "black",
    size = 2.6,
    stroke = 0.9
  ) +
  geom_text(
    data = panel_labels_two_way,
    aes(
      x = laiv_price,
      y = iiv_price,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = -0.05,
    vjust = 0,
    size = 4.4,
    fontface = "bold"
  ) +
  facet_grid(
    strategy_label ~ model_source
  ) +
  scale_fill_manual(
    name = "ICER",
    values = icer_category_palette,
    breaks = icer_category_levels,
    limits = icer_category_levels,
    drop = FALSE
  ) +
  scale_shape_manual(
    name = NULL,
    values = c(
      "Base case" = 21
    )
  ) +
  scale_x_continuous(
    labels = label_dollar(
      accuracy = 1,
      prefix = "$"
    ),
    breaks = c(
      10,
      20,
      30,
      40,
      50
    ),
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
  scale_y_continuous(
    labels = label_dollar(
      accuracy = 1,
      prefix = "$"
    ),
    breaks = c(
      5,
      15,
      25,
      35
    ),
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
  coord_cartesian(
    xlim = c(
      laiv_price_min,
      laiv_price_max
    ),
    ylim = c(
      iiv_price_min,
      iiv_price_max
    ),
    clip = "off"
  ) +
  labs(
    x = "LAIV vaccine price per dose",
    y = "IIV vaccine price per dose"
  ) +
  ACEFA_plot_theme_two_way_icer +
  guides(
    fill = guide_legend(
      order = 1
    ),
    shape = guide_legend(
      order = 2,
      override.aes = list(
        fill = "white",
        colour = "black",
        size = 2.8,
        stroke = 0.9
      )
    )
  )

two_way_price_icer_4panel

# ============================================================
# Step 10: Save as PNG
# ============================================================

ggsave(
  filename = file.path(
    output_dir,
    "two_way_sensitivity_laiv_iiv_price_icer_4panel_pink_purple_navy_005_increment_labels_above_basecase.png"
  ),
  plot = two_way_price_icer_4panel,
  width = 8.27,
  height = 6.8,
  units = "in",
  dpi = 600,
  bg = "white"
)
