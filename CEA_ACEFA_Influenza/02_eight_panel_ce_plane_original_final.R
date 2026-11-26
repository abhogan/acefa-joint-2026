# Optional figure code preserved from the final source block (not independently executed here).
# Requires run_analysis.R to have created the analysis objects.
# Outputs remain git-ignored until approved.

# 8-panel CE plane scatterplot
#
# Closer to the original figure:
# - uses ALL CE-plane points, not sampled points
# - lower alpha / small points
# - mean points calculated from all rows
#
# Keeps newer axis settings:
# - y-axis plotting range = -$1,300k to $750k
# - y-axis labelled range = -$1,000k to $500k
#
# Keeps legends on right-hand side
# with all legend boxes left-aligned
#
# WTP threshold lines:
# - $15,000/QALY gained = dashed line
# - $50,000/QALY gained = solid line
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(grid)

# ------------------------------------------------------------
# Clear old plot objects
# ------------------------------------------------------------

rm(
  list = intersect(
    c(
      "ce_plane_8panel",
      "ce_plane_8panel_plot",
      "ce_plane_eco_only",
      "ce_plane_iqr",
      "ce_plane_full",
      "ce_plane_quasi",
      "mean_points_8panel",
      "plot_points_8panel",
      "quasi_results_mean_econ"
    ),
    ls()
  )
)

gc()

# ------------------------------------------------------------
# Check required objects exist
# ------------------------------------------------------------

if (!exists("psa_results_mean_epi")) stop("psa_results_mean_epi is not in your R environment.")
if (!exists("psa_results_iqr_impact")) stop("psa_results_iqr_impact is not in your R environment.")
if (!exists("psa_results_full")) stop("psa_results_full is not in your R environment.")
if (!exists("df_cea")) stop("df_cea is not in your R environment.")
if (!exists("base_pars")) stop("base_pars is not in your R environment.")
if (!exists("run_cea_once")) stop("run_cea_once() is not in your R environment.")

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

output_dir <- file.path("outputs", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

wtp_line_data <- tibble(
  threshold_label = factor(
    c("$15,000/QALY", "$50,000/QALY"),
    levels = c("$15,000/QALY", "$50,000/QALY")
  ),
  threshold_value = c(
    15000,
    50000
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

strategy_levels_ceplane <- c(
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

scenario_palette_ceplane <- c(
  "LAIV 2–<5"  = "#B2182B",
  "LAIV 2–<12" = "#eb008b",
  "LAIV 2–<18" = "#0072B2",
  "LAIV 5–<12" = "#009E73",
  "LAIV 5–<18" = "#D55E00"
)

strategy_shapes_ceplane <- c(
  "LAIV 2–<5"  = 21,
  "LAIV 2–<12" = 22,
  "LAIV 2–<18" = 23,
  "LAIV 5–<12" = 24,
  "LAIV 5–<18" = 25
)

analysis_levels <- c(
  "Economic-only uncertainty",
  "IQR transmission-model uncertainty",
  "Full model simulations",
  "Quasi PSA: mean economic inputs"
)

y_limits_common <- c(
  -1300000,
  750000
)

y_breaks_common <- c(
  -1000000,
  -500000,
  0,
  500000
)

# ============================================================
# Helper function
# ============================================================

make_ceplane_component_all_points <- function(
    data,
    analysis_label_i,
    join_vars
) {
  
  data_small <- data %>%
    filter(
      scenario %in% central_plus_baseline
    ) %>%
    select(
      any_of(c(
        "model_source",
        "simulation_index",
        "psa_iter",
        "scenario",
        "total_cost",
        "total_qaly_loss"
      ))
    )
  
  baseline_small <- data_small %>%
    filter(
      scenario == "Baseline"
    ) %>%
    select(
      all_of(join_vars),
      base_cost = total_cost,
      base_qaly_loss = total_qaly_loss
    )
  
  inc_data <- data_small %>%
    filter(
      scenario %in% central_5
    ) %>%
    left_join(
      baseline_small,
      by = join_vars
    ) %>%
    mutate(
      inc_cost = total_cost - base_cost,
      inc_qalys = base_qaly_loss - total_qaly_loss,
      analysis_label = analysis_label_i,
      strategy_label = recode(
        scenario,
        !!!scenario_labels_central
      ),
      strategy_label = factor(
        strategy_label,
        levels = strategy_levels_ceplane
      ),
      model_source = factor(
        model_source,
        levels = c("UNSW", "UoM")
      ),
      analysis_label = factor(
        analysis_label,
        levels = analysis_levels
      )
    ) %>%
    filter(
      !is.na(inc_cost),
      !is.na(inc_qalys),
      !is.na(strategy_label),
      !is.na(model_source),
      !is.na(analysis_label)
    ) %>%
    select(
      model_source,
      scenario,
      strategy_label,
      inc_cost,
      inc_qalys,
      analysis_label
    )
  
  inc_data
}

# ============================================================
# Step 1: Economic-only uncertainty
# ============================================================

ce_plane_eco_only <- make_ceplane_component_all_points(
  data = psa_results_mean_epi,
  analysis_label_i = "Economic-only uncertainty",
  join_vars = c(
    "model_source",
    "psa_iter"
  )
)

# ============================================================
# Step 2: IQR transmission-model uncertainty
# ============================================================

ce_plane_iqr <- make_ceplane_component_all_points(
  data = psa_results_iqr_impact,
  analysis_label_i = "IQR transmission-model uncertainty",
  join_vars = c(
    "model_source",
    "simulation_index",
    "psa_iter"
  )
)

# ============================================================
# Step 3: Full model simulations
# ============================================================

ce_plane_full <- make_ceplane_component_all_points(
  data = psa_results_full,
  analysis_label_i = "Full model simulations",
  join_vars = c(
    "model_source",
    "simulation_index",
    "psa_iter"
  )
)

# ============================================================
# Step 4: Quasi PSA using mean economic inputs
# ============================================================

quasi_results_mean_econ <- run_cea_once(
  data = df_cea,
  pars = base_pars
) %>%
  filter(
    scenario %in% central_plus_baseline
  )

ce_plane_quasi <- make_ceplane_component_all_points(
  data = quasi_results_mean_econ,
  analysis_label_i = "Quasi PSA: mean economic inputs",
  join_vars = c(
    "model_source",
    "simulation_index"
  )
)

rm(quasi_results_mean_econ)

gc()

# ============================================================
# Step 5: Combine all CE-plane points
# ============================================================

ce_plane_8panel <- bind_rows(
  ce_plane_eco_only,
  ce_plane_iqr,
  ce_plane_full,
  ce_plane_quasi
)

rm(
  ce_plane_eco_only,
  ce_plane_iqr,
  ce_plane_full,
  ce_plane_quasi
)

gc()

# ============================================================
# Step 6: Mean points
# ============================================================

mean_points_8panel <- ce_plane_8panel %>%
  group_by(
    analysis_label,
    model_source,
    strategy_label
  ) %>%
  summarise(
    inc_qalys = mean(
      inc_qalys,
      na.rm = TRUE
    ),
    inc_cost = mean(
      inc_cost,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# ============================================================
# Step 7: Common x-axis limits
# Based on all points, not sampled points
# ============================================================

x_limits_common <- range(
  pretty(
    range(
      ce_plane_8panel$inc_qalys,
      mean_points_8panel$inc_qalys,
      na.rm = TRUE
    ),
    n = 5
  )
)

x_breaks_common <- pretty(
  x_limits_common,
  n = 5
)

# ============================================================
# Step 8: Panel labels
# ============================================================

panel_labels_8 <- expand_grid(
  analysis_label = factor(
    analysis_levels,
    levels = analysis_levels
  ),
  model_source = factor(
    c("UNSW", "UoM"),
    levels = c("UNSW", "UoM")
  )
) %>%
  arrange(
    analysis_label,
    model_source
  ) %>%
  mutate(
    label = paste0(
      LETTERS[row_number()],
      ")"
    ),
    x = x_limits_common[1],
    y = y_limits_common[2]
  )

# ============================================================
# Step 9: Theme
# Legends right-aligned as a block, with internal left alignment
# ============================================================

ACEFA_plot_theme_ceplane_8 <- theme_bw() +
  theme(
    axis.title = element_text(size = 10.5),
    axis.text = element_text(size = 9.2, colour = "black"),
    
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
    legend.box.just = "left",
    legend.justification = "center",
    
    legend.title = element_text(
      size = 9.2,
      hjust = 0
    ),
    legend.text = element_text(
      size = 9.2,
      hjust = 0
    ),
    
    legend.spacing.y = unit(0.15, "cm"),
    legend.key.height = unit(0.42, "cm"),
    legend.key.width = unit(0.60, "cm"),
    
    panel.border = element_blank(),
    axis.line = element_line(
      color = "black",
      linewidth = 0.7
    ),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.spacing.x = unit(2.0, "lines"),
    panel.spacing.y = unit(1.2, "lines"),
    
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 10,
      l = 10
    )
  )

# ============================================================
# Step 10: Plot
# ============================================================

ce_plane_8panel_plot <- ggplot(
  ce_plane_8panel,
  aes(
    x = inc_qalys,
    y = inc_cost
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    colour = "black"
  ) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.35,
    colour = "black"
  ) +
  geom_abline(
    data = wtp_line_data,
    aes(
      slope = threshold_value,
      intercept = 0,
      linetype = threshold_label
    ),
    colour = "black",
    linewidth = 0.45,
    inherit.aes = FALSE
  ) +
  geom_point(
    aes(
      colour = strategy_label
    ),
    alpha = 0.06,
    size = 0.35
  ) +
  geom_point(
    data = mean_points_8panel,
    aes(
      x = inc_qalys,
      y = inc_cost,
      fill = strategy_label,
      shape = strategy_label
    ),
    colour = "black",
    size = 2.4,
    stroke = 0.7,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = panel_labels_8,
    aes(
      x = x,
      y = y,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = -0.25,
    vjust = 1.25,
    size = 4.0
  ) +
  facet_grid(
    analysis_label ~ model_source
  ) +
  scale_colour_manual(
    name = "Single economic model iteration",
    values = scenario_palette_ceplane,
    breaks = strategy_levels_ceplane,
    limits = strategy_levels_ceplane,
    drop = FALSE
  ) +
  scale_fill_manual(
    name = "Mean estimate",
    values = scenario_palette_ceplane,
    breaks = strategy_levels_ceplane,
    limits = strategy_levels_ceplane,
    drop = FALSE
  ) +
  scale_shape_manual(
    name = "Mean estimate",
    values = strategy_shapes_ceplane,
    breaks = strategy_levels_ceplane,
    limits = strategy_levels_ceplane,
    drop = FALSE
  ) +
  scale_linetype_manual(
    name = "WTP threshold",
    values = c(
      "$15,000/QALY" = "dashed",
      "$50,000/QALY" = "solid"
    ),
    breaks = c(
      "$15,000/QALY",
      "$50,000/QALY"
    ),
    drop = FALSE
  ) +
  scale_x_continuous(
    labels = label_number(
      accuracy = 1,
      big.mark = ","
    ),
    breaks = x_breaks_common,
    limits = x_limits_common,
    expand = expansion(
      mult = c(0.02, 0.03)
    )
  ) +
  scale_y_continuous(
    labels = label_dollar(
      scale = 1 / 1000,
      suffix = "k",
      prefix = "$"
    ),
    breaks = y_breaks_common,
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
  coord_cartesian(
    xlim = x_limits_common,
    ylim = y_limits_common,
    clip = "off"
  ) +
  labs(
    x = "QALYs gained",
    y = "Incremental costs"
  ) +
  ACEFA_plot_theme_ceplane_8 +
  guides(
    colour = guide_legend(
      order = 1,
      ncol = 1,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0,
      label.hjust = 0,
      override.aes = list(
        alpha = 1,
        size = 2
      )
    ),
    fill = guide_legend(
      order = 2,
      ncol = 1,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0,
      label.hjust = 0
    ),
    shape = guide_legend(
      order = 2,
      ncol = 1,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0,
      label.hjust = 0
    ),
    linetype = guide_legend(
      order = 3,
      ncol = 1,
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0,
      label.hjust = 0
    )
  )

ce_plane_8panel_plot

# ============================================================
# Step 11: Save figure
# ============================================================

ggsave(
  filename = file.path(
    output_dir,
    "ce_plane_8panel_all_points_new_axes_legends_right_aligned.png"
  ),
  plot = ce_plane_8panel_plot,
  width = 8.27,
  height = 11.2,
  units = "in",
  dpi = 600,
  bg = "white"
)







# ============================================================
# OWSA / threshold analysis:
# LAIV vaccine price per dose
#
# Two panels:
# - UNSW
# - UoM
#
# Shows LAIV vaccine price from $0 to $100.
#
# Cuts each line off below ICER = $0/QALY
# and above ICER = $150,000/QALY.
#
# Includes WTP threshold lines:
# - $15,000/QALY gained = dashed horizontal line
# - $50,000/QALY gained = solid horizontal line
#
# Uses mean epidemiology and deterministic economic inputs.
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

if (!exists("df_mean_epi")) {
  stop("df_mean_epi is not in your R environment.")
}

if (!exists("base_pars")) {
  stop("base_pars is not in your R environment.")
}

if (!exists("run_cea_once_mean_epi")) {
  stop("run_cea_once_mean_epi() is not in your R environment.")
}

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

wtp_low <- 15000
wtp_high <- 50000
icer_y_max <- 150000

output_dir <- file.path("outputs", "figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

base_laiv_price <- base_pars$cost_laiv_vaccine

laiv_price_plot_min <- 0
laiv_price_plot_max <- 100

laiv_price_values <- sort(
  unique(
    c(
      seq(
        from = laiv_price_plot_min,
        to = laiv_price_plot_max,
        by = 0.25
      ),
      base_laiv_price
    )
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

strategy_labels <- c(
  "Central_LAIV_2-5yo"  = "LAIV 2–<5",
  "Central_LAIV_2-12yo" = "LAIV 2–<12",
  "Central_LAIV_2-18yo" = "LAIV 2–<18",
  "Central_LAIV_5-12yo" = "LAIV 5–<12",
  "Central_LAIV_5-18yo" = "LAIV 5–<18"
)

scenario_palette_owsa <- c(
  "LAIV 2–<5"  = "#B2182B",
  "LAIV 2–<12" = "#eb008b",
  "LAIV 2–<18" = "#0072B2",
  "LAIV 5–<12" = "#009E73",
  "LAIV 5–<18" = "#D55E00"
)

# ============================================================
# Step 1: Run LAIV price threshold analysis
# ============================================================

threshold_results_laiv <- map_dfr(
  laiv_price_values,
  function(price_i) {
    
    pars_i <- base_pars
    pars_i$cost_laiv_vaccine <- price_i
    
    results_i <- run_cea_once_mean_epi(
      data = df_mean_epi,
      pars = pars_i
    ) %>%
      filter(
        scenario %in% central_plus_baseline
      )
    
    baseline_i <- results_i %>%
      filter(
        scenario == "Baseline"
      ) %>%
      select(
        model_source,
        base_cost = total_cost,
        base_qaly_loss = total_qaly_loss
      )
    
    results_i %>%
      filter(
        scenario %in% central_5
      ) %>%
      left_join(
        baseline_i,
        by = "model_source"
      ) %>%
      mutate(
        laiv_price = price_i,
        inc_cost = total_cost - base_cost,
        inc_qalys = base_qaly_loss - total_qaly_loss,
        icer = inc_cost / inc_qalys
      )
  }
)

# ============================================================
# Step 2: Add boundary crossing points
# and suppress ICERs outside plotting range
# ============================================================

add_icer_plot_bounds <- function(df) {
  
  df <- df %>%
    arrange(
      laiv_price
    ) %>%
    filter(
      is.finite(icer)
    )
  
  if (nrow(df) < 2) {
    return(
      df %>%
        mutate(
          icer_plot = ifelse(
            icer >= 0 & icer <= icer_y_max,
            icer,
            NA_real_
          )
        )
    )
  }
  
  add_crossing <- function(data, boundary_value) {
    
    crossing_rows <- data %>%
      arrange(
        laiv_price
      ) %>%
      mutate(
        next_price = lead(laiv_price),
        next_icer = lead(icer),
        crosses_boundary = !is.na(next_icer) &
          (
            (icer <= boundary_value & next_icer > boundary_value) |
              (icer >= boundary_value & next_icer < boundary_value)
          )
      ) %>%
      filter(
        crosses_boundary
      )
    
    if (nrow(crossing_rows) == 0) {
      return(data)
    }
    
    boundary_rows <- crossing_rows %>%
      rowwise() %>%
      mutate(
        laiv_price = approx(
          x = c(
            icer,
            next_icer
          ),
          y = c(
            laiv_price,
            next_price
          ),
          xout = boundary_value
        )$y,
        icer = boundary_value
      ) %>%
      ungroup() %>%
      select(
        all_of(names(data))
      )
    
    bind_rows(
      data,
      boundary_rows
    )
  }
  
  df %>%
    add_crossing(
      boundary_value = 0
    ) %>%
    add_crossing(
      boundary_value = icer_y_max
    ) %>%
    arrange(
      laiv_price
    ) %>%
    mutate(
      icer_plot = ifelse(
        icer >= 0 & icer <= icer_y_max,
        icer,
        NA_real_
      )
    )
}

threshold_plot_data_laiv <- threshold_results_laiv %>%
  mutate(
    scenario_label = recode(
      scenario,
      !!!strategy_labels
    ),
    scenario_label = factor(
      scenario_label,
      levels = strategy_levels
    ),
    model_source = factor(
      model_source,
      levels = c("UNSW", "UoM")
    )
  ) %>%
  group_by(
    model_source,
    scenario,
    scenario_label
  ) %>%
  group_modify(
    ~ add_icer_plot_bounds(.x)
  ) %>%
  ungroup()

# ============================================================
# Step 3: Theme
# ============================================================

ACEFA_plot_theme_owsa_price <- theme_bw() +
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
    strip.background = element_blank(),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    
    panel.border = element_blank(),
    axis.line = element_line(
      color = "black",
      linewidth = 0.8
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    panel.spacing.x = unit(1.8, "lines"),
    
    plot.margin = margin(
      t = 8,
      r = 24,
      b = 8,
      l = 8
    )
  )

# ============================================================
# Step 4: Plot
# ============================================================

owsa_laiv_price_2panel <- ggplot(
  threshold_plot_data_laiv,
  aes(
    x = laiv_price,
    y = icer_plot,
    colour = scenario_label,
    group = scenario_label
  )
) +
  geom_hline(
    yintercept = wtp_low,
    linetype = "dashed",
    linewidth = 0.65,
    colour = "black"
  ) +
  geom_hline(
    yintercept = wtp_high,
    linetype = "solid",
    linewidth = 0.65,
    colour = "black"
  ) +
  geom_vline(
    xintercept = base_laiv_price,
    linetype = "dotted",
    linewidth = 0.7,
    colour = "black"
  ) +
  geom_line(
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  facet_grid(
    . ~ model_source
  ) +
  scale_colour_manual(
    name = "Strategy",
    values = scenario_palette_owsa,
    breaks = strategy_levels,
    limits = strategy_levels,
    drop = FALSE
  ) +
  scale_x_continuous(
    labels = label_dollar(
      accuracy = 1,
      prefix = "$"
    ),
    breaks = c(
      0,
      25,
      50,
      75,
      100
    ),
    expand = expansion(
      mult = c(0.02, 0.03)
    )
  ) +
  scale_y_continuous(
    labels = label_dollar(
      scale = 1 / 1000,
      suffix = "k",
      prefix = "$"
    ),
    breaks = seq(
      0,
      150000,
      by = 50000
    ),
    expand = expansion(
      mult = c(0, 0.04)
    )
  ) +
  coord_cartesian(
    xlim = c(
      laiv_price_plot_min,
      laiv_price_plot_max
    ),
    ylim = c(
      0,
      icer_y_max
    ),
    clip = "off"
  ) +
  labs(
    x = "LAIV vaccine price per dose",
    y = "ICER versus Baseline"
  ) +
  ACEFA_plot_theme_owsa_price +
  guides(
    colour = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )

owsa_laiv_price_2panel

# ============================================================
# Step 5: Save as PNG
# ============================================================

ggsave(
  filename = file.path(
    output_dir,
    "owsa_laiv_vaccine_price_2panel_truncated_0_to_150k_0_to_100_15k_dashed_50k_solid.png"
  ),
  plot = owsa_laiv_price_2panel,
  width = 8.27,
  height = 5.4,
  units = "in",
  dpi = 600,
  bg = "white"
)







# ============================================================
