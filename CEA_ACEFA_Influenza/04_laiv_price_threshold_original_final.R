# Optional source-derived analysis. Requires prior core objects.
# Original numerical analysis preserved, paths made portable.

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





