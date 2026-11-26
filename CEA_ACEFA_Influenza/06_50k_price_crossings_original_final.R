# Optional source-derived analysis. Requires prior core objects.
# Original numerical analysis preserved, paths made portable.

# Find LAIV vaccine price where ICER crosses $50,000/QALY
#
# Uses existing threshold_results_laiv from the LAIV price
# threshold analysis.
#
# Reports results for:
# - LAIV 5–<12
# - LAIV 5–<18
# for both UNSW and UoM.
# ============================================================

library(dplyr)
library(scales)

# ------------------------------------------------------------
# Check required object exists
# ------------------------------------------------------------

if (!exists("threshold_results_laiv")) {
  stop("threshold_results_laiv is not in your R environment. Run the LAIV threshold analysis first.")
}

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------

icer_threshold <- 50000

target_strategies <- c(
  "Central_LAIV_5-12yo",
  "Central_LAIV_5-18yo"
)

strategy_labels <- c(
  "Central_LAIV_5-12yo" = "LAIV 5–<12",
  "Central_LAIV_5-18yo" = "LAIV 5–<18"
)

# ------------------------------------------------------------
# Function to find the crossing point
# ------------------------------------------------------------

find_icer_threshold_crossing <- function(df, threshold = 50000) {
  
  df <- df %>%
    arrange(laiv_price) %>%
    filter(
      is.finite(icer),
      is.finite(laiv_price)
    )
  
  if (nrow(df) == 0) {
    return(
      tibble(
        laiv_price_at_50k = NA_real_,
        status = "No finite ICER values"
      )
    )
  }
  
  # If ICER is exactly equal to the threshold at a tested price
  exact_match <- df %>%
    filter(
      abs(icer - threshold) < 1e-8
    ) %>%
    slice(1)
  
  if (nrow(exact_match) > 0) {
    return(
      tibble(
        laiv_price_at_50k = exact_match$laiv_price,
        status = "Exact tested value"
      )
    )
  }
  
  # Find where the ICER crosses the threshold between two tested prices
  crossing_row <- df %>%
    mutate(
      next_price = lead(laiv_price),
      next_icer = lead(icer),
      crosses_threshold = !is.na(next_icer) &
        (
          (icer < threshold & next_icer > threshold) |
            (icer > threshold & next_icer < threshold)
        )
    ) %>%
    filter(
      crosses_threshold
    ) %>%
    slice(1)
  
  if (nrow(crossing_row) > 0) {
    
    crossing_price <- approx(
      x = c(
        crossing_row$icer,
        crossing_row$next_icer
      ),
      y = c(
        crossing_row$laiv_price,
        crossing_row$next_price
      ),
      xout = threshold
    )$y
    
    return(
      tibble(
        laiv_price_at_50k = crossing_price,
        status = "Interpolated crossing"
      )
    )
  }
  
  # If there is no crossing, report whether the ICER is always below or above
  if (all(df$icer < threshold, na.rm = TRUE)) {
    return(
      tibble(
        laiv_price_at_50k = NA_real_,
        status = "ICER remains below $50k across tested range"
      )
    )
  }
  
  if (all(df$icer > threshold, na.rm = TRUE)) {
    return(
      tibble(
        laiv_price_at_50k = NA_real_,
        status = "ICER remains above $50k across tested range"
      )
    )
  }
  
  tibble(
    laiv_price_at_50k = NA_real_,
    status = "No clear crossing"
  )
}

# ------------------------------------------------------------
# Apply to LAIV 5–<12 and LAIV 5–<18 for both models
# ------------------------------------------------------------

laiv_price_50k_crossings <- threshold_results_laiv %>%
  filter(
    scenario %in% target_strategies
  ) %>%
  mutate(
    strategy_label = recode(
      scenario,
      !!!strategy_labels
    )
  ) %>%
  group_by(
    model_source,
    scenario,
    strategy_label
  ) %>%
  group_modify(
    ~ find_icer_threshold_crossing(
      df = .x,
      threshold = icer_threshold
    )
  ) %>%
  ungroup() %>%
  arrange(
    strategy_label,
    model_source
  )

# ------------------------------------------------------------
# Print formatted result
# ------------------------------------------------------------

laiv_price_50k_crossings_formatted <- laiv_price_50k_crossings %>%
  mutate(
    laiv_price_at_50k = ifelse(
      is.na(laiv_price_at_50k),
      NA_character_,
      dollar(
        laiv_price_at_50k,
        accuracy = 0.01
      )
    )
  ) %>%
  select(
    model_source,
    strategy_label,
    laiv_price_at_50k,
    status
  )

laiv_price_50k_crossings_formatted %>%
  print(n = Inf)
