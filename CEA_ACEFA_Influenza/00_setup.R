# ACEFA economic evaluation: public computational workflow.
# Run from the repository root using run_analysis.R or run_demo.R.
required_packages <- c(
  "haven", "readxl", "readr", "dplyr", "tidyr", "purrr", "tibble",
  "ggplot2", "scales", "data.table"
)
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages)) {
  stop("Install the missing packages first: ", paste(missing_packages, collapse = ", "))
}
invisible(lapply(required_packages, library, character.only = TRUE))

central_5 <- c(
  "Central_LAIV_2-5yo", "Central_LAIV_2-12yo", "Central_LAIV_2-18yo",
  "Central_LAIV_5-12yo", "Central_LAIV_5-18yo"
)
central_plus_baseline <- c("Baseline", central_5)
scenario_labels_central <- c(
  "Baseline"="Baseline", "Central_LAIV_2-5yo"="LAIV 2–<5",
  "Central_LAIV_2-12yo"="LAIV 2–<12", "Central_LAIV_2-18yo"="LAIV 2–<18",
  "Central_LAIV_5-12yo"="LAIV 5–<12", "Central_LAIV_5-18yo"="LAIV 5–<18"
)
scenario_palette_ceac <- c(
  "Baseline"="#4D4D4D", "LAIV 2–<5"="#B2182B", "LAIV 2–<12"="#eb008b",
  "LAIV 2–<18"="#0072B2", "LAIV 5–<12"="#009E73", "LAIV 5–<18"="#D55E00"
)
analysis_wtp <- seq(0, 100000, by = 5000)
set.seed(12345)
