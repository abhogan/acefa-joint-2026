# Run from the root of this repository:
#   Rscript run_analysis.R
# Requires restricted source epidemiology data in data_private/.
# For a data-free example instead use: Rscript run_demo.R

source("R/00_setup.R")
source("R/01_parameters.R")
source("R/02_data_preparation.R")
source("R/03_cea_functions.R")
source("R/04_run_analysis.R")
source("R/05_results_tables.R")
source("R/06_ceac_figures.R")
source("R/07_ce_plane_figures.R")

demo <- identical(Sys.getenv("ACEFA_DEMO", "0"), "1")
n_economic <- as.integer(Sys.getenv("ACEFA_N_ECON", "10000"))
n_full <- as.integer(Sys.getenv("ACEFA_N_FULL", "1000"))
if (is.na(n_economic) || is.na(n_full) || n_economic < 1 || n_full < 1) {
  stop("ACEFA_N_ECON and ACEFA_N_FULL must be positive integers.")
}
cat("Analysis mode:", if (demo) "SYNTHETIC DEMONSTRATION" else "RESTRICTED ORIGINAL MODEL INPUTS", "\n")
cat("PSA draws - economic:", n_economic, " full:", n_full, "\n")

inputs <- prepare_cea_data(demo = demo)
df_cea <- inputs$df_cea
df_mean_epi <- inputs$df_mean_epi
results <- run_core_analysis(df_cea, df_mean_epi, n_psa_economic = n_economic,
                             n_psa_full = n_full, seed = 12345)

# Retain original object names for the optional manuscript figures.
base_results_full <- results$base_results_full
base_results_mean_epi <- results$base_results_mean_epi
psa_results_mean_epi <- results$psa_results_mean_epi
incremental_mean_epi <- results$incremental_mean_epi
psa_results_full <- results$psa_results_full
incremental_full <- results$incremental_full

output_dir <- file.path("outputs", if (demo) "synthetic_demo" else "figures")
table_dir <- file.path("outputs", if (demo) "synthetic_demo" else "tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

write_analysis_tables(results, table_dir)

# Pairwise and competing CEACs: economic-only and combined uncertainty.
pairwise_ceac_mean <- calc_pairwise_ceac(incremental_mean_epi)
competing_ceac_mean <- calc_competing_ceac(psa_results_mean_epi, full = FALSE)
pairwise_ceac_full <- calc_pairwise_ceac(incremental_full)
competing_ceac_full <- calc_competing_ceac(psa_results_full, full = TRUE)
make_4panel_ceac(pairwise_ceac_mean, competing_ceac_mean,
                 file.path(output_dir, "ceac_economic_only.png"))
make_4panel_ceac(pairwise_ceac_full, competing_ceac_full,
                 file.path(output_dir, "ceac_full_model.png"))

# Central 50% of epidemic simulations (based on mean cases averted, per source).
psa_results_iqr_impact <- select_iqr_impact(psa_results_full)
incremental_iqr_impact <- make_incrementals(psa_results_iqr_impact, full = TRUE)
make_4panel_ceac(
  calc_pairwise_ceac(incremental_iqr_impact),
  calc_competing_ceac(psa_results_iqr_impact, full = TRUE),
  file.path(output_dir, "ceac_iqr_transmission_model.png")
)
make_full_ce_plane(incremental_full, file.path(output_dir, "ce_plane_full.png"))

# Original manuscript figure blocks are kept separately as optional scripts.
# They use real PSA objects and can consume substantial RAM.
if (identical(Sys.getenv("ACEFA_RUN_OPTIONAL_FIGURES", "0"), "1")) {
  if (demo) message("Optional figures are being run on SYNTHETIC data.")
  source("R/optional/01_tornado_original_final.R")
  source("R/optional/02_eight_panel_ce_plane_original_final.R")
  source("R/optional/03_two_way_price_original_final.R")
  source("R/optional/04_laiv_price_threshold_original_final.R")
  source("R/optional/05_modelling_scenarios_ceac_original_final.R")
  source("R/optional/06_50k_price_crossings_original_final.R")
}

# Never publish these result objects without collaborator approval. They contain
# values derived from the restricted transmission-model inputs.
# No save.image() or unapproved cache write is performed.
cat("Completed. Review outputs/ before any public release.\n")
