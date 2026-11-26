# Restricted validation aid, NOT run by default.
# Before running the refactored workflow, save old results locally OUTSIDE Git:
#   saveRDS(base_results_mean_epi, 'data_private/old_base_mean_epi.rds')
#   saveRDS(base_results_full, 'data_private/old_base_full.rds')
# Then source run_analysis.R and run this check.
# Matching numerical results require the same original input data.

compare_base_table <- function(old_path, new_df, tolerance = 1e-8) {
  if (!file.exists(old_path)) stop('No original snapshot found at ', old_path)
  old <- readRDS(old_path)
  keys <- c('model_source', 'scenario')
  variables <- c('total_cost', 'total_qaly_loss', 'non_hosp_qaly_loss',
                 'hosp_qaly_loss', 'death_qaly_loss', 'disease_costs', 'vaccine_costs')
  old <- old %>% dplyr::select(dplyr::all_of(c(keys, variables))) %>%
    dplyr::arrange(model_source, scenario)
  current <- new_df %>% dplyr::select(dplyr::all_of(c(keys, variables))) %>%
    dplyr::arrange(model_source, scenario)
  stopifnot(identical(old[keys], current[keys]))
  for (v in variables) {
    if (!isTRUE(all.equal(old[[v]], current[[v]], tolerance = tolerance))) {
      stop('Comparison failed on ', v)
    }
  }
  message('Verified baseline calculations against original: ', old_path)
}

compare_base_table('data_private/old_base_mean_epi.rds', base_results_mean_epi)
# Full input contains one row per transmission simulation, so compare all rows.
# The following comparison requires sorting by simulation_index too:
old_full_path <- 'data_private/old_base_full.rds'
if (file.exists(old_full_path)) {
  o <- readRDS(old_full_path)
  n <- base_results_full
  keys <- c('model_source', 'simulation_index', 'scenario')
  vars <- c('total_cost', 'total_qaly_loss', 'non_hosp_qaly_loss',
            'hosp_qaly_loss', 'death_qaly_loss')
  o <- o %>% dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
  n <- n %>% dplyr::arrange(dplyr::across(dplyr::all_of(keys)))
  stopifnot(identical(o[keys], n[keys]))
  for (v in vars) stopifnot(isTRUE(all.equal(o[[v]], n[[v]], tolerance = 1e-8)))
  message('Verified full deterministic simulation calculations against original.')
}
