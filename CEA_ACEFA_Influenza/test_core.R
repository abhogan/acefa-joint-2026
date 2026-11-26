# Run after source('run_demo.R') or run_analysis.R in the same R session.
stopifnot(identical(base_pars$cost_laiv_vaccine, 25),
          identical(base_pars$cost_im_vaccine, 10),
          identical(base_pars$qaly_dec_nonhosp, 0.009),
          identical(base_pars$qaly_dec_hosp, 0.031))
stopifnot(all(df_cea$valuedisease_incidence >= df_cea$valueadmission_incidence))
stopifnot(isTRUE(all.equal(base_results_full$total_qaly_loss,
                            base_results_full$morbidity_qaly_loss + base_results_full$death_qaly_loss)))
stopifnot(isTRUE(all.equal(base_results_mean_epi$morbidity_qaly_loss,
                            base_results_mean_epi$non_hosp_qaly_loss +
                              base_results_mean_epi$hosp_qaly_loss)))
stopifnot(!anyNA(incremental_full$base_cost))
stopifnot(!anyNA(incremental_mean_epi$base_cost))
cat("Core accounting and pairing invariants passed.\n")
