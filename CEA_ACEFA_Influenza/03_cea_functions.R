# Functions copied verbatim from the source R analysis (Sections 13 and 14).
# Disease cases and admissions are mutually exclusive for morbidity QALY losses.

run_cea_once <- function(data, pars) {
  
  data %>%
    mutate(
      doses = ifelse(is.na(doses), 0, doses),
      valuefatality_incidence = ifelse(
        is.na(valuefatality_incidence),
        0,
        valuefatality_incidence
      ),
      
      non_hosp_cases = pmax(
        valuedisease_incidence - valueadmission_incidence,
        0
      ),
      
      hosp_cases = valueadmission_incidence,
      
      disease_cost_row =
        non_hosp_cases * pars$cost_case +
        hosp_cases * pars$cost_hosp,
      
      vaccine_product_cost_row = case_when(
        method == "IM"   ~ doses * pars$cost_im_vaccine,
        method == "LAIV" ~ doses * pars$cost_laiv_vaccine,
        TRUE ~ 0
      ),
      
      vaccine_program_cost_row = case_when(
        deliver == "other"  ~ doses * pars$cost_program_other,
        deliver == "school" ~ doses * pars$cost_program_school,
        TRUE ~ 0
      ),
      
      under5_extra_cost_row = case_when(
        age_group_cea %in% c("<1", "1-<5") ~
          doses * pars$cost_under5_extra,
        TRUE ~ 0
      ),
      
      vaccine_cost_row =
        vaccine_product_cost_row +
        vaccine_program_cost_row +
        under5_extra_cost_row,
      
      non_hosp_qaly_loss_row =
        non_hosp_cases * pars$qaly_dec_nonhosp,
      
      hosp_qaly_loss_row =
        hosp_cases * pars$qaly_dec_hosp,
      
      morbidity_qaly_loss_row =
        non_hosp_qaly_loss_row +
        hosp_qaly_loss_row,
      
      death_qaly_loss_row =
        valuefatality_incidence * qaly_loss_per_death,
      
      total_qaly_loss_row =
        morbidity_qaly_loss_row +
        death_qaly_loss_row
    ) %>%
    group_by(
      model_source,
      simulation_index,
      scenario
    ) %>%
    summarise(
      total_infections = sum(valueinfection_incidence, na.rm = TRUE),
      total_disease = sum(valuedisease_incidence, na.rm = TRUE),
      total_admissions = sum(valueadmission_incidence, na.rm = TRUE),
      total_fatalities = sum(valuefatality_incidence, na.rm = TRUE),
      
      disease_costs = sum(disease_cost_row, na.rm = TRUE),
      vaccine_costs = sum(vaccine_cost_row, na.rm = TRUE),
      
      non_hosp_qaly_loss = sum(non_hosp_qaly_loss_row, na.rm = TRUE),
      hosp_qaly_loss = sum(hosp_qaly_loss_row, na.rm = TRUE),
      morbidity_qaly_loss = sum(morbidity_qaly_loss_row, na.rm = TRUE),
      death_qaly_loss = sum(death_qaly_loss_row, na.rm = TRUE),
      total_qaly_loss = sum(total_qaly_loss_row, na.rm = TRUE),
      
      total_cost = disease_costs + vaccine_costs,
      
      .groups = "drop"
    )
}


run_cea_once_mean_epi <- function(data, pars) {
  
  data %>%
    mutate(
      doses = ifelse(is.na(doses), 0, doses),
      valuefatality_incidence = ifelse(
        is.na(valuefatality_incidence),
        0,
        valuefatality_incidence
      ),
      
      non_hosp_cases = pmax(
        valuedisease_incidence - valueadmission_incidence,
        0
      ),
      
      hosp_cases = valueadmission_incidence,
      
      disease_cost_row =
        non_hosp_cases * pars$cost_case +
        hosp_cases * pars$cost_hosp,
      
      vaccine_product_cost_row = case_when(
        method == "IM"   ~ doses * pars$cost_im_vaccine,
        method == "LAIV" ~ doses * pars$cost_laiv_vaccine,
        TRUE ~ 0
      ),
      
      vaccine_program_cost_row = case_when(
        deliver == "other"  ~ doses * pars$cost_program_other,
        deliver == "school" ~ doses * pars$cost_program_school,
        TRUE ~ 0
      ),
      
      under5_extra_cost_row = case_when(
        age_group_cea %in% c("<1", "1-<5") ~
          doses * pars$cost_under5_extra,
        TRUE ~ 0
      ),
      
      vaccine_cost_row =
        vaccine_product_cost_row +
        vaccine_program_cost_row +
        under5_extra_cost_row,
      
      non_hosp_qaly_loss_row =
        non_hosp_cases * pars$qaly_dec_nonhosp,
      
      hosp_qaly_loss_row =
        hosp_cases * pars$qaly_dec_hosp,
      
      morbidity_qaly_loss_row =
        non_hosp_qaly_loss_row +
        hosp_qaly_loss_row,
      
      death_qaly_loss_row =
        valuefatality_incidence * qaly_loss_per_death,
      
      total_qaly_loss_row =
        morbidity_qaly_loss_row +
        death_qaly_loss_row
    ) %>%
    group_by(
      model_source,
      scenario
    ) %>%
    summarise(
      total_infections = sum(valueinfection_incidence, na.rm = TRUE),
      total_disease = sum(valuedisease_incidence, na.rm = TRUE),
      total_admissions = sum(valueadmission_incidence, na.rm = TRUE),
      total_fatalities = sum(valuefatality_incidence, na.rm = TRUE),
      
      disease_costs = sum(disease_cost_row, na.rm = TRUE),
      vaccine_costs = sum(vaccine_cost_row, na.rm = TRUE),
      
      non_hosp_qaly_loss = sum(non_hosp_qaly_loss_row, na.rm = TRUE),
      hosp_qaly_loss = sum(hosp_qaly_loss_row, na.rm = TRUE),
      morbidity_qaly_loss = sum(morbidity_qaly_loss_row, na.rm = TRUE),
      death_qaly_loss = sum(death_qaly_loss_row, na.rm = TRUE),
      total_qaly_loss = sum(total_qaly_loss_row, na.rm = TRUE),
      
      total_cost = disease_costs + vaccine_costs,
      
      .groups = "drop"
    )
}

