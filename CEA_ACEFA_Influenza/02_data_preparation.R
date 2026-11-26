# Read model outputs with local-only restricted files, or labelled synthetic examples.
# The real epidemiological files must never be pushed to GitHub.

discount_life_years <- function(years, rate = 0.05) {
  ifelse(years <= 0, 0,
         ifelse(rate == 0, years, (1 - (1 + rate)^(-years)) / rate))
}

prepare_cea_data <- function(demo = FALSE) {
  if (demo) {
    df_uom <- readr::read_csv("data_example/UoM_synthetic.csv", show_col_types = FALSE)
    df_unsw <- readr::read_csv("data_example/UNSW_synthetic.csv", show_col_types = FALSE)
    life_table <- readr::read_csv("data_example/life_table_synthetic.csv", show_col_types = FALSE)
    vax_alloc <- readr::read_csv("data_example/vaccine_allocation_synthetic.csv", show_col_types = FALSE)
  } else {
    data_files <- c("data_private/UoM_model_outputs.dta", "data_private/UNSW_model_outputs.dta",
                    "inputs/Australian Life Table.xlsx", "inputs/vaccine_allocation_table.csv")
    absent <- data_files[!file.exists(data_files)]
    if (length(absent)) {
      stop("Missing local data inputs: ", paste(absent, collapse = ", "),
           ". See README for filenames and data restrictions.")
    }
    df_uom <- haven::read_dta(data_files[1])
    df_unsw <- haven::read_dta(data_files[2])
    life_table <- readxl::read_excel(data_files[3])
    vax_alloc <- readr::read_csv(data_files[4], show_col_types = FALSE)
  }

  df_uom <- df_uom %>% dplyr::mutate(model_source = "UoM", scenario = as.character(scenario),
                                    age_group = as.character(age_group))
  df_unsw <- df_unsw %>% dplyr::mutate(model_source = "UNSW", scenario = as.character(scenario),
                                      age_group = as.character(age_group))
  df_all <- dplyr::bind_rows(df_uom, df_unsw)

  required_epi <- c("model_source", "simulation_index", "scenario", "age_group",
                    "valueinfection_incidence", "valuedisease_incidence",
                    "valueadmission_incidence", "valuefatality_incidence")
  missing_epi <- setdiff(required_epi, names(df_all))
  if (length(missing_epi)) stop("Missing epidemiological columns: ", paste(missing_epi, collapse = ", "))

  df_cea <- df_all %>%
    dplyr::filter(age_group != "All") %>%
    dplyr::mutate(
      model_source = as.character(model_source), scenario = trimws(as.character(scenario)),
      age_group = as.character(age_group),
      age_group_cea = dplyr::case_when(
        age_group == "<1" ~ "<1", age_group == "1-<5" ~ "1-<5",
        age_group == "5-<12" ~ "5-<12", age_group == "12-<18" ~ "12-<18",
        age_group == "18-<65" ~ "18-<65", age_group == "65-<80" ~ "65-<80",
        age_group == "80+" ~ "80+", TRUE ~ age_group
      )
    )

  # Matches source analysis: average remaining life expectancy across each band,
  # discount at 5%, and multiply by background utility 0.86.
  life_table_band <- life_table %>%
    dplyr::mutate(
      Age = as.numeric(Age), life_expectancy = as.numeric(Combined),
      age_group_cea = dplyr::case_when(
        Age < 1 ~ "<1", Age >= 1 & Age < 5 ~ "1-<5",
        Age >= 5 & Age < 12 ~ "5-<12", Age >= 12 & Age < 18 ~ "12-<18",
        Age >= 18 & Age < 65 ~ "18-<65", Age >= 65 & Age < 80 ~ "65-<80",
        Age >= 80 ~ "80+", TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(age_group_cea)) %>%
    dplyr::group_by(age_group_cea) %>%
    dplyr::summarise(remaining_life_expectancy = mean(life_expectancy, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      discount_rate = 0.05,
      discounted_life_years = discount_life_years(remaining_life_expectancy, discount_rate),
      qaly_loss_per_death = discounted_life_years * 0.86
    )

  df_cea <- df_cea %>%
    dplyr::select(-dplyr::any_of(c("remaining_life_expectancy", "discounted_life_years",
                                   "qaly_loss_per_death", "discount_rate"))) %>%
    dplyr::left_join(life_table_band, by = "age_group_cea")
  if (anyNA(df_cea$qaly_loss_per_death)) stop("Life-table merge left missing QALY loss per death.")

  vax_alloc <- vax_alloc %>%
    dplyr::mutate(scenario = trimws(as.character(scenario)),
                  age_group_cea = trimws(as.character(age_group_cea)),
                  method = toupper(trimws(as.character(method))),
                  deliver = tolower(trimws(as.character(deliver))))
  if (anyDuplicated(vax_alloc[c("scenario", "age_group_cea")])) {
    stop("Vaccine allocation table contains multiple rows for a scenario/age band.")
  }
  df_cea <- df_cea %>%
    dplyr::mutate(age_group_cea = trimws(as.character(age_group_cea))) %>%
    dplyr::select(-dplyr::any_of(c("doses", "method", "deliver"))) %>%
    dplyr::left_join(vax_alloc, by = c("scenario", "age_group_cea")) %>%
    dplyr::mutate(doses = ifelse(is.na(doses), 0, doses))

  if (any(df_cea$doses > 0 & (is.na(df_cea$method) | is.na(df_cea$deliver)))) {
    stop("Positive vaccine doses found without method or delivery setting.")
  }
  # Original source explicitly excludes one unwanted UNSW scenario.
  df_cea <- df_cea %>%
    dplyr::filter(!(model_source == "UNSW" & scenario == "CentralLow_LAIV_2-5yo"))

  # Prevent silent multiplication of input rows at merges.
  keys <- c("model_source", "scenario", "simulation_index", "age_group_cea")
  if (anyDuplicated(df_cea[keys])) stop("Duplicate model/scenario/simulation/age rows after merges.")
  if (any(df_cea$valueadmission_incidence > df_cea$valuedisease_incidence, na.rm = TRUE)) {
    warning("Some admissions exceed symptomatic cases; original model uses pmax(cases - admissions, 0).")
  }
  if (anyNA(df_cea[required_epi])) stop("Some required epidemiological fields contain NA.")

  # Keep grouping and aggregation exactly as in Section 11 of the source.
  df_mean_epi <- df_cea %>%
    dplyr::group_by(model_source, scenario, age_group_cea, method, deliver, doses,
                    remaining_life_expectancy, discounted_life_years,
                    qaly_loss_per_death, discount_rate) %>%
    dplyr::summarise(
      valueinfection_incidence = mean(valueinfection_incidence, na.rm = TRUE),
      valuedisease_incidence = mean(valuedisease_incidence, na.rm = TRUE),
      valueadmission_incidence = mean(valueadmission_incidence, na.rm = TRUE),
      valuefatality_incidence = mean(valuefatality_incidence, na.rm = TRUE),
      .groups = "drop"
    )

  list(df_all = df_all, df_cea = df_cea, df_mean_epi = df_mean_epi,
       life_table_band = life_table_band, vax_alloc = vax_alloc)
}
