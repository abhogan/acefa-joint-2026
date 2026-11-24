
### population-level tables ###

fig_avertedpoplevelbyscen_data_unsw <- read.csv(file.path("visualisation-data", "fig_avertedpoplevelbyscen_data_unsw.csv"))
fig_avertedpoplevelbyscen_data_uom <- read.csv(file.path("visualisation-data", "fig_avertedpoplevelbyscen_data_uom.csv"))

fig_infctallavrt_data <- fig_avertedpoplevelbyscen_data_unsw |>
  bind_rows(fig_avertedpoplevelbyscen_data_uom) |>
  arrange(team == "UNSW") 

summary_avrt_iqr_table_percent <- fig_infctallavrt_data |>
  group_by(scenario, team, target, uncertainty, LAIV_age) |>
  summarise(median = round(quantile(perc_reduction, 0.5), 1), 
            quant_25 = round(quantile(perc_reduction, 0.025), 1), # 50%
            quant_75 = round(quantile(perc_reduction, 0.75), 1), # 50%
            
  ) |> 
  mutate(value = paste0(median, " (", quant_25, ", ", quant_75, ")")) |>
  select(-median, -quant_25, -quant_75)

summary_avrt_iqr_table_abs <- fig_infctallavrt_data |>
  ungroup() |>
  #convert fatality results to per million
  mutate(change_reduction = if_else(target == "fatality_incidence", change_reduction*10, change_reduction)) |>
  group_by(scenario, team, target, uncertainty, LAIV_age) |>
  summarise(median = round(quantile(change_reduction, 0.5), 1), 
            quant_25 = round(quantile(change_reduction, 0.025), 1), # 50%
            quant_75 = round(quantile(change_reduction, 0.75), 1), # 50%
            
  ) |> 
  mutate(value = paste0(median, " (", quant_25, ", ", quant_75, ")")) |>
  select(-median, -quant_25, -quant_75)

summary_avrt_iqr_table_percent
summary_avrt_iqr_table_abs

write.csv(summary_avrt_iqr_table_percent, file.path("visualisation-output", "Data summaries", "PopLevel_percentage_averted_table.csv"), row.names = FALSE)
write.csv(summary_avrt_iqr_table_abs, file.path("visualisation-output", "Data summaries", "PopLevel_absolute_averted_table.csv"), row.names = FALSE)

# create table for paper
table_out_percent <- summary_avrt_iqr_table_percent |>
  mutate(age_uncert = paste0(LAIV_age, uncertainty)) |>
  ungroup() |>
  select(age_uncert, team, value, target) |>
  filter(target != "infection_incidence") |>
  mutate(team_target = paste0(team, target)) |>
  select(age_uncert, team_target, value) |>
  pivot_wider(names_from = team_target, values_from = value)

write.csv(table_out_percent, "table_out_percent.csv")

# create table for paper
table_out_absolute <- summary_avrt_iqr_table_abs |>
  mutate(age_uncert = paste0(LAIV_age, uncertainty)) |>
  ungroup() |>
  select(age_uncert, team, value, target) |>
  filter(target != "infection_incidence") |>
  mutate(team_target = paste0(team, target)) |>
  select(age_uncert, team_target, value) |>
  pivot_wider(names_from = team_target, values_from = value) %>%
  select(age_uncert, UNSWdisease_incidence, UNSWadmission_incidence, UNSWfatality_incidence, UoMdisease_incidence, UoMadmission_incidence, UoMfatality_incidence)

write.csv(table_out_absolute, "table_out_absolute.csv")


### BY AGE ###

#### HOSPITALISATIONS ###
fig_averted_hosp_unsw <- read.csv(file.path("visualisation-data", "fig_avrtbyage_hosp_data_unsw.csv")) |> make_age_factor()
fig_averted_hosp_uom <- read.csv(file.path("visualisation-data", "fig_avrtbyage_hosp_data_uom.csv")) |> make_age_factor()

# Combine UNSW and UoM
fig_averted <- fig_averted_hosp_unsw |>
  bind_rows(fig_averted_hosp_uom) |>
  arrange(team == "UNSW")

summary_avrt_iqr_table_percent_age <- calculate_percent(fig_averted)
summary_avrt_iqr_table_abs_age <- calculate_absolute(fig_averted)

age_table_percent <- summary_avrt_iqr_table_percent_age %>%
  filter(target == "admission_incidence")
age_table_percent <- create_percent_table_age(age_table_percent)
  
age_table_abs <- summary_avrt_iqr_table_abs_age %>%
  filter(target == "admission_incidence")
age_table_abs <- create_absolute_table_age(age_table_abs)
  
table_out <- bind_rows(age_table_percent, age_table_abs) %>%
  select(scenario, team, type, everything())

write.csv(table_out, file.path("visualisation-output", "Data summaries", "Age_hosps_summary.csv"), row.names = FALSE)

## FATAL ##

fig_averted_fatal_unsw <- read.csv(file.path("visualisation-data", "fig_avrtbyage_fatal_data_unsw.csv")) |> make_age_factor()
fig_averted_fatal_uom <- read.csv(file.path("visualisation-data", "fig_avrtbyage_fatal_data_uom.csv")) |> make_age_factor()

# Combine UNSW and UoM
fig_averted <- fig_averted_fatal_unsw |>
  bind_rows(fig_averted_fatal_uom) |>
  arrange(team == "UNSW")

summary_avrt_iqr_table_percent_age <- calculate_percent(fig_averted)
summary_avrt_iqr_table_abs_age <- calculate_absolute_death(fig_averted)

age_table_percent <- summary_avrt_iqr_table_percent_age %>%
  filter(target == "fatality_incidence")
age_table_percent <- create_percent_table_age(age_table_percent)

age_table_abs <- summary_avrt_iqr_table_abs_age %>%
  filter(target == "fatality_incidence")
age_table_abs <- create_absolute_table_age(age_table_abs)

table_out <- bind_rows(age_table_percent, age_table_abs) %>%
  select(scenario, team, type, everything())

write.csv(table_out, file.path("visualisation-output", "Data summaries", "Age_fatal_summary.csv"), row.names = FALSE)

### cases ###

fig_averted_cases_unsw <- read.csv(file.path("visualisation-data", "fig_avertedbyage_cases_unsw.csv")) |> make_age_factor()
fig_averted_cases_uom <- read.csv(file.path("visualisation-data", "fig_avertedbyage_cases_uom.csv")) |> make_age_factor()

# Combine UNSW and UoM
fig_averted <- fig_averted_cases_unsw |>
  bind_rows(fig_averted_cases_uom) |>
  arrange(team == "UNSW")

summary_avrt_iqr_table_percent_age <- calculate_percent(fig_averted)
summary_avrt_iqr_table_abs_age <- calculate_absolute(fig_averted)

age_table_percent <- summary_avrt_iqr_table_percent_age %>%
  filter(target == "disease_incidence")
age_table_percent <- create_percent_table_age(age_table_percent)

age_table_abs <- summary_avrt_iqr_table_abs_age %>%
  filter(target == "disease_incidence")
age_table_abs <- create_absolute_table_age(age_table_abs)

table_out <- bind_rows(age_table_percent, age_table_abs) %>%
  select(scenario, team, type, everything())

write.csv(table_out, file.path("visualisation-output", "Data summaries", "Age_cases_summary.csv"), row.names = FALSE)

### infections ###

fig_averted_infections_unsw <- read.csv(file.path("visualisation-data", "fig_avertedbyage_infct_unsw.csv")) |> make_age_factor()
fig_averted_infections_uom <- read.csv(file.path("visualisation-data", "fig_avertedbyage_infct_uom.csv")) |> make_age_factor()

# Combine UNSW and UoM
fig_averted <- fig_averted_infections_unsw |>
  bind_rows(fig_averted_infections_uom) |>
  arrange(team == "UNSW")

summary_avrt_iqr_table_percent_age <- calculate_percent(fig_averted)
summary_avrt_iqr_table_abs_age <- calculate_absolute_inf(fig_averted)

age_table_percent <- summary_avrt_iqr_table_percent_age %>%
  filter(target == "infection_incidence")
age_table_percent <- create_percent_table_age(age_table_percent)

age_table_abs <- summary_avrt_iqr_table_abs_age %>%
  filter(target == "infection_incidence")
age_table_abs <- create_absolute_table_age(age_table_abs)

table_out <- bind_rows(age_table_percent, age_table_abs) %>%
  select(scenario, team, type, everything())

write.csv(table_out, file.path("visualisation-output", "Data summaries", "Age_infections_summary.csv"), row.names = FALSE)

