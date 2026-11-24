# Script for making manuscript Figure 2 
# Prepared by Freya Shearer and Alys Young

# #############
# sort data

# lines 18-51 will not run due to the NNDSS dataset not being publicly available
# see data availability statement of manuscript for access details
# case data
case_dat <- read.csv('data/Daily_case_counts_20150101_to_20250917_NNDSSonly.csv') 

case_dat_clean <- case_dat %>% 
  mutate(year = year(ymd(notification_date))) %>% 
  filter(year %in% c(2015, 2016, 2017, 2018, 2019, 2023, 2024),
         #filter(year %in% c(2023),
         location %in% c(
           'NSW',
           'QLD',
           'SA',
           'VIC',
           'WA',
           'TAS',
           'ACT')) %>% 
  select(location, age_categories, cases, notification_date, year) %>% 
  group_by(location, notification_date, year) %>% 
  summarise(total_cases=sum(cases))

# size of each state population
state_pop_dat <- read.csv('data/pop_by_state.csv') %>% 
  select(-NT) %>% 
  pivot_longer(cols=c("NSW","QLD","SA","VIC","WA","ACT","TAS"),
               names_to="location",
               values_to="pop_size") %>% 
  rename("year"="X")

# now adjust case data by pop size
case_dat_adjusted <- case_dat_clean %>%
  left_join(state_pop_dat) %>% 
  mutate(cases_per_100000 = total_cases*100000/pop_size) %>% 
  group_by(location, year) %>% 
  mutate(rolling_7day_mean = rollmean(cases_per_100000, k = 7, align = "center", fill = NA)) %>% 
  mutate(horizon = lubridate::yday(notification_date))

# sort target hospitalisation data
calibration_data_hosp_weekly <- read.csv(file.path("visualisation-data", "AIHW_Hospitalisations_2023.csv"))
aus_pop_2023 <- read.csv(file.path("visualisation-data", "aus_pop_2023.csv")) |> pull(x)
calibration_target_hospitalisations <- data.frame(value = sum(calibration_data_hosp_weekly$Hospitalisations)/aus_pop_2023 * 100000, # should be 92.78153
                                                  target = "admission_incidence",
                                                  data_type = "Calibration data")

# #############
# sort model outputs

# uom
submission_folder_path <- file.path(getwd(), 'model-output', 'uom-model', 'UoM Final Results with waning')

# sort model outputs
model_data_uom <- open_dataset(submission_folder_path) %>% 
  filter(scenario == "Status_quo",
         target %in% c("infection_incidence", "disease_incidence", "admission_incidence", 'fatality_incidence')) %>%
  select(-c(setting, team, scenario)) %>%
  group_by(simulation_index, run_nr, horizon, target) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  collect() %>% 
  mutate(value=value/10) %>% # converting to per 100,000
  arrange(simulation_index, run_nr, horizon, target) %>%
  group_by(simulation_index, run_nr, target) %>%
  mutate(cum_val = cumsum(value)) %>%
  ungroup() %>% 
  mutate(team='UoM')

# this is the step that does not seem to work properly in using dplyr!!!
fig_data_uom <- model_data_uom[model_data_uom$run_nr == 0, ] %>% 
  select(-run_nr)

# get random sample of UoM sim index
sims <- unique(fig_data_uom %>% pull(simulation_index))
sim_idx <- sample(sims, size=150)

fig_data_uom <- fig_data_uom[fig_data_uom$simulation_index %in% sim_idx,] 

# unsw
model_data_unsw <- read.csv(file.path("visualisation-data", "fig_cali_data.csv")) 

fig_data_unsw <- model_data_unsw %>% 
  filter(target %in% c('infection_incidence', 'disease_incidence', 'admission_incidence', 'fatality_incidence'), team=='UNSW', age_group=='All') %>% 
  select(-X, -data_type, -scenario, -setting, -age_group) %>% 
  arrange(simulation_index, horizon, target) %>%
  group_by(simulation_index, target) %>%
  mutate(cum_val = cumsum(value)) %>%
  ungroup()

# combine them
fig_data <- fig_data_uom %>% 
  rbind(fig_data_unsw) %>% 
  mutate(team=factor(team, levels=c('UoM', 'UNSW'))) %>% 
  mutate(target=factor(target, levels=c("infection_incidence", 
                                        "disease_incidence", 
                                        "admission_incidence", 
                                        "fatality_incidence")))

# #############
# make plot theme
fs_fig_theme <- theme_bw() + theme(legend.text = element_text(size = 12),
                                   axis.title  = element_text(size = 12),
                                   strip.text = element_text(size = 12),
                                   legend.position = "none",
                                   aspect.ratio = 1.1,
                                   panel.grid.major.x = element_blank(),
                                   panel.grid.minor.x = element_blank(),
                                   panel.grid.minor.y = element_blank(),
                                   strip.background = element_blank(),
                                   panel.border = element_blank(),
                                   axis.line = element_line(color = "black", size = 1)) 

# colour palettes
scenario_palettes <- list()

scenario_palettes$team <- c("UoM"  = "#262261",
                            "UNSW" = "#eb008b")

############################
# make plots

# daily cases modelled
fig2a <- ggplot() +
  
  geom_line(data = fig_data %>% filter(target=='disease_incidence'), aes(x = horizon, y = value, group = simulation_index, colour=team), alpha = 0.2, linewidth = 0.5) +
  fs_fig_theme +
  scale_colour_manual(values = scenario_palettes$team) +
  facet_grid(~team, scales = "fixed") +
  ylim(0, 35) +
  labs(x = "Horizon (days)",
       y = "Daily modelled\ncases")

fig2a

# daily cases actual
fig2b <- ggplot() +
  
  geom_line(data = case_dat_adjusted, aes(x = horizon, y = rolling_7day_mean, group = interaction(location, year)), alpha = 0.2, linewidth = 0.5, colour='grey30') +
  fs_fig_theme +
  ylim(0, 35) +
  labs(x = "Day of year",
       y = "Daily reported\ncases")

fig2b

# cumulative all targets
# labels
new_labels <- c(
  "infection_incidence"= "Infections",
  "disease_incidence" = 'Cases',
  "admission_incidence" = 'Hospitalisations',
  "fatality_incidence" = 'Deaths')

# specify hospitalisation calibration target
calibration_targets <- calibration_target_hospitalisations %>% 
  rbind(data.frame(value=1.74, target='fatality_incidence', data_type='Calibration data')) %>% 
  mutate(target=factor(target, levels=c("infection_incidence", "disease_incidence", "admission_incidence", "fatality_incidence")))

cumul_overlap <- ggplot() +
  
  geom_line(data = fig_data, aes(x = horizon, y = cum_val, group = interaction(simulation_index, team), colour = team), alpha = 0.1, linewidth = 0.5) +
  fs_fig_theme +
  scale_colour_manual(values = scenario_palettes$team) +
  facet_wrap(~target, scales='free_y', labeller = labeller(target = new_labels), ncol=4) +
  geom_hline(data=calibration_targets, aes(yintercept = value), color = "black", linetype = "dashed", linewidth = 0.8) +
  labs(x = "Horizon (days)",
       y = "Cumulative modelled\noutcomes",
       colour='Team')

cumul_overlap

fig2 <- (fig2a + fig2b)/cumul_overlap + plot_annotation(tag_levels = 'A')
fig2

ggsave(plot = fig2, filename = file.path("visualisation-output", "Plots", "PNG",  "figure2.png"))

### make supplementary plots
# infections
sfig_inf <- ggplot() +
  
  geom_line(data = fig_data %>% filter(target=='infection_incidence'), aes(x = horizon, y = value, group = simulation_index), alpha = 0.2, linewidth = 0.5, colour='grey30') +
  fs_fig_theme +
  facet_grid(~team, scales = "fixed") +
  labs(x = "",
       y = "Modelled infections")

sfig_inf

sfig_inf_cumul <- ggplot() +
  
  geom_line(data = fig_data %>% filter(target=='infection_incidence'), aes(x = horizon, y = cum_val, group = simulation_index), alpha = 0.2, linewidth = 0.5, colour='grey30') +
  fs_fig_theme +
  theme(strip.text = element_blank()) +
  facet_grid(~team, scales = "fixed") +
  ylim(0, 50000) +
  labs(x = "Horizon (days)",
       y = "Cumulative modelled infections")

sfig_inf_cumul  

sfig_final <- sfig_inf + sfig_inf_cumul + plot_layout(nrow=2) + plot_annotation(tag_levels = 'A')
sfig_final

ggsave(plot = sfig_final, filename = file.path("visualisation-output", "Plots", "PNG",  "Supp-infections.png"),  height = 17, width = 14, units = "cm")

# hospitalisations
sfig_hosp <- ggplot() +
  
  geom_line(data = fig_data %>% filter(target=='admission_incidence'), aes(x = horizon, y = value, group = simulation_index), alpha = 0.2, linewidth = 0.5, colour='grey30') +
  fs_fig_theme +
  facet_grid(~team, scales = "fixed") +
  ylim(0, 3) +
  labs(x = "",
       y = "Modelled hospitalisations")

sfig_hosp

sfig_hosp_cumul <- ggplot() +
  
  geom_line(data = fig_data %>% filter(target=='admission_incidence'), aes(x = horizon, y = cum_val, group = simulation_index), alpha = 0.2, linewidth = 0.5, colour='grey30') +
  fs_fig_theme +
  theme(strip.text = element_blank()) +
  facet_grid(~team, scales = "fixed") +
  geom_text(data = calibration_target_hospitalisations, aes(x = 364, y = value), colour = "#eb008b", label = "X") + 
  ylim(0, 125) +
  labs(x = "Horizon (days)",
       y = "Cumulative modelled hospitalisations")

sfig_hosp_cumul  

sfig2_final <- sfig_hosp + sfig_hosp_cumul + plot_layout(nrow=2) + plot_annotation(tag_levels = 'A')
sfig2_final

ggsave(plot = sfig_final, filename = file.path("visualisation-output", "Plots", "PNG",  "Supp-hosp.png"),  height = 17, width = 14, units = "cm")


