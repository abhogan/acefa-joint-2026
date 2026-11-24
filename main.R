library(dplyr)
library(tidyr)
library(stringr)
library(arrow)
library(readr)
library(lubridate)
library(zoo)
library(ggplot2)
library(patchwork)
library(ggpubr)
library(grid)
library(cowplot)
library(ggh4x)
library(purrr)

if (!dir.exists(file.path("visualisation-output", "Plots"))) {
  dir.create(file.path("visualisation-output", "Plots"), recursive = TRUE)
  dir.create(file.path("visualisation-output", "Plots", "PNG"))
  dir.create(file.path("visualisation-output", "Plots", "JPEG"))
  dir.create(file.path("visualisation-output", "Plots", "RDS"))
  
}
# devtools::install_github("idem-lab/idpalette")
library(idpalette) # https://github.com/acefa-hubs/idpalette

scenario_palettes <- list()

scenario_palettes$team <- c("UoM"  = "#262261",
                            "UNSW" = "#eb008b")

scenario_palettes$data_type2 <- c("Modelled (median trajectory)" = "#262261",
                                  "Modelled (all)" = "grey",
                                  "Calibration data"  = "#eb008b")

scenario_palettes$team <- c("UoM"  = "#262261",
                            "UNSW" = "#eb008b")


labeller_target <- c("infection_incidence" = "Infections",
                     "disease_incidence"   = "Cases",
                     "admission_incidence" = "Hospitalisations",
                     "fatality_incidence" = "Deaths")

labeller_scenario <- c("Baseline"                = "Status quo", 
                       "Pessimistic_LAIV_5-12yo" = "LAIV 5 to <12 year olds\n(Pessimistic)", 
                       "Central_LAIV_5-12yo"     = "LAIV 5 to <12 year olds\n(Central)",
                       "Optimistic_LAIV_5-12yo"  = "LAIV 5 to <12 year olds\n(Optimistic)",
                       "Pessimistic_LAIV_5-18yo" = "LAIV 5 to <18 year olds\n(Pessimistic)",
                       "Central_LAIV_5-18yo"     = "LAIV 5 to <18 year olds\n(Central)",
                       "Optimistic_LAIV_5-18yo"  = "LAIV 5 to <18 year olds\n(Optimistic)",
                       "Central_LAIV_2-5yo"      = "LAIV 2 to <5 year olds\n(Central)",
                       "Central_LAIV_2-12yo"     = "LAIV 2 to <12 year olds\n(Central)",
                       "Central_LAIV_2-18yo"     = "LAIV 2 to <18 year olds\n(Central)",
                       "CentralLow_LAIV_2-5yo"   = "LAIV 2 to <5 year olds\n(Pessimistic Central)",
                       "CentralLow_LAIV_2-12yo"  = "LAIV 2 to <12 year olds\n(Pessimistic Central)",
                       "CentralLow_LAIV_2-18yo"  = "LAIV 2 to <18 year olds\n(Pessimistic Central)",
                       "CentralLow_LAIV_5-12yo"  = "LAIV 5 to <12 year olds\n(Pessimistic Central)",
                       "CentralLow_LAIV_5-18yo"  = "LAIV 5 to <18 year olds\n(Pessimistic Central)")

labeller_resulttype <- c("change_reduction" = "Averted per 100,000",
                         "perc_reduction"   = "Percentage reduction")

labeller_LAIV <- c("LAIV_2-12yo" = "LAIV 2 to <12 year olds",
                   "LAIV_2-18yo" = "LAIV 2 to <18 year olds",
                   "LAIV_2-5yo" = "LAIV 2 to <5 year olds",
                   "LAIV_5-12yo" = "LAIV 5 to <12 year olds",
                   "LAIV_5-18yo" = "LAIV 5 to <18 year olds")


ACEFA_plot_theme <- theme_bw() +
  theme(legend.text = element_text(size = 12), # you may need to change font sizes as you see fit
        axis.title  = element_text(size = 12),
        strip.text = element_text(size = 9), # change font sizes as you see fit
        legend.position = "bottom", # again change as you need
        #aspect.ratio = 1.1,
        panel.border = element_blank(),          # Removes the full outer border
        axis.line = element_line(color = "black", size = 1), # Adds back only the left and bottom lines,
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank() # use if you need, depends on figure
  )

# FUNCTIONS FOR TABLES

# Make the age column a factors. With joins and reading in the data, this keeps changing to a character
make_age_factor <- function(df){
  df |> mutate(age_group = factor(age_group, levels = c("<1", "1-<5", "5-<12", "12-<18", "18-<65", "65-<80", "80+", "All")))
  
}

# Make the target column a factor in the right order. With reading in the data, this keeps changing to a character
make_target_factor <- function(df){
  df |> mutate(target = factor(target, levels = c("infection_incidence", "disease_incidence", "admission_incidence", "fatality_incidence")))
  
}

# functions #

calculate_percent <- function(df){
  df |>
    group_by(scenario, team, target, age_group) |>
    summarise(median = round(quantile(perc_reduction, 0.5), 1), 
              quant_25 = round(quantile(perc_reduction, 0.025), 1), # 50%
              quant_75 = round(quantile(perc_reduction, 0.75), 1), # 50%
              
    ) |> 
    mutate(value = paste0(median, " (", quant_25, ", ", quant_75, ")")) |>
    select(-median, -quant_25, -quant_75)
}

calculate_absolute <- function(df){
  df |> 
    group_by(scenario, team, target, age_group) |>
    summarise(median = round(quantile(change_reduction, 0.5), 1), 
              quant_25 = round(quantile(change_reduction, 0.025), 1), # 50%
              quant_75 = round(quantile(change_reduction, 0.75), 1), # 50%
              
    ) |> 
    mutate(value = paste0(median, " (", quant_25, ", ", quant_75, ")")) |>
    select(-median, -quant_25, -quant_75)
}

# for infections - no decimal places
calculate_absolute_inf <- function(df){
  df |> 
    group_by(scenario, team, target, age_group) |>
    summarise(median = round(quantile(change_reduction, 0.5), 0), 
              quant_25 = round(quantile(change_reduction, 0.025), 0), # 50%
              quant_75 = round(quantile(change_reduction, 0.75), 0), # 50%
              
    ) |> 
    mutate(value = paste0(median, " (", quant_25, ", ", quant_75, ")")) |>
    select(-median, -quant_25, -quant_75)
}

# for deaths - per million
calculate_absolute_death <- function(df){
  df |> 
    group_by(scenario, team, target, age_group) |>
    summarise(median = round(quantile(change_reduction*10, 0.5), 1), 
              quant_25 = round(quantile(change_reduction*10, 0.025), 1), # 50%
              quant_75 = round(quantile(change_reduction*10, 0.75), 1), # 50%
              
    ) |> 
    mutate(value = paste0(median, " (", quant_25, ", ", quant_75, ")")) |>
    select(-median, -quant_25, -quant_75)
}

create_percent_table_age <- function(df, filterlist = c("Central_")){
  df |>
    filter(map_lgl(scenario, ~ any(str_starts(.x, filterlist)))) %>%
    ungroup() %>%
    select(scenario, team, age_group, value) %>%
    ungroup() %>%
    pivot_wider(names_from = age_group, values_from = value) %>%
    arrange(team) %>%
    mutate(scenario = str_replace(scenario, "Central_", "")) %>%
    mutate(scenario = factor(scenario, levels = c("LAIV_5-12yo", "LAIV_5-18yo", "LAIV_2-5yo","LAIV_2-12yo", "LAIV_2-18yo"))) %>%
    arrange(team, scenario) %>%
    mutate(scenario = str_replace(scenario, "LAIV_2-5yo", "LAIV 2--$<$5")) %>%
    mutate(scenario = str_replace(scenario, "LAIV_5-12yo", "LAIV 5--$<$12")) %>%
    mutate(scenario = str_replace(scenario, "LAIV_2-12yo", "LAIV 2--$<$12")) %>%
    mutate(scenario = str_replace(scenario, "LAIV_5-18yo", "LAIV 5--$<$18")) %>%
    mutate(scenario = str_replace(scenario, "LAIV_2-18yo", "LAIV 2--$<$18")) %>%
    mutate(type = "percent")
  
}

create_absolute_table_age <- function(df, filterlist = c("Central_")){
  df |> filter(map_lgl(scenario, ~ any(str_starts(.x, filterlist)))) %>%
    ungroup() %>%
    select(scenario, team, age_group, value) %>%
    ungroup() %>%
    pivot_wider(names_from = age_group, values_from = value) %>%
    arrange(team) %>%
    mutate(scenario = str_replace(scenario, "Central_", "")) %>%
    mutate(scenario = factor(scenario, levels = c("LAIV_5-12yo", "LAIV_5-18yo", "LAIV_2-5yo","LAIV_2-12yo", "LAIV_2-18yo"))) %>%
    arrange(team, scenario) %>%
    mutate(scenario = str_replace(scenario, "LAIV_2-5yo", "LAIV 2--$<$5")) %>%
    mutate(scenario = str_replace(scenario, "LAIV_5-12yo", "LAIV 5--$<$12")) %>%
    mutate(scenario = str_replace(scenario, "LAIV_2-12yo", "LAIV 2--$<$12")) %>%
    mutate(scenario = str_replace(scenario, "LAIV_5-18yo", "LAIV 5--$<$18")) %>%
    mutate(scenario = str_replace(scenario, "LAIV_2-18yo", "LAIV 2--$<$18")) %>%
    mutate(type = "absolute")
}

###########################################
# produce Figure 2
source("produce_outputs/figure2.R")

# produce Figure 3, Figure S6
source("produce_outputs/figure3.R")

# produce Figure 4, Figures S7-S10
source("produce_outputs/figure4.R")

# produce Tables
source("produce_outputs/tables.R")
