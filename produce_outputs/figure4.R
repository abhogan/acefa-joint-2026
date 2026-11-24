
######################################.
##  Relabelling figure panel titles ## -----
######################################.
labeller_target <- c("infection_incidence" = "Infections",
                     "disease_incidence"   = "Cases",
                     "admission_incidence" = "Hospitalisations",
                     "fatality_incidence" = "Deaths")

labeller_scenario <- c("Baseline"                = "Status quo", 
                       "Pessimistic_LAIV_5-12yo" = "LAIV 5 to <12 years\n(Pessimistic)", 
                       "Central_LAIV_5-12yo"     = "LAIV 5 to <12 years\n(Central)",
                       "Optimistic_LAIV_5-12yo"  = "LAIV 5 to <12 years\n(Optimistic)",
                       "Pessimistic_LAIV_5-18yo" = "LAIV 5 to <18 years\n(Pessimistic)",
                       "Central_LAIV_5-18yo"     = "LAIV 5 to <18 years\n(Central)",
                       "Optimistic_LAIV_5-18yo"  = "LAIV 5 to <18 years\n(Optimistic)",
                       "Central_LAIV_2-5yo"      = "LAIV 2 to <5 years\n(Central)",
                       "Central_LAIV_2-12yo"     = "LAIV 2 to <12 years\n(Central)",
                       "Central_LAIV_2-18yo"     = "LAIV 2 to <18 years\n(Central)",
                       "CentralLow_LAIV_2-5yo"   = "LAIV 2 to <5 years\n(Pessimistic-Central)",
                       "CentralLow_LAIV_2-12yo"  = "LAIV 2 to <12 yeas\n(Pessimistic-Central)",
                       "CentralLow_LAIV_2-18yo"  = "LAIV 2 to <18 years\n(Pessimistic-Central)",
                       "CentralLow_LAIV_5-12yo"  = "LAIV 5 to <12 years\n(Pessimistic-Central)",
                       "CentralLow_LAIV_5-18yo"  = "LAIV 5 to <18 years\n(Pessimistic-Central)")

labeller_resulttype <- c("change_reduction" = "Averted per 100,000",
                         "perc_reduction"   = "Percentage reduction")

labeller_LAIV <- c("LAIV_2-12yo" = "LAIV 2 to <12 years",
                   "LAIV_2-18yo" = "LAIV 2 to <18 years",
                   "LAIV_2-5yo" = "LAIV 2 to <5 years",
                   "LAIV_5-12yo" = "LAIV 5 to <12 years",
                   "LAIV_5-18yo" = "LAIV 5 to <18 years")

################################################################################.
#                                                                              #.
#                            Age-specific results                              # ------ 
#                                                                              #.
################################################################################.

fig_averted_hosp_unsw <- read.csv(file.path("visualisation-data", "fig_avrtbyage_hosp_data_unsw.csv"))
fig_averted_hosp_uom <- read.csv(file.path("visualisation-data", "fig_avrtbyage_hosp_data_uom.csv"))

# for UoM, the 2-5yo scenario for Pessimistic Central is the same as the Central scenario, so we will replicate the Central scenario for the Pessimistic Central scenario to make the plot look better
fig_averted_hosp_uom_temp <- fig_averted_hosp_uom |>
  filter(scenario == "Central_LAIV_2-5yo" & team == "UoM") |>
  mutate(scenario = "CentralLow_LAIV_2-5yo")

fig_averted_hosp_uom <- rbind(fig_averted_hosp_uom_temp, fig_averted_hosp_uom) |>
  arrange(team == "UNSW") 

# Combine UNSW and UoM
fig_averted_hosp_main <- fig_averted_hosp_unsw |>
  bind_rows(fig_averted_hosp_uom) |>
  arrange(team == "UNSW") # forces UNSW to be plotted on top because it has much less variation

# Find the median to add as a line to the plot
fig_averted_hosp_summary_main <- fig_averted_hosp_main |>
  group_by(scenario, target, age_group, team) |>
  summarise(
    median_change_reduction = median(change_reduction),
    median_perc_reduction = median(perc_reduction),
    max_reduction_count = max(change_reduction),
    max_increase_count = min(change_reduction),
    mean_change = mean(change_reduction), 
    .groups = "drop"
  )

write.csv(fig_averted_hosp_summary_main, file.path("visualisation-output", "Data summaries", "Averted_hospitalisations_summary.csv"), row.names = FALSE)

fig_averted_fatal_unsw <- read.csv(file.path("visualisation-data", "fig_avrtbyage_fatal_data_unsw.csv"))
fig_averted_fatal_uom <- read.csv(file.path("visualisation-data", "fig_avrtbyage_fatal_data_uom.csv"))

fig_averted_fatal_uom_temp <- fig_averted_fatal_uom |>
  filter(scenario == "Central_LAIV_2-5yo" & team == "UoM") |>
  mutate(scenario = "CentralLow_LAIV_2-5yo")

fig_averted_fatal_uom <- rbind(fig_averted_fatal_uom_temp, fig_averted_fatal_uom) |>
  arrange(team == "UNSW") 

# Combine UNSW and UoM
fig_averted_fatal_main <- fig_averted_fatal_unsw |>
  bind_rows(fig_averted_fatal_uom) |>
  arrange(team == "UNSW") # forces UNSW to be plotted on top because it has much less variation

# Find the median to add as a line to the plot
fig_averted_fatal_summary_main <- fig_averted_fatal_main |>
  group_by(scenario, target, age_group, team) |>
  summarise(
    median_change_reduction = median(change_reduction),
    median_perc_reduction = median(perc_reduction),
    max_reduction_count = max(change_reduction),
    max_increase_count = min(change_reduction),
    mean_change = mean(change_reduction), 
    .groups = "drop"
  )

write.csv(fig_averted_fatal_summary_main, file.path("visualisation-output", "Data summaries", "Averted_deaths_summary.csv"), row.names = FALSE)

fig_averted_inf_unsw <- read.csv(file.path("visualisation-data", "fig_avertedbyage_infct_unsw.csv"))
fig_averted_inf_uom <- read.csv(file.path("visualisation-data", "fig_avertedbyage_infct_uom.csv"))

# for UoM, the 2-5yo scenario for Pessimistic Central is the same as the Central scenario, so we will replicate the Central scenario for the Pessimistic Central scenario to make the plot look better
fig_averted_inf_uom_temp <- fig_averted_inf_uom |>
  filter(scenario == "Central_LAIV_2-5yo" & team == "UoM") |>
  mutate(scenario = "CentralLow_LAIV_2-5yo")

fig_averted_inf_uom <- rbind(fig_averted_inf_uom_temp, fig_averted_inf_uom) |>
  arrange(team == "UNSW") 

# Combine UNSW and UoM
fig_averted_inf_main <- fig_averted_inf_unsw |>
  bind_rows(fig_averted_inf_uom) |>
  arrange(team == "UNSW") # forces UNSW to be plotted on top because it has much less variation

# Find the median to add as a line to the plot
fig_averted_inf_summary_main <- fig_averted_inf_main |>
  group_by(scenario, target, age_group, team) |>
  summarise(
    median_change_reduction = median(change_reduction),
    median_perc_reduction = median(perc_reduction),
    max_reduction_count = max(change_reduction),
    max_increase_count = min(change_reduction),
    mean_change = mean(change_reduction), 
    .groups = "drop"
  )

write.csv(fig_averted_inf_summary_main, file.path("visualisation-output", "Data summaries", "Averted_infections_summary.csv"), row.names = FALSE)

fig_averted_cases_unsw <- read.csv(file.path("visualisation-data", "fig_avertedbyage_cases_unsw.csv"))
fig_averted_cases_uom <- read.csv(file.path("visualisation-data", "fig_avertedbyage_cases_uom.csv"))

# for UoM, the 2-5yo scenario for Pessimistic Central is the same as the Central scenario, so we will replicate the Central scenario for the Pessimistic Central scenario to make the plot look better
fig_averted_cases_uom_temp <- fig_averted_inf_uom |>
  filter(scenario == "Central_LAIV_2-5yo" & team == "UoM") |>
  mutate(scenario = "CentralLow_LAIV_2-5yo")

fig_averted_cases_uom <- rbind(fig_averted_cases_uom_temp, fig_averted_cases_uom) |>
  arrange(team == "UNSW") 

# Combine UNSW and UoM
fig_averted_cases_main <- fig_averted_cases_unsw |>
  bind_rows(fig_averted_cases_uom) |>
  arrange(team == "UNSW") # forces UNSW to be plotted on top because it has much less variation

# Find the median to add as a line to the plot
fig_averted_cases_summary_main <- fig_averted_cases_main |>
  group_by(scenario, target, age_group, team) |>
  summarise(
    median_change_reduction = median(change_reduction),
    median_perc_reduction = median(perc_reduction),
    max_reduction_count = max(change_reduction),
    max_increase_count = min(change_reduction),
    mean_change = mean(change_reduction), 
    .groups = "drop"
  )

write.csv(fig_averted_cases_summary_main, file.path("visualisation-output", "Data summaries", "Averted_cases_summary.csv"), row.names = FALSE)

###############################.
#### Plot school-based #### --
###############################
output_scen <- c("Central")

for (i in 1){
fig_averted_hosp <- fig_averted_hosp_main |>
  filter(scenario %in% c(paste0(output_scen[i], "_LAIV_5-12yo"), paste0(output_scen[i], "_LAIV_5-18yo"))
  ) |>
  mutate(age_group = factor(age_group, levels = c("<1", "1-<5", "5-<12", "12-<18", "18-<65", "65-<80", "80+", "All"))
) 
  
fig_averted_hosp_summary<- fig_averted_hosp_summary_main |>
  filter(scenario %in% c(paste0(output_scen[i], "_LAIV_5-12yo"), paste0(output_scen[i], "_LAIV_5-18yo"))
  ) |>
  mutate(age_group = factor(age_group, levels = c("<1", "1-<5", "5-<12", "12-<18", "18-<65", "65-<80", "80+", "All"))
  )

p_averted_hosp <- ggplot() +
  
  # Add solid lines - at 0 and to separate the all group
  geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
  geom_vline(xintercept = 7.55, colour = "white", linewidth = 1.5) +
  geom_vline(xintercept = 7.55, colour = "grey80", linewidth = 0.7) +
  
  # Plot the data in grey
  geom_point(data = fig_averted_hosp,
             aes(x = age_group, y = change_reduction, alpha = team), colour = "grey",
             shape = 16, size = 1,
             position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6), show.legend = FALSE) +
  
  # Use the medians to get the legend without being displayed on the plot
  geom_errorbar(data = fig_averted_hosp_summary,
                aes(x = age_group, ymin = median_change_reduction, ymax = median_change_reduction, group = team, colour = team),
                width = 0.6, alpha = 0.8, linewidth = 0.5,
                position = position_dodge(width = 0.6), show.legend = c(group = FALSE)) +
  
  # Adjust the colours and opacity
  scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
  scale_colour_manual(values = scenario_palettes$team) +
  
  # Fix the layout, labels, and theme
  facet_wrap(~scenario, drop = FALSE, labeller = labeller(scenario = labeller_scenario), nrow = 1) +
  ACEFA_plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
  labs(x = "Age groups", y = "Absolute reduction in\nhospitalisations (per 100,000)", colour = "Team")

p1 <- p_averted_hosp+ ylim(c(-10, 200))
p1 <- p_averted_hosp

###################################.
#### Plot percentage #### --
###################################.

p_averted_perc_hosp <- ggplot() +
  
  # Add solid lines - at 0 and to separate the all group
  geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
  geom_vline(xintercept = 7.55, colour = "white", linewidth = 1.5) +
  geom_vline(xintercept = 7.55, colour = "grey80", linewidth = 0.7) +
  
  # Plot the data in grey
  geom_point(data = fig_averted_hosp,
             aes(x = age_group, y = perc_reduction, alpha = team), colour = "grey",
             shape = 16, size = 1,
             position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6), show.legend = FALSE) +
  
  # Median error bars
  geom_errorbar(data = fig_averted_hosp_summary,
                aes(x = age_group, ymin = median_perc_reduction, ymax = median_perc_reduction, group = team, colour = team),
                width = 0.6, alpha = 1, linewidth = 0.5,
                position = position_dodge(width = 0.6), show.legend = c(group = FALSE)) +
  
  # Manual adjustment of the colours and opacity
  scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
  scale_colour_manual(values = scenario_palettes$team) +
  scale_y_continuous(breaks = c(0, 25,50, 75,100), labels = c(0, 25,50, 75,100)) + 
  
  # Adjust the theme, legend, layout, labels
  facet_wrap(~scenario, drop = FALSE, labeller = labeller(scenario = labeller_scenario), nrow = 1) +
  ACEFA_plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
  labs(x = "Age groups", y = "Percentage reduction in\nhospitalisations", colour = "Team")

p2 <- p_averted_perc_hosp
p2

p1 <- p1 +
  theme(
    legend.text = element_text(size = 10),    # Shrinks legend item text
    legend.title = element_text(size = 10),   # Shrinks legend title text
    legend.position="bottom"
  )

extracted_legendA <- get_legend(p1)

p2 <- p2 + theme(legend.position = "none") + labs(x = NULL) +
  theme(panel.border = element_blank(),          # Removes the full outer border
        axis.line = element_line(color = "black"), # Adds back only the left and bottom lines,
        strip.background = element_blank(),
        axis.text.y = element_text(size = 8.5),
        axis.title = element_text(size = 9.5),
        axis.text.x = element_blank(),
        strip.text.x = element_text(size=9.5),
        panel.grid.minor.y=element_blank()
  )
p1 <- p1 + theme(legend.position = "none") + labs(x = NULL)+
  theme(panel.border = element_blank(),          # Removes the full outer border
        axis.line = element_line(color = "black"), # Adds back only the left and bottom lines,
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size = 8.5),
        axis.text.y = element_text(size = 8.5),
        axis.title = element_text(size = 9.5),
        panel.grid.minor.y=element_blank()
        
  )

Fig4_hosp_noleg <- ggarrange(p2, p1, nrow = 2, heights = c(1,0.9),  align = "v")
Fig4_hosp_leg <- ggarrange(p2, p1, extracted_legendA, nrow = 3, heights = c(1,1.1, 0.1),  align = "v")

# add a plot title
Fig4_hosp_leg2 <- annotate_figure(Fig4_hosp_leg,
                                 top = textGrob(paste0("Age-specific impact"),
                                                
                                                x = unit(0.18, "npc"),
                                                y = unit(0.6, "npc"),
                                                gp = gpar(cex = 1, fontface = "bold")),
                                 bottom = textGrob("", gp = gpar(cex = 1)))

Fig4_hosp_leg2

saveRDS(Fig4_hosp_noleg, file = file.path("visualisation-output", "Plots", "RDS",  paste0("Fig4_hosp_",output_scen[i],"_nolegend.rds")))
saveRDS(Fig4_hosp_leg2, file = file.path("visualisation-output", "Plots", "RDS",  paste0("Fig4_hosp_",output_scen[i],".rds")))
ggsave(file.path("visualisation-output", "Plots", "PNG",  paste0("Fig4_hosp_",output_scen[i],".png")), device = "png", height = 12, width = 15, units = "cm")


}
# save the legend as an RDS
saveRDS(extracted_legendA, file = file.path("visualisation-output", "Plots", "RDS",  paste0("Fig4_legend.rds")))

####################################
# Plot the preschool scenarios
######################################
output_scen <- c("Central")
output_scen_label <- c("Central")

for (i in 1){
  fig_averted_hosp <- fig_averted_hosp_main |>
    filter(scenario %in% c(paste0(output_scen[i], "_LAIV_2-5yo"), paste0(output_scen[i], "_LAIV_2-12yo"), paste0(output_scen[i], "_LAIV_2-18yo"))
    ) |>
    mutate(age_group = factor(age_group, levels = c("<1", "1-<5", "5-<12", "12-<18", "18-<65", "65-<80", "80+", "All"))
    )|>
    mutate(scenario = factor(scenario, levels = c(paste0(output_scen[i], "_LAIV_2-5yo"), paste0(output_scen[i], "_LAIV_2-12yo"), paste0(output_scen[i], "_LAIV_2-18yo")),
                             labels = c(paste0("LAIV 2 to 5 years\n (", output_scen_label[i], ")"),
                                        paste0("LAIV 2 to 12 years\n (", output_scen_label[i], ")"),
                                        paste0("LAIV 2 to 18 years\n (",output_scen_label[i], ")"))))
  
  fig_averted_hosp_summary<- fig_averted_hosp_summary_main |>
    filter(scenario %in% c(paste0(output_scen[i], "_LAIV_2-5yo"), paste0(output_scen[i], "_LAIV_2-12yo"), paste0(output_scen[i], "_LAIV_2-18yo"))
    ) |>
    mutate(age_group = factor(age_group, levels = c("<1", "1-<5", "5-<12", "12-<18", "18-<65", "65-<80", "80+", "All"))
    )|>
    mutate(scenario = factor(scenario, levels = c(paste0(output_scen[i], "_LAIV_2-5yo"), paste0(output_scen[i], "_LAIV_2-12yo"), paste0(output_scen[i], "_LAIV_2-18yo")),
                             labels = c(paste0("LAIV 2 to 5 years\n (", output_scen_label[i], ")"),
                                        paste0("LAIV 2 to 12 years\n (", output_scen_label[i], ")"),
                                        paste0("LAIV 2 to 18 years\n (",output_scen_label[i], ")"))))
  
  p_averted_hosp <- ggplot() +
    
    # Add solid lines - at 0 and to separate the all group
    geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
    geom_vline(xintercept = 7.55, colour = "white", linewidth = 1.5) +
    geom_vline(xintercept = 7.55, colour = "grey80", linewidth = 0.7) +
    
    # Plot the data in grey
    geom_point(data = fig_averted_hosp,
               aes(x = age_group, y = change_reduction, alpha = team), colour = "grey",
               shape = 16, size = 1,
               position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6), show.legend = FALSE) +
    
    # Use the medians to get the legend without being displayed on the plot
    geom_errorbar(data = fig_averted_hosp_summary,
                  aes(x = age_group, ymin = median_change_reduction, ymax = median_change_reduction, group = team, colour = team),
                  width = 0.6, alpha = 0.8, linewidth = 0.5,
                  position = position_dodge(width = 0.6), show.legend = c(group = FALSE)) +
    
    # Adjust the colours and opacity
    scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
    scale_colour_manual(values = scenario_palettes$team) +
    
    # Fix the layout, labels, and theme
    facet_wrap(~scenario, drop = FALSE, labeller = labeller(scenario = labeller_scenario), nrow = 1) +
    ACEFA_plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
    labs(x = "Age groups", y = "Absolute reduction in \n hospitalisations (per 100,000)", colour = "Team")
  
  p1 <- p_averted_hosp+ ylim(c(-10, 200))
  p1 <- p_averted_hosp
  
  ###################################.
  #### Plot percentage #### --
  ###################################.
  
  p_averted_perc_hosp <- ggplot() +
    
    # Add solid lines - at 0 and to separate the all group
    geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
    geom_vline(xintercept = 7.55, colour = "white", linewidth = 1.5) +
    geom_vline(xintercept = 7.55, colour = "grey80", linewidth = 0.7) +
    
    # Plot the data in grey
    geom_point(data = fig_averted_hosp,
               aes(x = age_group, y = perc_reduction, alpha = team), colour = "grey",
               shape = 16, size = 1,
               position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6), show.legend = FALSE) +
    
    # Median error bars
    geom_errorbar(data = fig_averted_hosp_summary,
                  aes(x = age_group, ymin = median_perc_reduction, ymax = median_perc_reduction, group = team, colour = team),
                  width = 0.6, alpha = 1, linewidth = 0.5,
                  position = position_dodge(width = 0.6), show.legend = c(group = FALSE)) +
    
    # Manual adjustment of the colours and opacity
    scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
    scale_colour_manual(values = scenario_palettes$team) +
    scale_y_continuous(breaks = c(0, 25,50, 75,100), labels = c(0, 25,50, 75,100)) + 
    
    # Adjust the theme, legend, layout, labels
    facet_wrap(~scenario, drop = FALSE, labeller = labeller(scenario = labeller_scenario), nrow = 1) +
    ACEFA_plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
    labs(x = "Age groups", y = "Percentage reduction in\nhospitalisations", colour = "Team")
  
  p2 <- p_averted_perc_hosp
  p2
  
  p1 <- p1 +
    theme(
      legend.text = element_text(size = 10),    # Shrinks legend item text
      legend.title = element_text(size = 10),   # Shrinks legend title text
      legend.position="bottom"
    )
  
  extracted_legendA <- get_legend(p1)
  
  p2 <- p2 + theme(legend.position = "none") + labs(x = NULL) +
    theme(panel.border = element_blank(),          # Removes the full outer border
          axis.line = element_line(color = "black"), # Adds back only the left and bottom lines,
          strip.background = element_blank(),
          #axis.text.x = element_text(size = 8.5),
          axis.title = element_text(size = 9.5),
          axis.text.x=element_blank(),
          strip.text.x=element_text(size = 9.5),
          panel.grid.minor.y=element_blank()
    )
  p1 <- p1 + theme(legend.position = "none") + labs(x = "Age group (years)")+
    theme(panel.border = element_blank(),          # Removes the full outer border
          axis.line = element_line(color = "black"), # Adds back only the left and bottom lines,
          strip.background = element_blank(),
          axis.text.x = element_text(size = 8.5),
          axis.text.y = element_text(size = 8.5),
          axis.title = element_text(size = 9.5),
          strip.text.x = element_blank(),
          panel.grid.minor.y=element_blank()
    )
  
  
  Fig4_hosp <- ggarrange(ggarrange(p2, p1, align = "v",nrow = 2, heights = c(1,1.1)), extracted_legendA, ncol = 1, nrow = 2, heights = c(10,1))
  Fig4_hosp
  
  # add a plot title
  Fig4_hosp2 <- annotate_figure(Fig4_hosp,
                                    top = textGrob(paste0("Age-specific impact: preschool scenarios"),
                                                   
                                                   x = unit(0.35, "npc"),
                                                   y = unit(0.6, "npc"),
                                                   gp = gpar(cex = 1, fontface = "bold")),
                                    bottom = textGrob("", gp = gpar(cex = 1)))
  
  Fig4_hosp2
  
  
  ggsave(plot = Fig4_hosp2, filename = file.path("visualisation-output", "Plots", "PNG",  paste0("Fig4_hosp_preschool_",output_scen[i],".png")),  height = 12, width = 14, units = "cm", dpi = 300)
  
  
  # save a version of each without the legend, as an rds file, so can combine with deaths panel
  Fig4_hosp_noleg <- ggarrange(p2, p1, heights = c(1,1),  align = "v", nrow = 2)
  saveRDS(Fig4_hosp_noleg, file = file.path("visualisation-output", "Plots", "RDS",  paste0("Fig4_hosp_preschool_",output_scen[i],"_nolegend.rds")))
  }

################################################################################.
## DEATHS ## ------ 
################################################################################.

###############################.
#### Plot school-based #### --
###############################
output_scen <- c("Central")

for (i in 1){
  fig_averted_fatal <- fig_averted_fatal_main |>
    filter(scenario %in% c(paste0(output_scen[i], "_LAIV_5-12yo"), paste0(output_scen[i], "_LAIV_5-18yo"))
    ) |>
    mutate(age_group = factor(age_group, levels = c("<1", "1-<5", "5-<12", "12-<18", "18-<65", "65-<80", "80+", "All"))
    )
  
  fig_averted_fatal_summary<- fig_averted_fatal_summary_main |>
    filter(scenario %in% c(paste0(output_scen[i], "_LAIV_5-12yo"), paste0(output_scen[i], "_LAIV_5-18yo"))
    ) |>
    mutate(age_group = factor(age_group, levels = c("<1", "1-<5", "5-<12", "12-<18", "18-<65", "65-<80", "80+", "All"))
    )
  
  p_averted_fatal <- ggplot() +
    
    # Add solid lines - at 0 and to separate the all group
    geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
    geom_vline(xintercept = 7.55, colour = "white", linewidth = 1.5) +
    geom_vline(xintercept = 7.55, colour = "grey80", linewidth = 0.7) +
    
    # Plot the data in grey
    geom_point(data = fig_averted_fatal,
               aes(x = age_group, y = change_reduction*10, alpha = team), colour = "grey",
               shape = 16, size = 1,
               position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6), show.legend = FALSE) +
    
    # Use the medians to get the legend without being displayed on the plot
    geom_errorbar(data = fig_averted_fatal_summary,
                  aes(x = age_group, ymin = median_change_reduction*10, ymax = median_change_reduction*10, group = team, colour = team),
                  width = 0.6, alpha = 0.8, linewidth = 0.5,
                  position = position_dodge(width = 0.6), show.legend = c(group = FALSE)) +
    
    # Adjust the colours and opacity
    scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
    scale_colour_manual(values = scenario_palettes$team) +
    
    # Fix the layout, labels, and theme
    facet_wrap(~scenario, drop = FALSE, labeller = labeller(scenario = labeller_scenario), nrow = 1) +
    ACEFA_plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
    labs(x = "Age groups", y = "Absolute reduction in\ndeaths (per million)", colour = "Team")
  
  p1 <- p_averted_fatal#+ ylim(c(-4, 50))

    ###################################.
  #### Plot percentage #### --
  ###################################.
  
  p_averted_perc_fatal <- ggplot() +
    
    # Add solid lines - at 0 and to separate the all group
    geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
    geom_vline(xintercept = 7.55, colour = "white", linewidth = 1.5) +
    geom_vline(xintercept = 7.55, colour = "grey80", linewidth = 0.7) +
    
    # Plot the data in grey
    geom_point(data = fig_averted_fatal,
               aes(x = age_group, y = perc_reduction, alpha = team), colour = "grey",
               shape = 16, size = 1,
               position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6), show.legend = FALSE) +
    
    # Median error bars
    geom_errorbar(data = fig_averted_fatal_summary,
                  aes(x = age_group, ymin = median_perc_reduction, ymax = median_perc_reduction, group = team, colour = team),
                  width = 0.6, alpha = 1, linewidth = 0.5,
                  position = position_dodge(width = 0.6), show.legend = c(group = FALSE)) +
    
    # Manual adjustment of the colours and opacity
    scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
    scale_colour_manual(values = scenario_palettes$team) +
    scale_y_continuous(breaks = c(0, 25,50, 75,100), labels = c(0, 25,50, 75,100)) + 
    
    # Adjust the theme, legend, layout, labels
    facet_wrap(~scenario, drop = FALSE, labeller = labeller(scenario = labeller_scenario), nrow = 1) +
    ACEFA_plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
    labs(x = "Age groups", y = "Percentage reduction in\ndeaths", colour = "Team")
  
  p2 <- p_averted_perc_fatal
  p2
  
  p1 <- p1 +
    theme(
      legend.text = element_text(size = 10),    # Shrinks legend item text
      legend.title = element_text(size = 10),   # Shrinks legend title text
      legend.position="bottom"
    )

  
  extracted_legendA <- get_legend(p1)
  
  p2 <- p2 + theme(legend.position = "none") + labs(x = NULL) +
    theme(panel.border = element_blank(),          # Removes the full outer border
          axis.line = element_line(color = "black"), # Adds back only the left and bottom lines,
          strip.background = element_blank(),
          axis.text.x = element_blank(),
          strip.text.x=element_text(size = 9.5),
          axis.text.y = element_text(size = 8.5),
          axis.title = element_text(size = 9.5),
          panel.grid.minor.y=element_blank()
          
    )
  p1 <- p1 + theme(legend.position = "none") + labs(x = "Age group (years)")+
    theme(panel.border = element_blank(),          # Removes the full outer border
          axis.line = element_line(color = "black"), # Adds back only the left and bottom lines,
          strip.background = element_blank(),
          axis.text.x = element_text(size = 8.5),
          strip.text.x=element_blank(),
          axis.text.y = element_text(size = 8.5),
          axis.title = element_text(size = 9.5),
          panel.grid.minor.y=element_blank()
    )
  
  # save a version of each without the legend, as an rds file, so can combine with deaths panel
  Fig4_fatal_noleg <- ggarrange(p2, p1, nrow = 2, heights = c(1,1.1),  align = "v")
  saveRDS(Fig4_fatal_noleg, file = file.path("visualisation-output", "Plots", "RDS",  paste0("Fig4_fatal_",output_scen[i],"_nolegend.rds")))
  
  Fig4_fatal_leg <- ggarrange(p2, p1, extracted_legendA, nrow = 3, heights = c(1,1.1, 0.1),  align = "v")
  
  # add a plot title
  Fig4_fatal_leg2 <- annotate_figure(Fig4_fatal_leg,
                                    top = textGrob(paste0("Age-specific impact"),
                                                   
                                                   x = unit(0.18, "npc"),
                                                   y = unit(0.6, "npc"),
                                                   gp = gpar(cex = 1, fontface = "bold")),
                                    bottom = textGrob("", gp = gpar(cex = 1)))
  
  Fig4_fatal_leg2
  
  
  
  ggsave(file.path("visualisation-output", "Plots", "PNG",  paste0("Fig4_fatal_",output_scen[i],".png")), device = "png", height = 12, width = 15, units = "cm")
  
}

####################################
# Plot the preschool scenarios
######################################
output_scen <- c("Central")

for (i in 1){
  fig_averted_fatal <- fig_averted_fatal_main |>
    filter(scenario %in% c(paste0(output_scen[i], "_LAIV_2-5yo"), paste0(output_scen[i], "_LAIV_2-12yo"), paste0(output_scen[i], "_LAIV_2-18yo"))
    ) |>
    mutate(age_group = factor(age_group, levels = c("<1", "1-<5", "5-<12", "12-<18", "18-<65", "65-<80", "80+", "All"))
    )|>
    mutate(scenario = factor(scenario, levels = c(paste0(output_scen[i], "_LAIV_2-5yo"), paste0(output_scen[i], "_LAIV_2-12yo"), paste0(output_scen[i], "_LAIV_2-18yo")),
                             labels = c(paste0(output_scen[i], " LAIV 2-5yo"), paste0(output_scen[i], " LAIV 2-12yo"), paste0(output_scen[i], " LAIV 2-18yo"))))
  
  fig_averted_fatal_summary<- fig_averted_fatal_summary_main |>
    filter(scenario %in% c(paste0(output_scen[i], "_LAIV_2-5yo"), paste0(output_scen[i], "_LAIV_2-12yo"), paste0(output_scen[i], "_LAIV_2-18yo"))
    ) |>
    mutate(age_group = factor(age_group, levels = c("<1", "1-<5", "5-<12", "12-<18", "18-<65", "65-<80", "80+", "All"))
    )|>
    mutate(scenario = factor(scenario, levels = c(paste0(output_scen[i], "_LAIV_2-5yo"), paste0(output_scen[i], "_LAIV_2-12yo"), paste0(output_scen[i], "_LAIV_2-18yo")),
                             labels = c(paste0(output_scen[i], " LAIV 2-5yo"), paste0(output_scen[i], " LAIV 2-12yo"), paste0(output_scen[i], " LAIV 2-18yo"))))
  
  p_averted_fatal <- ggplot() +
    
    # Add solid lines - at 0 and to separate the all group
    geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
    geom_vline(xintercept = 7.55, colour = "white", linewidth = 1.5) +
    geom_vline(xintercept = 7.55, colour = "grey80", linewidth = 0.7) +
    
    # Plot the data in grey
    geom_point(data = fig_averted_fatal,
               aes(x = age_group, y = change_reduction, alpha = team), colour = "grey",
               shape = 16, size = 1,
               position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6), show.legend = FALSE) +
    
    # Use the medians to get the legend without being displayed on the plot
    geom_errorbar(data = fig_averted_fatal_summary,
                  aes(x = age_group, ymin = median_change_reduction, ymax = median_change_reduction, group = team, colour = team),
                  width = 0.6, alpha = 0.8, linewidth = 0.5,
                  position = position_dodge(width = 0.6), show.legend = c(group = FALSE)) +
    
    # Adjust the colours and opacity
    scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
    scale_colour_manual(values = scenario_palettes$team) +
    
    # Fix the layout, labels, and theme
    facet_wrap(~scenario, drop = FALSE, labeller = labeller(scenario = labeller_scenario), nrow = 1) +
    ACEFA_plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
    labs(x = "Age groups", y = "Absolute reduction in\ndeaths (per 100,000)", colour = "Team")
  
  p1 <- p_averted_fatal#+ ylim(c(-0.4, 10))
  
  ###################################.
  #### Plot percentage #### --
  ###################################.
  
  p_averted_perc_fatal <- ggplot() +
    
    # Add solid lines - at 0 and to separate the all group
    geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
    geom_vline(xintercept = 7.55, colour = "white", linewidth = 1.5) +
    geom_vline(xintercept = 7.55, colour = "grey80", linewidth = 0.7) +
    
    # Plot the data in grey
    geom_point(data = fig_averted_fatal,
               aes(x = age_group, y = perc_reduction, alpha = team), colour = "grey",
               shape = 16, size = 1,
               position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6), show.legend = FALSE) +
    
    # Median error bars
    geom_errorbar(data = fig_averted_fatal_summary,
                  aes(x = age_group, ymin = median_perc_reduction, ymax = median_perc_reduction, group = team, colour = team),
                  width = 0.6, alpha = 1, linewidth = 0.5,
                  position = position_dodge(width = 0.6), show.legend = c(group = FALSE)) +
    
    # Manual adjustment of the colours and opacity
    scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
    scale_colour_manual(values = scenario_palettes$team) +
    scale_y_continuous(breaks = c(0, 25,50, 75,100), labels = c(0, 25,50, 75,100)) + 
    
    # Adjust the theme, legend, layout, labels
    facet_wrap(~scenario, drop = FALSE, labeller = labeller(scenario = labeller_scenario), nrow = 1) +
    ACEFA_plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
    labs(x = "Age groups", y = "Percentage reduction in\ndeaths", colour = "Team")
  
  p2 <- p_averted_perc_fatal
  p2
  
  p1 <- p1 +
    theme(
      legend.text = element_text(size = 10),    # Shrinks legend item text
      legend.title = element_text(size = 10),   # Shrinks legend title text
      legend.position="bottom"
    )
  
  extracted_legendA <- get_legend(p1)
  
  p2 <- p2 + theme(legend.position = "none") + labs(x = NULL) +
    theme(panel.border = element_blank(),          # Removes the full outer border
          axis.line = element_line(color = "black"), # Adds back only the left and bottom lines,
          strip.background = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_text(size = 9.5),
          strip.text.x=element_text(size=9.5),
          panel.grid.minor.y=element_blank()
          
    )
  p1 <- p1 + theme(legend.position = "none") + labs(x = "Age group (years)")+
    theme(panel.border = element_blank(),          # Removes the full outer border
          axis.line = element_line(color = "black"), # Adds back only the left and bottom lines,
          strip.background = element_blank(),
          axis.text.x = element_text(size = 8.5),
          axis.text.y = element_text(size = 8.5),
          axis.title = element_text(size = 9.5),
          panel.grid.minor.y=element_blank(),
          strip.text.x=element_blank()
    )
  
  Fig4_fatal <- ggarrange(ggarrange(p2, p1, labels = c("A", "B"),nrow = 2, align= "v",heights = c(1,1.1)), extracted_legendA, ncol = 1, nrow = 2, heights = c(10,1))
  Fig4_fatal
  
  # add a plot title
  Fig4_fatal2 <- annotate_figure(Fig4_fatal,
                                top = textGrob(paste0("Age-specific impact: preschool scenarios"),
                                               
                                               x = unit(0.35, "npc"),
                                               y = unit(0.6, "npc"),
                                               gp = gpar(cex = 1, fontface = "bold")),
                                bottom = textGrob("", gp = gpar(cex = 1)))
  
  Fig4_fatal2
  
  
  ggsave(plot = Fig4_fatal2, filename = file.path("visualisation-output", "Plots", "PNG",  paste0("Fig4_fatal_preschool_",output_scen[i],".png")),  height = 12, width = 14, units = "cm", dpi = 300)
  
  
  # save a version of each without the legend, as an rds file, so can combine with deaths panel
  Fig4_fatal_noleg <- ggarrange(p2, p1, nrow = 2, heights = c(1,0.9),  align = "v")
  saveRDS(Fig4_fatal_noleg, file = file.path("visualisation-output", "Plots", "RDS",  paste0("Fig4_fatal_preschool_",output_scen[i],"_nolegend.rds")))
  
}

###################################.
#### CASES #### --
###################################.

output_scen <- c("Central")
i <-1

  fig_averted_cases <- fig_averted_cases_main |>
    filter(scenario %in% c(paste0(output_scen[i], "_LAIV_5-12yo"), paste0(output_scen[i], "_LAIV_5-18yo"))
    ) |>
    mutate(age_group = factor(age_group, levels = c("<1", "1-<5", "5-<12", "12-<18", "18-<65", "65-<80", "80+", "All"))
    )
  
  fig_averted_cases_summary<- fig_averted_cases_summary_main |>
    filter(scenario %in% c(paste0(output_scen[i], "_LAIV_5-12yo"), paste0(output_scen[i], "_LAIV_5-18yo"))
    ) |>
    mutate(age_group = factor(age_group, levels = c("<1", "1-<5", "5-<12", "12-<18", "18-<65", "65-<80", "80+", "All"))
    )
  
  p_averted_cases <- ggplot() +
    
    # Add solid lines - at 0 and to separate the all group
    geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
    geom_vline(xintercept = 7.55, colour = "white", linewidth = 1.5) +
    geom_vline(xintercept = 7.55, colour = "grey80", linewidth = 0.7) +
    
    # Plot the data in grey
    geom_point(data = fig_averted_cases,
               aes(x = age_group, y = change_reduction, alpha = team), colour = "grey",
               shape = 16, size = 1,
               position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6), show.legend = FALSE) +
    
    # Use the medians to get the legend without being displayed on the plot
    geom_errorbar(data = fig_averted_cases_summary,
                  aes(x = age_group, ymin = median_change_reduction, ymax = median_change_reduction, group = team, colour = team),
                  width = 0.6, alpha = 0.8, linewidth = 0.5,
                  position = position_dodge(width = 0.6), show.legend = c(group = FALSE)) +
    
    # Adjust the colours and opacity
    scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
    scale_colour_manual(values = scenario_palettes$team) +
    
    # Fix the layout, labels, and theme
    facet_wrap(~scenario, drop = FALSE, labeller = labeller(scenario = labeller_scenario), nrow = 1) +
    ACEFA_plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
    labs(x = "Age groups", y = "Absolute reduction in\ncases (per 100,000)", colour = "Team")
  
  p1 <- p_averted_cases#+ ylim(c(-500, 3000))
  
  ###################################.
  #### Plot percentage #### --
  ###################################.
  
  p_averted_perc_cases <- ggplot() +
    
    # Add solid lines - at 0 and to separate the all group
    geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
    geom_vline(xintercept = 7.55, colour = "white", linewidth = 1.5) +
    geom_vline(xintercept = 7.55, colour = "grey80", linewidth = 0.7) +
    
    # Plot the data in grey
    geom_point(data = fig_averted_cases,
               aes(x = age_group, y = perc_reduction, alpha = team), colour = "grey",
               shape = 16, size = 1,
               position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6), show.legend = FALSE) +
    
    # Median error bars
    geom_errorbar(data = fig_averted_cases_summary,
                  aes(x = age_group, ymin = median_perc_reduction, ymax = median_perc_reduction, group = team, colour = team),
                  width = 0.6, alpha = 1, linewidth = 0.5,
                  position = position_dodge(width = 0.6), show.legend = c(group = FALSE)) +
    
    # Manual adjustment of the colours and opacity
    scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
    scale_colour_manual(values = scenario_palettes$team) +
    scale_y_continuous(breaks = c(0, 25,50, 75,100), labels = c(0, 25,50, 75,100)) + 
    
    # Adjust the theme, legend, layout, labels
    facet_wrap(~scenario, drop = FALSE, labeller = labeller(scenario = labeller_scenario), nrow = 1) +
    ACEFA_plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
    labs(x = "Age groups", y = "Percentage reduction in\ncases", colour = "Team")
  
  p2 <- p_averted_perc_cases
  p2
  
  p1 <- p1 +
    theme(
      legend.text = element_text(size = 10),    # Shrinks legend item text
      legend.title = element_text(size = 10),   # Shrinks legend title text
      legend.position="bottom"
    )
  
  extracted_legendA <- get_legend(p1)
  
  p2 <- p2 + theme(legend.position = "none") + labs(x = NULL) +
    theme(panel.border = element_blank(),          # Removes the full outer border
          axis.line = element_line(color = "black"), # Adds back only the left and bottom lines,
          strip.background = element_blank(),
          axis.text.y = element_text(size = 8.5),
          axis.title = element_text(size = 9.5),
          axis.text.x = element_blank(),
          strip.text.x = element_text(size=9.5),
          panel.grid.minor.y=element_blank()
          
    )
  p1 <- p1 + theme(legend.position = "none") + labs(x = "Age group (years)")+
    theme(panel.border = element_blank(),          # Removes the full outer border
          axis.line = element_line(color = "black"), # Adds back only the left and bottom lines,
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.text.x = element_text(size = 8.5),
          axis.text.y = element_text(size = 8.5),
          axis.title = element_text(size = 9.5),
          panel.grid.minor.y=element_blank()
    )
  
  Fig4_cases_noleg <- ggarrange(p2, p1, nrow = 2, heights = c(1,1.1),  align = "v")
  
  Fig4_cases_leg <- ggarrange(Fig4_cases_noleg, extracted_legendA, nrow = 2, heights = c(1,0.1))
  
  Fig4_cases_leg2 <- annotate_figure(Fig4_cases_leg,
                                     top = textGrob(paste0("Age-specific impact"),
                                                    
                                                    x = unit(0.18, "npc"),
                                                    y = unit(0.6, "npc"),
                                                    gp = gpar(cex = 1, fontface = "bold")),
                                     bottom = textGrob("", gp = gpar(cex = 1)))
  
  Fig4_cases_leg2
  
  
  
  saveRDS(Fig4_cases_noleg, file = file.path("visualisation-output", "Plots", "RDS",  paste0("Fig4_cases_",output_scen[i],"_nolegend.rds")))
  saveRDS(Fig4_cases_leg2, file = file.path("visualisation-output", "Plots", "RDS",  paste0("Fig4_cases_",output_scen[i],".rds")))
  ggsave(file.path("visualisation-output", "Plots", "PNG",  paste0("Fig4_cases_",output_scen[i],".png")), device = "png", height = 12, width = 15, units = "cm")
  

  ###################################.
  #### INFECTIONS #### --
  ###################################.

output_scen <- c("Central")

  fig_averted_inf <- fig_averted_inf_main |>
    filter(scenario %in% c(paste0(output_scen[i], "_LAIV_5-12yo"), paste0(output_scen[i], "_LAIV_5-18yo"))
    ) |>
    mutate(age_group = factor(age_group, levels = c("<1", "1-<5", "5-<12", "12-<18", "18-<65", "65-<80", "80+", "All"))
    )
  
  fig_averted_inf_summary<- fig_averted_inf_summary_main |>
    filter(scenario %in% c(paste0(output_scen[i], "_LAIV_5-12yo"), paste0(output_scen[i], "_LAIV_5-18yo"))
    ) |>
    mutate(age_group = factor(age_group, levels = c("<1", "1-<5", "5-<12", "12-<18", "18-<65", "65-<80", "80+", "All"))
    )
  
  p_averted_inf <- ggplot() +
    
    # Add solid lines - at 0 and to separate the all group
    geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
    geom_vline(xintercept = 7.55, colour = "white", linewidth = 1.5) +
    geom_vline(xintercept = 7.55, colour = "grey80", linewidth = 0.7) +
    
    # Plot the data in grey
    geom_point(data = fig_averted_inf,
               aes(x = age_group, y = change_reduction, alpha = team), colour = "grey",
               shape = 16, size = 1,
               position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6), show.legend = FALSE) +
    
    # Use the medians to get the legend without being displayed on the plot
    geom_errorbar(data = fig_averted_inf_summary,
                  aes(x = age_group, ymin = median_change_reduction, ymax = median_change_reduction, group = team, colour = team),
                  width = 0.6, alpha = 0.8, linewidth = 0.5,
                  position = position_dodge(width = 0.6), show.legend = c(group = FALSE)) +
    
    # Adjust the colours and opacity
    scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
    scale_colour_manual(values = scenario_palettes$team) +
    
    # Fix the layout, labels, and theme
    facet_wrap(~scenario, drop = FALSE, labeller = labeller(scenario = labeller_scenario), nrow = 1) +
    ACEFA_plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
    labs(x = "Age groups", y = "Absolute reduction in\ninfections (per 100,000)", colour = "Team")
  
  p1 <- p_averted_inf#+ ylim(c(-500, 50000))
  p1
  
  ###################################.
  #### Plot percentage #### --
  ###################################.
  
  p_averted_perc_inf <- ggplot() +
    
    # Add solid lines - at 0 and to separate the all group
    geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
    geom_vline(xintercept = 7.55, colour = "white", linewidth = 1.5) +
    geom_vline(xintercept = 7.55, colour = "grey80", linewidth = 0.7) +
    
    # Plot the data in grey
    geom_point(data = fig_averted_inf,
               aes(x = age_group, y = perc_reduction, alpha = team), colour = "grey",
               shape = 16, size = 1,
               position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6), show.legend = FALSE) +
    
    # Median error bars
    geom_errorbar(data = fig_averted_inf_summary,
                  aes(x = age_group, ymin = median_perc_reduction, ymax = median_perc_reduction, group = team, colour = team),
                  width = 0.6, alpha = 1, linewidth = 0.5,
                  position = position_dodge(width = 0.6), show.legend = c(group = FALSE)) +
    
    # Manual adjustment of the colours and opacity
    scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
    scale_colour_manual(values = scenario_palettes$team) +
    scale_y_continuous(breaks = c(0, 25,50, 75,100), labels = c(0, 25,50, 75,100)) + 
    
    # Adjust the theme, legend, layout, labels
    facet_wrap(~scenario, drop = FALSE, labeller = labeller(scenario = labeller_scenario), nrow = 1) +
    ACEFA_plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
    labs(x = "Age groups", y = "Percentage reduction in\ninfections", colour = "Team")
  
  p2 <- p_averted_perc_inf
  p2
  
  p1 <- p1 +
    theme(
      legend.text = element_text(size = 10),    # Shrinks legend item text
      legend.title = element_text(size = 10),   # Shrinks legend title text
      legend.position="bottom"
    )

  extracted_legendA <- get_legend(p1)
  
  p2 <- p2 + theme(legend.position = "none") + labs(x = NULL) +
    theme(panel.border = element_blank(),          # Removes the full outer border
          axis.line = element_line(color = "black"), # Adds back only the left and bottom lines,
          strip.background = element_blank(),
          axis.text.y = element_text(size = 8.5),
          axis.title = element_text(size = 9.5),
          axis.text.x = element_blank(),
          strip.text.x = element_text(size=9.5),
          panel.grid.minor.y=element_blank()
          
    )
  p1 <- p1 + theme(legend.position = "none") + labs(x = "Age group (years)")+
    theme(panel.border = element_blank(),          # Removes the full outer border
          axis.line = element_line(color = "black"), # Adds back only the left and bottom lines,
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.text.x = element_text(size = 8.5),
          axis.text.y = element_text(size = 8.5),
          axis.title = element_text(size = 9.5),
          panel.grid.minor.y=element_blank()
    )
  
  Fig4_inf_noleg <- ggarrange(p2, p1, nrow = 2, heights = c(1,1.1),  align = "v")
  
  Fig4_inf_leg <- ggarrange(Fig4_inf_noleg, extracted_legendA, nrow = 2, heights = c(1,0.1))
  
  Fig4_inf_leg2 <- annotate_figure(Fig4_inf_leg,
                                     top = textGrob(paste0("Age-specific impact"),
                                                    
                                                    x = unit(0.18, "npc"),
                                                    y = unit(0.6, "npc"),
                                                    gp = gpar(cex = 1, fontface = "bold")),
                                     bottom = textGrob("", gp = gpar(cex = 1)))
  
  Fig4_inf_leg2
  
  
  saveRDS(Fig4_inf_noleg, file = file.path("visualisation-output", "Plots", "RDS",  paste0("Fig4_inf_",output_scen[i],"_nolegend.rds")))
  saveRDS(Fig4_inf_leg2, file = file.path("visualisation-output", "Plots", "RDS",  paste0("Fig4_inf_",output_scen[i],".rds")))
  ggsave(file.path("visualisation-output", "Plots", "PNG",  paste0("Fig4_inf_",output_scen[i],".png")), device = "png", height = 12, width = 15, units = "cm")
  
