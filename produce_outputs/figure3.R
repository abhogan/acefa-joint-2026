
# Create Figure 3

######################################.
##  Relabelling figure panel titles ## -----
######################################.

labeller_target <- c("infection_incidence" = "Infections",
                     "disease_incidence"   = "Cases",
                     "admission_incidence" = "Hospitalisations",
                     "fatality_incidence" = "Deaths")

labeller_scenario <- c("Baseline"                = "Status quo", 
                       "Pessimistic_LAIV_5-12yo" = "5 to <12 years\n(Pessimistic)", 
                       "Central_LAIV_5-12yo"     = "5 to <12 years\n(Central)",
                       "Optimistic_LAIV_5-12yo"  = "5 to <12 years\n(Optimistic)",
                       "Pessimistic_LAIV_5-18yo" = "5 to <18 years\n(Pessimistic)",
                       "Central_LAIV_5-18yo"     = "5 to <18 years\n(Central)",
                       "Optimistic_LAIV_5-18yo"  = "5 to <18 years\n(Optimistic)",
                       "Central_LAIV_2-5yo"      = "2 to <5 years\n(Central)",
                       "Central_LAIV_2-12yo"     = "2 to <12 years\n(Central)",
                       "Central_LAIV_2-18yo"     = "2 to <18 years\n(Central)",
                       "CentralLow_LAIV_2-5yo"   = "2 to <5 years\n(Pessimistic-Central)",
                       "CentralLow_LAIV_2-12yo"  = "2 to <12 yeas\n(Pessimistic-Central)",
                       "CentralLow_LAIV_2-18yo"  = "2 to <18 years\n(Pessimistic-Central)",
                       "CentralLow_LAIV_5-12yo"  = "5 to <12 years\n(Pessimistic-Central)",
                       "CentralLow_LAIV_5-18yo"  = "5 to <18 years\n(Pessimistic-Central)")

labeller_resulttype <- c("change_reduction" = "Averted per 100,000",
                         "perc_reduction"   = "Percentage reduction")

labeller_LAIV <- c("LAIV_2-12yo" = "2 to <12 years",
                   "LAIV_2-18yo" = "2 to <18 years",
                   "LAIV_2-5yo" = "2 to <5 years",
                   "LAIV_5-12yo" = "5 to <12 years",
                   "LAIV_5-18yo" = "5 to <18 years")

labeller_LAIV2 <- c("LAIV_2-12yo" = "LAIV 2 to <12 years",
                   "LAIV_2-18yo" = "LAIV 2 to <18 years",
                   "LAIV_2-5yo" = "LAIV 2 to <5 years",
                   "LAIV_5-12yo" = "LAIV 5 to <12 years",
                   "LAIV_5-18yo" = "LAIV 5 to <18 years")

###########################################.
##  Order of factors for plotting panels ## -----
###########################################.

order_scenario <- c("Baseline",
                    "Pessimistic_LAIV_5-12yo", "Central_LAIV_5-12yo", "Optimistic_LAIV_5-12yo",
                    "Pessimistic_LAIV_5-18yo", "Central_LAIV_5-18yo", "Optimistic_LAIV_5-18yo",
                    "Central_LAIV_2-5yo",      "Central_LAIV_2-12yo", "Central_LAIV_2-18yo")

################################################################################.
## Pop-level outcomes                                                     ## ------ 
################################################################################.

fig_poplevelbyscen_data_unsw <- read.csv(file.path("visualisation-data", "fig_poplevelbyscen_data_unsw.csv"))
fig_poplevelbyscen_data_uom <- read.csv(file.path("visualisation-data", "fig_poplevelbyscen_data_uom.csv"))

################################.
## Finish processing the data ## --
################################.

# Combine
fig_all_data <- fig_poplevelbyscen_data_unsw |>
  bind_rows(fig_poplevelbyscen_data_uom)

# Clean up
rm(fig_poplevelbyscen_data_unsw, fig_poplevelbyscen_data_uom)

# Process the median
fig_all_data <- fig_all_data |>
  group_by(scenario, target, age_group, team, uncertainty, LAIV_age) |>
  summarise(median_sum_value = median(sum_value, na.rm = TRUE),
            .groups = "drop") |>
  mutate(uncertainty = factor(uncertainty, levels = c( "Pessimistic", "Central", "CentralLow", "Optimistic")),
         LAIV_age    = factor(LAIV_age, levels = c("LAIV_5-12yo", "LAIV_2-12yo", "LAIV_5-18yo", "LAIV_2-18yo", "", "LAIV_2-5yo")))

################################################################################.
## Figure: Averted events                                                 ## ------ 
################################################################################.

fig_avertedpoplevelbyscen_data_unsw <- read.csv(file.path("visualisation-data", "fig_avertedpoplevelbyscen_data_unsw.csv"))
fig_avertedpoplevelbyscen_data_uom <- read.csv(file.path("visualisation-data", "fig_avertedpoplevelbyscen_data_uom.csv"))

# Combine
fig_avrt_data <- fig_avertedpoplevelbyscen_data_unsw |>
  bind_rows(fig_avertedpoplevelbyscen_data_uom) |>
  arrange(team == "UNSW") 

# Find the median to add as a line to the plot
fig_avrt_median_data <- fig_avrt_data |>
  group_by(scenario, target, age_group, team, uncertainty, LAIV_age) |>
  summarise(median_change_reduction = median(change_reduction),
            median_perc_reduction = median(perc_reduction),
            max_reduction_count = max(change_reduction),
            max_increase_count = min(change_reduction),
            mean_change = mean(change_reduction), 
            .groups = "drop") |>
  mutate(uncertainty = factor(uncertainty, levels = c("Pessimistic", "Central", "CentralLow", "Optimistic")),
         LAIV_age    = factor(LAIV_age,    levels = c("LAIV_5-12yo", "LAIV_2-12yo", "LAIV_5-18yo", "LAIV_2-18yo", "", "LAIV_2-5yo")))

dat_panA_uncert <- fig_avrt_data |>
  filter(LAIV_age %in% c("LAIV_5-12yo", "LAIV_5-18yo")) |>
  mutate(uncertainty = factor(uncertainty,
                              levels = c("Pessimistic", "CentralLow", "Central", "Optimistic"),
                              labels = c("Pessimistic", "Pessimistic-\nCentral", "Central", "Optimistic")))

dat_panA <- fig_avrt_median_data |>
  filter(LAIV_age %in% c("LAIV_5-12yo", "LAIV_5-18yo")) |>
  mutate(uncertainty = factor(uncertainty,
                              levels = c("Pessimistic", "CentralLow", "Central", "Optimistic"),
                              labels = c("Pessimistic", "Pessimistic-\nCentral", "Central", "Optimistic")))

dat_panB_uncert <- fig_avrt_data |>
  filter(!LAIV_age %in% c("LAIV_5-12yo", "LAIV_5-18yo")) |>
  mutate(uncertainty = factor(uncertainty,
                              levels = c("Pessimistic", "CentralLow", "Central", "Optimistic"),
                              labels = c("Pessimistic", "Pessimistic-\nCentral", "Central", "Optimistic"))) %>%
  mutate(LAIV_age = factor(LAIV_age, levels = c("LAIV_2-5yo", "LAIV_2-12yo", "LAIV_2-18yo")))

# for UoM, the 2-5yo scenario for Pessimistic Central is the same as the Central scenario, so we will replicate the Central scenario for the Pessimistic Central scenario to make the plot look better
dat_panB_uncert_temp <- dat_panB_uncert |>
  filter(uncertainty == "Central" & LAIV_age == "LAIV_2-5yo" & team == "UoM") |>
  mutate(uncertainty = "CentralLow") %>%
  mutate(uncertainty = factor(uncertainty,
                              levels = c("Pessimistic", "CentralLow", "Central", "Optimistic"),
                              labels = c("Pessimistic", "Pessimistic-\nCentral", "Central", "Optimistic")))


dat_panB_uncert <- rbind(dat_panB_uncert, dat_panB_uncert_temp) |>
  arrange(team == "UNSW")

dat_panB <- fig_avrt_median_data |>
  filter(!LAIV_age %in% c("LAIV_5-12yo", "LAIV_5-18yo")) |>
  mutate(uncertainty = factor(uncertainty,
                              levels = c("Pessimistic", "CentralLow", "Central", "Optimistic"),
                              labels = c("Pessimistic", "Pessimistic-\nCentral", "Central", "Optimistic")))%>%
  mutate(LAIV_age = factor(LAIV_age, levels = c("LAIV_2-5yo", "LAIV_2-12yo", "LAIV_2-18yo")))

dat_panB_temp <- dat_panB |>
  filter(uncertainty == "Central" & LAIV_age == "LAIV_2-5yo" & team == "UoM") |>
  mutate(uncertainty = factor("CentralLow")) |>
  mutate(uncertainty = factor(uncertainty,
                              levels = c("CentralLow"),
                              labels = c("Pessimistic-\nCentral")),
         scenario = "CentralLow_LAIV_2-5yo")

dat_panB <- rbind(dat_panB, dat_panB_temp) |>
  arrange(team == "UNSW")

target_outcome <- c("admission_incidence")
label_outcome <- c("hospitalisations")

# percentage impact

for (i in 1){
  
panA <- ggplot() +
  
  # Horizontal line at 0
  geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
  
  # Scenario data as grey points
  geom_point(data = filter(dat_panA_uncert, target == target_outcome[i]),
             aes(x = uncertainty, y = perc_reduction, alpha = team),
             shape = 16, size = 1, colour = "grey",
             position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6),
             show.legend = FALSE) +
  
  # Median error bars
  geom_errorbar(data = filter(dat_panA, target == target_outcome[i]),
                aes(x = uncertainty,  ymin = median_perc_reduction , ymax = median_perc_reduction, group = team, colour = team),
                inherit.aes = TRUE, width = 0.3, alpha = 1, linewidth = 0.8,
                position = position_dodge(width = 0.6)) +
  
  # Manual adjustment of the colours and opacity
  scale_colour_manual(values = scenario_palettes$team) +
  scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
  
  # Adjust the theme, legend, layout, labels
  guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
  facet_wrap(~LAIV_age, drop = FALSE, nrow = 1, scales = "free_x", labeller = labeller(LAIV_age = labeller_LAIV2)) +  #labeller_LAIV
  ACEFA_plot_theme + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "LAIV scenario", y = paste0("Percentage reduction in\n", label_outcome[i]), colour = "Model")

panB <- ggplot() +
  
  # Horizontal line at 0
  geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
  
  # Scenario data as grey points
  geom_point(data = filter(dat_panB_uncert, target == target_outcome[i]),
             aes(x = uncertainty, y = perc_reduction, alpha = team),
             shape = 16, size = 1, colour = "grey",
             position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6),
             show.legend = FALSE) +
  
  # Median error bars
  geom_errorbar(data = filter(dat_panB, target == target_outcome[i]),
                aes(x = uncertainty,  ymin = median_perc_reduction , ymax = median_perc_reduction, group = team, colour = team),
                inherit.aes = TRUE, width = 0.3, alpha = 1, linewidth = 0.8,
                position = position_dodge(width = 0.6)) +
  
  # Manual adjustment of the colours and opacity
  scale_colour_manual(values = scenario_palettes$team) +
  scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
  
  # Adjust the theme, legend, layout, labels
  guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
  facet_wrap(~LAIV_age, drop = FALSE, nrow = 1, scales = "free_x", labeller = labeller(LAIV_age = labeller_LAIV2)) +  #labeller_LAIV
  ACEFA_plot_theme + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "LAIV scenario", y = paste0("Percentage reduction in\n", label_outcome[i]), colour = "Model")

panA <- panA +
  theme(
    legend.text = element_text(size = 9),    # Shrinks legend item text
    legend.title = element_text(size = 9),   # Shrinks legend title text
    legend.position="right",
    legend.key.size = unit(0.5, "cm")
  )
panB <- panB +
  theme(
    legend.text = element_text(size = 10),    # Shrinks legend item text
    legend.title = element_text(size = 10)    # Shrinks legend title text
  )

extracted_legendA <- get_legend(panA)
extracted_legend <- get_legend(panB)

panA <- panA + theme(legend.position = "none") + labs(x = "") +
  theme(axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 11)
  )
panB <- panB + theme(legend.position = "none") + labs(x = "")+
  theme(axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 11),
        legend.text = element_text(size = 10),    # Shrinks legend item text
        legend.title = element_text(size = 10)    # Shrinks legend title text
)


panA <- panA +
  theme(
    legend.text = element_text(size = 9),    # Shrinks legend item text
    legend.title = element_text(size = 9),   # Shrinks legend title text
    legend.position="right",
    legend.key.size = unit(0.5, "cm")
  ) + labs(col = "Team")
panB <- panB +
  theme(
    legend.text = element_text(size = 10),    # Shrinks legend item text
    legend.title = element_text(size = 10)    # Shrinks legend title text
  ) + labs(col = "Team")

extracted_legendA <- get_legend(panA)
extracted_legend <- get_legend(panB)

panA1 <- panA + theme(legend.position = "none") + labs(x = "") +
  theme(axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(),
  )
panB1 <- panB + theme(legend.position = "none") + labs(x = "")+
  theme(axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 10),    # Shrinks legend item text
        legend.title = element_text(size = 10)    # Shrinks legend title text
  )
fig_percent2 <- ggarrange(panA1,
                         ggarrange(panB1,extracted_legendA, nrow = 1, ncol = 2, widths=c(1,0.25)),
                         nrow = 2)

fig_percent2 <- annotate_figure(fig_percent2,
                left = textGrob(paste0("Percentage reduction in\n", label_outcome[i]), rot = 90,

                                x = unit(0.5, "npc"),
                                y = unit(0.6, "npc"),
                                gp = gpar(cex = 0.9)),
                bottom = textGrob("", gp = gpar(cex = 1)))
fig_percent2

fig_percent3 <- annotate_figure(fig_percent2,
                                top = textGrob(paste0("Population-level impact"),
                                                
                                                x = unit(0.23, "npc"),
                                                y = unit(0.6, "npc"),
                                                gp = gpar(cex = 1, fontface = "bold")),
                                bottom = textGrob("", gp = gpar(cex = 1)))
fig_percent3

fig_percent <- ggarrange(panA,
                             ggarrange(panB,extracted_legendA, nrow = 1, ncol = 2, widths=c(1,0.25)),
                             nrow = 2)
fig_percent

ggsave(plot = fig_percent3, filename = file.path("visualisation-output", "Plots", "PNG",  paste0("Fig_LAIVscenario_averted",target_outcome[i],".png")),  height = 15, width = 15, units = "cm")

ggsave(plot = fig_percent3, filename = file.path("visualisation-output", "Plots", "JPEG", paste0("Fig_LAIVscenario_averted",target_outcome[i],".jpg")), height = 12.5, width = 12, units = "cm")
}


## absolute population-level

for (i in 1){
  
  panA <- ggplot() +
    
    # Horizontal line at 0
    geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
    
    # Scenario data as grey points
    geom_point(data = filter(dat_panA_uncert, target == target_outcome[i]),
               aes(x = uncertainty, y = change_reduction, alpha = team),
               shape = 16, size = 1, colour = "grey",
               position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6),
               show.legend = FALSE) +
    
    # Median error bars
    geom_errorbar(data = filter(dat_panA, target == target_outcome[i]),
                  aes(x = uncertainty,  ymin = median_change_reduction , ymax = median_change_reduction, group = team, colour = team),
                  inherit.aes = TRUE, width = 0.3, alpha = 1, linewidth = 0.8,
                  position = position_dodge(width = 0.6)) +
    
    # Manual adjustment of the colours and opacity
    scale_colour_manual(values = scenario_palettes$team) +
    scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
    
    # Adjust the theme, legend, layout, labels
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
    facet_wrap(~LAIV_age, drop = FALSE, nrow = 1, scales = "free_x", labeller = labeller(LAIV_age = labeller_LAIV2)) +  #labeller_LAIV
    ACEFA_plot_theme + 
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "LAIV scenario", y = paste0("Absolute reduction in\n", label_outcome[i], "per 100,000"), colour = "Team")
  
  panB <- ggplot() +
    
    # Horizontal line at 0
    geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.2, linetype = "dotted") +
    
    # Scenario data as grey points
    geom_point(data = filter(dat_panB_uncert, target == target_outcome[i]),
               aes(x = uncertainty, y = change_reduction, alpha = team),
               shape = 16, size = 1, colour = "grey",
               position = position_jitterdodge(jitter.width = 0.15, jitter.height = 0, dodge.width = 0.6),
               show.legend = FALSE) +
    
    # Median error bars
    geom_errorbar(data = filter(dat_panB, target == target_outcome[i]),
                  aes(x = uncertainty,  ymin = median_change_reduction , ymax = median_change_reduction, group = team, colour = team),
                  inherit.aes = TRUE, width = 0.3, alpha = 1, linewidth = 0.8,
                  position = position_dodge(width = 0.6)) +
    
    # Manual adjustment of the colours and opacity
    scale_colour_manual(values = scenario_palettes$team) +
    scale_alpha_manual(values = c(UNSW = 0.4, UoM = 0.05)) +
    
    # Adjust the theme, legend, layout, labels
    guides(color = guide_legend(override.aes = list(linewidth = 2, alpha = 1))) + 
    facet_wrap(~LAIV_age, drop = FALSE, nrow = 1, scales = "free_x", labeller = labeller(LAIV_age = labeller_LAIV2)) +  #labeller_LAIV
    ACEFA_plot_theme + 
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(x = "LAIV scenario", y = paste0("Absolute reduction in\n", label_outcome[i], "per 100,000"), colour = "Team")
  
  panA <- panA +
    theme(
      legend.text = element_text(size = 9),    # Shrinks legend item text
      legend.title = element_text(size = 9),   # Shrinks legend title text
      legend.position="right",
      legend.key.size = unit(0.5, "cm"),
      plot.margin = margin(t = 0, r=3, b = 1, l=2,"pt")
    )
  panB <- panB +
    theme(
      legend.text = element_text(size = 10),    # Shrinks legend item text
      legend.title = element_text(size = 10),    # Shrinks legend title text
      plot.margin = margin(t = 0, r=3, b = 1, l=2,"pt")
    )
  
  extracted_legendA <- get_legend(panA)
  extracted_legend <- get_legend(panB)
  
  panA <- panA + theme(legend.position = "none") + labs(x = "") +
    theme(axis.text.x = element_text(size = 8),
          axis.title.y = element_text(size = 11)
    )
  panB <- panB + theme(legend.position = "none") + labs(x = "")+
    theme(axis.text.x = element_text(size = 8),
          axis.title.y = element_text(size = 11),
          legend.text = element_text(size = 10),    # Shrinks legend item text
          legend.title = element_text(size = 10)    # Shrinks legend title text
    )
  
  
  panA <- panA +
    theme(
      legend.text = element_text(size = 9),    # Shrinks legend item text
      legend.title = element_text(size = 9),   # Shrinks legend title text
      legend.position="right",
      legend.key.size = unit(0.5, "cm")
    )
  panB <- panB +
    theme(
      legend.text = element_text(size = 10),    # Shrinks legend item text
      legend.title = element_text(size = 10)    # Shrinks legend title text
    )
  
  panA1 <- panA + theme(legend.position = "none") + labs(x = "") +
    theme(axis.text.x = element_text(size = 8),
          axis.title.y = element_blank(),
    )
  panB1 <- panB + theme(legend.position = "none") + labs(x = "")+
    theme(axis.text.x = element_text(size = 8),
          axis.title.y = element_blank(),
          legend.text = element_text(size = 10),    # Shrinks legend item text
          legend.title = element_text(size = 10)    # Shrinks legend title text
    )
  fig_absolute2 <- ggarrange(panA1,
                            ggarrange(panB1,extracted_legendA, nrow = 1, ncol = 2, widths=c(1,0.25)),
                            nrow = 2)
  
  fig_absolute2 <- annotate_figure(fig_absolute2, 
                                  left = textGrob(paste0("Absolute reduction in\n", label_outcome[i], " per 100,000"), rot = 90,
                                                  
                                                  x = unit(0.5, "npc"),
                                                  y = unit(0.6, "npc"),
                                                  gp = gpar(cex = 0.9)),
                                  bottom = textGrob("", gp = gpar(cex = 1)))
  fig_absolute2
  
  
  fig_absolute3 <- annotate_figure(fig_absolute2,
                                  top = textGrob(paste0("Population-level impact"),
                                                 
                                                 x = unit(0.23, "npc"),
                                                 y = unit(0.6, "npc"),
                                                 gp = gpar(cex = 1, fontface = "bold")),
                                  bottom = textGrob("", gp = gpar(cex = 1)))
  fig_absolute3
  fig_absolute <- ggarrange(panA,
                           ggarrange(panB,extracted_legendA, nrow = 1, ncol = 2, widths=c(1,0.2)),
                           nrow = 2, labels = c("A", "B"))
  
  
  ggsave(plot = fig_absolute3, filename = file.path("visualisation-output", "Plots", "PNG",  paste0("Fig_LAIVscenario_averted",target_outcome[i],"_absolute.png")),  height = 15, width = 15, units = "cm")
  
  ggsave(plot = fig_absolute3, filename = file.path("visualisation-output", "Plots", "JPEG", paste0("Fig_LAIVscenario_averted",target_outcome[i],"_absolute.jpg")), height = 12.5, width = 15.5, units = "cm")

}
