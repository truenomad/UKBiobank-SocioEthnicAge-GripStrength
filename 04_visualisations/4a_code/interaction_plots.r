################################################################################
#                                                                              #
# Purpose:       Creating Interaction plots                                    #
#                                                                              #
# Author:        Mo Yusuf (https://github.com/truenomad)                       #
# Contact:       mohamedayusuf87@gmail.com                                     #
#                                                                              #
# Code created:  2021-06-18                                                    #
# Last updated:  2023-10-02                                                    #
#                                                                              #
################################################################################

# loads relevant packages using the pacman package
pacman::p_load(
  tidyverse,     # data management and visualization
  ggforestplot,  # for beautiful forestplots
  cowplot)       # for stacking plots together

# load custom functions
source("00_source_code/visualisation_functions.R")

# Get imputed dataset
res_int <- readRDS(
  "02_data_analyses/2b_model_outputs/res_grip_sep_interact.rds")

################################################################################
##                        Education & grip strength                           ##
################################################################################

edu_plot <- res_int$edu |> 
  filter(model_strat == "Age and sex",
         model == "M1: unadjusted") |> 
  interaction_plot(
    x_label = "")

################################################################################
##                        Education & grip strength                           ##
################################################################################

imd_plot <- res_int$imd |> 
  filter(model_strat == "Age and sex",
         model == "M1: unadjusted") |> 
  interaction_plot(
    y_label = "Index of Multiple Deprivation")

################################################################################
##                             Stack plots and save                           ##
################################################################################

# Stack the plots vertically
cowplot::plot_grid(
  edu_plot, imd_plot, nrow = 2, rel_heights = c(1, 1))

# save the stacked plots
ggsave(
  "04_visualisations/4b_visualisation_outputs/interaction_plot_age_sex.pdf", 
  width = 12, height = 12, dpi = 320, scale = 1.0,
  useDingbats = TRUE)
