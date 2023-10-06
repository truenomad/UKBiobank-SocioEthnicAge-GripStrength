################################################################################
#                                                                              #
# Purpose:       Regression modelling                                          #
#                                                                              #
# Author:        Mo Yusuf (https://github.com/truenomad)                       #
# Contact:       mohamedayusuf87@gmail.com                                     #
#                                                                              #
# Code created:  2021-06-18                                                    #
# Last updated:  2023-10-05                                                    #
#                                                                              #
################################################################################

# loads relevant packages using the pacman package
pacman::p_load(
  tidyverse,     # data management and visualization
  gtsummary,     # for beautiful tables
  pbmcapply,     # for parallel processing
  purrr,         # for purrr functions
  tidyr,         # for tidying mixed models
  broom.mixed,   # for tidying mixed models
  mice,          # for managing imputation data
  tidyverse,     # data management and visualization
  ggforestplot,  # for beautiful forestplots
  cowplot)       # for stacking plots together

# load custom functions
source("00_input/0a_source_code/modelling_functions.R")
source("00_input/0a_source_code/visualisation_functions.R")

# Get imputed dataset
ukb_imp <- loadRData("00_input/0b_data/cleaned/uk_biobank_imp_df.Rdata") 
ukb_men <- mice::filter(ukb_imp, sex == "Men")
ukb_women <- mice::filter(ukb_imp, sex == "Women")

# set plot paths
plot_path <- "02_outputs/2c_visualisation_outputs/"

################################################################################
##                               Run models                                   ##
################################################################################

# Education & grip strength  ---------------------------------------------------

# sex interaction (age adjusted)
res_edu_sex <- run_models(
  data = ukb_imp,
  exposure = "education",
  additional_adjustment = c("age_group_0", "ethnic_group"),
  interaction_vars = c("sex")
)

# age and sex interaction
res_edu_age <- run_models(
  data = ukb_imp,
  exposure = "education",
  interaction_vars = c("age_group_0", "sex")
)

# age, sex and ethnicty interaction
res_edu_age_ethn <- run_models(
  data = ukb_imp,
  exposure = "education",
  interaction_vars = c("age_group_0", "sex", "ethnic_group")
)

# sex and ethnicity interaction 
res_edu_eth_sex <- run_models(
  data = ukb_imp,
  exposure = "education",
  interaction_vars = c("sex", "ethnic_group")
)

# combine results
res_edu <- bind_rows(
  res_edu_age, res_edu_age_ethn, res_edu_eth_sex, res_edu_sex)

# IMD & grip strength  ---------------------------------------------------------

# sex interaction (age adjusted)
res_imd_sex <- run_models(
  data = ukb_imp,
  exposure = "imd_quantile",
  additional_adjustment = c("age_group_0", "ethnic_group"),
  interaction_vars = c("sex")
)

# age and sex interaction
res_imd_age <- run_models(
  data = ukb_imp,
  exposure = "imd_quantile",
  interaction_vars = c("age_group_0", "sex")
)

# age, sex and ethnicty interaction
res_imd_age_ethn <- run_models(
  data = ukb_imp,
  exposure = "imd_quantile",
  interaction_vars = c("age_group_0", "sex", "ethnic_group")
)

# sex and ethnicity interaction 
res_imd_eth_sex <- run_models(
  data = ukb_imp,
  exposure = "imd_quantile",
  interaction_vars = c("sex", "ethnic_group")
)

# combine results
res_imd <- bind_rows(
  res_imd_age, res_imd_eth_sex, res_imd_age_ethn, res_imd_sex)

# Ethnicity & grip strength  ---------------------------------------------------

res_age_eth_sex <- run_models(
  data = ukb_imp,
  exposure = "ethnic_group",
  interaction_vars = c("sex", "age_group_0")
)

res_eth_sex <- run_models(
  data = ukb_imp,
  exposure = "ethnic_group",
  additional_adjustment = c("age_group_0"),
  interaction_vars = c("sex")
)

# combine results
res_eth_sex <- bind_rows(res_age_eth_sex, res_eth_sex)

# Save results   ---------------------------------------------------------------

regression_mods <- list(edu = res_edu, imd = res_imd, eth = res_eth_sex)

# save results into a list
saveRDS(
  regression_mods, 
  "02_outputs/2a_model_outputs/res_grip_sep_eth_results.rds")

################################################################################
##                        Produce Interaction plots                           ##
################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~ Education & grip strength (by age) ~~~~~~~~~~~~~~~~~~~#

edu_plot <- regression_mods$edu |> 
  filter(model_strat == "Age and sex", 
         model == "M1: unadjusted") |> 
  interaction_plot(
    x_label = "")

#~~~~~~~~~~~~~~~~~~~~~~~~~~ IMD & grip strength (by age) ~~~~~~~~~~~~~~~~~~~~~~#

imd_plot <- regression_mods$imd |> 
  filter(model_strat == "Age and sex",
         model == "M1: unadjusted") |> 
  interaction_plot(
    y_label = "Index of Multiple Deprivation")

# Stack the plots vertically
cowplot::plot_grid(
  edu_plot, imd_plot, nrow = 2, rel_heights = c(1, 1))

# save plot
ggsave(
  paste0(plot_path, "/interaction_plot_age_sex.pdf"), 
  width = 12, height = 12, dpi = 320, scale = 0.86,
  useDingbats = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~ Ethnicity & grip strength (by age) ~~~~~~~~~~~~~~~~~~~#

regression_mods$eth |> 
  filter(model_strat == "Age and sex", 
         model == "M1: unadjusted") |> 
  interaction_plot(
    y_label = "Ethnicity",  include_path = FALSE)

ggsave(paste0(plot_path, "/eth_grip_age_bysex.pdf"), 
       width= 12, height = 8, dpi = 320, scale = 0.94, 
       useDingbats = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~ Education & grip strength (by ethnicity) ~~~~~~~~~~~~~#

edu_eth_plot <- regression_mods$edu |> 
  filter(model_strat == "Sex and ethnicity", 
         model == "M1: unadjusted") |> 
  interaction_plot(
    x_label = "", 
    y_facet = "ethnic_group")

#~~~~~~~~~~~~~~~~~~~~~~~~~~ IMD & grip strength (by ethnicity)~~~~~~~~~~~~~~~~~#

imd_eth_plot <- regression_mods$imd |> 
  filter(model_strat == "Sex and ethnicity",
         model == "M1: unadjusted") |> 
  interaction_plot(
    y_label = "Index of Multiple Deprivation",
    y_facet = "ethnic_group")

# Stack the plots vertically
cowplot::plot_grid(
  edu_eth_plot, imd_eth_plot, nrow = 2, rel_heights = c(1, 1))

# save plot
ggsave(
  paste0(plot_path, "/interaction_plot_eth_sex.pdf"), 
  width = 12, height = 12, dpi = 320, scale = 0.86,
  useDingbats = TRUE)

################################################################################
##                         Produce regression plots                           ##
################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~ Education & grip strength ~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# men (by age) ------
regression_mods$edu |> 
  filter(model_strat == "Age and sex", sex == "Men") |> 
  main_plot(plot_type = "men")

ggsave(paste0(plot_path, "/edu_grip_age_men.pdf"), 
       width= 12, height = 8, dpi = 320, scale = 0.94, 
       useDingbats = TRUE)

# women (adjusted for age) ----
edu_women <- regression_mods$edu |>
  filter(model_strat == "Sex", sex == "Women") |> 
  main_plot(plot_type = "women")

# save plot
# ggsave(
#   plot = edu_women,
#   paste0(plot_path, "/edu_grip_women.pdf"), 
#   width = 9.89, height = 5.66, dpi = 320, scale = 0.875, 
#   useDingbats = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~ IMD & grip strength ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# imd (by sex) ------
imd_bysex <- regression_mods$imd |> 
  filter(model_strat == "Sex") |> 
  main_plot(
    y_label = "Index of Multiple Deprivation",
    plot_type = "both") + theme(legend.position = "none")

# Stack the plots vertically
cowplot::plot_grid(
  edu_women, imd_bysex, nrow = 2, 
  rel_heights = c(1, 1), labels = "auto")

# # save plot
ggsave(
  paste0(plot_path, "/edu_fem_imd_sex.pdf"),
  width = 12, height = 12, dpi = 320, scale = 0.9,
  useDingbats = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~ IMD & grip strength ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

regression_mods$eth |> 
  filter(model_strat == "Sex") |> 
  main_plot(
    y_label = "Ethnicity",
    plot_type = "both")

# save plot
ggsave(
  paste0(plot_path, "/eth_grip_bysex.pdf"), 
  width = 10, height = 8, dpi = 320, scale = 0.9,
  useDingbats = TRUE)

#~~~~~~~~~~~~~Education and grip strength (by ethnicity for men)~~~~~~~~~~~~~~~#

# get ethnicity levels
eth_group <- levels(ukb_men$data$ethnic_group)

for (ethnicity in eth_group) {
  
  # plot the results
  plot <- regression_mods$edu |> 
    filter(
      model_strat == "Age, sex and ethnicity",
      sex == "Men",
      ethnic_group == ethnicity) |> 
    main_plot(plot_type = "men")
  
  # get ethnicity string 
  eth_strings <- epiCleanr::clean_names_strings(ethnicity)
  
  # save results 
  ggsave(
    plot = plot,
    paste0(plot_path, "/edu_grip_age_men_", eth_strings, ".pdf"), 
    width= 12, height = 8, dpi = 320, scale = 0.94, 
    useDingbats = TRUE)
}
