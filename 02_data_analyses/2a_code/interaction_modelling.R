################################################################################
#                                                                              #
# Purpose:       Interactions modelling                                        #
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
  tidyverse, # data management and visualization
  gtsummary,  # for beautiful tables
  pbmcapply,  # for parallel processing
  purrr,      # for purrr functions
  tidyr,      # for tidying mixed models
  broom.mixed, # for tidying mixed models
  mice)      # for managing imputation data

# load custom functions
source("00_source_code/modelling_functions.R")

# Get imputed dataset
ukb_imp <- loadRData("01_data_cleaning/1c_cleaned_data/uk_biobank_imp_df.Rdata")

################################################################################
##                        Education & grip strength                           ##
################################################################################

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

# combine results
res_edu <- bind_rows(res_edu_age, res_edu_age_ethn)

################################################################################
##                        IMD & grip strength                                 ##
################################################################################

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

# combine results
res_imd <- bind_rows(res_imd_age, res_imd_age_ethn)

################################################################################
#########################       Save results      ##############################
################################################################################

# save results into a list
saveRDS(
  list(edu = res_edu, imd = res_imd), 
  "02_data_analyses/2b_model_outputs/res_grip_sep_interact.rds")
