################################################################################
#                                                                              #
# Purpose:       Create multiple imputed dataset                               #
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
  tidyverse,  # data management and visualization
  mice,       # multiple imputation
  miceadds)   # For saving imputed datasets 

# load clean data
ukb <- readr::read_rds(
  "00_input/0b_data/cleaned/uk_biobank_cleaned_2023-Oct-05.rds")

# pre-process data
ukb <- ukb |>
  filter(
    if_all(
      c(handgrip_max_0_imp, sex, age_group_0, ethnic_group), ~ !is.na(.)))  |> 
  # remove outliers (it's implausible to be 75cm tall)
  mutate(height_0 = ifelse(height_0 == 75, NA, height_0)) %>%
  # select relevant variables for imputation
  select(
    f.eid, 
    handgrip_max_0_imp, 
    sex, 
    age_0, 
    age_group_0, 
    ethnic_group, 
    age_group_retirement_0,
    ethnicity,
    education, 
    imd_quantile, 
    nssec_5class, 
    height_0, 
    waist_circum_0, 
    hip_circum_0, 
    bmi_0, 
    body_fat_perc_0, 
    days_wks_vigorous_physical_activity_0, 
    healthcat_0, 
    occupational_activity_0, 
    sedentary_hours_day_0, 
    smoking_status_0
  )

# Set up predictor matrix for the multiple imputation --------------------------

# Create predictor matrix
ukb_predmat <- make.predictorMatrix(ukb)

# Remove some variables as predictors
ukb_predmat[, c(
  "handgrip_max_0_imp", "age_0", "age_group_0",  "age_group_retirement_0",
  "ethnic_group", "f.eid", "bmi_0", "hip_circum_0", "waist_circum_0")] <- 0

# Don't predict demographics and outcome variables
ukb_predmat[, c(
  "sex", "age_0", "age_group_0", "handgrip_max_0_imp", 
  "age_group_retirement_0", "f.eid", "ethnic_group", "ethnicity")] <- 0

# Set specific rows to 0
ukb_predmat[c("occupational_activity_0"),] <- 0

# Update specific entries in the predictor matrix
ukb_predmat["occupational_activity_0", 
            c("education","imd_quantile", "nssec_5class", "height_0")] <- 1

# Display predictor matrix
ukb_predmat

# Multiple imputation using cart -----------------------------------------------

# conduct imputation
ukb_imp <-  parlmice(
  ukb,
  m = 12, 
  maxit = 5,
  method = "cart",
  pred = ukb_predmat,
  cluster.seed = 1234,
  cl.type = "FORK")


# Save imputed data in Rdata format --------------------------------------------

# get the id of those unable to do grip for health reason
id <- ukb %>%
  filter(rsn_grip_missing_cat == "Unable for health reasons") %>%
  pull(f.eid)

# process before saving
ukb_imp <- ukb_imp |> 
  mice::complete("long", include = TRUE) |>
  # create unable to do grip for health reason variable
  mutate(unable_hlth = ifelse(f.eid %in% id, TRUE, FALSE)) |>
  # create waist hip ratio
  mutate(waist_hip_0 = waist_circum_0/hip_circum_0) %>%
  as.mids()

# save imputed data
miceadds::save.data(
  ukb_imp,
  filename = "uk_biobank_imp_df",
  type = "Rdata", path = "00_input/0b_data/cleaned/")

# get session information
sessionInfo()
