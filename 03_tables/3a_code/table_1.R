################################################################################
#                                                                              #
# Purpose:       Creating Table 1                                              #
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
  tidyverse, # data management and visualization
  gtsummary
) # for beautiful tables

# load custom functions
source("02_data_analyses/2a_code/modelling_functions.R")

# Get imputed dataset
ukb_imp <- loadRData("01_data_cleaning/1c_cleaned_data/uk_biobank_imp_df.Rdata")

# pre-process data
ukb <- ukb_imp$data |>
  # pre-process data
  filter(
    if_all(c(handgrip_max_0_imp, sex, age_group_0, ethnic_group), ~ !is.na(.))
  ) |>
  # remove outliers (it's implausible to be 75cm tall)
  mutate(height_0 = ifelse(height_0 == 75, NA, height_0)) |>
  # select relevant variables for imputation
  select(
    handgrip_max_0_imp, sex,
    # age_0,  bmi_0,
    age_group_0, ethnic_group,
    education, imd_quantile, nssec_5class,
    smoking_status_0, sedentary_hours_day_0,
    days_wks_vig_act_0,
    healthcat_0, occupational_activity_0,
    height_0, waist_circum_0, hip_circum_0,
    waist_hip_0, body_fat_perc_0
  )

################################################################################
##                                   Table 1:                                 ##
################################################################################

# Table 1 - Population characteristic  ------------------------------------

tbl_1 <- ukb |>
  tbl_summary(
    by = sex,
    # type = all_continuous() ~ "continuous",
    label = list(
      c(handgrip_max_0_imp) ~ "Maximum grip strength (kg)",
      c(age_group_0) ~ "Age group at baseline (years)",
      c(ethnic_group) ~ "Ethnicity",
      c(education) ~ "Highest educational qualification",
      c(imd_quantile) ~ "IMD (quantiles)",
      c(nssec_5class) ~ "Occupational class (NS-SEC)",
      c(height_0) ~ "Height (cm)",
      c(waist_circum_0) ~ "Waist circumference (cm)",
      c(hip_circum_0) ~ "Hip circumference (cm",
      c(waist_hip_0) ~ "Waist to hip ratio",
      c(body_fat_perc_0) ~ "Body fat percentage (%)",
      c(healthcat_0) ~ "Co-morbidity",
      c(smoking_status_0) ~ "Smoking status",
      c(sedentary_hours_day_0) ~ "Sedentariness",
      c(days_wks_vig_act_0) ~ "Vigorous physical activity (10min+)",
      c(occupational_activity_0) ~ "Occupational activity"
    ),
    # control digits of freq and percentages
    digits = list(everything() ~ c(0, 1)),
    #    sort = list(c(ethnic_group) ~ "frequency"),
    statistic = list(
      everything() ~ c(
        "{mean} ({sd})"
      ),
      all_categorical() ~ "{n} ({p})"
    ),
    missing = "ifany",
    missing_text = "Missing"
  ) %>%
  add_overall() %>%
  add_p() %>%
  modify_spanning_header(all_stat_cols() ~ "**N (%)**", ) %>%
  italicize_levels() %>%
  bold_labels() %>%
  tbl_butcher() %>%
  modify_table_styling(
    columns = label,
    rows = label == "Maximum grip strength (kg)",
    footnote = paste(
      "Includes those unable to do grip strength for health reasons N=269",
      "(187 women & 82 men). These individuals were allocated grip strength",
      "values equivalent to the mean of the bottom sex-specific fifth."
    )
  ) |>
  modify_table_styling(
    columns = label,
    rows = label == "Co-morbidity",
    footnote = paste(
      "Conditions include heart attack, angina, stroke, high blood",
      "pressure, blood clot in leg, blood clot in lung, emphysema/chronic",
      "bronchitis, asthma, diabetes, cancer or any other serious medical",
      "conditions."
    )
  ) |>
  modify_footnote(
    everything() ~
      "Maximum N, though N varies due to missing data on some covariates"
  )
