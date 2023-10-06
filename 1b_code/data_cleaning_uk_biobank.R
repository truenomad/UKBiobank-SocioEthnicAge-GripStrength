################################################################################
#                                                                              #
# Purpose:       Cleaning the UK Biobank dataset                               #
#                                                                              #
# Author:        Mo Yusuf (https://github.com/truenomad)                       #
# Contact:       mohamedayusuf87@gmail.com                                     #
#                                                                              #
# Code created:  2021-06-18                                                    #
# Last updated:  2023-10-02                                                    #
#                                                                              #
################################################################################

# Library, functions and datasets-----------------------------------------------

# download pacman package if not installed, otherwise load it
if (!require(pacman)) install.packages(pacman)

# loads relevant packages using the pacman package
pacman::p_load(
  tidyverse,  # data management and visualization
  data.table, # for manipulating large datasets
  labelled,   # for labelling variables
  dvmisc,     # for breaking down continous vars into quantiles
  Hmisc,      # for %nin% (not in) function
  dataMaid,   # for creating a codebook
  doParallel, # for parallel processing
  janitor,    # for cleaning names
  here,       # for defining paths
  glue)       # elegant way t0 paste strings

# Get helper functions
source("01_data_cleaning/1b_code/cleaning_functions.R")

# define path using here package
data_path <- here(
  "01_data_cleaning")

# the ID's of those who withdrew from the study
withdrawn <- read_csv(
  file.path(data_path, "1a_data/ukb_biobank_withdrew.csv"))[1] |>  pull()

# load UK Biobank dataset
ukb_mess <- readr::read_rds(
  file.path(data_path, "1a_data/ukb4789_280521.Rdata")) |>
  # this line removes those withdrawn from the study
  # the %nin% function just means 'not in'
  filter(f.eid %nin% withdrawn) 

# load the SOC and NS-SEC derivation data
soc_nssec <- readxl::read_xlsx(
  file.path(data_path, "1a_data/soc2000_nssec_derivation.xlsx")) |> 
  janitor::clean_names()

# The variables in UKB are cleaned by series of functions that are defined in 
# the cleaning_functions.R function (source in line 30)

################################################################################
##                               Socio-demographics                           ##
################################################################################

ukb <- ukb_mess |> 
  # age and birth cohort
  process_age_cohort_vars() |> 
  # ethnicity
  process_ethnicity_vars() |>
  # education
  process_education_vars() |>
  # occupational-based variables
  process_occupational_vars() |>
  # household income
  process_income_vars() |> 
  # housing variables
  process_housing_vars()  |> 
  # deprivation variables
  process_deprivation_vars() |> 
  # information on loss to follow-up
  process_follow_up_vars() 

################################################################################
##                    Early life, health and lifestyle factors                ##
################################################################################
ukb <-  ukb |> 
  # early life factors    
  process_earlylife_vars() |>
  # physical activity life factors    
  process_physical_activity_vars() |> 
  # sedentary behaviour vars
  process_sedentary_vars() |> 
  # occupational activity
  process_occup_activity_vars() |>
  # smoking 
  process_smoking_vars() |> 
  # alcohol 
  process_alcohol_vars() |> 
  # height, weight, body fat etc.,
  process_bodysize_vars() |> 
  # health, co-morbidities etc.,
  process_health_vars() |> 
  # handgrip strength
  process_gripstrength_vars() 

################################################################################
##                      Produce codebook and save dataset                     ##
################################################################################

# select relevant variables
ukb_final <- ukb |> 
  select(
    any_of(
      get_vars_of_interest(ukb)
    )
  ) |> select(f.eid, everything())

# Produce codebook -------------------------------------------------------------

# produce a codebook for the dataset with date stamp
dataMaid::makeDataReport(
  data = ukb_final,
  mode = c("summarize", "visualize"),
  codebook = TRUE,
  replace = TRUE,
  file = "01_data_cleaning/1c_cleaned_data/uk_biobank_codebook.Rmd", 
  reportTitle = paste0("uk_biobank_codebook_", format(Sys.time(), "%Y-%b-%d"))
)

# Save dataset ----------------------------------------------------------------

# In order to save the below dataset in your format of choice, remove the
# hash-tag and run the code.
# save clean data in rds format with date stamp  (Size: ~40MB)
saveRDS(ukb_final,
        paste0("01_data_cleaning/1c_cleaned_data/",
               "uk_biobank_cleaned_",
               format(Sys.time(), "%Y-%b-%d"), ".rds"))
