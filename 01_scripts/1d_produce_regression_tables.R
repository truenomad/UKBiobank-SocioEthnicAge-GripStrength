################################################################################
#                                                                              #
# Purpose:       Produce tables for regression models                          #
#                                                                              #
# Author:        Mo Yusuf (https://github.com/truenomad)                       #
# Contact:       mohamedayusuf87@gmail.com                                     #
#                                                                              #
# Code created:  2021-06-18                                                    #
# Last updated:  2024-01-31                                                    #
#                                                                              #
################################################################################

# load custom functions
source("00_input/0a_source_code/modelling_functions.R")

# Get imputed dataset
ukb_imp <- loadRData("00_input/0b_data/cleaned/uk_biobank_imp_df.Rdata") 

ukb_men <- mice::filter(ukb_imp, sex == "Men")
ukb_women <- mice::filter(ukb_imp, sex == "Women")

################################################################################
##                     Run models and produce tables                          ##
################################################################################

# Unadjusted results (for multiple imputation) ---------------------------------

# for education level (men)
sensitivity_mi_edu_mal <- generate_gt_table(
  data = ukb_men$data,
  exposure = "education",
  outcome = "handgrip_max_0_imp", 
  interaction_vars = c("age_group_0"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  sex_groups = "Men"
)

# for education level (women)
sensitivity_mi_edu_fem <- generate_gt_table(
  data = ukb_women$data,
  exposure = "education",
  outcome = "handgrip_max_0_imp", 
  sex_groups = "Women"
)

# for IMD (men)
sensitivity_mi_imd_men <- generate_gt_table(
  data = ukb_men$data,
  exposure = "imd_quantile",
  outcome = "handgrip_max_0_imp", 
  sex_groups = "Men"
)

# for IMD (women)
sensitivity_mi_imd_fem <- generate_gt_table(
  data = ukb_women$data,
  exposure = "imd_quantile",
  outcome = "handgrip_max_0_imp", 
  sex_groups = "Women"
)

# Join them together
sensitivity_mi_imd <- gtsummary::tbl_stack(
  tbls = list(sensitivity_mi_imd_fem, sensitivity_mi_imd_men))

# Interaction tables -----------------------------------------------------------

# for education level
interaction_tbl_edu <- generate_gt_table(
  data = ukb_imp,
  exposure = "education",
  outcome = "handgrip_max_0_imp",
  interaction_vars = c("age_group_0"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_sex",
  sex_groups = c("Women", "Men")
)

# for IMD
interaction_tbl_imd <- generate_gt_table(
  data = ukb_imp,
  exposure = "imd_quantile",
  outcome = "handgrip_max_0_imp",
  interaction_vars = c("age_group_0"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_sex",
  sex_groups = c("Women", "Men")
)

# for ethnic group
interaction_tbl_eth <- generate_gt_table(
  data = ukb_imp,
  exposure = "ethnic_group",
  outcome = "handgrip_max_0_imp",
  interaction_vars = c("age_group_0"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_sex",
  sex_groups = c("Women", "Men")
)


# sex and ethnicity interaction (edu)
interaction_tbl_eth_edu <- generate_gt_table(
  data = ukb_imp,
  exposure = "education",
  outcome = "handgrip_max_0_imp",
  interaction_vars = c("ethnic_group"),
  interaction_groups = levels(ukb_men$data$ethnic_group),
  mode = "by_sex",
  sex_groups = c("Women", "Men")
)

# sex and ethnicity interaction (imd)
interaction_tbl_eth_imd <- generate_gt_table(
  data = ukb_imp,
  exposure = "imd_quantile",
  outcome = "handgrip_max_0_imp",
  interaction_vars = c("ethnic_group"),
  interaction_groups = levels(ukb_men$data$ethnic_group),
  mode = "by_sex",
  sex_groups = c("Women", "Men")
)

# Education and grip strength --------------------------------------------------

# run models for mens
table_edu_men <- generate_gt_table(
  data = ukb_men,
  exposure = "education",
  outcome = "handgrip_max_0_imp",
  interaction_vars = c("age_group_0"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_formula"
)

# run models for womens
table_edu_women <- generate_gt_table(
  data = ukb_women,
  exposure = "education",
  outcome = "handgrip_max_0_imp",
  interaction_vars = NULL,
  additional_adjustment = c("age_group_0","ethnic_group"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_formula"
)

# Education and grip strength (for men and by ethnicity) -----------------------

# get ethnicity levels
eth_group <- levels(ukb_men$data$ethnic_group)

# initialize an empty list to store tables
table_edu_men_list <- list()

for (ethnicity in eth_group) {
  
  # subset data
  subset_data <- ukb_men |> 
    mice::filter(ethnic_group == {{ethnicity}})
  
  table <- generate_gt_table(
    data = subset_data,
    exposure = "education",
    outcome = "handgrip_max_0_imp",
    interaction_vars = c("age_group_0"),
    interaction_groups = levels(ukb_men$data$age_group_0),
    mode = "by_formula"
  )
  
  # Clean ethnicity string for use in variable names
  eth_strings <- epiCleanr::clean_names_strings(ethnicity)
  
  # Assign table to the list with a dynamic key
  table_edu_men_list[[eth_strings]] <- table
  
}

# IMD and grip strength --------------------------------------------------------

# run models for mens
table_imd_men <- generate_gt_table(
  data = ukb_men,
  exposure = "imd_quantile",
  outcome = "handgrip_max_0_imp",
  interaction_vars = NULL,
  additional_adjustment = c("age_group_0","ethnic_group"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_formula"
)

# run models for womens
table_imd_women <- generate_gt_table(
  data = ukb_women,
  exposure = "imd_quantile",
  outcome = "handgrip_max_0_imp",
  interaction_vars = NULL,
  additional_adjustment = c("age_group_0","ethnic_group"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_formula"
)

table_imd <- gtsummary::tbl_merge(
  tbls = list(table_imd_women, table_imd_men), 
  tab_spanner = c("Women", "Men"))

# Ethnicty and grip strength ---------------------------------------------------

# run models for mens
table_ethni_women <- generate_gt_table(
  data = ukb_women,
  exposure = "ethnic_group",
  outcome = "handgrip_max_0_imp",
  interaction_vars = NULL,
  additional_adjustment = c("age_group_0"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_formula"
)

table_ethni_men <- generate_gt_table(
  data = ukb_men,
  exposure = "ethnic_group",
  outcome = "handgrip_max_0_imp",
  interaction_vars = NULL,
  additional_adjustment = c("age_group_0"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_formula"
)

table_eth <- gtsummary::tbl_merge(
  tbls = list(table_ethni_women, table_ethni_men), 
  tab_spanner = c("Women", "Men"))

# Sensitivty analyses to exclude participants unable to ------------------------
# do grip strength for health reasons

# run models for mens
table_edu_unabh_men <- generate_gt_table(
  data = mice::filter(ukb_men, unable_hlth == FALSE),
  exposure = "education",
  outcome = "handgrip_max_0_imp",
  interaction_vars = c("age_group_0"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_formula"
)

# run models for womens
table_edu_unabh_women <- generate_gt_table(
  data = mice::filter(ukb_women, unable_hlth == FALSE),
  exposure = "education",
  outcome = "handgrip_max_0_imp",
  interaction_vars = NULL,
  additional_adjustment = c("age_group_0","ethnic_group"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_formula"
)

# run models for mens
table_imd_unabh_men <- generate_gt_table(
  data = mice::filter(ukb_men, unable_hlth == FALSE),
  exposure = "imd_quantile",
  outcome = "handgrip_max_0_imp",
  interaction_vars = NULL,
  additional_adjustment = c("age_group_0","ethnic_group"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_formula"
)

# run models for womens
table_imd_unabh_women <- generate_gt_table(
  data = mice::filter(ukb_women, unable_hlth == FALSE),
  exposure = "imd_quantile",
  outcome = "handgrip_max_0_imp",
  interaction_vars = NULL,
  additional_adjustment = c("age_group_0","ethnic_group"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_formula"
)

table_unabh_imd <- gtsummary::tbl_merge(
  tbls = list(table_imd_unabh_women, table_imd_unabh_men), 
  tab_spanner = c("Women", "Men"))

# Sensitivity analysis to examine the impact of the ----------------------------
# different categorisations used for ethnicity

# run models for mens
table_ethni2_women <- generate_gt_table(
  data = ukb_women,
  exposure = "ethnic_group2",
  outcome = "handgrip_max_0_imp",
  interaction_vars = NULL,
  additional_adjustment = c("age_group_0"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_formula"
)

table_ethni2_men <- generate_gt_table(
  data = ukb_men,
  exposure = "ethnic_group2",
  outcome = "handgrip_max_0_imp",
  interaction_vars = NULL,
  additional_adjustment = c("age_group_0"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_formula"
)

table_eth_sensi <- gtsummary::tbl_merge(
  tbls = list(table_ethni2_women, table_ethni2_men), 
  tab_spanner = c("Women", "Men"))

# Sensitivity analysis focusing exclusively on England for IMD -----------------

# run models for mens
table_imd_eng_men <- generate_gt_table(
  data = mice::filter(ukb_men, region_england == "Yes"),
  exposure = "imd_quantile",
  outcome = "handgrip_max_0_imp",
  interaction_vars = NULL,
  additional_adjustment = c("age_group_0","ethnic_group"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_formula"
)

# run models for womens
table_imd_eng_women <- generate_gt_table(
  data = mice::filter(ukb_women, region_england == "Yes"),
  exposure = "imd_quantile",
  outcome = "handgrip_max_0_imp",
  interaction_vars = NULL,
  additional_adjustment = c("age_group_0","ethnic_group"),
  interaction_groups = levels(ukb_men$data$age_group_0),
  mode = "by_formula"
)

table_england_imd <- gtsummary::tbl_merge(
  tbls = list(table_imd_eng_women, table_imd_eng_men), 
  tab_spanner = c("Women", "Men"))

################################################################################
##                    Save Regression tables in word format                   ##
################################################################################

# Export interaction tables ----------------------------------------------------

# Add tables to the Word document, each on a new page
doc_interaction <- officer::read_docx() |> 
  officer::body_add_par(
    "Interaction table: Associations between highest qualification and grip strength in men and women. \n", 
    style = "heading 2") |>  
  # officer::body_add_break() |>
  flextable::body_add_flextable(
    gtsummary::as_flex_table(
      interaction_tbl_edu) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::height_all(height = 0.31) |> 
      flextable::set_table_properties(width = 1, layout = "autofit")) |> 
  officer::body_add_break() |>
  officer::body_add_par(
    "Interaction table: Associations between IMD and grip strength in men and women. \n", 
    style = "heading 2") |>  
  # officer::body_add_break() |> 
  flextable::body_add_flextable(
    gtsummary::as_flex_table(interaction_tbl_imd) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")) |> 
  officer::body_add_break() |>
  officer::body_add_par(
    "Interaction table: Associations between ethnicity and grip strength in men and women. \n", 
    style = "heading 2") |>  
  # officer::body_add_break() |> 
  flextable::body_add_flextable(
    gtsummary::as_flex_table(interaction_tbl_eth) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")) |> 
  officer::body_add_break() |>
  officer::body_add_par(
    "Interaction table: Associations between education and grip strength in men and women, stratified by ethnicity. \n", 
    style = "heading 2") |>  
  # officer::body_add_break() |> 
  flextable::body_add_flextable(
    gtsummary::as_flex_table(interaction_tbl_eth_edu) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")) |> 
  officer::body_add_break() |>
  officer::body_add_par(
    "Interaction table: Associations between IMD and grip strength in men and women, stratified by ethnicity. \n", 
    style = "heading 2") |>  
  # officer::body_add_break() |> 
  flextable::body_add_flextable(
    gtsummary::as_flex_table(interaction_tbl_eth_imd) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")) |> 
  officer::body_end_section_landscape() 

# save the Word document
print(
  doc_interaction,
  target = "02_outputs/2b_table_outputs/interaction_tables.docx")

# Export main model tables -----------------------------------------------------

# Add tables to the Word document, each on a new page
main_tables <- officer::read_docx() |> 
  officer::body_add_par(
    "Associations between highest qualification and grip strength in men. \n", 
    style = "heading 2") |>  
  # officer::body_add_break() |> 
  flextable::body_add_flextable(
    gtsummary::as_flex_table(table_edu_men) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")|> 
      flextable::hrule(rule = "auto", part = "body")) |> 
  officer::body_add_break() |>
  officer::body_end_section_landscape() |> 
  officer::body_add_par(
    "Associations between highest qualification and grip strength in women. \n", 
    style = "heading 2") |>  
  flextable::body_add_flextable(
    gtsummary::as_flex_table(
      table_edu_women) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")|> 
      flextable::hrule(rule = "auto", part = "body")) |> 
  officer::body_add_break()  |> 
  officer::body_add_par(
    "Associations between IMD and grip strength in men and women \n", 
    style = "heading 2") |>  
  flextable::body_add_flextable(
    gtsummary::as_flex_table(
      table_imd) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")|> 
      flextable::hrule(rule = "auto", part = "body")) |> 
  officer::body_add_break()  |> 
  officer::body_add_par(
    "Associations between ethnicity and grip strength in men and women \n", 
    style = "heading 2") |>  
  flextable::body_add_flextable(
    gtsummary::as_flex_table(
      table_eth) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")|> 
      flextable::hrule(rule = "auto", part = "body")) |> 
  officer::body_add_break()  |>
  officer::body_add_par(
    "Associations between highest qualification and grip strength in White men. \n", 
    style = "heading 2") |>  
  # officer::body_add_break() |> 
  flextable::body_add_flextable(
    gtsummary::as_flex_table(table_edu_men_list$white) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")|> 
      flextable::hrule(rule = "auto", part = "body")) |> 
  officer::body_add_break() |>
  officer::body_add_par(
    "Associations between highest qualification and grip strength in South Asian men. \n", 
    style = "heading 2") |>  
  # officer::body_add_break() |> 
  flextable::body_add_flextable(
    gtsummary::as_flex_table(table_edu_men_list$south_asian) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")|> 
      flextable::hrule(rule = "auto", part = "body")) |> 
  officer::body_add_break() |>
  officer::body_add_par(
    "Associations between highest qualification and grip strength in Black men. \n", 
    style = "heading 2") |>  
  # officer::body_add_break() |> 
  flextable::body_add_flextable(
    gtsummary::as_flex_table(table_edu_men_list$black) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")|> 
      flextable::hrule(rule = "auto", part = "body")) |> 
  officer::body_add_break() |>
  officer::body_add_par(
    "Associations between highest qualification and grip strength in men of Mixed ethnicity . \n", 
    style = "heading 2") |>  
  # officer::body_add_break() |> 
  flextable::body_add_flextable(
    gtsummary::as_flex_table(table_edu_men_list$mixed) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")|> 
      flextable::hrule(rule = "auto", part = "body")) |> 
  officer::body_add_break() |>
  officer::body_add_par(
    "Associations between highest qualification and grip strength in men of Other ethnicity \n", 
    style = "heading 2") |>  
  # officer::body_add_break() |> 
  flextable::body_add_flextable(
    gtsummary::as_flex_table(table_edu_men_list$other) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")|> 
      flextable::hrule(rule = "auto", part = "body")) |> 
  officer::body_add_break() |>
  officer::body_end_section_landscape()

# save the Word document
print(
  main_tables,
  target = "02_outputs/2b_table_outputs/main_regression_tables.docx")

# Sensitivity analyses Tables --------------------------------------------------

# Add tables to the Word document, each on a new page
sensitivity_mi <- officer::read_docx() |> 
  officer::body_add_par(
    "Observed associations between highest qualification and grip strength in men. \n", 
    style = "heading 2") |>  
  # officer::body_add_break() |> 
  flextable::body_add_flextable(
    gtsummary::as_flex_table(sensitivity_mi_edu_mal) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")) |> 
  officer::body_add_break() |>
  officer::body_end_section_landscape() |> 
  officer::body_add_par(
    "Observed associations between highest qualification and grip strength in women. \n", 
    style = "heading 2") |>  
  flextable::body_add_flextable(
    gtsummary::as_flex_table(
      sensitivity_mi_edu_fem) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")) |> 
  officer::body_add_break()  |> 
  officer::body_add_par(
    "Observed associations between IMD and grip strength in men and women. \n", 
    style = "heading 2") |>    
  flextable::body_add_flextable(
    gtsummary::as_flex_table(
      sensitivity_mi_imd) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")) |> 
  officer::body_add_break()  |> 
  officer::body_add_par(
    "Associations between education and grip strength in men, excluding those unable to grip trength for health reasons. \n", 
    style = "heading 2") |>    
  flextable::body_add_flextable(
    gtsummary::as_flex_table(
      table_edu_unabh_men) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")) |> 
  officer::body_add_break()  |> 
  officer::body_add_par(
    "Associations between education and grip strength in women, excluding those unable to grip trength for health reasons. \n", 
    style = "heading 2") |>    
  flextable::body_add_flextable(
    gtsummary::as_flex_table(
      table_edu_unabh_women) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit"))  |> 
  officer::body_add_break()  |> 
  officer::body_add_par(
    "Associations between IMD and grip strength in men and women, excluding those unable to grip trength for health reasons. \n", 
    style = "heading 2") |>    
  flextable::body_add_flextable(
    gtsummary::as_flex_table(
      table_unabh_imd) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit")) |> 
  officer::body_add_break()  |> 
  officer::body_add_par(
    "Sensitivity analysis to examine the impact of the different categorisations used for ethnicity. \n", 
    style = "heading 2") |>    
  flextable::body_add_flextable(
    gtsummary::as_flex_table(
      table_eth_sensi) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit"))   |> 
  officer::body_add_break()  |> 
  officer::body_add_par(
    "Sensitivity analysis focusing exclusively on England for IMD. \n", 
    style = "heading 2") |>    
  flextable::body_add_flextable(
    gtsummary::as_flex_table(
      table_england_imd) |> 
      flextable::fontsize(size = 9, part = "all") |> 
      flextable::set_table_properties(width = 1, layout = "autofit"))  

# save the Word document
print(
  sensitivity_mi,
  target = "02_outputs/2b_table_outputs/sensitivity_analyses_tables.docx")

