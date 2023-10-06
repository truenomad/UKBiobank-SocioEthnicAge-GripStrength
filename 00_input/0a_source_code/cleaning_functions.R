################################################################################
#          Functions for data  cleaning                                        #
################################################################################

# Function to categorise and label age groups and birth cohort -----------------

process_age_cohort_vars <- function(df) {
  # Function to categorize age
  categorise_age <- function(df, age_var) {
    breaks <- c(-Inf, 45, 50, 55, 60, 65, Inf)
    labels <- c(
      "Below 45", "45 to <50", "50 to <55",
      "55 to <60", "60 to <65", "Above 65"
    )
    retire_labels <- c(
      "Below 45", "45 to <50", "50 to <55",
      "55 to <60", "60 to 65"
    )
    
    df |>
      mutate(
        age_group = cut(
          .data[[age_var]],
          breaks = breaks,
          labels = labels,
          right = FALSE,
          include.lowest = TRUE
        ),
        age_group_retirement = cut(
          .data[[age_var]],
          breaks = breaks[-length(breaks)],
          labels = retire_labels,
          right = FALSE,
          include.lowest = TRUE
        ),
        !!paste0(
          "age_group_",
          substr(age_var, 5, 5)
        ) := factor(age_group, levels = labels),
        !!paste0(
          "age_group_retirement_",
          substr(age_var, 5, 5)
        ) := factor(age_group_retirement,
                    levels = retire_labels
        )
      ) |>
      select(-age_group, -age_group_retirement) 
    
  }
  
  # Rename and label variables
  df <- df |>
    rename(
      birth_year = f.34.0.0,
      birth_month = f.52.0.0,
      age_0 = f.21003.0.0,
      age_1 = f.21003.1.0,
      age_2 = f.21003.2.0,
      age_3 = f.21003.3.0,
      sex = f.31.0.0
    ) |>
    mutate(
      birth_year = structure(birth_year, label = "Year of birth"),
      birth_month = structure(birth_month, label = "Month of birth"),
      sex = structure(sex, label = "Sex"),
      age_0 = structure(age_0, label = "Age, baseline"),
      age_1 = structure(age_1, label = "Age, wave 1"),
      age_2 = structure(age_2, label = "Age, wave 2"),
      age_3 = structure(age_3, label = "Age, wave 3"),
      f.eid = structure(f.eid, label = "Participant ID")
    ) |> 
    mutate(
      sex = ifelse(sex == "Men", "Men", " Women")
    )
  
  # Create birth cohort variable
  df <- df |>
    mutate(
      birth_cohort = case_when(
        birth_year >= 1930 & birth_year <= 1939 ~ "1930s",
        birth_year >= 1940 & birth_year <= 1949 ~ "1940s",
        birth_year >= 1950 & birth_year <= 1959 ~ "1950s",
        birth_year >= 1960 & birth_year <= 1969 ~ "1960s",
        birth_year >= 1970 & birth_year <= 1979 ~ "1970s"
      ),
      birth_cohort = factor(birth_cohort, 
                            levels = c(
                              "1930s","1940s","1950s",
                              "1960s","1970s")
      ),
      birth_cohort = structure(birth_cohort, label = "Birth cohort")
    )
  
  # List of age variables
  age_vars <- paste0("age_", 0:3)
  
  # Loop over each age variable and apply the categorise_age function
  for (i in seq_along(age_vars)) {
    df <- categorise_age(df, age_vars[i])
  }
  
  df <- df |>
    mutate(across(
      starts_with("age_group"),
      ~ {
        wave_number <- str_extract(cur_column(), "\\d+$")
        structure(
          .,
          label = if (wave_number == "0") {
            "Age group, baseline"
          } else {
            glue::glue("Age group, wave {wave_number}")
          }
        )
      }
    ))
  
  
  return(df)
}

# Function to handle ethnicity categorisation for each wave --------------------

process_ethnicity_vars <- function(df) {
  df <- df |>
    mutate(
      ethnicity = f.21000.0.0,
      ethnic_group = case_when(
        ethnicity %in% c(
          "White", "British", "Irish",
          "Any other white background"
        ) ~ "White",
        ethnicity %in% c(
          "Asian or Asian British", "Indian",
          "Pakistani", "Bangladeshi",
          "Any other Asian background"
        ) ~ "South Asian",
        ethnicity == "Chinese" ~ "Chinese",
        ethnicity %in% c(
          "African", "Caribbean",
          "Black or Black British",
          "Any other Black background"
        ) ~ "Black",
        ethnicity %in% c(
          "Mixed", "White and Black Caribbean",
          "White and Black African", "White and Asian",
          "Any other mixed background"
        ) ~ "Mixed",
        ethnicity == "Other ethnic group" ~ "Other",
        TRUE ~ NA_character_
      ),
      ethnic_group = factor(ethnic_group, levels = c(
        "White", "South Asian", "Chinese",
        "Black", "Mixed", "Other"
      )),
      white_BAME = case_when(
        ethnic_group == "White" ~ "White",
        TRUE ~ "BAME"
      ),
      white_BAME = factor(white_BAME, levels = c(
        "White", "BAME")
      )) |> 
    mutate(ethnic_group2 = case_when(
      ethnicity == "White" | ethnicity == "British" |
        ethnicity == "Irish" | ethnicity == "Any other white background" ~ "White British",
      ethnicity == "Indian" ~  "Indian", 
      ethnicity == "Pakistani" ~  "Pakistani", 
      ethnicity == "Caribbean" ~  "Black Caribbean", 
      ethnicity == "African" ~  "Black African", 
      ethnicity == "Chinese" ~  "Chinese", 
      ethnicity == "Mixed" | ethnicity == "White and Black Caribbean" |
        ethnicity == "White and Black African" | ethnicity == "White and Asian" |
        ethnicity == "Any other mixed background" ~ "Mixed",
      ethnicity == "Other ethnic group" | ethnicity == "Asian or Asian British" |
        ethnicity == "Bangladeshi" |  ethnicity == "Any other Asian background" | 
        ethnicity == "Black or Black British" |
        ethnicity == "Any other Black background" ~ "Other"),
      ethnic_group2 = factor(
        ethnic_group2, levels = c(
          "White British", "Indian", 
          "Pakistani", "Black Caribbean",
          "Black African", "Chinese", "Mixed", "Other"))) |> 
    mutate(
      ethnic_group = structure(ethnic_group, label = "Ethnicity group"),
      ethnicity = structure(ethnicity, label = "Ethnicity"),
      white_BAME = structure(white_BAME, label = "White or Black and Minority Ethnic")
    )
  
  return(df)
}

# Function to handle education categorisation for each wave --------------------

process_education_vars <- function(df) {
  for (wave in 0:3) {
    # Education variables
    wave_var_edu <- paste0("f.6138.", wave, ".0")
    new_var_label_edu <- paste0("education_", wave)
    
    df <- df |>
      mutate(
        !!new_var_label_edu := case_when(
          !!sym(wave_var_edu) == "College or University degree" ~ "College or University degree",
          !!sym(wave_var_edu) == "A levels/AS levels or equivalent" ~ "A levels/AS levels or equivalent",
          !!sym(wave_var_edu) == "O levels/GCSEs or equivalent" ~ "O levels/GCSEs or equivalent",
          !!sym(wave_var_edu) == "CSEs or equivalent" ~ "CSEs or equivalent",
          !!sym(wave_var_edu) == "NVQ or HND or HNC or equivalent" ~ "NVQ or HND or HNC or equivalent",
          !!sym(wave_var_edu) == "Other professional qualifications eg: nursing, teaching" ~ "Other professional qualifications",
          !!sym(wave_var_edu) == "None of the above" ~ "None of the above",
          !!sym(wave_var_edu) == "Prefer not to answer" | 
            is.na(!!sym(wave_var_edu)) ~ NA_character_,
          TRUE ~ "0"
        ),
        !!new_var_label_edu := factor(!!sym(new_var_label_edu),  levels = c(
          "College or University degree",
          "A levels/AS levels or equivalent",
          "O levels/GCSEs or equivalent",
          "CSEs or equivalent",
          "NVQ or HND or HNC or equivalent",
          "Other professional qualifications",
          "None of the above"
        ))) |>
      mutate(
        !!new_var_label_edu := structure(
          !!sym(new_var_label_edu),
          label = if (wave == 0) {
            paste0("Highest educational qualification", ", baseline")
          } else {
            glue("Highest educational qualification, wave {wave}")
          }
        )
      )
    
    # Age completion variables
    if (wave < 3) {
      wave_var_age <- paste0("f.845.", wave, ".0")
      new_var_label_age <- paste0("age_compl_edu_", wave)
      
      df <- df |>
        mutate(
          !!new_var_label_age := coalesce(!!sym(wave_var_age), 0),
          !!new_var_label_age := ifelse(
            !!sym(new_var_label_age) %in% c(-1, -2, -3),
            NA,
            !!sym(new_var_label_age)
          ),
          !!new_var_label_age := structure(
            !!sym(new_var_label_age),
            label = if (wave == 0) {
              paste0("Age completed full-time education", ", baseline")
            } else {
              glue("Age completed full-time education, wave {wave}")
            }
          )
        )
    }
  }
  
  df <- df |>
    mutate(
      # Deriving education variable from all waves
      education = coalesce(education_0, education_1, education_2, education_3),
      education = case_when(
        education  %like%  "College or University degree" ~ "Degree",
        education %like% "A levels/AS levels" |  
          education %like% "Other professional" | 
          education %like% "NVQ or HND or HNC" ~  "A-levels, professional, or equiv.",
        education  %like%  "O levels/GCSEs" | education  %like% "CSEs or equivalent" ~ "O-levels, CSEs, or equiv.",
        education  %like%  "None of the above" ~  "No qualification",
        TRUE ~ NA_character_
      )) |>
    mutate(education = factor(education,
                              levels = c(
                                "Degree",
                                "A-levels, professional, or equiv.",
                                "O-levels, CSEs, or equiv.",
                                "No qualification"))
    ) |>
    # label new variable
    mutate(education = structure(education,
                                 label = "Highest educational qualification"
    )) |> 
    select(-education_0 , -education_1, -education_2, -education_3)
  
  
  
  return(df)
}

# Define a function to handle the occupational-based variables coding ----------

process_occupational_vars <- function(df) {
  
  # Define a named list to map column names to their respective labels
  label_names <- list(
    job_name_soc_0 = "Job category based on Standard Occupational Classification 2000",
    nssec_analytical = "Occupational class (NS-SEC analytical code)",
    nssec_5class = "Occupational class (NS-SEC analytical code, 5 category)",
    nssec_5class_v2 = "Occupational class (NS-SEC analytical code, 5 category)"
  )
  
  # Process the df dataset to derive new columns and labels
  df <- df |>
    # rename waves
    rename(
      soc2000_0 = f.20277.0.0,
      soc2000_1 = f.20277.1.0,
      soc2000_2 = f.20277.2.0,
      soc2000_3 = f.20277.3.0,
    ) |>
    mutate(
      soc2000 = coalesce(soc2000_0, soc2000_1, soc2000_2, soc2000_3),
      job_name_soc_0 = soc_nssec$standard_occupational_classification_2000[match(soc2000_0, soc_nssec$soc_group)],
      nssec_0 = soc_nssec$simplified_nssec[match(soc2000_0, soc_nssec$soc_group)],
      # Create nssec_analytical column based on the value ranges of nssec_0
      nssec_analytical = factor(case_when(
        nssec_0 == 2 ~ "1.1 - Large employers & higher managerial",
        nssec_0 > 2 & nssec_0 < 4 ~ "1.2 - Higher professional occupations",
        nssec_0 >= 4 & nssec_0 < 7 ~ "2 - Lower managl/admin & profes. occupationss",
        nssec_0 >= 7 & nssec_0 < 8 ~ "3 - Intermediate occupations",
        nssec_0 >= 8 & nssec_0 < 10 ~ "4 - Small employers & own account workers",
        nssec_0 >= 10 & nssec_0 < 12 ~ "5 - Lower supervisory & technical occupationss",
        nssec_0 >= 12 & nssec_0 < 13 ~ "6 - Semi-routine occupationss",
        nssec_0 >= 13 & nssec_0 < 14 ~ "7 - Routine occupations",
        nssec_0 == 14 ~ "8- Never worked/unemployed"
      )),
      # Create nssec_5class column based on the value ranges of nssec_0
      nssec_5class = factor(case_when(
        nssec_0 >= 2 & nssec_0 < 7 ~ "Managerial/professional occupations",
        nssec_0 >= 7 & nssec_0 < 8 ~ "Intermediate occupations",
        nssec_0 >= 8 & nssec_0 < 10 ~ "Small employers & owner account workers",
        nssec_0 >= 10 & nssec_0 < 12 ~ "Lower supervisory & technical occupations",
        nssec_0 >= 12 & nssec_0 < 14 ~ "Semi-routine/routine occupations",
        is.na(nssec_0) ~ "Never worked/unemployed"
      )),
      # Create nssec_5class_v2 column based on the value ranges of nssec_0, excluding the unemployed category
      nssec_5class_v2 = factor(case_when(
        nssec_0 >= 2 & nssec_0 < 7 ~ "Managerial/professional occupations",
        nssec_0 >= 7 & nssec_0 < 8 ~ "Intermediate occupations",
        nssec_0 >= 8 & nssec_0 < 10 ~ "Small employers & owner account workers",
        nssec_0 >= 10 & nssec_0 < 12 ~ "Lower supervisory & technical occupations",
        nssec_0 >= 12 & nssec_0 < 14 ~ "Semi-routine/routine occupations"
      ))
    ) |>
    # Apply labels to the derived columns based on the label_names list
    mutate(across(
      all_of(names(label_names)),
      ~ structure(.x, label = label_names[[cur_column()]])
    ))
  
  original_values <- c(
    "In paid employment or self-employed",
    "Unemployed",
    "Retired",
    "Unable to work because of sickness or disability",
    "Looking after home and/or family",
    "Doing unpaid or voluntary work",
    "Full or part-time student"
  )
  
  new_values <- 1:7
  new_labels <- c(
    "Employed",
    "Unemployed",
    "Retired",
    "Unable to work due to illness",
    "Caring for home/family",
    "Unpaid work",
    "Student"
  )
  
  for (wave in 0:3) {
    wave_var <- paste0("f.6142.", wave, ".0")
    new_var_label <- paste0("job_status_", wave)
    
    df <- df |>
      mutate(
        !!new_var_label := factor(!!sym(wave_var), levels = original_values, labels = new_values),
        !!new_var_label := factor(!!sym(new_var_label), levels = new_values, labels = new_labels),
        !!new_var_label := structure(!!sym(new_var_label), label = paste0("Occupation status, wave ", wave))
      )
  }
  return(df)
}

# Function to process income for each wave of data -----------------------------

process_income_vars <- function(data) {
  for (wave_suffix in 0:3) {
    wave_var <- paste0("f.738.", wave_suffix, ".0")
    income_var <- paste0("house_income_", wave_suffix)
    
    data <- data |>
      mutate(
        across(c(all_of(wave_var)), 
               ~ case_when(
                 . == "Greater than 100,000" ~ 1,
                 . == "52,000 to 100,000" ~ 2,
                 . == "31,000 to 51,999" ~ 3,
                 . == "18,000 to 30,999" ~ 4,
                 . == "Less than 18,000" ~ 5,
                 . == "Prefer not to answer" | . == "Do not know" ~ NA_integer_,
                 TRUE ~ 0
               ), 
               .names = "{col}")
      )
  }
  return(data)
}

# Function to process occupational activity variables for each wave ------------

# Define a function to process each wave of data
process_occup_activity_vars <- function(data) {
  categorise_activity <- function(df, wave, wlk_var, manual_var) {
    occup_activity_var <- paste0("occupational_activity_", wave)
    df |>
      mutate(
        !!wlk_var := case_when(
          .data[[wlk_var]] == "Never/rarely" ~ "Never/rarely",
          .data[[wlk_var]] == "Sometimes" ~ "Sometimes",
          .data[[wlk_var]] == "Usually" | .data[[wlk_var]] == "Always" ~ "Usually/always"
        ),
        !!manual_var := case_when(
          .data[[manual_var]] == "Never/rarely" ~ "Never/rarely",
          .data[[manual_var]] == "Sometimes" ~ "Sometimes",
          .data[[manual_var]] == "Usually" | .data[[manual_var]] == "Always" ~ "Usually/always"
        ),
        !!wlk_var := factor(.data[[wlk_var]], levels = c("Never/rarely", "Sometimes", "Usually/always")),
        !!manual_var := factor(.data[[manual_var]], levels = c("Never/rarely", "Sometimes", "Usually/always")),
        !!occup_activity_var := case_when(
          (.data[[wlk_var]] == "Never/rarely") & (.data[[manual_var]] %in% c("Never/rarely", "Sometimes", "Usually/always")) ~ "No manual, No standing/walking",
          (.data[[manual_var]] == "Never/rarely") & (.data[[wlk_var]] == "Sometimes") ~ "No manual, Some standing/walking",
          (.data[[manual_var]] == "Never/rarely") & (.data[[wlk_var]] == "Usually/always") ~ "No manual, Mostly standing/walking",
          (.data[[manual_var]] == "Sometimes") & (.data[[wlk_var]] == "Sometimes") ~ "Some manual, Some standing/walking",
          (.data[[manual_var]] == "Sometimes") & (.data[[wlk_var]] == "Usually/always") ~ "Some manual, Mostly standing/walking",
          (.data[[manual_var]] == "Usually/always") | (.data[[wlk_var]] %in% c("Usually/always", "Sometimes")) ~ "Mostly manual, Mostly standing/walking"
        ),
        !!occup_activity_var := factor(.data[[occup_activity_var]], levels = c(
          "No manual, No standing/walking",
          "No manual, Some standing/walking",
          "No manual, Mostly standing/walking",
          "Some manual, Some standing/walking",
          "Some manual, Mostly standing/walking",
          "Mostly manual, Mostly standing/walking"
        ))
      ) |> mutate(
        !!occup_activity_var := structure(
          !!sym(occup_activity_var),
          label = if (wave == 0) {
            "Occupational activity, baseline"
          } else {
            glue("Occupational activity, wave {wave}")
          }
        )
      )
  }
  
  for (wave in 0:3) {
    wlk_stnd_var <- paste0("f.806.", wave, ".0")
    manual_phys_var <- paste0("f.816.", wave, ".0")
    
    data <- categorise_activity(data, wave, wlk_stnd_var, manual_phys_var)
  }
  
  return(data)
}
# Function to process housing variables for each wave --------------------------

process_housing_vars <- function(df) {
  
  process_var <- function(df, wave, var_type) {
    old_col_name <- if (var_type == "housing_tenure") 
      glue("f.680.{wave}.0") else glue("f.670.{wave}.0")
    new_col_name <- glue("{var_type}_{wave}")
    
    if (var_type == "housing_tenure") {
      df <- df |>
        mutate(!!new_col_name := case_when(
          str_detect(.data[[old_col_name]], "Own outright") ~ "Own outright",
          str_detect(.data[[old_col_name]], "Own with a mortgage") ~ "Own with a mortgage",
          str_detect(.data[[old_col_name]], "Rent - from local") ~ "Rent - from local",
          str_detect(.data[[old_col_name]], "Rent - from private land") ~ "Rent - from private land",
          str_detect(.data[[old_col_name]], "Pay part rent and part mortgage") ~ "Pay part rent and part mortgage",
          TRUE ~ NA_character_
        )) |>
        mutate(
          !!new_col_name := structure(
            !!sym(new_col_name),
            label = if (wave == 0) {
              "Housing tenure, baseline"
            } else {
              glue("Housing tenure, wave {wave}")
            }
          )
        )
      
    } else if (var_type == "type_of_accom") {
      
      df <- df |>
        mutate(!!new_col_name := case_when(
          .data[[old_col_name]] == "A house or bungalow" ~ "A house or bungalow",
          .data[[old_col_name]] == "A flat, maisonette or apartment" ~ "A flat, maisonette or apartment",
          str_detect(.data[[old_col_name]], "Mobile or temporary") ~ "Mobile or temporary",
          .data[[old_col_name]] == "Sheltered accommodation" ~ "Sheltered accommodation",
          .data[[old_col_name]] == "Care home" ~ "Care home",
          TRUE ~ NA_character_
        )) |>
        mutate(
          !!new_col_name := structure(
            !!sym(new_col_name),
            label = if (wave == 0) {
              "Type of accomodation, baseline"
            } else {
              glue("Type of accomodation, wave {wave}")
            }
          )
        )
      
    } else if (var_type == "no_in_household") {
      household_col <- glue("f.709.{wave}.0")
      df <- df |>
        mutate(!!new_col_name := if_else(
          !!sym(household_col) == -1 | !!sym(household_col) == -3, 
          NA_real_, !!sym(household_col))) |> 
        mutate(
          !!new_col_name := structure(
            !!sym(new_col_name),
            label = if (wave == 0) {
              "Number in household, baseline"
            } else {
              glue("Number in household, wave {wave}")
            }
          )
        )
      
    }
    
    df <- df |>
      mutate(
        !!new_col_name := {
          lbl <- attr(!!sym(new_col_name), "label")
          val <- as.factor(!!sym(new_col_name))
          structure(val, label = lbl)
        }
      )
    
    return(df)
  }
  
  crossed_args <- cross2(0:3, c("housing_tenure", "type_of_accom", "no_in_household"))
  
  result <- reduce(
    crossed_args,
    function(df, args) {
      wave <- args[[1]]
      var_type <- args[[2]]
      return(process_var(df, wave, var_type))
    },
    .init = df
  )
  
  return(result)
}

# Function to process deprivation variables ------------------------------------
process_deprivation_vars <- function(data) {
  
  # function to factorise quantile
  factor_quantile <- function(df, var_name) {
    var_sym <- sym(var_name)
    df |>
      mutate(
        !!var_sym := factor(
          !!var_sym,
          levels = c("1 (most affluent)", "2", "3", "4", "5 (least affluent)")
        )
      )
  }
  
  data <- data |>
    mutate(
      town_dep_index = f.189.0.0,
      town_dep_index_grps = dvmisc::quant_groups(town_dep_index, 5),
      town_dep_index = case_when(
        town_dep_index_grps == "[-6.26,-3.93]" ~ "Townsend quintile 1",
        town_dep_index_grps == "(-3.93,-2.76]" ~ "Townsend quintile 2",
        town_dep_index_grps == "(-2.76,-1.3]" ~ "Townsend quintile 3",
        town_dep_index_grps == "(-1.3,1.35]" ~ "Townsend quintile 4",
        town_dep_index_grps == "(1.35,11]" ~ "Townsend quintile 5",
        TRUE ~ NA_character_
      ),
      imd_england = f.26410.0.0,
      imd_eng_grps = dvmisc::quant_groups(imd_england, 5),
      imd_eng_quintile = case_when(
        imd_eng_grps == "[0.61,6.61]" ~ "1 (most affluent)",
        imd_eng_grps == "(6.61,10.6]" ~ "2",
        imd_eng_grps == "(10.6,16.3]" ~ "3",
        imd_eng_grps == "(16.3,27.3]" ~ "4",
        imd_eng_grps == "(27.3,82]" ~ "5 (least affluent)",
        TRUE ~ NA_character_
      ),
      imd_scotland = f.26427.0.0,
      imd_scot_grps = dvmisc::quant_groups(imd_scotland, 5),
      imd_scot_quintile = case_when(
        imd_scot_grps == "[0.677,3.81]" ~ "1 (most affluent)",
        imd_scot_grps == "(3.81,6.42]" ~ "2",
        imd_scot_grps == "(6.42,11.8]" ~ "3",
        imd_scot_grps == "(11.8,23.6]" ~ "4",
        imd_scot_grps == "(23.6,90]" ~ "5 (least affluent)",
        TRUE ~ NA_character_
      ),
      imd_wales = f.26426.0.0,
      imd_wales_grps = dvmisc::quant_groups(imd_wales, 5),
      imd_wales_quintile = case_when(
        imd_wales_grps == "[2.2,5.1]" ~ "1 (most affluent)",
        imd_wales_grps == "(5.1,8]" ~ "2",
        imd_wales_grps == "(8,12.8]" ~ "3",
        imd_wales_grps == "(12.8,22.9]" ~ "4",
        imd_wales_grps == "(22.9,74.5]" ~ "5 (least affluent)",
        TRUE ~ NA_character_
      )
    ) |> 
    factor_quantile("imd_eng_quintile") |> 
    factor_quantile("imd_wales_quintile") |> 
    factor_quantile("imd_scot_quintile") |> 
    mutate(
      imd_quantile = coalesce(imd_eng_quintile, 
                              imd_wales_quintile, imd_scot_quintile),
      imd_england = structure(imd_eng_quintile, label = "IMD for England, baseline"),
      imd_wales = structure(imd_wales_quintile, label = "IMD for Wales, baseline"),
      imd_scotland = structure(imd_scot_quintile, label = "IMD for Scotland, baseline"),
      imd_quantile = structure(imd_quantile, label = "IMD quantiles, baseline"),
      town_dep_index = structure(town_dep_index, label = "Townsend deprivation index, baseline")
    ) |> 
    mutate(region_england = ifelse(is.na(imd_england), "No", "Yes")) |> 
    select(
      -imd_wales_grps, -imd_wales_quintile, 
      -imd_eng_grps, -imd_eng_quintile,
      -imd_scot_grps, -imd_scot_quintile,
      -town_dep_index_grps
    )
  
  return(data)
}

# Function to process follow-up variables --------------------------------------

process_follow_up_vars <- function(df) {
  
  df <- df |>
    mutate(
      rsn_lost_follow_up = case_when(
        f.190.0.0 == "NHS records indicate they have left the UK" | 
          str_detect(f.190.0.0, "UK Biobank sources") ~ 1,
        str_detect(f.190.0.0, "Death reported") ~ 2,
        str_detect(f.190.0.0, "Participant has withdrawn") ~ 3,
        TRUE ~ NA_real_
      )
    ) |>
    mutate(
      rsn_lost_follow_up = as.factor(rsn_lost_follow_up),
      rsn_lost_follow_up = fct_recode(
        rsn_lost_follow_up,
        "Emigration" = "1",
        "Death" = "2",
        "Dropped out" = "3"
      )
    ) |>
    mutate(
      rsn_lost_follow_up = structure(
        rsn_lost_follow_up,
        label = "Reason lost to follow-up, baseline"
      )
    ) |>
    mutate(
      date_lost_follow_up = f.191.0.0
    ) |>
    mutate(
      date_lost_follow_up = structure(date_lost_follow_up, label = "Date lost to follow-up")
    )
  
  return(df)
}

# Function to process early-life factors ----------------------------------------

process_earlylife_vars <- function(data) {
  data <- data |>
    # Part of a multiple birth
    mutate(part_of_multip_birth = case_when(
      f.1777.0.0 == "No" ~ 1,
      f.1777.0.0 == "Yes" ~ 2,
      TRUE ~ NA_real_
    )) |>
    mutate(
      part_of_multip_birth = as.factor(part_of_multip_birth),
      part_of_multip_birth = fct_recode(part_of_multip_birth,
                                        "No" = "1",
                                        "Yes" = "2"
      ),
      part_of_multip_birth = structure(part_of_multip_birth,
                                       label = "Part of a multiple birth"
      )
    ) |>
    
    # Birth weight
    mutate(
      birth_weight = coalesce(f.20022.0.0, f.20022.1.0, f.20022.2.0),
      birth_weight = ifelse(is.na(birth_weight), NA_real_, birth_weight)
    ) |>
    mutate(
      birth_weight = structure(birth_weight,
                               label = "Birth weight (kg)"
      )
    ) |>
    
    # Comparative body size at age 10
    mutate(
      body_size10yr = coalesce(f.1687.0.0, f.1687.1.0, f.1687.2.0),
      body_size10yr = case_when(
        body_size10yr == "About average" ~ 1,
        body_size10yr == "Plumper" ~ 2,
        body_size10yr == "Thinner" ~ 3,
        TRUE ~ NA_real_
      )) |>
    mutate(
      body_size10yr = as.factor(body_size10yr),
      body_size10yr = fct_recode(body_size10yr,
                                 "About average" = "1",
                                 "Plumper" = "2",
                                 "Thinner" = "3"
      ),
      body_size10yr = structure(body_size10yr,
                                label = "Comparative body size at age 10"
      )
    ) |>
    # Comparative height at age 10
    mutate(
      height_10yr = coalesce(f.1697.0.0, f.1697.1.0, f.1697.2.0),
      height_10yr = case_when(
        height_10yr == "About average" ~ 1,
        height_10yr == "Taller" ~ 2,
        height_10yr == "Shorter" ~ 3,
        TRUE ~ NA_real_
      )) |>
    mutate(
      height_10yr = as.factor(height_10yr),
      height_10yr = fct_recode(height_10yr,
                               "About average" = "1",
                               "Taller" = "2",
                               "Shorter" = "3"
      ),
      height_10yr = structure(height_10yr,
                              label = "Comparative height at age 10"
      )
    ) |>
    
    # Breastfed as a baby
    mutate(
      breastfed = coalesce(f.1677.0.0, f.1677.1.0, f.1677.2.0),
      breastfed = case_when(
        breastfed == "No" ~ 1,
        breastfed == "Yes" ~ 2,
        TRUE ~ NA_real_
      )) |>
    mutate(
      breastfed = as.factor(breastfed),
      breastfed = fct_recode(breastfed,
                             "No" = "1",
                             "Yes" = "2"
      ),
      breastfed = structure(breastfed,
                            label = "Breastfed as a baby"
      )
    ) |>
    
    mutate(
      maternal_smoking = coalesce(f.1787.0.0, f.1787.1.0, f.1787.2.0),
      maternal_smoking = case_when(
        maternal_smoking == "No" ~ 1,
        maternal_smoking == "Yes" ~ 2,
        TRUE ~ NA_real_
      )) |>
    mutate(
      maternal_smoking = as.factor(maternal_smoking),
      maternal_smoking = fct_recode(maternal_smoking,
                                    "No" = "1",
                                    "Yes" = "2"
      ),
      maternal_smoking = structure(maternal_smoking,
                                   label = "Maternal smoking around birth"
      )
    ) 
  
  return(data)
}

# Function to process physical activity variables ------------------------------

process_physical_activity_vars <- function(df) {
  
  
  df <- df |>
    mutate(
      abv_modr_vig_recommend = case_when(
        f.22035.0.0 == "No" ~ 1,
        f.22035.0.0 == "Yes" ~ 2,
        TRUE ~ NA_real_
      ),
      abv_modr_vig_recommend = as.factor(abv_modr_vig_recommend),
      abv_modr_vig_recommend = fct_recode(
        abv_modr_vig_recommend,
        "No" = "1",
        "Yes" = "2"
      ),
      abv_modr_vig_recommend = structure(
        abv_modr_vig_recommend,
        label = "Above moderate/vigorous recommendation (MET)"
      ),
      
      abv_modr_vig_wlk_recommend = case_when(
        f.22036.0.0 == "No" ~ 1,
        f.22036.0.0 == "Yes" ~ 2,
        TRUE ~ NA_real_
      ),
      abv_modr_vig_wlk_recommend = as.factor(abv_modr_vig_wlk_recommend),
      abv_modr_vig_wlk_recommend = fct_recode(
        abv_modr_vig_wlk_recommend,
        "No" = "1",
        "Yes" = "2"
      ),
      abv_modr_vig_wlk_recommend = structure(
        abv_modr_vig_wlk_recommend,
        label = "Above moderate/vigorous walking recommendation (MET)"
      ),
      
      ipaq_active_grp = case_when(
        f.22032.0.0 == "high" ~ 1,
        f.22032.0.0 == "moderate" ~ 2,
        f.22032.0.0 == "low" ~ 3,
        TRUE ~ NA_real_
      ),
      ipaq_active_grp = as.factor(ipaq_active_grp),
      ipaq_active_grp = fct_recode(
        ipaq_active_grp,
        "High" = "1",
        "Moderate" = "2",
        "Low" = "3"
      ),
      ipaq_active_grp = structure(
        ipaq_active_grp,
        label = "IPAQ activity group"
      ),
      
      met_mpw_moderate_activ = ifelse(is.na(f.22038.0.0), 
                                      NA_real_, f.22038.0.0),
      met_mpw_moderate_activ = structure(
        met_mpw_moderate_activ,
        label = "MET minutes per week for moderate activity"
      ),
      
      met_mpw_vigorous_activ = ifelse(is.na(f.22039.0.0), 
                                      NA_real_, f.22039.0.0),
      met_mpw_vigorous_activ = structure(
        met_mpw_vigorous_activ,
        label = "MET minutes per week for vigorous activity"
      ),
      
      met_mpw_walking = ifelse(is.na(f.22037.0.0), 
                               NA_real_, f.22037.0.0),
      met_mpw_walking = structure(
        met_mpw_walking,
        label = "MET minutes per week for walking"
      ),
      
      summed_minutes_activity = ifelse(is.na(f.22034.0.0), 
                                       NA_real_, f.22034.0.0),
      summed_minutes_activity = structure(
        summed_minutes_activity,
        label = "Summed minutes activity"
      ),
      
      summed_days_activity = ifelse(is.na(f.22033.0.0), 
                                    NA_real_, f.22033.0.0),
      summed_days_activity = structure(
        summed_days_activity,
        label = "Summed days activity"
      )
    )
  
  
  process_var <- function(df, wave, activity_type) {
    old_col_name <- glue("f.{ifelse(wave == 0, 884, 904)}.{wave}.0")
    new_col_name <- glue("days_wks_{activity_type}_{wave}")
    
    df <- df |>
      mutate(
        !!new_col_name := !!sym(old_col_name),
        !!new_col_name := case_when(
          !!sym(old_col_name) < 0 ~ NA_real_,
          TRUE ~ !!sym(old_col_name)
        )
      )
    
    
    # Assign labels separately to avoid issues with mutate
    attr(df[[new_col_name]], "label") <- glue("Days of {activity_type} 10+ minutes, wave {wave}")
    
    return(df)
  }
  
  # Assuming df is your data frame
  for (wave in 0:3) {
    df <- process_var(df, wave, "mod_act")
    df <- process_var(df, wave, "vig_act")
  }
  
  for (wave in 0:3) {
    df <- process_var(df, wave, "mod_act")
    df <- process_var(df, wave, "vig_act")
  }
  
  return(df)
}

# Function to process smoking variables ----------------------------------------

process_smoking_vars <- function(df) {
  
  process_var <- function(df, wave) {
    old_col_name <- glue("f.20116.{wave}.0")
    new_col_name <- glue("smoking_status_{wave}")
    
    df <- df |>
      mutate(!!new_col_name := case_when(
        .data[[old_col_name]] == "Never" ~ 1,
        .data[[old_col_name]] == "Previous" ~ 2,
        .data[[old_col_name]] == "Current" ~ 3,
        .data[[old_col_name]] == "Prefer not to answer" |
          .data[[old_col_name]] == 0 ~ NA_real_,
        TRUE ~ NA_real_
      )) |>
      mutate(!!new_col_name := as.factor(!!sym(new_col_name))) |>
      mutate(!!new_col_name := structure(
        !!sym(new_col_name),
        label = glue("Smoking status, wave {wave}")
      ))
    
    return(df)
  }
  
  result <- df
  
  for (wave in 0:3) {
    result <- process_var(result, wave)
  }
  
  return(result)
}

# Function to process alcohol variables ----------------------------------------
process_alcohol_vars <- function(df) {
  
  process_var <- function(df, wave) {
    old_col_name <- glue("f.1558.{wave}.0")
    new_col_name <- glue("alcohol_freq_{wave}")
    
    df <- df |>
      mutate(!!new_col_name := case_when(
        .data[[old_col_name]] == "Daily or almost daily" ~ 1,
        .data[[old_col_name]] == "Three or four times a week" ~ 2,
        .data[[old_col_name]] == "Once or twice a week" ~ 3,
        .data[[old_col_name]] == "One to three times a month" ~ 4,
        .data[[old_col_name]] == "Special occasions only" ~ 5,
        .data[[old_col_name]] == "Never" ~ 6,
        .data[[old_col_name]] == "Prefer not to answer" |
          .data[[old_col_name]] == 0 ~ NA_real_,
        TRUE ~ NA_real_
      )) |>
      mutate(!!new_col_name := as.factor(!!sym(new_col_name))) |>
      mutate(!!new_col_name := structure(
        !!sym(new_col_name),
        label = glue("Frequency of drinking alcohol, wave {wave}")
      ))
    
    return(df)
  }
  
  result <- df
  
  for (wave in 0:3) {
    result <- process_var(result, wave)
  }
  
  return(result)
}

# Function to process sedentary behaviour variables ----------------------------

process_sedentary_vars <- function(df) {
  df <- df |>
    mutate(computer_use_hours_days_0 = f.1080.0.0) |>
    mutate(computer_use_hours_days_0 = ifelse(computer_use_hours_days_0 < 0, NA, computer_use_hours_days_0)) |>
    mutate(computer_use_hours_days_0 = structure(computer_use_hours_days_0, label = "Computer use (hours/day)")) |>
    mutate(tv_watching_hours_days_0 = f.1070.0.0) |>
    mutate(tv_watching_hours_days_0 = ifelse(tv_watching_hours_days_0 < 0, NA, tv_watching_hours_days_0)) |>
    mutate(tv_watching_hours_days_0 = structure(tv_watching_hours_days_0, label = "TV viewing (hours/day)")) |>
    mutate(sedentary_hours_day_0 = case_when(
      is.na(computer_use_hours_days_0) & !is.na(tv_watching_hours_days_0) ~ tv_watching_hours_days_0,
      is.na(tv_watching_hours_days_0) & !is.na(computer_use_hours_days_0) ~ computer_use_hours_days_0,
      !is.na(tv_watching_hours_days_0) & !is.na(computer_use_hours_days_0) ~ computer_use_hours_days_0 + tv_watching_hours_days_0,
      TRUE ~ NA_integer_
    )) |>
    mutate(sedentary_hours_day_0 = structure(sedentary_hours_day_0, label = "Sedentary behaviour (hours/day)")) |>
    mutate(sedentary_hours_day_0_log = log(sedentary_hours_day_0 + 1))  |>
    mutate(sedentary_hours_day_0_log = structure(sedentary_hours_day_0_log, label = "Sedentary behaviour (hours/day) ln")) |>
    mutate(sedentary_hours_day_0_cat = case_when(
      sedentary_hours_day_0 <= 1 ~ "1hr or less a day",
      sedentary_hours_day_0 == 2 ~ "2hrs a day",
      sedentary_hours_day_0 == 3 ~ "3hrs a day",
      sedentary_hours_day_0 == 4 ~ "4hrs a day",
      sedentary_hours_day_0 == 5 ~ "5hrs a day",
      sedentary_hours_day_0 >= 6 ~ "6hrs or more a day"
    ),
    sedentary_hours_day_0 = factor(
      sedentary_hours_day_0_cat,
      levels = c("1hr or less a day", "2hrs a day", "3hrs a day", "4hrs a day", "5hrs a day", "6hrs or more a day")
    )) |> 
    select(-sedentary_hours_day_0_cat, -sedentary_hours_day_0_log) |> 
    mutate(sedentary_hours_day_0 = structure(sedentary_hours_day_0, 
                                             label = "Sedentary behaviour (hours/day) categories")
    )
  
  
  return(df)
}

# Function to process body size variables from UK Biobank ----------------------

process_bodysize_vars <- function(df) {
  
  variable_labels <- list(
    weight = "21002",
    height = "50",
    waist_circum = "48",
    hip_circum = "49",
    bmi = "21001",
    body_fat_perc = "23099"
  )
  
  for(label in names(variable_labels)) {
    for(wave in 0:3) {
      var_name <- paste0(label, "_", wave)
      code <- paste0("f.", variable_labels[[label]], ".", wave, ".0")
      
      df <- df |> mutate(!!var_name := !!sym(code)) |> 
        mutate(!!var_name := if_else(is.na(!!sym(var_name)), NA_real_, !!sym(var_name))) 
      
      label_from_switch <- switch(
        label,
        "height" = "Height",
        "weight" = "Weight",
        "bmi" = "BMI",
        "hip_circum" = "Hip circumference",
        "waist_circum" = "Waist circumference",
        "body_fat_perc" = "Body fat percentage"
      )
      
      # label vars
      df <- df |> 
        mutate(
          !!var_name := structure(
            !!sym(var_name),
            label = if (wave == 0) {
              paste0(label_from_switch, ", baseline")
            } else {
              glue("{label_from_switch}, wave {wave}")
            }
          )
        )
      
    }
  }
  
  return(df)
}

# Function to process handgrip strength variables ------------------------------
process_gripstrength_vars <- function(df) {
  
  df <- df |>
    mutate(
      handgrip_right_0 = case_when(
        !is.na(f.47.0.0) ~ f.47.0.0,
        TRUE ~ NA_real_
      )
    ) |>
    mutate(
      handgrip_right_0 = structure(handgrip_right_0, 
                                   label = "Hand grip strength (right), baseline")
    ) |>
    mutate(
      handgrip_left_0 = case_when(
        !is.na(f.46.0.0) ~ f.46.0.0,
        TRUE ~ NA_real_
      )
    ) |>
    mutate(
      handgrip_left_0 = structure(handgrip_left_0, 
                                  label = "Hand grip strength (right), baseline")
    ) |> 
    mutate(
      handgrip_max_0 = handgrip_right_0,
      handgrip_max_0 = pmax(handgrip_right_0, handgrip_left_0, na.rm = TRUE)
    ) |>
    mutate(
      handgrip_max_0 = case_when(
        handgrip_max_0 == 0 ~ NA_real_,
        TRUE ~ handgrip_max_0
      ),
      handgrip_max_0 = structure(handgrip_max_0, label = "Max grip strength (kg), baseline")
    )
  
  df <- df |> 
    mutate(rsn_miss_hgs_right =  case_when(
      f.20043.0.0  == 101 ~ "Against participant wishes", 
      f.20043.0.0  == 102 ~ "Lack of time", 
      f.20043.0.0  == 103 ~ "Feeling unwell", 
      f.20043.0.0  %in% c(201, 202) ~ "Equipment failure", 
      f.20043.0.0  == 402 ~ "Unable due to being amputee", 
      f.20043.0.0  == 404 ~ "Unable due to limb injury", 
      f.20043.0.0  == 415 ~ "Unable due to arthritis", 
      f.20043.0.0  == 416 ~ "Unable due to recent surgery", 
      f.20043.0.0  == 417 ~ "Unable due to stroke/weakness/paralysis", 
      f.20043.0.0  == 405 ~ "Unable due to other health reason", 
      f.20043.0.0  == 407 ~ "Unable due to unknown reason", 
      f.20043.0.0  == 300 ~ "Reason not known", 
      TRUE ~ NA_character_ ), 
      rsn_miss_hgs_right = factor(
        rsn_miss_hgs_right, 
        levels = c(
          "Against participant wishes", 
          "Lack of time", "Feeling unwell", 
          "Equipment failure", 
          "Unable due to being amputee", 
          "Unable due to limb injury", 
          "Unable due to arthritis", 
          "Unable due to recent surgery", 
          "Unable due to stroke/weakness/paralysis", 
          "Unable due to other health reason", 
          "Unable due to unknown reason", 
          "Reason not known" )))
  
  df <- df |> 
    mutate( rsn_miss_hgs_left =  case_when(
      f.20044.0.0 == 101 ~ "Against participant wishes", 
      f.20044.0.0 == 102 ~ "Lack of time", 
      f.20044.0.0 == 103 ~ "Feeling unwell", 
      f.20044.0.0 %in% c(201, 202) ~ "Equipment failure", 
      f.20044.0.0 == 402 ~ "Unable due to being amputee", 
      f.20044.0.0 == 404 ~ "Unable due to limb injury", 
      f.20044.0.0 == 415 ~ "Unable due to arthritis", 
      f.20044.0.0 == 416 ~ "Unable due to recent surgery", 
      f.20044.0.0 == 417 ~ "Unable due to stroke/weakness/paralysis", 
      f.20044.0.0 == 405 ~ "Unable due to other health reason", 
      f.20044.0.0 == 407 ~ "Unable due to unknown reason", 
      f.20044.0.0 == 300 ~ "Reason not known", 
      TRUE ~ NA_character_ ), 
      rsn_miss_hgs_left = factor(
        rsn_miss_hgs_left, 
        levels = c(
          "Against participant wishes", 
          "Lack of time", "Feeling unwell", 
          "Equipment failure", 
          "Unable due to being amputee", 
          "Unable due to limb injury", 
          "Unable due to arthritis", 
          "Unable due to recent surgery", 
          "Unable due to stroke/weakness/paralysis", 
          "Unable due to other health reason", 
          "Unable due to unknown reason", 
          "Reason not known" )))
  
  
  
  df <- df |>
    mutate(rsn_grip_missing = case_when(
      (rsn_miss_hgs_right == "Against participant wishes" |
         rsn_miss_hgs_left == "Against participant wishes") & is.na(handgrip_max_0)  ~ "Against participant wishes",
      (rsn_miss_hgs_right == "Lack of time" |
         rsn_miss_hgs_left == "Lack of time") & is.na(handgrip_max_0)  ~ "Lack of time",
      (rsn_miss_hgs_right == "Feeling unwell" |
         rsn_miss_hgs_left == "Feeling unwell") & is.na(handgrip_max_0)  ~ "Feeling unwell",
      (rsn_miss_hgs_right == "Equipment failure" |
         rsn_miss_hgs_left == "Equipment failure") & is.na(handgrip_max_0)  ~ "Equipment failure",
      (rsn_miss_hgs_right == "Unable due to being amputee" |
         rsn_miss_hgs_left == "Unable due to being amputee") & is.na(handgrip_max_0)  ~ "Unable due to being amputee",
      (rsn_miss_hgs_right == "Unable due to limb injury" |
         rsn_miss_hgs_left == "Unable due to limb injury") & is.na(handgrip_max_0)  ~ "Unable due to limb injury",
      (rsn_miss_hgs_right == "Unable due to arthritis" |
         rsn_miss_hgs_left == "Unable due to arthritis") & is.na(handgrip_max_0)  ~ "Unable due to arthritis",
      (rsn_miss_hgs_right == "Unable due to recent surgery" |
         rsn_miss_hgs_left == "Unable due to recent surgery") & is.na(handgrip_max_0)  ~ "Unable due to recent surgery",
      (rsn_miss_hgs_right == "Unable due to stroke/weakness/paralysis" |
         rsn_miss_hgs_left == "Unable due to stroke/weakness/paralysis") & is.na(handgrip_max_0)  ~ "Unable due to stroke/weakness/paralysis",
      (rsn_miss_hgs_right == "Unable due to other health reason" |
         rsn_miss_hgs_left == "Unable due to other health reason") & is.na(handgrip_max_0)  ~ "Unable due to other health reason",
      (rsn_miss_hgs_right == "Reason not known" | rsn_miss_hgs_left == "Reason not known") & is.na(handgrip_max_0)  ~ "Reason not known")) |>
    mutate(rsn_grip_missing_cat = case_when(
      rsn_grip_missing == "Unable due to stroke/weakness/paralysis" |
        rsn_grip_missing == "Unable due to recent surgery" |
        rsn_grip_missing ==  "Unable due to other health reason" |
        rsn_grip_missing ==  "Unable due to limb injury" |
        rsn_grip_missing == "Unable due to being amputee" | rsn_grip_missing == "Feeling unwell" |
        rsn_grip_missing == "Unable due to arthritis" ~ "Unable for health reasons",
      rsn_grip_missing == "Against participant wishes" | rsn_grip_missing == "Feeling unwell" ~ "Unwilling",
      rsn_grip_missing == "Reason not known" ~ "Unknown reason",
      rsn_grip_missing == "Equipment failure" ~ "Equipment failure",
      rsn_grip_missing == "Lack of time" ~ "Lack of time"))
  #
  # ukb |> filter(rsn_grip_missing_cat == "Unable for health reasons") |> count(rsn_grip_missing) |> arrange(desc(n)) |>
  #   mutate(percent = glue::glue("{n} ({round(n/274*100,1)})"))
  #
  
  # using quant_groups function from to make quantiles of the grip strength var
  # male
  # ukb |>
  #   filter(sex =="Male") |>
  #   arrange(handgrip_max_0) |> # arrange maxgrip in rank order
  #   mutate(quant_grps = dvmisc::quant_groups(handgrip_max_0, 5)) |>  # create quantiles
  #   group_by(quant_grps) |>  # group by the quantiles
  #   summarise(mean = mean(handgrip_max_0)) # now get the mean
  #
  #
  # # female
  # ukb |>
  #   filter(sex =="Female") |>
  #   arrange(handgrip_max_0) |> # arrange maxgrip in rank order
  #   mutate(quant_grps = dvmisc::quant_groups(handgrip_max_0, 5)) |>  # create quantiles
  #   group_by(quant_grps) |>  # group by the quantiles
  #   summarise(mean = mean(handgrip_max_0)) # now get the mean
  #
  # # the mean for the bottom fifth quartile is 35.9 kgs for men and 22.1kgs for women.
  # # Now use this value for this unable to perfromgrip strength due th ealth reaosns
  # Men
  
  df <-  df |>
    mutate(handgrip_max_0_imp = case_when(
      rsn_grip_missing_cat == "Unable for health reasons" & sex == "Men" ~ 29.5,
      rsn_grip_missing_cat == "Unable for health reasons" & sex == "Women" ~ 17,
      TRUE ~ handgrip_max_0
    ))|>
    select(-matches("f.2004[3|4].0.0"), -matches("handgrip_[left]"),
           -matches("handgrip_[right]"),
           -matches("rsn_miss_hgs_[left|right]"), -rsn_grip_missing) |>
    mutate(
      rsn_grip_missing_cat := structure(
        rsn_grip_missing_cat,
        label = "Reason for missing handgrip strength assessment")) |>
    mutate(
      handgrip_max_0_imp := structure(
        handgrip_max_0_imp,
        label = "Maximum handgrip strength (kg) (imputed), baseline"))
  
  return(df)
}

# Function to process health variables for each wave --------------------------

process_health_vars <- function(df) {
  
  # Process health variables
  for (wave in 0:3) {
    for (var_base in c("f.2453", "f.2188", "f.2443")) {
      var_name <- paste0(var_base, ".", wave, ".0")
      new_var_name <- switch(var_base,
                             "f.2453" = paste0("cancer_diagnosed_", wave),
                             "f.2188" = paste0("ill_disab_status_", wave),
                             "f.2443" = paste0("diabetes_diagnosed_", wave)
      )
      
      df <- df |>
        mutate(
          !!new_var_name := case_when(
            !!sym(var_name) == "No" ~ 0,
            !!sym(var_name) %like% "Yes" ~ 1,
            TRUE ~ NA_integer_
          )
        )
    }
  }
  
  # Process health variables
  for (wave in 0:3) {
    var_name <- paste0("f.6152", ".", wave, ".0")
    new_var_name <- paste0("misc_condition_diagnosed_", wave)
    
    df <- df |>
      mutate(
        !!new_var_name := case_when(
          !!sym(var_name) == "None of the above" ~ "None of the above",
          !!sym(var_name) == "Blood clot in the leg (DVT)" ~ "Blood clot in the leg (DVT)",
          !!sym(var_name) == "Emphysema/chronic bronchitis" ~ "Emphysema/chronic bronchitis",
          !!sym(var_name) == "Blood clot in the lung" ~ "Blood clot in the lung",
          !!sym(var_name) == "Asthma" ~ "Asthma",
          !!sym(var_name) == "Hayfever, allergic rhinitis or eczema" ~ "Hayfever, allergic rhinitis or eczema",
          TRUE ~ NA_character_
        )
      )
  }
  
  
  # Process health variables
  for (wave in 0:2) {
    var_name <- paste0("f.6150", ".", wave, ".0")
    new_var_name <- switch("f.6150",
                           "f.6150" = paste0("vasc_hrt_diagnosed_", wave)
    )
    
    df <- df |>
      mutate(
        !!new_var_name := case_when(
          !!sym(var_name) == "None of the above" ~ "None of the above",
          !!sym(var_name) == "Heart attack" ~ "Heart attack",
          !!sym(var_name) == "Angina" ~ "Angina",
          !!sym(var_name) == "Stroke" ~ "Stroke",
          !!sym(var_name) == "High blood pressure" ~ "High blood pressure",
          TRUE ~ NA_character_
        )
      )
  }
  
  # Process health variables
  df <- df |>
    mutate(
      emphysema_bronchitis = if_else(misc_condition_diagnosed_0 == "Emphysema/chronic bronchitis", 1, 0),
      blood_clot_lung_0 = if_else(misc_condition_diagnosed_0 == "Blood clot in the lung", 1, 0),
      blood_clot_leg_0 = if_else(misc_condition_diagnosed_0 == "Blood clot in the leg (DVT)", 1, 0),
      high_blood_press_0 = if_else(vasc_hrt_diagnosed_0 == "High blood pressure", 1, 0),
      stroke_0 = if_else(vasc_hrt_diagnosed_0 == "Stroke", 1, 0),
      angina_0 = if_else(vasc_hrt_diagnosed_0 == "Angina", 1, 0),
      heart_attack_0 = if_else(vasc_hrt_diagnosed_0 == "Heart attack", 1, 0)
    )
  
  # Final processing
  df <- df |>
    mutate(
      health_cat = rowSums(
        select(df, starts_with("emphysema_bronchitis_"), 
               starts_with("blood_clot_lung_"), 
               starts_with("blood_clot_leg_"), 
               starts_with("high_blood_press_"), 
               starts_with("stroke_"), 
               starts_with("angina_"), 
               starts_with("heart_attack_"), 
               starts_with("cancer_diagnosed_"), 
               starts_with("ill_disab_status_")), na.rm = TRUE),
      healthcat_0 = case_when(
        health_cat == 0 ~ "No condition",
        health_cat == 1 ~ "1 condition",
        health_cat == 2 ~ "2 conditions",
        health_cat >= 3 ~ "3+ conditions",
        TRUE ~ NA_character_
      )
    ) |>
    mutate(healthcat_0 = as.factor(healthcat_0)) |> 
    mutate(healthcat_0 = structure(healthcat_0, 
                                   label = "Comorbidties, baseline"))
  
  return(df)
}

# Function to get variable names of interest -----------------------------------
get_vars_of_interest <- function(df, vars) {
  vars <- df |> 
    colnames() |> as.data.frame() |> rename(vars = 1) |> 
    # remove original variables and variables of other waves
    filter(!str_detect(vars, "f\\.|_1$|_2$|_3$")) |> pull()
  
  # health variables
  health_vars <- c(
    "cancer_diagnosed_0", "cancer_diagnosed_0", 
    "diabetes_diagnosed_0", "misc_condition_diagnosed_0", 
    "vasc_hrt_diagnosed_0", "emphysema_bronchitis", 
    "blood_clot_lung_0", "blood_clot_leg_0", 
    "days_wks_moderate_physical_activity_0",
    "high_blood_press_0", "stroke_0", "angina_0", "nssec_0",
    "heart_attack_0", "health_cat", "ill_disab_status_0",
    "soc2000_0", "soc2000", 'missing_handgrip', 'job_name_soc_0')
  
  # Remove health_vars from vars
  vars <- setdiff(vars, health_vars)
  
  return(c(
    vars, 
    'f.eid'
  ))
  
}
