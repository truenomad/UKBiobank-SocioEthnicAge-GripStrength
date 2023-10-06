
# Function to load RData files -------------------------------------------------

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# This function runs a series of linear regression models on the UK Biobank data
run_models <- function(data, exposure, 
                       outcome = "handgrip_max_0_imp", interaction_vars) {
  
  # configure model formula
  models <- c(
    paste(outcome, "~", exposure),
    paste(outcome, "~", exposure, "+ height_0"),
    paste(outcome, "~", exposure, "+ height_0 + body_fat_perc_0 + waist_hip_0"),
    paste(outcome, "~", exposure, "+ height_0 + body_fat_perc_0 + waist_hip_0", 
          "+ healthcat_0 + smoking_status_0 + sedentary_hours_day_0",
          "+ days_wks_vig_act_0 + occupational_activity_0")
  )
  
  # set model names
  model_names <- c(
    "Model 1: unadjusted",
    "Model 2: adjusted by height",
    "Model 3: model 2 + bodysize",
    "Model 4: model 3 + health and behavioural factors"
  )
  
  # Use parallel computing to fit the regression models for each combination 
  # of multiple imputations and interaction variables.
  results <- pbmcapply::pbmclapply(seq_along(models), function(i) {
    model_formula <- as.formula(models[i])
    
    result <- data %>%
      mice::complete("long", include = FALSE) %>%
      group_by(.imp, !!!syms(interaction_vars)) %>%
      nest() %>%
      mutate(lm_model = map(data, ~ lm(formula = model_formula, data = .))) %>%
      group_by(!!!syms(interaction_vars)) %>%
      summarise(model = list(tidy(pool(lm_model), conf.int = TRUE))) %>%
      unnest_wider(model) %>%
      unnest(cols = c(
        term, estimate, statistic, p.value, conf.low, conf.high
      )) %>%
      select(-b, -df, -dfcom, -fmi, -lambda, -m, -riv, -ubar) %>%
      mutate(model = model_names[i])
    
    return(result)
  }, mc.cores = parallel::detectCores())
  
  # combine the results into one df
  combined_results <- bind_rows(results)
  
  # clean up the results  
  cleaned_df = clean_results(combined_results,  
                             exposure, 
                             interaction_vars)
  
  return(cleaned_df)
}

# This function cleans up the model results produced by run_models -------------
clean_results <- function(results, exposure, interaction_vars) {
  
  # clean up strings of interaction variables
  cleaned_results <- results %>%
    filter(grepl('Inter', term) | grepl(exposure, term)) %>%
    select(model, !!!syms(interaction_vars), 
           term, estimate, conf.low, conf.high) %>%
    mutate(term = ifelse(term == "(Intercept)",
                         ifelse(exposure == "education", 
                                "Degree (ref)", "1 (ref)"),
                         term)) %>%
    mutate(term = case_when(
      exposure == "education" & 
        term == "educationA-levels, professional, or equiv." ~ 
        "A-levels, professional, or equiv.",
      exposure == "education" & 
        term == "educationO-levels, CSEs, or equiv." ~ 
        "O-levels, CSEs, or equiv.",
      exposure == "education" & 
        term == "educationNo qualification" ~ "No qualification",
      exposure == "imd" & term == "imd_quantile2" ~ "2",
      exposure == "imd" & term == "imd_quantile3" ~ "3",
      exposure == "imd" & term == "imd_quantile4" ~ "4",
      exposure == "imd" & term == "imd_quantile5 (least affluent)" ~ "5",
      TRUE ~ term
    )) %>%
    mutate(across(c(estimate, conf.low, conf.high), 
                  ~ replace(., str_detect(term, "(ref)"), 0))) %>%
    mutate(across(starts_with("estim") | starts_with("conf"), 
                  \(x) round(x, digits = 3))) %>% 
    mutate(term = factor(term)) %>%
    mutate(model = factor(model)) %>%
    mutate(age_group_0 = factor(paste0(age_group_0, "y"), levels = c(
      "Below 45y", "45 to <50y", "50 to <55y", 
      "55 to <60y", "60 to <65y", "Above 65y"
    ))) 
  
  # determine the model stratification lable based on 
  # whether ethnicity is used for interaction
  if (any(interaction_vars == 'ethnic_group')) {
    cleaned_results <- cleaned_results %>%
      mutate(ethnic_group = factor(ethnic_group, levels = c(
        "White", "South Asian", "Black", "Mixed", "Other"
      ))) %>%
      mutate(model_strat = "Age, sex and ethnicity")
  } else {
    cleaned_results <- cleaned_results %>%
      mutate(model_strat = "Age and sex")
  }
  
  return(cleaned_results)
}
