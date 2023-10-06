# Function to load RData files -------------------------------------------------

loadRData <- function(fileName) {
  # loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# Function to build formula  ---------------------------------------------------

build_lm_formula <- function(outcome, exposure, additional_adjustment,
                             interaction_vars) {
  # Check if additional_adjustment and interaction_vars contain the same variables
  if (any(additional_adjustment %in% interaction_vars)) {
    stop(
      "additional_adjustment and interaction_vars",
      " cannot contain the same variables."
    )
  }
  
  # Configure model formula
  # If additional_adjustment is not null, add it to the model
  if (c("age_group_0") %in% additional_adjustment) {
    # Join all additional adjustment variables with ' + '
    additional_adjustment_str <- paste("age_group_0", collapse = " + ")
    
    # Add additional adjustment to all models
    models <- c(
      paste(outcome, "~", exposure, "+", "age_group_0"),
      paste(outcome, "~", exposure, "+ height_0 +", "age_group_0"),
      paste(outcome, "~", exposure, "+ height_0 +",
            " body_fat_perc_0 + waist_hip_0 +", "age_group_0"),
      paste(
        outcome, "~", exposure, "+ height_0 + body_fat_perc_0 + waist_hip_0",
        "+ healthcat_0 + smoking_status_0 + sedentary_hours_day_0",
        "+ days_wks_vig_act_0 + occupational_activity_0 +",
        "age_group_0"
      )
    )
  } else {
    # If additional_adjustment is null, just use the original models
    models <- c(
      paste(outcome, "~", exposure),
      paste(outcome, "~", exposure, "+ height_0"),
      paste(
        outcome, "~", exposure,
        "+ height_0 + body_fat_perc_0 + waist_hip_0"
      ),
      paste(
        outcome, "~", exposure, "+ height_0 + body_fat_perc_0 + waist_hip_0",
        "+ healthcat_0 + smoking_status_0 + sedentary_hours_day_0",
        "+ days_wks_vig_act_0 + occupational_activity_0"
      )
    )
  }
  
  if (c("ethnic_group") %in% additional_adjustment) {
    model_5 <- paste0(models[4], " + ethnic_group")
    models <- c(models, model_5)
  }
  
  return(models)
}

# Function to generate gt tables  formula  -------------------------------------

generate_gt_table <- function(data, exposure, outcome,
                              additional_adjustment = NULL, 
                              interaction_vars = NULL, 
                              interaction_groups = NULL,
                              sex_groups = NULL,
                              mode = "by_sex") {
  
  gtsummary::set_gtsummary_theme(list(
    "tbl_summary-fn:percent_fun" = function(x) style_percent(x, symbol = FALSE),
    "tbl_summary-str:categorical_stat" = "{n} ({p})",
    "style_number-arg:big.mark" = ","
  ))
  
  gtsummary::theme_gtsummary_journal("jama")
  
  if (exposure %in% c("education", "education_0")) {
    label <- "**Highest Qualification**"
  } else if (exposure %in% c("imd_quantile", "imd_quantile_0")) {
    label <- "**Index of Multiple Deprivation**"
  } else {
    label <- "**Characteristic**"
  }
  
  formulas <- build_lm_formula(outcome, 
                               exposure, 
                               additional_adjustment, 
                               interaction_vars)
  
  merged_tables_by_model <- list()
  
  # Loop through formulas or by sex
  loop_set <- if (mode == "by_sex") 
    sex_groups else seq_along(formulas)
  
  
  for (i in seq_along(loop_set)) {
    data_subset <- if (mode == "by_sex") {
      mice::filter(data, sex == loop_set[i])
    } else {
      data
    }
    
    formula <- if (mode == "by_sex") formulas[[1]] else formulas[i]
    
    header <- if (mode == "by_sex") loop_set[i] else paste("Model:", i)
    
    model_tables <- list() 
    model_spanners <- list()
    merged_tables <- list()
    
    if (is.null(interaction_vars)) {
      # Process without interaction variables
      table <- with(
        data_subset, lm(as.formula(formula))) |>
        gtsummary::tbl_regression(
          include = 1,
          estimate_fun = function(x) gtsummary::style_number(x, digits = 2)
        ) |>
        gtsummary::modify_table_styling(
          column = estimate, 
          rows = !is.na(estimate),
          cols_merge_pattern = "{estimate} [{conf.low}, {conf.high}]",
        ) |> 
        gtsummary::modify_table_styling(
          columns = estimate,
          rows = reference_row %in% TRUE,
          missing_symbol = "0.00 (ref)"
        ) |> 
        gtsummary::tbl_butcher()
      model_tables[[1]] <- table
      model_spanners[[1]] <- "Overall"
    } else {
      # Process with interaction variables
      for (group in interaction_groups) {
        data_filtered <- data_subset |> 
          mice::filter(.data[[interaction_vars]] == group)
        
        table <- with(
          data_filtered, 
          lm(as.formula(formula))) |>
          gtsummary::tbl_regression(
            include = 1, 
            estimate_fun = function(x) gtsummary::style_number(x, digits = 2)) |> 
          gtsummary::modify_table_styling(
            column = estimate, 
            rows = !is.na(estimate),
            cols_merge_pattern = "{estimate} [{conf.low}, {conf.high}]",
          ) |> 
          gtsummary::modify_table_styling(
            columns = estimate,
            rows = reference_row %in% TRUE,
            missing_symbol = "0.00 (ref)"
          ) |>
          gtsummary::tbl_butcher()
        
        model_tables[[paste(header, group)]] <- table
        model_spanners[[length(model_spanners) + 1]] <- group
      }
    }
    
    
    # Merge tables for the current model with age groups as spanners if needed
    merged_table <- if (length(model_tables) > 1) {
      gtsummary::tbl_merge(
        tbls = model_tables, tab_spanner = unlist(model_spanners)) |>
        gtsummary::modify_table_styling(
          column = gtsummary::starts_with("p.value"), hide = TRUE) |>
        gtsummary::bold_labels() |>
        gtsummary::modify_header(
          update = list(
            label ~ label, 
            starts_with("estimate_") ~ "**Coef. (95% CI)**")) |>
        gtsummary::modify_footnote(
          gtsummary::starts_with("estimate_") ~ 
            paste0(
              "Coefficient: Difference in mean outcome",
              " (unit) (95% Confidence Interval)"),  
          gtsummary::starts_with("ci_") ~ NA, 
          abbreviation = TRUE) |>
        gtsummary::modify_table_body(
          ~ .x |> 
            dplyr::mutate(
              label = ifelse(header_row_1 == TRUE, 
                             paste(header), label)))
    } else {
      model_tables[[1]] |>
        gtsummary::modify_table_styling(
          column = gtsummary::starts_with("p.value"), hide = TRUE) |>
        gtsummary::bold_labels() |>
        gtsummary::modify_header(
          update = list(
            label ~ label, 
            starts_with("estimate") ~ "**Coef. (95% CI)**")) |>
        gtsummary::modify_footnote(
          gtsummary::starts_with("estimate") ~ 
            paste0(
              "Coefficient: Difference in mean outcome",
              " (unit) (95% Confidence Interval)"), 
          gtsummary::starts_with("ci_") ~ NA, 
          abbreviation = TRUE) |>
        gtsummary::modify_table_body(
          ~ .x |> 
            dplyr::mutate(
              label = ifelse(header_row == TRUE, 
                             paste(header), label)))
    }
    
    merged_tables_by_model[[header]] <- merged_table
  }
  
  stacked_table <- gtsummary::tbl_stack(
    tbls = merged_tables_by_model, 
    quiet = T) |>
    gtsummary::modify_table_styling(
      column = gtsummary::starts_with("p.value"), hide = TRUE)
  
  return(stacked_table)
}


# Function to run lm modes -----------------------------------------------------

# This function runs a series of linear regression models on the UK Biobank data
run_models <- function(data, exposure,
                       outcome = "handgrip_max_0_imp",
                       additional_adjustment = NULL,
                       interaction_vars) {
  # build model formula
  models <- build_lm_formula(
    outcome, exposure, additional_adjustment, interaction_vars
  )
  
  # set model names
  model_names <- c(
    "M1: unadjusted",
    "M2: adjusted by height",
    "M3: M2 + adiposity",
    "M4: M3 + health and behavioural factors"
  )
  
  # Add additional adjustment to model names
  if ("age_group_0" %in% additional_adjustment) {
    model_names[1] <- "M1: age adjusted"
  }
  
  if ("ethnic_group" %in% additional_adjustment) {
    model_names <- c(model_names, "M5: M4 + ethnicity")
  }
  
  # Run model ------------------------------------------------------------------
  
  # Use parallel computing to fit the regression models for each combination
  # of multiple imputations and interaction variables.
  results <- pbmcapply::pbmclapply(seq_along(models), function(i) {
    model_formula <- as.formula(models[i])
    
    result <- data |>
      mice::complete("long", include = FALSE) |>
      dplyr::group_by(.imp, !!!rlang::syms(interaction_vars)) |>
      tidyr::nest() |>
      dplyr::mutate(
        lm_model = purrr::map(data, ~ lm(formula = model_formula, data = .))
      ) |>
      group_by(!!!rlang::syms(interaction_vars)) |>
      dplyr::summarise(model = list(
        broom::tidy(mice::pool(lm_model), conf.int = TRUE)
      )) |>
      tidyr::unnest_wider(model) |>
      tidyr::unnest(cols = c(
        term, estimate, statistic, p.value, conf.low, conf.high
      )) |>
      dplyr::select(
        -b, -df, -std.error, -dfcom, -fmi, -lambda, -m, -riv, -ubar
      ) |>
      dplyr::mutate(model = model_names[i])
    
    return(result)
  }, mc.cores = parallel::detectCores())
  
  # combine the results into one df
  combined_results <- dplyr::bind_rows(results)
  
  # clean up the results
  cleaned_df <- clean_results(
    combined_results,
    exposure,
    interaction_vars
  )
  
  return(cleaned_df)
}

# Function to clean model outputs ----------------------------------------------

clean_results <- function(results,
                          exposure,
                          interaction_vars) {
  # clean up strings of interaction variables
  cleaned_results <- results |>
    dplyr::filter(grepl("Inter", term) | grepl(exposure, term)) |>
    dplyr::select(
      model, !!!rlang::syms(interaction_vars),
      term, estimate, conf.low, conf.high # nolint: object_usage_linter.
    ) |>
    dplyr::mutate(term = dplyr::case_when(
      stringr::str_detect(term, "Intercept") &
        exposure == "education" ~ "Degree & higher (ref)",
      stringr::str_detect(term, "Intercept") &
        exposure == "imd_quantile" ~ "1 (most affluent) (ref)",
      stringr::str_detect(term, "Intercept") &
        exposure == "ethnic_group" ~ "White (ref)",
      stringr::str_detect(term, "Black") &
        exposure == "ethnic_group" ~ "Black",
      stringr::str_detect(term, "South Asian") &
        exposure == "ethnic_group" ~ "South Asian",
      stringr::str_detect(term, "Mixed") &
        exposure == "ethnic_group" ~ "Mixed",
      stringr::str_detect(term, "Other") &
        exposure == "ethnic_group" ~ "Other",
      stringr::str_detect(level, "White") ~ "White",
      stringr::str_detect(level, "Black Caribbean") ~ "Black Caribbean",
      stringr::str_detect(level, "Chinese") ~ "Chinese",
      stringr::str_detect(level, "Black African") ~ "Black African",
      stringr::str_detect(level, "Pakistani") ~ "Pakistani",
      stringr::str_detect(level, "Indian") ~ "Indian",
      exposure == "education" &
        stringr::str_detect(term, "A-levels") ~
        "A-levels, profess., or equiv.",
      exposure == "education" &
        stringr::str_detect(term, "O-levels") ~
        "O-levels, CSEs, or equiv.",
      exposure == "education" &
        stringr::str_detect(term, "No qualification") ~ "No qualification",
      exposure == "imd_quantile" & term == "imd_quantile2" ~ "2",
      exposure == "imd_quantile" & term == "imd_quantile3" ~ "3",
      exposure == "imd_quantile" & term == "imd_quantile4" ~ "4",
      exposure == "imd_quantile" &
        term == "imd_quantile5 (least affluent)" ~ "5 (least affluent)",
      TRUE ~ term
    )) |>
    dplyr::mutate(
      dplyr::across(
        c(estimate, conf.low, conf.high),
        ~ replace(., str_detect(term, "(ref)"), 0)
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("estim") | tidyselect::starts_with("conf"),
        \(x) round(x, digits = 3)
      )
    ) |>
    dplyr::mutate(term = factor(
      term,
      levels = c(
        "Degree & higher (ref)",
        "A-levels, profess., or equiv.",
        "O-levels, CSEs, or equiv.",
        "No qualification",
        "1 (most affluent) (ref)", "2", "3", "4", "5 (least affluent)",
        "White (ref)", "South Asian",  "Black", "Mixed", "Other",
        "White British (ref)", "Indian", "Pakistani", 
        "Black Caribbean", "Black African", "Chinese", "Mixed", "Other"
      )
    )) |>
    dplyr::mutate(model = factor(model))
  
  # determine the model stratification lable based on
  # whether age is used for interaction
  if ("age_group_0" %in% interaction_vars) {
    cleaned_results <- cleaned_results |>
      dplyr::mutate(age_group_0 = factor(paste0(age_group_0, "y"), levels = c(
        "Below 45y", "45 to <50y", "50 to <55y",
        "55 to <60y", "60 to <65y", "Above 65y"
      ))) |>
      dplyr::ungroup()
  }
  
  # determine the model stratification lable based on
  # whether ethnicity is used for interaction
  if ("ethnic_group" %in% interaction_vars) {
    cleaned_results <- cleaned_results |>
      dplyr::mutate(ethnic_group = factor(ethnic_group, levels = c(
        "White", "South Asian", "Black", "Mixed", "Other"
      )))
  }
  
  if (all(c("sex", "age_group_0", "ethnic_group") %in%
          colnames(cleaned_results))) {
    cleaned_results <- cleaned_results |>
      dplyr::mutate(model_strat = "Age, sex and ethnicity")
  } else if (all(c("sex", "age_group_0") %in%
                 colnames(cleaned_results))) {
    cleaned_results <- cleaned_results |>
      dplyr::mutate(model_strat = "Age and sex")
  } else if (all(c("sex", "ethnic_group") %in%
                 colnames(cleaned_results))) {
    cleaned_results <- cleaned_results |>
      dplyr::mutate(model_strat = "Sex and ethnicity")
  } else {
    cleaned_results <- cleaned_results |>
      dplyr::mutate(model_strat = "Sex")
  }
  
  return(cleaned_results)
}
