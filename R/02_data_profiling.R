# ============================================================
# Human Decision Intelligence
# 02_data_profiling.R
#
# Purpose:
#   Profile the raw CPC18 dataset as received.
#   This stage does not clean, transform, filter, impute,
#   or engineer features.
# ============================================================

# Packages
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)


# General dataset profiling

profile_dataset <- function(data) {
  
  overview <- tibble(
    rows = nrow(data),
    columns = ncol(data),
    total_cells = nrow(data) * ncol(data),
    missing_values = sum(is.na(data)),
    complete_rows = sum(complete.cases(data)),
    duplicate_rows = sum(duplicated(data))
  )
  
  variables <- tibble(
    variable = names(data),
    class = map_chr(data, ~ paste(class(.x), collapse = ", ")),
    missing = map_int(data, ~ sum(is.na(.x))),
    missing_pct = map_dbl(data, ~ mean(is.na(.x)) * 100),
    unique_values = map_int(data, ~ n_distinct(.x, na.rm = TRUE))
  )
  
  numeric_summary <- data %>%
    select(where(is.numeric)) %>%
    summarise(across(
      everything(),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      )
    )) %>%
    pivot_longer(
      everything(),
      names_to = c("variable", "statistic"),
      names_pattern = "^(.*)_(mean|sd|min|median|max)$",
      values_to = "value"
    )
  
  categorical_data <- data %>%
    select(where(~ is.character(.x) || is.factor(.x)))
  
  categorical_summary <- map_dfr(
    names(categorical_data),
    function(variable) {
      
      categorical_data %>%
        count(.data[[variable]], sort = TRUE, name = "frequency") %>%
        rename(level = 1) %>%
        mutate(variable = variable, .before = 1)
    }
  )
  
  list(
    overview = overview,
    variables = variables,
    numeric_summary = numeric_summary,
    categorical_summary = categorical_summary
  )
}


# CPC18-specific profiling

profile_cpc18 <- function(data) {
  
  profile <- profile_dataset(data)
  
  profile$cpc18 <- list(
    
    subjects = n_distinct(data$subj_id),
    games = n_distinct(data$game_id),
    sets = sort(unique(data$set)),
    trials = sort(unique(data$trial)),
    orders = sort(unique(data$order)),
    
    choices = count(data, b, name = "observations"),
    feedback = count(data, feedback, name = "observations"),
    ambiguity = count(data, amb, name = "observations"),
    correlation = count(data, corr, name = "observations"),
    
    location = count(data, location, name = "observations"),
    gender = count(data, gender, name = "observations"),
    condition = count(data, condition, name = "observations"),
    button = count(data, button, name = "observations"),
    lottery_shape_a = count(data, lot_shape_a, name = "observations"),
    lottery_shape_b = count(data, lot_shape_b, name = "observations")
  )
  
  
  # Reaction-time availability
  # RT is structurally available only for specific CPC18 sets.
  
  profile$cpc18$reaction_time <- tibble(
    total = nrow(data),
    available = sum(!is.na(data$rt)),
    missing = sum(is.na(data$rt)),
    available_pct = mean(!is.na(data$rt)) * 100,
    missing_pct = mean(is.na(data$rt)) * 100
  )
  
  profile$cpc18$reaction_time_by_set <- data %>%
    group_by(set) %>%
    summarise(
      observations = n(),
      rt_available = sum(!is.na(rt)),
      rt_missing = sum(is.na(rt)),
      rt_available_pct = mean(!is.na(rt)) * 100,
      .groups = "drop"
    )
  
  
  # Subject-level observation structure
  
  subject_observations <- data %>%
    count(subj_id, name = "observations")
  
  profile$cpc18$subject_observations <- subject_observations %>%
    summarise(
      subjects = n(),
      min = min(observations),
      median = median(observations),
      max = max(observations)
    )
  
  
  # Game-level observation structure
  
  game_observations <- data %>%
    count(game_id, name = "observations")
  
  profile$cpc18$game_observations <- game_observations %>%
    summarise(
      games = n(),
      min = min(observations),
      median = median(observations),
      max = max(observations)
    )
  profile
}


# CPC18 Profiling
cpc18_profile <- profile_cpc18(cpc18_raw)

cpc18_profile