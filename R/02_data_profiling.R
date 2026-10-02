# 02_data_profiling.R
# Comprehensive profiling of the raw CPC18 and choices13k datasets.
#
# NOTE: This script describes the data as received. It does not clean,
# transform, filter, impute, or engineer features.

# Loading Libraries
library(dplyr)
library(purrr)
library(tibble)

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
    
    # class() identifies the R class used to represent each variable.
    class = map_chr(data, ~ paste(class(.x), collapse = ", ")),
    
    # sum(is.na()) counts observations with missing values.
    missing = map_int(data, ~ sum(is.na(.x))),
    
    # mean(is.na()) gives the proportion of missing observations.
    missing_pct = map_dbl(data, ~ mean(is.na(.x)) * 100),
    
    # n_distinct() counts different observed values in each variable.
    unique_values = map_int(data, ~ n_distinct(.x, na.rm = TRUE))
  )
  
  numeric_data <- data %>%
    # where() selects columns according to their data type.
    select(where(is.numeric))
  
  numeric_summary <- numeric_data %>%
    # across() applies the same summary functions to every numeric column.
    summarise(across(
      everything(),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      )
    ))
  
  categorical_data <- data %>%
    # Character and factor variables are treated as categorical variables here.
    select(where(~ is.character(.x) || is.factor(.x)))
  
  categorical_summary <- map_dfr(
    names(categorical_data),
    function(variable) {
      
      counts <- categorical_data %>%
        # count() gives the frequency of each observed category.
        count(.data[[variable]], sort = TRUE, name = "frequency")
      
      names(counts)[1] <- "level"
      
      counts %>%
        # Add the original variable name so results from all variables can be combined.
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
    
    # count() gives the number of observations for each recorded choice.
    choices = count(data, b, name = "observations"),
    
    # count() shows the distribution of feedback conditions.
    feedback = count(data, feedback, name = "observations"),
    
    # count() shows the distribution of ambiguous and non-ambiguous trials.
    ambiguity = count(data, amb, name = "observations"),
    
    # count() shows the distribution of payoff correlation conditions.
    correlation = count(data, corr, name = "observations")
  )
  
  # NOTE: RT has structured availability in CPC18, so its missingness
  # is profiled separately rather than treated as ordinary missing data.
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
  
  # count() checks how many observations each subject contributes.
  subject_observations <- data %>%
    count(subj_id, name = "observations")
  
  profile$cpc18$subject_observations <- subject_observations %>%
    summarise(
      subjects = n(),
      min = min(observations),
      median = median(observations),
      max = max(observations)
    )
  
  # count() checks the number of observations recorded for each game.
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


# choices13k-specific profiling

profile_choices13k <- function(data) {
  
  profile <- profile_dataset(data)
  
  profile$choices13k <- list(
    problems = n_distinct(data$problem),
    
    # count() shows the distribution of feedback conditions.
    feedback = count(data, feedback, name = "observations"),
    
    # count() shows how observations are distributed across blocks.
    blocks = count(data, block, name = "observations"),
    
    # count() shows the distribution of ambiguous and non-ambiguous problems.
    ambiguity = count(data, amb, name = "observations"),
    
    # count() shows the distribution of payoff correlation conditions.
    correlation = count(data, corr, name = "observations")
  )
  
  # count() shows how frequently each problem occurs in the dataset.
  problem_observations <- data %>%
    count(problem, name = "observations")
  
  profile$choices13k$problem_observations <- problem_observations %>%
    summarise(
      problems = n(),
      min = min(observations),
      median = median(observations),
      max = max(observations)
    )
  
  profile$choices13k$b_rate <- summary(data$b_rate)
  profile$choices13k$b_rate_std <- summary(data$b_rate_std)
  
  profile
}


# CPC18 Profiling
profile_cpc18(cpc18_raw)

# Choices 13k profiling
profile_choices13k(choices13k_raw)