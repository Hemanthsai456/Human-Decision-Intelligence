# ============================================================
# 03_data_cleaning.R
# Human Decision Intelligence
#
# Purpose:
#   Clean and validate the raw CPC18 and choices13k datasets
#   while preserving the original observations and semantics.
# 
# NOTE : Raw files are never modified.


# 1. Required packages
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

# 2. Expected schema
CPC18_REQUIRED_COLUMNS <- c(
  "subj_id",
  "location",
  "gender",
  "age",
  "set",
  "condition",
  "game_id",
  "ha",
  "p_ha",
  "la",
  "lot_shape_a",
  "lot_num_a",
  "hb",
  "p_hb",
  "lb",
  "lot_shape_b",
  "lot_num_b",
  "amb",
  "corr",
  "order",
  "trial",
  "button",
  "b",
  "payoff",
  "forgone",
  "rt",
  "apay",
  "bpay",
  "feedback",
  "block"
)


CHOICES13K_REQUIRED_COLUMNS <- c(
  "problem",
  "feedback",
  "n",
  "block",
  "ha",
  "p_ha",
  "la",
  "hb",
  "p_hb",
  "lb",
  "lot_shape_b",
  "lot_num_b",
  "amb",
  "corr",
  "b_rate",
  "b_rate_std"
)


# 3. Generic schema validation
validate_columns <- function(data, required_columns, dataset_name) {
  
  missing_columns <- setdiff(required_columns, names(data))
  
  unexpected_columns <- setdiff(names(data), required_columns)
  
  if (length(missing_columns) > 0) {
    stop(paste0(dataset_name,
        " is missing required columns: ",
        paste(missing_columns, collapse = ", "))
    )
  }
  
  list(missing_columns = missing_columns, unexpected_columns = unexpected_columns)
}


# 4. Generic duplicate check
check_duplicate_rows <- function(data) {
  tibble(
    rows = nrow(data),
    duplicate_rows = sum(duplicated(data)),
    duplicate_pct = if (nrow(data) == 0) {
      0
    } else {
      mean(duplicated(data)) * 100
    }
  )
}


# 5. CPC18 cleaning
clean_cpc18 <- function(data) {
  
  # Validate schema
  schema_check <- validate_columns(
    data,
    CPC18_REQUIRED_COLUMNS,
    "CPC18"
  )

  # Preserve original row count
  original_rows <- nrow(data)
  
  # Standardise data types
  cleaned <- data %>%
    mutate(
      
      # Identifiers
      subj_id = as.integer(subj_id),
      game_id = as.integer(game_id),
      
      # Demographics
      age = as.integer(age),
      
      # Experimental structure
      set = as.integer(set),
      order = as.integer(order),
      trial = as.integer(trial),
      block = as.integer(block),
      
      # Probabilities / numerical variables
      p_ha = as.numeric(p_ha),
      p_hb = as.numeric(p_hb),
      
      # Choice
      b = as.integer(b),
      
      # Payoff variables
      payoff = as.numeric(payoff),
      forgone = as.numeric(forgone),
      apay = as.numeric(apay),
      bpay = as.numeric(bpay),
      
      # Reaction time
      rt = as.numeric(rt),
      rt_available = !is.na(rt),
      
      # Binary experimental variables
      amb = as.integer(amb),
      corr = as.integer(corr),
      feedback = as.integer(feedback)
    )
  
  # Structural validation

  validation <- list(
    original_rows = original_rows,
    cleaned_rows = nrow(cleaned),
    rows_removed = original_rows - nrow(cleaned),
    duplicates = check_duplicate_rows(cleaned),
    missing_values = sum(is.na(cleaned)),
    missing_rt = sum(is.na(cleaned$rt)),
    rt_available = sum(cleaned$rt_available),
    rt_unavailable = sum(!cleaned$rt_available)
  )
  
  # Logical/range validation checks

  validation$invalid_age <- sum(
    !is.na(cleaned$age) &
      (cleaned$age < 0 | cleaned$age > 120)
  )
  
  validation$invalid_probabilities <- sum(
    !is.na(cleaned$p_ha) &
      (cleaned$p_ha < 0 | cleaned$p_ha > 1)
  ) +
    sum(
      !is.na(cleaned$p_hb) &
        (cleaned$p_hb < 0 | cleaned$p_hb > 1)
    )
  
  validation$invalid_choice <- sum(
    !is.na(cleaned$b) &
      !cleaned$b %in% c(0, 1)
  )

  validation$invalid_ambiguity <- sum(
    !is.na(cleaned$amb) &
      !cleaned$amb %in% c(0, 1)
  )
  
  validation$invalid_feedback <- sum(
    !is.na(cleaned$feedback) &
      !cleaned$feedback %in% c(0, 1)
  )

  validation$invalid_correlation <- sum(
    !is.na(cleaned$corr) &
      !cleaned$corr %in% c(-1, 0, 1)
  )

  validation$invalid_trial <- sum(
    !is.na(cleaned$trial) &
      (cleaned$trial < 1 | cleaned$trial > 25)
  )

  validation$invalid_order <- sum(
    !is.na(cleaned$order) &
      (cleaned$order < 1 | cleaned$order > 30)
  )

  validation$invalid_block <- sum(
    !is.na(cleaned$block) &
      (cleaned$block < 1 | cleaned$block > 5)
  )

  validation$negative_rt <- sum(
    !is.na(cleaned$rt) &
      cleaned$rt < 0
  )

  # RT structure
  # Missing RT is intentionally preserved.

  validation$rt_by_set <- cleaned %>%
    group_by(set) %>%
    summarise(
      observations = n(),
      rt_available = sum(rt_available),
      rt_missing = sum(!rt_available),
      rt_available_pct = mean(rt_available) * 100,
      .groups = "drop"
    )
  
  # Subject structure

  validation$subject_structure <- cleaned %>%
    count(subj_id, name = "observations") %>%
    summarise(
      subjects = n(),
      min_observations = min(observations),
      median_observations = median(observations),
      max_observations = max(observations)
    )

  # Game structure

  validation$game_structure <- cleaned %>%
    count(game_id, name = "observations") %>%
    summarise(
      games = n(),
      min_observations = min(observations),
      median_observations = median(observations),
      max_observations = max(observations)
    )
  
  
  # Final row-count integrity check

  if (nrow(cleaned) != original_rows) {
    stop(
      "CPC18 cleaning changed the number of rows unexpectedly."
    )
  }
  
  list(
    data = cleaned,
    validation = validation,
    schema = schema_check
  )
}

# 6. choices13k cleaning
clean_choices13k <- function(data) {
  
  # Validate schema
  schema_check <- validate_columns(
    data,
    CHOICES13K_REQUIRED_COLUMNS,
    "choices13k"
  )
  
  # Preserve original row count
  original_rows <- nrow(data)
  
  # Standardise data types

  cleaned <- data %>%
    mutate(
      
      # Identifiers / structure
      problem = as.integer(problem),
      n = as.integer(n),
      block = as.integer(block),
      
      # Probabilities
      p_ha = as.numeric(p_ha),
      p_hb = as.numeric(p_hb),
      
      # Experimental variables
      lot_shape_b = as.integer(lot_shape_b),
      lot_num_b = as.integer(lot_num_b),
      corr = as.integer(corr),
      
      # Binary variables
      feedback = as.logical(feedback),
      amb = as.logical(amb),
      
      # Choice-rate variables
      b_rate = as.numeric(b_rate),
      b_rate_std = as.numeric(b_rate_std)
    )
  
  # Structural validation

  validation <- list(
    original_rows = original_rows,
    cleaned_rows = nrow(cleaned),
    rows_removed = original_rows - nrow(cleaned),
    duplicates = check_duplicate_rows(cleaned),
    missing_values = sum(is.na(cleaned))
  )
  
  # Logical/range validation
  validation$invalid_probabilities <- sum(
    !is.na(cleaned$p_ha) &
      (cleaned$p_ha < 0 | cleaned$p_ha > 1)
  ) +
    sum(
      !is.na(cleaned$p_hb) &
        (cleaned$p_hb < 0 | cleaned$p_hb > 1)
    )
  
  validation$invalid_choice_rates <- sum(
    !is.na(cleaned$b_rate) &
      (cleaned$b_rate < 0 | cleaned$b_rate > 1)
  )

  validation$invalid_standardised_choice_rates <- sum(
    !is.na(cleaned$b_rate_std) &
      (cleaned$b_rate_std < 0 | cleaned$b_rate_std > 1)
  )

  validation$invalid_correlation <- sum(
    !is.na(cleaned$corr) &
      !cleaned$corr %in% c(-1, 0, 1)
  )

  validation$invalid_block <- sum(
    !is.na(cleaned$block) &
      (cleaned$block < 1 | cleaned$block > 5)
  )
  
  validation$invalid_n <- sum(
    !is.na(cleaned$n) &
      cleaned$n <= 0
  )

  # Problem structure
  validation$problem_structure <- cleaned %>%
    count(problem, name = "observations") %>%
    summarise(
      problems = n(),
      min_observations = min(observations),
      median_observations = median(observations),
      max_observations = max(observations)
    )
  
  # Final row-count integrity check

  if (nrow(cleaned) != original_rows) {
    stop(
      "choices13k cleaning changed the number of rows unexpectedly."
    )
  }
  
  list(
    data = cleaned,
    validation = validation,
    schema = schema_check
  )
}

# 7. Run cleaning
cpc18_cleaned_result <- clean_cpc18(cpc18_raw)

choices13k_cleaned_result <- clean_choices13k(
  choices13k_raw
)

# 8. Extract cleaned datasets
cpc18_clean <- cpc18_cleaned_result$data

choices13k_clean <- choices13k_cleaned_result$data

# 9. Extract validation reports
cpc18_cleaning_report <- cpc18_cleaned_result$validation

choices13k_cleaning_report <- choices13k_cleaned_result$validation

# 10. Compact cleaning summary
cleaning_summary <- tibble(
  
  dataset = c(
    "CPC18",
    "choices13k"
  ),
  
  original_rows = c(
    nrow(cpc18_raw),
    nrow(choices13k_raw)
  ),
  
  cleaned_rows = c(
    nrow(cpc18_clean),
    nrow(choices13k_clean)
  ),
  
  rows_removed = c(
    nrow(cpc18_raw) - nrow(cpc18_clean),
    nrow(choices13k_raw) - nrow(choices13k_clean)
  ),
  
  missing_values_after_cleaning = c(
    sum(is.na(cpc18_clean)),
    sum(is.na(choices13k_clean))
  ),
  
  duplicate_rows_after_cleaning = c(
    sum(duplicated(cpc18_clean)),
    sum(duplicated(choices13k_clean))
  )
)

# 11. Save cleaned datasets
dir.create(
  "data/processed",
  showWarnings = FALSE,
  recursive = TRUE
)

write.csv(
  cpc18_clean,
  "data/processed/CPC18_clean.csv",
  row.names = FALSE
)

write.csv(
  choices13k_clean,
  "data/processed/choices13k_clean.csv",
  row.names = FALSE
)

# 12. Save cleaning reports
saveRDS(
  cpc18_cleaning_report,
  "data/processed/CPC18_cleaning_report.rds"
)

saveRDS(
  choices13k_cleaning_report,
  "data/processed/choices13k_cleaning_report.rds"
)

saveRDS(
  cleaning_summary,
  "data/processed/cleaning_summary.rds"
)