# ============================================================
# Human Decision Intelligence
# 04_data_validation.R
#
# Purpose:
#   Validate logical consistency and documented constraints
#   in the cleaned CPC18 analytical dataset.
# ============================================================

# Packages
library(dplyr)


# Load cleaned dataset

cpc18_cleaned <- read.csv("data/processed/cpc18_cleaned.csv")


# Validation results

validation_results <- tibble(
  check = character(),
  invalid_rows = integer(),
  status = character()
)


# Helper function

add_check <- function(name, condition) {
  
  invalid <- sum(condition, na.rm = TRUE)
  
  validation_results <<- bind_rows(
    validation_results,
    tibble(
      check = name,
      invalid_rows = invalid,
      status = ifelse(invalid == 0, "PASS", "FLAG")
    )
  )
}


# Categorical value validation

add_check(
  "Location values",
  !cpc18_cleaned$location %in% c("Technion", "Rehovot")
)

add_check(
  "Gender values",
  !cpc18_cleaned$gender %in% c("M", "F")
)

add_check(
  "Condition values",
  !cpc18_cleaned$condition %in% c("ByProb", "ByFB")
)

add_check(
  "Lottery shape A values",
  !cpc18_cleaned$lottery_shape_a %in% c("-", "Symm", "L-skew", "R-skew")
)

add_check(
  "Lottery shape B values",
  !cpc18_cleaned$lottery_shape_b %in% c("-", "Symm", "L-skew", "R-skew")
)

add_check(
  "Selected button values",
  !cpc18_cleaned$selected_button %in% c("L", "R")
)


# Binary variable validation

add_check(
  "Ambiguity coding",
  !cpc18_cleaned$ambiguous_b %in% c(0, 1)
)

add_check(
  "Choice B coding",
  !cpc18_cleaned$chose_b %in% c(0, 1)
)

add_check(
  "Feedback coding",
  !cpc18_cleaned$full_feedback %in% c(0, 1)
)


# Numeric range validation

add_check(
  "Probability A range",
  cpc18_cleaned$probability_a < 0 |
    cpc18_cleaned$probability_a > 1
)

add_check(
  "Probability B range",
  cpc18_cleaned$probability_b < 0 |
    cpc18_cleaned$probability_b > 1
)

add_check(
  "Correlation coding",
  !cpc18_cleaned$payoff_correlation %in% c(-1, 0, 1)
)

add_check(
  "Game order range",
  cpc18_cleaned$game_order < 1 |
    cpc18_cleaned$game_order > 30
)

add_check(
  "Trial number range",
  cpc18_cleaned$trial_number < 1 |
    cpc18_cleaned$trial_number > 25
)

add_check(
  "Time block range",
  cpc18_cleaned$time_block < 1 |
    cpc18_cleaned$time_block > 5
)


# Trial-block consistency

add_check(
  "Trial and block consistency",
  cpc18_cleaned$time_block != ceiling(cpc18_cleaned$trial_number / 5)
)


# Payoff consistency

add_check(
  "Realized payoff consistency",
  ifelse(
    cpc18_cleaned$chose_b == 1,
    cpc18_cleaned$realized_payoff != cpc18_cleaned$payoff_b,
    cpc18_cleaned$realized_payoff != cpc18_cleaned$payoff_a
  )
)

add_check(
  "Forgone payoff consistency",
  ifelse(
    cpc18_cleaned$chose_b == 1,
    cpc18_cleaned$forgone_payoff != cpc18_cleaned$payoff_a,
    cpc18_cleaned$forgone_payoff != cpc18_cleaned$payoff_b
  )
)


# Reaction-time validation

add_check(
  "Negative reaction times",
  !is.na(cpc18_cleaned$reaction_time_ms) &
    cpc18_cleaned$reaction_time_ms < 0
)

add_check(
  "RT availability indicator",
  cpc18_cleaned$rt_available != !is.na(cpc18_cleaned$reaction_time_ms)
)


# Save validation results

write.csv(
  validation_results,
  "data/processed/cpc18_validation_results.csv",
  row.names = FALSE
)