# ============================================================
# Human Decision Intelligence
# 03_data_cleaning.R
#
# Purpose:
#   Create a clean analytical copy of the raw CPC18 dataset.
# NOTE:
#   The raw source file is never modified.
# ============================================================

# Packages
library(dplyr)


# Create clean analytical dataset

cpc18_cleaned <- cpc18_raw %>%
  
  # Assign meaningful names based on the CPC18 data dictionary.
  rename(
    subject_id = subj_id,
    expected_value_a = ha,
    probability_a = p_ha,
    low_payoff_a = la,
    lottery_shape_a = lot_shape_a,
    lottery_outcomes_a = lot_num_a,
    expected_value_b = hb,
    probability_b = p_hb,
    low_payoff_b = lb,
    lottery_shape_b = lot_shape_b,
    lottery_outcomes_b = lot_num_b,
    ambiguous_b = amb,
    payoff_correlation = corr,
    game_order = order,
    trial_number = trial,
    selected_button = button,
    chose_b = b,
    realized_payoff = payoff,
    forgone_payoff = forgone,
    reaction_time_ms = rt,
    payoff_a = apay,
    payoff_b = bpay,
    full_feedback = feedback,
    time_block = block
  ) %>%
  
  # Standardize whitespace in character variables.
  mutate(across(
    where(is.character),
    ~ trimws(.x)
  )) %>%
  
  # Convert empty character values to NA.
  mutate(across(
    where(is.character),
    ~ na_if(.x, "")
  ))


# Validate expected data types

expected_types <- c(
  subject_id = "integer",
  location = "character",
  gender = "character",
  age = "integer",
  set = "integer",
  condition = "character",
  game_id = "integer",
  expected_value_a = "integer",
  probability_a = "numeric",
  low_payoff_a = "integer",
  lottery_shape_a = "character",
  lottery_outcomes_a = "integer",
  expected_value_b = "integer",
  probability_b = "numeric",
  low_payoff_b = "integer",
  lottery_shape_b = "character",
  lottery_outcomes_b = "integer",
  ambiguous_b = "integer",
  payoff_correlation = "integer",
  game_order = "integer",
  trial_number = "integer",
  selected_button = "character",
  chose_b = "integer",
  realized_payoff = "integer",
  forgone_payoff = "integer",
  reaction_time_ms = "integer",
  payoff_a = "integer",
  payoff_b = "integer",
  full_feedback = "integer",
  time_block = "integer"
)

actual_types <- sapply(cpc18_cleaned, function(x) {
  if (is.numeric(x) && !is.integer(x)) "numeric" else class(x)[1]
})

type_mismatch <- names(expected_types)[
  actual_types[names(expected_types)] != expected_types
]

if (length(type_mismatch) > 0) {
  stop(
    paste(
      "Unexpected data type(s):",
      paste(type_mismatch, collapse = ", ")
    )
  )
}


# Add reaction-time availability indicator

cpc18_cleaned <- cpc18_cleaned %>%
  mutate(
    rt_available = !is.na(reaction_time_ms)
  )


# Remove exact duplicate rows if any are present.

cpc18_cleaned <- cpc18_cleaned %>%
  distinct()


# Confirmation

cat(
  "CPC18 cleaned dataset:",
  nrow(cpc18_cleaned),
  "rows x",
  ncol(cpc18_cleaned),
  "columns\n"
)

cat(
  "Rows removed:",
  nrow(cpc18_raw) - nrow(cpc18_cleaned),
  "\n"
)

cat(
  "RT available:",
  sum(cpc18_cleaned$rt_available),
  "observations\n"
)

cat(
  "RT unavailable:",
  sum(!cpc18_cleaned$rt_available),
  "observations\n"
)

# Save the cleaned dataset

fwrite(cpc18_cleaned, "data/processed/cpc18_cleaned.csv")