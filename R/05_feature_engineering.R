# ============================================================
# Human Decision Intelligence
# 05_feature_engineering.R
#
# Purpose:
# Create mathematically and analytically meaningful
# decision-problem features from the validated CPC18 data.
# ============================================================

# Packages

library(dplyr)

# Load validated dataset

cpc18_validated <- read.csv("data/processed/cpc18_cleaned.csv")

# Create decision-problem features

cpc18_features <- cpc18_validated %>%
  mutate(
        # Complete expected payoff of each option
    expected_payoff_a =
      probability_a * expected_value_a +
      (1 - probability_a) * low_payoff_a,
    
    expected_payoff_b =
      probability_b * expected_value_b +
      (1 - probability_b) * low_payoff_b,
    
    # Relative expected-payoff advantage of B over A
    expected_payoff_diff =
      expected_payoff_b - expected_payoff_a,
    
    # Magnitude of the expected-payoff difference
    absolute_expected_payoff_diff =
      abs(expected_payoff_diff),
    
    # Difference in probability of receiving the high-lottery outcome
    probability_diff =
      probability_b - probability_a,
    
    # Magnitude of the probability difference
    absolute_probability_diff =
      abs(probability_diff),
    
    # Difference in low payoffs
    low_payoff_diff =
      low_payoff_b - low_payoff_a,
    
    # Difference in expected value of the high lotteries
    high_lottery_ev_diff =
      expected_value_b - expected_value_a,
    
    # High-lottery expected value relative to the low payoff
    payoff_spread_a =
      expected_value_a - low_payoff_a,
    
    payoff_spread_b =
      expected_value_b - low_payoff_b,
    
    # Difference in payoff spread
    payoff_spread_diff =
      payoff_spread_b - payoff_spread_a,
    
    # Difference in number of lottery outcomes
    lottery_outcomes_diff =
      lottery_outcomes_b - lottery_outcomes_a

  )

# Pre-decision feature set

pre_decision_features <- c(
  "expected_payoff_a",
  "expected_payoff_b",
  "expected_payoff_diff",
  "absolute_expected_payoff_diff",
  "probability_diff",
  "absolute_probability_diff",
  "low_payoff_diff",
  "high_lottery_ev_diff",
  "payoff_spread_a",
  "payoff_spread_b",
  "payoff_spread_diff",
  "lottery_outcomes_diff",
  "ambiguous_b",
  "payoff_correlation",
  "full_feedback",
  "game_order",
  "trial_number",
  "time_block"
)

# Save feature-enhanced dataset

data.table::fwrite(cpc18_features, "data/processed/cpc18_features.csv")

# Confirmation

cat("CPC18 feature-enhanced dataset:", nrow(cpc18_features),
    "rows x", ncol(cpc18_features), "columns\n")

cat("Engineered features:", length(pre_decision_features), "\n")
