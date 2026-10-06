# Create the lightweight dataset required by the Shiny application
library(dplyr)

cpc18_features <- read.csv("data/processed/cpc18_features.csv")

shiny_choice_data <- cpc18_features %>%
  select(
    chose_b,
    expected_payoff_diff,
    probability_diff,
    payoff_spread_diff,
    low_payoff_diff,
    high_lottery_ev_diff,
    lottery_outcomes_diff,
    ambiguous_b,
    payoff_correlation,
    full_feedback,
    game_order,
    trial_number,
    time_block
  )

write.csv(shiny_choice_data,"data/processed/shiny_choice_data.csv",
  row.names = FALSE)

cat("Shiny dataset created:", nrow(shiny_choice_data),
  "rows x", ncol(shiny_choice_data), "columns\n")