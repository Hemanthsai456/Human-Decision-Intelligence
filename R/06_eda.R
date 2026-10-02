# ============================================================
# Human Decision Intelligence
# 06_eda.R
#
# Purpose:
#   Explore decision behavior, experimental conditions,
#   engineered features, and available reaction-time data.
# ============================================================

# Packages
library(dplyr)
library(ggplot2)

cpc18_features <- read.csv(
  "data/processed/cpc18_features.csv"
)

# Choice summary
choice_summary <- cpc18_features %>%
  summarise(
    observations = n(),
    chose_b = sum(chose_b),
    chose_a = sum(chose_b == 0),
    choice_b_rate = mean(chose_b)
  )

# Choice by expected payoff difference
choice_by_ev <- cpc18_features %>%
  mutate(
    ev_difference_group = cut(
      expected_payoff_diff,
      breaks = 7,
      include.lowest = TRUE
    )
  ) %>%
  group_by(ev_difference_group) %>%
  summarise(
    observations = n(),
    choice_b_rate = mean(chose_b),
    .groups = "drop"
  )

# Choice by probability difference
choice_by_probability <- cpc18_features %>%
  mutate(
    probability_difference_group = cut(
      probability_diff,
      breaks = 7,
      include.lowest = TRUE
    )
  ) %>%
  group_by(probability_difference_group) %>%
  summarise(
    observations = n(),
    choice_b_rate = mean(chose_b),
    .groups = "drop"
  )

# Experimental conditions
choice_by_condition <- cpc18_features %>%
  group_by(
    ambiguous_b,
    payoff_correlation,
    full_feedback
  ) %>%
  summarise(
    observations = n(),
    choice_b_rate = mean(chose_b),
    .groups = "drop"
  )

# Temporal behavior
choice_by_block <- cpc18_features %>%
  group_by(time_block) %>%
  summarise(
    observations = n(),
    choice_b_rate = mean(chose_b),
    .groups = "drop"
  )

choice_by_order <- cpc18_features %>%
  group_by(game_order) %>%
  summarise(
    observations = n(),
    choice_b_rate = mean(chose_b),
    .groups = "drop"
  )

# Participant context
choice_by_context <- cpc18_features %>%
  group_by(location, gender) %>%
  summarise(
    observations = n(),
    choice_b_rate = mean(chose_b),
    .groups = "drop"
  )

# Feature relationships
feature_correlations <- cor(
  cpc18_features[
    c(
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
      "lottery_outcomes_diff"
    )
  ],
  use = "pairwise.complete.obs"
)

# Reaction-time summary
rt_summary <- cpc18_features %>%
  summarise(
    observations = n(),
    available = sum(rt_available),
    unavailable = sum(!rt_available),
    availability_rate = mean(rt_available),
    zero_rt = sum(reaction_time_ms == 0, na.rm = TRUE),
    median_rt = median(reaction_time_ms, na.rm = TRUE),
    mean_rt = mean(reaction_time_ms, na.rm = TRUE),
    p95_rt = quantile(reaction_time_ms, 0.95, na.rm = TRUE),
    p99_rt = quantile(reaction_time_ms, 0.99, na.rm = TRUE),
    maximum_rt = max(reaction_time_ms, na.rm = TRUE)
  )

# Key plots
eda_plots <- list(
  
  choice_distribution =
    ggplot(cpc18_features, aes(x = factor(chose_b))) +
    geom_bar() +
    labs(
      x = "Choice",
      y = "Observations",
      title = "Overall Choice Distribution"
    ),
  
  choice_by_ev =
    ggplot(
      choice_by_ev,
      aes(x = ev_difference_group, y = choice_b_rate)
    ) +
    geom_col() +
    labs(
      x = "Expected Payoff Difference (B - A)",
      y = "Choice B Rate",
      title = "Choice B Rate Across Expected Payoff Differences"
    ),
  
  choice_by_probability =
    ggplot(
      choice_by_probability,
      aes(
        x = probability_difference_group,
        y = choice_b_rate
      )
    ) +
    geom_col() +
    labs(
      x = "Probability Difference (B - A)",
      y = "Choice B Rate",
      title = "Choice B Rate Across Probability Differences"
    ),
  
  choice_by_ambiguity =
    ggplot(
      cpc18_features,
      aes(
        x = factor(ambiguous_b),
        fill = factor(chose_b)
      )
    ) +
    geom_bar(position = "fill") +
    labs(
      x = "B Ambiguity",
      y = "Proportion",
      fill = "Choice B",
      title = "Choice Distribution by Ambiguity"
    ),
  
  choice_by_feedback =
    ggplot(
      cpc18_features,
      aes(
        x = factor(full_feedback),
        fill = factor(chose_b)
      )
    ) +
    geom_bar(position = "fill") +
    labs(
      x = "Full Feedback",
      y = "Proportion",
      fill = "Choice B",
      title = "Choice Distribution by Feedback"
    ),
  
  choice_by_block =
    ggplot(
      choice_by_block,
      aes(x = time_block, y = choice_b_rate)
    ) +
    geom_line(group = 1) +
    geom_point() +
    labs(
      x = "Time Block",
      y = "Choice B Rate",
      title = "Choice B Rate Across Time Blocks"
    ),
  
  choice_by_order =
    ggplot(
      choice_by_order,
      aes(x = game_order, y = choice_b_rate)
    ) +
    geom_line() +
    labs(
      x = "Game Order",
      y = "Choice B Rate",
      title = "Choice B Rate Across Game Order"
    ),
  
  reaction_time_distribution =
    ggplot(
      cpc18_features %>%
        filter(!is.na(reaction_time_ms), reaction_time_ms > 0),
      aes(x = reaction_time_ms)
    ) +
    geom_histogram(bins = 50) +
    scale_x_log10() +
    labs(
      x = "Reaction Time (ms, log scale)",
      y = "Observations",
      title = "Reaction-Time Distribution"
    )
)

# Save key summaries
write.csv(choice_summary, "data/processed/eda_choice_summary.csv", row.names = FALSE)

write.csv(choice_by_ev, "data/processed/eda_choice_by_ev.csv", row.names = FALSE)

write.csv(choice_by_probability, "data/processed/eda_choice_by_probability.csv", row.names = FALSE)

write.csv(rt_summary, "data/processed/eda_rt_summary.csv", row.names = FALSE)

# Save all plots

dir.create(
  "outputs/eda",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  "outputs/eda/choice_distribution.png",
  eda_plots$choice_distribution,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "outputs/eda/choice_by_expected_payoff.png",
  eda_plots$choice_by_ev,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "outputs/eda/choice_by_probability.png",
  eda_plots$choice_by_probability,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "outputs/eda/choice_by_ambiguity.png",
  eda_plots$choice_by_ambiguity,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "outputs/eda/choice_by_feedback.png",
  eda_plots$choice_by_feedback,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "outputs/eda/choice_by_time_block.png",
  eda_plots$choice_by_block,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "outputs/eda/choice_by_game_order.png",
  eda_plots$choice_by_order,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "outputs/eda/reaction_time_distribution.png",
  eda_plots$reaction_time_distribution,
  width = 8,
  height = 5,
  dpi = 300
)


cat("CPC18 EDA completed.\n")
cat("Observations:", nrow(cpc18_features), "\n")
cat("Plots generated:", length(eda_plots), "\n")