# ============================================================
# Human Decision Intelligence
# 10_ml_training.R
#
# Purpose:
#   Create subject-grouped training, validation, and testing
#   splits for supervised choice prediction.
#
# NOTE:
#   The same participant must never appear in more than one
#   split. The test set remains untouched until final evaluation.
# ============================================================

# Packages
library(dplyr)
library(tidyr)

# Load feature-engineered data
cpc18_features <- read.csv("data/processed/cpc18_features.csv")

# 1. Subject-grouped train / validation / test split

set.seed(42)

subjects <- sort(unique(cpc18_features$subject_id))

n_subjects <- length(subjects)
n_train <- floor(0.70 * n_subjects)
n_validation <- floor(0.15 * n_subjects)

train_subjects <- sample(subjects, n_train)

remaining_subjects <- setdiff(subjects, train_subjects)

validation_subjects <- sample(
  remaining_subjects,
  n_validation
)

test_subjects <- setdiff(
  remaining_subjects,
  validation_subjects
)

subject_split <- tibble(
  subject_id = subjects,
  split = case_when(
    subject_id %in% train_subjects ~ "train",
    subject_id %in% validation_subjects ~ "validation",
    subject_id %in% test_subjects ~ "test"
  )
)

# 2. Verify subject separation

split_sets <- split(subject_split$subject_id, subject_split$split)

stopifnot(
  length(intersect(split_sets$train, split_sets$validation)) == 0,
  length(intersect(split_sets$train, split_sets$test)) == 0,
  length(intersect(split_sets$validation, split_sets$test)) == 0
)

stopifnot(nrow(subject_split) == n_subjects, 
          sum(table(subject_split$split)) == n_subjects)

# 3. Apply subject split to observations

ml_split <- cpc18_features %>% left_join(subject_split, by = "subject_id")

stopifnot(nrow(ml_split) == nrow(cpc18_features), !any(is.na(ml_split$split)))

# 4. Split summary

ml_split_summary <- ml_split %>%
  group_by(split) %>%
  summarise(
    subjects = n_distinct(subject_id),
    observations = n(),
    chose_b = sum(chose_b),
    choice_rate_b = mean(chose_b),
    .groups = "drop"
  )

print(ml_split_summary)

# 5. Save split definitions

write.csv(subject_split, "data/processed/ml_subject_split.csv",
          row.names = FALSE)

write.csv(ml_split_summary, "data/processed/ml_split_summary.csv",
          row.names = FALSE)

cat("\nML subject-grouped split completed.\n")
cat("Total subjects:", n_subjects, "\n")
cat("Training subjects:", length(train_subjects), "\n")
cat("Validation subjects:", length(validation_subjects), "\n")
cat("Testing subjects:", length(test_subjects), "\n")
cat("Total observations:", nrow(ml_split), "\n")

# 6. Define legitimate pre-decision predictors

ml_features <- c(
  "expected_payoff_diff",
  "absolute_expected_payoff_diff",
  "probability_diff",
  "absolute_probability_diff",
  "low_payoff_diff",
  "high_lottery_ev_diff",
  "payoff_spread_diff",
  "lottery_outcomes_diff",
  "ambiguous_b",
  "payoff_correlation",
  "full_feedback",
  "game_order",
  "trial_number",
  "time_block"
)

target <- "chose_b"

required_columns <- c("subject_id", "split", target, ml_features)

missing_columns <- setdiff(required_columns, names(ml_split))

if (length(missing_columns) > 0) {
  stop(paste(
      "Missing required ML column(s):",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# 7. Create the ML dataset

ml_data <- ml_split %>% select(all_of(required_columns))

# 8. Verify that only pre-decision information is used

post_decision_variables <- c(
  "realized_payoff",
  "forgone_payoff",
  "payoff_a",
  "payoff_b",
  "reaction_time_ms",
  "selected_button"
)

leakage_columns <- intersect(post_decision_variables, names(ml_data))

stopifnot(length(leakage_columns) == 0)

# 9. Check missing values before preprocessing

missing_summary <- ml_data %>%
  summarise(
    across(
      all_of(c(target, ml_features)),
      ~ sum(is.na(.x))
    )
  ) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing"
  )

print(missing_summary)

# 10. Remove observations with missing ML inputs or target

rows_before_cleaning <- nrow(ml_data)

ml_data <- ml_data %>%
  filter(
    if_all(
      all_of(c(target, ml_features)),
      ~ !is.na(.x)
    )
  )

rows_after_cleaning <- nrow(ml_data)

cat(
  "Rows removed during ML cleaning:",
  rows_before_cleaning - rows_after_cleaning,
  "\n"
)

# 11. Convert categorical predictors to factors

ml_data <- ml_data %>%
  mutate(
    payoff_correlation = factor(
      payoff_correlation,
      levels = c(-1, 0, 1)
    )
  )

# 12. Separate the cleaned datasets

ml_train <- ml_data %>%
  filter(split == "train") %>%
  select(-split)

ml_validation <- ml_data %>%
  filter(split == "validation") %>%
  select(-split)

ml_test <- ml_data %>%
  filter(split == "test") %>%
  select(-split)

# 13. Verify subject separation after cleaning

train_ids <- unique(ml_train$subject_id)
validation_ids <- unique(ml_validation$subject_id)
test_ids <- unique(ml_test$subject_id)

stopifnot(
  length(intersect(train_ids, validation_ids)) == 0,
  length(intersect(train_ids, test_ids)) == 0,
  length(intersect(validation_ids, test_ids)) == 0
)

# 14. Verify target coding

stopifnot(
  all(ml_train$chose_b %in% c(0, 1)),
  all(ml_validation$chose_b %in% c(0, 1)),
  all(ml_test$chose_b %in% c(0, 1))
)

# 15. Save cleaned ML datasets

write.csv(ml_train, "data/processed/ml_train.csv", row.names = FALSE)

write.csv(ml_validation, "data/processed/ml_validation.csv",row.names = FALSE)

write.csv(ml_test, "data/processed/ml_test.csv", row.names = FALSE)

# 16. Save ML feature definition

write.csv(data.frame(feature = ml_features, role = "pre_decision_predictor"),
  "data/processed/ml_feature_definition.csv",row.names = FALSE)

# 17. Print final dataset summary

ml_dataset_summary <- bind_rows(
  ml_train %>%
    summarise(
      split = "train",
      subjects = n_distinct(subject_id),
      observations = n(),
      chose_b = sum(chose_b),
      choice_rate_b = mean(chose_b)
    ),
  ml_validation %>%
    summarise(
      split = "validation",
      subjects = n_distinct(subject_id),
      observations = n(),
      chose_b = sum(chose_b),
      choice_rate_b = mean(chose_b)
    ),
  ml_test %>%
    summarise(
      split = "test",
      subjects = n_distinct(subject_id),
      observations = n(),
      chose_b = sum(chose_b),
      choice_rate_b = mean(chose_b)
    )
)

print(ml_dataset_summary)

cat("ML preprocessing foundation completed.\n")