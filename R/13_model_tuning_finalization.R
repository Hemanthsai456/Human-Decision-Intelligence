# ============================================================
# Human Decision Intelligence
# 13_model_tuning_finalization.R
#
# Purpose:
#   Tune the strongest candidate models from Stage 12,
#   finalize the selected model, and evaluate it once on
#   the previously untouched test set.
# ============================================================

# Packages
library(dplyr)
library(ranger)
library(xgboost)

# Load ML data and feature definition
ml_train <- read.csv("data/processed/ml_train.csv")
ml_validation <- read.csv("data/processed/ml_validation.csv")
ml_test <- read.csv("data/processed/ml_test.csv")

# Predictors, without the redundant payoff-spread feature removed in Stage 11
ml_features <- setdiff(read.csv("data/processed/ml_feature_definition.csv")$feature,
                       "payoff_spread_diff")

# Factorize categorical predictor consistently
ml_train$payoff_correlation <- factor(ml_train$payoff_correlation, levels = c(-1, 0, 1))
ml_validation$payoff_correlation <- factor(ml_validation$payoff_correlation, levels = c(-1, 0, 1))
ml_test$payoff_correlation <- factor(ml_test$payoff_correlation, levels = c(-1, 0, 1))

# Evaluation function (same metric definitions as Stage 11)
evaluate_model <- function(actual, probability, threshold = 0.5) {
  predicted <- ifelse(probability >= threshold, 1, 0)
  
  tp <- sum(predicted == 1 & actual == 1)
  tn <- sum(predicted == 0 & actual == 0)
  fp <- sum(predicted == 1 & actual == 0)
  fn <- sum(predicted == 0 & actual == 1)
  
  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
  f1 <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  
  # ROC-AUC using the rank statistic
  n_positive <- sum(actual == 1)
  n_negative <- sum(actual == 0)
  roc_auc <- (sum(rank(probability)[actual == 1]) - n_positive * (n_positive + 1) / 2) /
    (as.numeric(n_positive) * as.numeric(n_negative))
  
  # PR-AUC
  pr_actual <- actual[order(probability, decreasing = TRUE)]
  true_positives <- cumsum(pr_actual == 1)
  false_positives <- cumsum(pr_actual == 0)
  pr_auc <- sum(diff(c(0, true_positives / n_positive)) *
                  (true_positives / (true_positives + false_positives)))
  
  # Logistic calibration intercept and slope
  clipped <- pmin(pmax(probability, 1e-6), 1 - 1e-6)
  calibration <- unname(coef(glm(actual ~ qlogis(clipped), family = binomial)))
  
  tibble(
    accuracy = (tp + tn) / length(actual),
    precision = precision,
    recall = recall,
    f1 = f1,
    roc_auc = roc_auc,
    pr_auc = pr_auc,
    brier_score = mean((probability - actual)^2),
    calibration_intercept = calibration[1],
    calibration_slope = calibration[2]
  )
}

# Model helpers
fit_ranger <- function(data, p, importance = "none") {
  ranger(factor(chose_b) ~ ., data = data %>% select(chose_b, all_of(ml_features)),
         num.trees = p$num.trees, mtry = p$mtry, min.node.size = p$min.node.size,
         sample.fraction = p$sample.fraction, probability = TRUE,
         importance = importance, seed = 42)
}

predict_ranger <- function(model, data) {
  predict(model, data = data %>% select(all_of(ml_features)))$predictions[, "1"]
}

xgb_features <- setdiff(ml_features, "payoff_correlation")

prepare_xgb <- function(data) {
  data %>%
    mutate(correlation_negative = as.integer(payoff_correlation == -1),
           correlation_positive = as.integer(payoff_correlation == 1)) %>%
    select(all_of(xgb_features), correlation_negative, correlation_positive) %>%
    as.matrix()
}

fit_xgb <- function(data, p) {
  xgboost(x = prepare_xgb(data), y = factor(data$chose_b, levels = c(0, 1)),
          objective = "binary:logistic", eval_metric = "logloss",
          nrounds = p$nrounds, max_depth = p$max_depth, learning_rate = p$learning_rate,
          min_child_weight = p$min_child_weight, subsample = p$subsample,
          colsample_bytree = p$colsample_bytree, seed = 42, verbosity = 0)
}

# Ranger tuning grid
ranger_grid <- expand.grid(mtry = c(3, 5, 7), min.node.size = c(5, 10, 20),
                           sample.fraction = 0.8, num.trees = 500,
                           KEEP.OUT.ATTRS = FALSE)

ranger_results <- lapply(seq_len(nrow(ranger_grid)), function(i) {
  params <- ranger_grid[i, ]
  model <- fit_ranger(ml_train, params)
  bind_cols(params, evaluate_model(ml_validation$chose_b, predict_ranger(model, ml_validation)))
}) %>%
  bind_rows()

write.csv(ranger_results, "data/processed/ranger_tuning_results.csv", row.names = FALSE)

# Select Ranger configuration using validation ROC-AUC
ranger_best <- ranger_results %>% arrange(desc(roc_auc), desc(pr_auc), brier_score) %>% slice(1)

# Best Ranger model
ranger_final_validation <- fit_ranger(ml_train, ranger_best, importance = "impurity")

ranger_validation_metrics <- evaluate_model(
  ml_validation$chose_b, predict_ranger(ranger_final_validation, ml_validation)
) %>%
  mutate(model = "Ranger")

saveRDS(ranger_final_validation, "data/processed/ranger_tuned_model.rds")

# XGBoost tuning grid
xgb_grid <- expand.grid(max_depth = c(4, 6), learning_rate = c(0.03, 0.05),
                        min_child_weight = c(5, 10), subsample = 0.8,
                        colsample_bytree = 0.8, nrounds = 300,
                        KEEP.OUT.ATTRS = FALSE)

xgb_results <- lapply(seq_len(nrow(xgb_grid)), function(i) {
  params <- xgb_grid[i, ]
  model <- fit_xgb(ml_train, params)
  bind_cols(params, evaluate_model(ml_validation$chose_b,
                                   predict(model, prepare_xgb(ml_validation))))
}) %>%
  bind_rows()

write.csv(xgb_results, "data/processed/xgboost_tuning_results.csv", row.names = FALSE)

# Select XGBoost configuration using validation ROC-AUC
xgb_best <- xgb_results %>% arrange(desc(roc_auc), desc(pr_auc), brier_score) %>% slice(1)

# Best XGBoost model
xgb_final_validation <- fit_xgb(ml_train, xgb_best)

xgb_validation_metrics <- evaluate_model(
  ml_validation$chose_b, predict(xgb_final_validation, prepare_xgb(ml_validation))
) %>%
  mutate(model = "XGBoost")

saveRDS(xgb_final_validation, "data/processed/xgboost_tuned_model.rds")

# Compare tuned candidates
tuned_validation_metrics <- bind_rows(ranger_validation_metrics, xgb_validation_metrics) %>%
  select(model, everything())

write.csv(tuned_validation_metrics, "data/processed/tuned_model_validation_metrics.csv",
          row.names = FALSE)

# Select final model using validation evidence (ROC-AUC, then Brier score)
final_model_name <- tuned_validation_metrics %>%
  arrange(desc(roc_auc), brier_score) %>%
  slice(1) %>%
  pull(model)

write.csv(data.frame(final_model = final_model_name),
          "data/processed/final_model_selection.csv", row.names = FALSE)

# Refit the selected model on training + validation data, then predict the test set
ml_train_validation <- bind_rows(ml_train, ml_validation)

if (final_model_name == "Ranger") {
  final_model <- fit_ranger(ml_train_validation, ranger_best, importance = "impurity")
  test_probability <- predict_ranger(final_model, ml_test)
} else {
  final_model <- fit_xgb(ml_train_validation, xgb_best)
  test_probability <- predict(final_model, prepare_xgb(ml_test))
}

saveRDS(final_model, "data/processed/final_model.rds")

# Final test evaluation
final_test_metrics <- evaluate_model(ml_test$chose_b, test_probability) %>%
  mutate(model = final_model_name, dataset = "test")

write.csv(final_test_metrics, "data/processed/final_test_metrics.csv", row.names = FALSE)

final_test_predictions <- data.frame(
  subject_id = ml_test$subject_id,
  actual = ml_test$chose_b,
  probability_b = test_probability,
  predicted = ifelse(test_probability >= 0.5, 1, 0)
)

write.csv(final_test_predictions, "data/processed/final_test_predictions.csv",
          row.names = FALSE)

cat("CPC18 model tuning and finalization completed.\n")
cat("Tuned candidates: Ranger and XGBoost\n")
cat("Selected final model:", final_model_name, "\n")
cat("Final test evaluation completed once.\n")