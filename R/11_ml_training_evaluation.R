# ============================================================
# Human Decision Intelligence
# 11_ml_training_evaluation.R
#
# Purpose:
#   Train and evaluate supervised choice-prediction models.
#
#   Models using:
#   Logistic Regression (GLM)
#   GAM
#   Random Forest
#   XGBoost
# ============================================================

# Packages
library(dplyr)
library(mgcv)
library(ranger)
library(xgboost)

# Load prepared ML datasets
ml_train <- read.csv("data/processed/ml_train.csv")
ml_validation <- read.csv("data/processed/ml_validation.csv")

# Convert categorical predictors to factors
ml_train$payoff_correlation <- factor(ml_train$payoff_correlation, levels = c(-1, 0, 1))
ml_validation$payoff_correlation <- factor(ml_validation$payoff_correlation, levels = c(-1, 0, 1))

# Define predictors
ml_features <- read.csv("data/processed/ml_feature_definition.csv")$feature

# Remove the redundant predictor identified during GLM fitting
ml_features <- setdiff(ml_features, "payoff_spread_diff")

# Build formula
glm_formula <- as.formula(paste("chose_b ~", paste(ml_features, collapse = " + ")))

# Train logistic regression on training data only
glm_model <- glm(glm_formula, data = ml_train, family = binomial(link = "logit"))

print(summary(glm_model))

# Generate validation probabilities
validation_probability <- predict(glm_model, newdata = ml_validation, type = "response")

# Convert probabilities to class predictions
validation_prediction <- ifelse(validation_probability >= 0.5, 1, 0)

actual <- ml_validation$chose_b
predicted <- validation_prediction

# Calculate confusion matrix components
true_positive <- sum(actual == 1 & predicted == 1)
true_negative <- sum(actual == 0 & predicted == 0)
false_positive <- sum(actual == 0 & predicted == 1)
false_negative <- sum(actual == 1 & predicted == 0)

# Calculate classification metrics
accuracy <- mean(predicted == actual)

precision <- ifelse(true_positive + false_positive == 0, NA,
                    true_positive / (true_positive + false_positive))

recall <- ifelse(true_positive + false_negative == 0, NA,
                 true_positive / (true_positive + false_negative))

f1 <- ifelse(is.na(precision) || is.na(recall) || precision + recall == 0, NA,
             2 * precision * recall / (precision + recall))

# Calculate ROC-AUC using the rank statistic
n_positive <- sum(actual == 1)
n_negative <- sum(actual == 0)

probability_ranks <- rank(validation_probability)

roc_auc <- (sum(probability_ranks[actual == 1]) - n_positive * (n_positive + 1) / 2) /
  (as.numeric(n_positive) * as.numeric(n_negative))

# Calculate PR-AUC
pr_order <- order(validation_probability, decreasing = TRUE)
pr_actual <- actual[pr_order]

true_positives <- cumsum(pr_actual == 1)
false_positives <- cumsum(pr_actual == 0)

precision_curve <- true_positives / (true_positives + false_positives)
recall_curve <- true_positives / sum(actual == 1)

pr_auc <- sum(diff(c(0, recall_curve)) * precision_curve)

# Calculate calibration metrics
clipped_probability <- pmin(pmax(validation_probability, 1e-6), 1 - 1e-6)

brier_score <- mean((validation_probability - actual)^2)

calibration_model <- glm(actual ~ qlogis(clipped_probability), family = binomial)

calibration_intercept <- coef(calibration_model)[1]
calibration_slope <- coef(calibration_model)[2]

# Store evaluation results
glm_metrics <- data.frame(
  model = "GLM",
  dataset = "validation",
  accuracy = accuracy,
  precision = precision,
  recall = recall,
  f1 = f1,
  roc_auc = roc_auc,
  pr_auc = pr_auc,
  brier_score = brier_score,
  calibration_intercept = calibration_intercept,
  calibration_slope = calibration_slope,
  threshold = 0.5
)

print(glm_metrics)

# Store confusion matrix
glm_confusion_matrix <- data.frame(
  actual = c(0, 0, 1, 1),
  predicted = c(0, 1, 0, 1),
  count = c(true_negative, false_positive, false_negative, true_positive)
)

print(glm_confusion_matrix)

# Store validation predictions
glm_predictions <- data.frame(
  subject_id = ml_validation$subject_id,
  actual = actual,
  probability_b = validation_probability,
  predicted = predicted
)

# Store model coefficients
glm_coefficients <- data.frame(
  term = names(coef(glm_model)),
  estimate = coef(glm_model),
  odds_ratio = exp(coef(glm_model))
)

# Save model
saveRDS(glm_model, "data/processed/glm_model.rds")

# Save evaluation outputs
write.csv(glm_metrics, "data/processed/glm_metrics.csv", row.names = FALSE)
write.csv(glm_confusion_matrix, "data/processed/glm_confusion_matrix.csv", row.names = FALSE)
write.csv(glm_predictions, "data/processed/glm_validation_predictions.csv", row.names = FALSE)
write.csv(glm_coefficients, "data/processed/glm_coefficients.csv", row.names = FALSE)

cat("\nGLM training and evaluation completed.\n")
cat("Training observations:", nrow(ml_train), "\n")
cat("Validation observations:", nrow(ml_validation), "\n")
cat("Test data was not used.\n")

# GAM: Generalized Additive Model

# Continuous predictors modeled with smooth functions
gam_formula <- as.formula(paste(
  "chose_b ~",
  "s(expected_payoff_diff) + s(absolute_expected_payoff_diff) +",
  "s(probability_diff) + s(absolute_probability_diff) +",
  "s(low_payoff_diff) + s(high_lottery_ev_diff) + s(lottery_outcomes_diff) +",
  "ambiguous_b + payoff_correlation + full_feedback +",
  "game_order + trial_number + time_block"
))

# Train GAM on training data only
gam_model <- mgcv::gam(gam_formula, data = ml_train,
                       family = binomial(link = "logit"), method = "REML")

print(summary(gam_model))

# Generate validation probabilities
gam_validation_probability <- predict(gam_model, newdata = ml_validation, type = "response")

# Convert probabilities to class predictions
gam_validation_prediction <- ifelse(gam_validation_probability >= 0.5, 1, 0)

gam_actual <- ml_validation$chose_b
gam_predicted <- gam_validation_prediction

# Confusion matrix components
gam_true_positive <- sum(gam_actual == 1 & gam_predicted == 1)
gam_true_negative <- sum(gam_actual == 0 & gam_predicted == 0)
gam_false_positive <- sum(gam_actual == 0 & gam_predicted == 1)
gam_false_negative <- sum(gam_actual == 1 & gam_predicted == 0)

# Classification metrics
gam_accuracy <- mean(gam_predicted == gam_actual)

gam_precision <- ifelse(gam_true_positive + gam_false_positive == 0, NA,
                        gam_true_positive / (gam_true_positive + gam_false_positive))

gam_recall <- ifelse(gam_true_positive + gam_false_negative == 0, NA,
                     gam_true_positive / (gam_true_positive + gam_false_negative))

gam_f1 <- ifelse(is.na(gam_precision) || is.na(gam_recall) || gam_precision + gam_recall == 0, NA,
                 2 * gam_precision * gam_recall / (gam_precision + gam_recall))

# ROC-AUC
gam_n_positive <- sum(gam_actual == 1)
gam_n_negative <- sum(gam_actual == 0)

gam_probability_ranks <- rank(gam_validation_probability)

gam_roc_auc <- (sum(gam_probability_ranks[gam_actual == 1]) -
                  gam_n_positive * (gam_n_positive + 1) / 2) /
  (as.numeric(gam_n_positive) * as.numeric(gam_n_negative))

# PR-AUC
gam_pr_order <- order(gam_validation_probability, decreasing = TRUE)
gam_pr_actual <- gam_actual[gam_pr_order]

gam_true_positives <- cumsum(gam_pr_actual == 1)
gam_false_positives <- cumsum(gam_pr_actual == 0)

gam_precision_curve <- gam_true_positives / (gam_true_positives + gam_false_positives)
gam_recall_curve <- gam_true_positives / sum(gam_actual == 1)

gam_pr_auc <- sum(diff(c(0, gam_recall_curve)) * gam_precision_curve)

# Calibration metrics
gam_clipped_probability <- pmin(pmax(gam_validation_probability, 1e-6), 1 - 1e-6)

gam_brier_score <- mean((gam_validation_probability - gam_actual)^2)

gam_calibration_model <- glm(gam_actual ~ qlogis(gam_clipped_probability), family = binomial)

gam_calibration_intercept <- coef(gam_calibration_model)[1]
gam_calibration_slope <- coef(gam_calibration_model)[2]

# Store GAM evaluation results
gam_metrics <- data.frame(
  model = "GAM",
  dataset = "validation",
  accuracy = gam_accuracy,
  precision = gam_precision,
  recall = gam_recall,
  f1 = gam_f1,
  roc_auc = gam_roc_auc,
  pr_auc = gam_pr_auc,
  brier_score = gam_brier_score,
  calibration_intercept = gam_calibration_intercept,
  calibration_slope = gam_calibration_slope,
  threshold = 0.5
)

print(gam_metrics)

# Store GAM confusion matrix
gam_confusion_matrix <- data.frame(
  actual = c(0, 0, 1, 1),
  predicted = c(0, 1, 0, 1),
  count = c(gam_true_negative, gam_false_positive, gam_false_negative, gam_true_positive)
)

print(gam_confusion_matrix)

# Store GAM validation predictions
gam_predictions <- data.frame(
  subject_id = ml_validation$subject_id,
  actual = gam_actual,
  probability_b = gam_validation_probability,
  predicted = gam_predicted
)

# Save GAM model
saveRDS(gam_model, "data/processed/gam_model.rds")

# Save GAM evaluation outputs
write.csv(gam_metrics, "data/processed/gam_metrics.csv", row.names = FALSE)
write.csv(gam_confusion_matrix, "data/processed/gam_confusion_matrix.csv", row.names = FALSE)
write.csv(gam_predictions, "data/processed/gam_validation_predictions.csv", row.names = FALSE)

cat("\nGAM training and evaluation completed.\n")
cat("Training observations:", nrow(ml_train), "\n")
cat("Validation observations:", nrow(ml_validation), "\n")
cat("Test data was not used.\n")

# Ranger Random Forest

ranger_formula <- as.formula(paste("factor(chose_b) ~", paste(ml_features, collapse = " + ")))

# Train Random Forest on training data only
ranger_model <- ranger(
  formula = ranger_formula,
  data = ml_train,
  num.trees = 500,
  mtry = floor(sqrt(length(ml_features))),
  min.node.size = 10,
  probability = TRUE,
  importance = "impurity",
  seed = 42
)

print(ranger_model)

# Generate validation probabilities
ranger_prediction <- predict(ranger_model, data = ml_validation)

ranger_validation_probability <- ranger_prediction$predictions[, "1"]

# Convert probabilities to class predictions
ranger_validation_prediction <- ifelse(ranger_validation_probability >= 0.5, 1, 0)

ranger_actual <- ml_validation$chose_b
ranger_predicted <- ranger_validation_prediction

# Confusion matrix components
ranger_true_positive <- sum(ranger_actual == 1 & ranger_predicted == 1)
ranger_true_negative <- sum(ranger_actual == 0 & ranger_predicted == 0)
ranger_false_positive <- sum(ranger_actual == 0 & ranger_predicted == 1)
ranger_false_negative <- sum(ranger_actual == 1 & ranger_predicted == 0)

# Classification metrics
ranger_accuracy <- mean(ranger_predicted == ranger_actual)

ranger_precision <- ifelse(ranger_true_positive + ranger_false_positive == 0, NA,
                           ranger_true_positive / (ranger_true_positive + ranger_false_positive))

ranger_recall <- ifelse(ranger_true_positive + ranger_false_negative == 0, NA,
                        ranger_true_positive / (ranger_true_positive + ranger_false_negative))

ranger_f1 <- ifelse(is.na(ranger_precision) || is.na(ranger_recall) ||
                      ranger_precision + ranger_recall == 0, NA,
                    2 * ranger_precision * ranger_recall / (ranger_precision + ranger_recall))

# ROC-AUC
ranger_n_positive <- sum(ranger_actual == 1)
ranger_n_negative <- sum(ranger_actual == 0)

ranger_probability_ranks <- rank(ranger_validation_probability)

ranger_roc_auc <- (sum(ranger_probability_ranks[ranger_actual == 1]) -
                     ranger_n_positive * (ranger_n_positive + 1) / 2) /
  (as.numeric(ranger_n_positive) * as.numeric(ranger_n_negative))

# PR-AUC
ranger_pr_order <- order(ranger_validation_probability, decreasing = TRUE)
ranger_pr_actual <- ranger_actual[ranger_pr_order]

ranger_true_positives <- cumsum(ranger_pr_actual == 1)
ranger_false_positives <- cumsum(ranger_pr_actual == 0)

ranger_precision_curve <- ranger_true_positives / (ranger_true_positives + ranger_false_positives)
ranger_recall_curve <- ranger_true_positives / sum(ranger_actual == 1)

ranger_pr_auc <- sum(diff(c(0, ranger_recall_curve)) * ranger_precision_curve)

# Calibration metrics
ranger_clipped_probability <- pmin(pmax(ranger_validation_probability, 1e-6), 1 - 1e-6)

ranger_brier_score <- mean((ranger_validation_probability - ranger_actual)^2)

ranger_calibration_model <- glm(ranger_actual ~ qlogis(ranger_clipped_probability),
                                family = binomial)

ranger_calibration_intercept <- coef(ranger_calibration_model)[1]
ranger_calibration_slope <- coef(ranger_calibration_model)[2]

# Store Ranger evaluation results
ranger_metrics <- data.frame(
  model = "Ranger",
  dataset = "validation",
  accuracy = ranger_accuracy,
  precision = ranger_precision,
  recall = ranger_recall,
  f1 = ranger_f1,
  roc_auc = ranger_roc_auc,
  pr_auc = ranger_pr_auc,
  brier_score = ranger_brier_score,
  calibration_intercept = ranger_calibration_intercept,
  calibration_slope = ranger_calibration_slope,
  threshold = 0.5
)

print(ranger_metrics)

# Store Ranger confusion matrix
ranger_confusion_matrix <- data.frame(
  actual = c(0, 0, 1, 1),
  predicted = c(0, 1, 0, 1),
  count = c(ranger_true_negative, ranger_false_positive,
            ranger_false_negative, ranger_true_positive)
)

print(ranger_confusion_matrix)

# Store Ranger validation predictions
ranger_predictions <- data.frame(
  subject_id = ml_validation$subject_id,
  actual = ranger_actual,
  probability_b = ranger_validation_probability,
  predicted = ranger_predicted
)

# Store feature importance
ranger_importance <- data.frame(
  feature = names(ranger_model$variable.importance),
  importance = as.numeric(ranger_model$variable.importance)
) %>%
  arrange(desc(importance))

# Save Ranger model
saveRDS(ranger_model, "data/processed/ranger_model.rds")

# Save Ranger evaluation outputs
write.csv(ranger_metrics, "data/processed/ranger_metrics.csv", row.names = FALSE)
write.csv(ranger_confusion_matrix, "data/processed/ranger_confusion_matrix.csv", row.names = FALSE)
write.csv(ranger_predictions, "data/processed/ranger_validation_predictions.csv", row.names = FALSE)
write.csv(ranger_importance, "data/processed/ranger_feature_importance.csv", row.names = FALSE)

cat("\nRanger training and evaluation completed.\n")
cat("Training observations:", nrow(ml_train), "\n")
cat("Validation observations:", nrow(ml_validation), "\n")
cat("Test data was not used.\n")

# XGBoost

xgb_features <- setdiff(ml_features, "payoff_correlation")

xgb_train <- ml_train %>%
  mutate(correlation_negative = as.integer(payoff_correlation == -1),
         correlation_positive = as.integer(payoff_correlation == 1)) %>%
  select(all_of(xgb_features), correlation_negative, correlation_positive)

xgb_validation <- ml_validation %>%
  mutate(correlation_negative = as.integer(payoff_correlation == -1),
         correlation_positive = as.integer(payoff_correlation == 1)) %>%
  select(all_of(xgb_features), correlation_negative, correlation_positive)

xgb_train_matrix <- as.matrix(xgb_train)
xgb_validation_matrix <- as.matrix(xgb_validation)

xgb_train_label <- ml_train$chose_b
xgb_validation_label <- ml_validation$chose_b

# Train XGBoost on training data only
xgb_model <- xgboost(
  x = xgb_train_matrix,
  y = factor(xgb_train_label, levels = c(0, 1)),
  objective = "binary:logistic",
  eval_metric = "logloss",
  nrounds = 300,
  max_depth = 6,
  learning_rate = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 5,
  seed = 42
)

print(xgb_model)

# Generate validation probabilities
xgb_validation_probability <- predict(xgb_model, newdata = xgb_validation_matrix)

# Convert probabilities to class predictions
xgb_validation_prediction <- ifelse(xgb_validation_probability >= 0.5, 1, 0)

xgb_actual <- xgb_validation_label
xgb_predicted <- xgb_validation_prediction

# Confusion matrix components
xgb_true_positive <- sum(xgb_actual == 1 & xgb_predicted == 1)
xgb_true_negative <- sum(xgb_actual == 0 & xgb_predicted == 0)
xgb_false_positive <- sum(xgb_actual == 0 & xgb_predicted == 1)
xgb_false_negative <- sum(xgb_actual == 1 & xgb_predicted == 0)

# Classification metrics
xgb_accuracy <- mean(xgb_predicted == xgb_actual)

xgb_precision <- ifelse(xgb_true_positive + xgb_false_positive == 0, NA,
                        xgb_true_positive / (xgb_true_positive + xgb_false_positive))

xgb_recall <- ifelse(xgb_true_positive + xgb_false_negative == 0, NA,
                     xgb_true_positive / (xgb_true_positive + xgb_false_negative))

xgb_f1 <- ifelse(is.na(xgb_precision) || is.na(xgb_recall) || xgb_precision + xgb_recall == 0, NA,
                 2 * xgb_precision * xgb_recall / (xgb_precision + xgb_recall))

# ROC-AUC
xgb_n_positive <- sum(xgb_actual == 1)
xgb_n_negative <- sum(xgb_actual == 0)

xgb_probability_ranks <- rank(xgb_validation_probability)

xgb_roc_auc <- (sum(xgb_probability_ranks[xgb_actual == 1]) -
                  xgb_n_positive * (xgb_n_positive + 1) / 2) /
  (as.numeric(xgb_n_positive) * as.numeric(xgb_n_negative))

# PR-AUC
xgb_pr_order <- order(xgb_validation_probability, decreasing = TRUE)
xgb_pr_actual <- xgb_actual[xgb_pr_order]

xgb_true_positives <- cumsum(xgb_pr_actual == 1)
xgb_false_positives <- cumsum(xgb_pr_actual == 0)

xgb_precision_curve <- xgb_true_positives / (xgb_true_positives + xgb_false_positives)
xgb_recall_curve <- xgb_true_positives / sum(xgb_actual == 1)

xgb_pr_auc <- sum(diff(c(0, xgb_recall_curve)) * xgb_precision_curve)

# Calibration metrics
xgb_clipped_probability <- pmin(pmax(xgb_validation_probability, 1e-6), 1 - 1e-6)

xgb_brier_score <- mean((xgb_validation_probability - xgb_actual)^2)

xgb_calibration_model <- glm(xgb_actual ~ qlogis(xgb_clipped_probability), family = binomial)

xgb_calibration_intercept <- coef(xgb_calibration_model)[1]
xgb_calibration_slope <- coef(xgb_calibration_model)[2]

# Store XGBoost evaluation results
xgb_metrics <- data.frame(
  model = "XGBoost",
  dataset = "validation",
  accuracy = xgb_accuracy,
  precision = xgb_precision,
  recall = xgb_recall,
  f1 = xgb_f1,
  roc_auc = xgb_roc_auc,
  pr_auc = xgb_pr_auc,
  brier_score = xgb_brier_score,
  calibration_intercept = xgb_calibration_intercept,
  calibration_slope = xgb_calibration_slope,
  threshold = 0.5
)

print(xgb_metrics)

# Store XGBoost confusion matrix
xgb_confusion_matrix <- data.frame(
  actual = c(0, 0, 1, 1),
  predicted = c(0, 1, 0, 1),
  count = c(xgb_true_negative, xgb_false_positive, xgb_false_negative, xgb_true_positive)
)

print(xgb_confusion_matrix)

# Store validation predictions
xgb_predictions <- data.frame(
  subject_id = ml_validation$subject_id,
  actual = xgb_actual,
  probability_b = xgb_validation_probability,
  predicted = xgb_predicted
)

# Store feature importance
xgb_importance <- xgb.importance(feature_names = colnames(xgb_train_matrix), model = xgb_model)

# Save XGBoost model
saveRDS(xgb_model, "data/processed/xgboost_model.rds")

# Save XGBoost evaluation outputs
write.csv(xgb_metrics, "data/processed/xgboost_metrics.csv", row.names = FALSE)
write.csv(xgb_confusion_matrix, "data/processed/xgboost_confusion_matrix.csv", row.names = FALSE)
write.csv(xgb_predictions, "data/processed/xgboost_validation_predictions.csv", row.names = FALSE)
write.csv(xgb_importance, "data/processed/xgboost_feature_importance.csv", row.names = FALSE)

cat("\nXGBoost training and evaluation completed.\n")
cat("Training observations:", nrow(ml_train), "\n")
cat("Validation observations:", nrow(ml_validation), "\n")
cat("Test data was not used.\n")