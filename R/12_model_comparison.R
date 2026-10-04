# ============================================================
# Human Decision Intelligence
# 12_model_comparison.R
#
# Purpose:
#   Compare the validation performance of the candidate ML models
#   trained in Stage 11 and identify a candidate for Stage 13.
# ============================================================

# Packages
library(dplyr)
library(ggplot2)
library(purrr)

models <- c(GLM = "glm", GAM = "gam", Ranger = "ranger", XGBoost = "xgboost")

# Read one Stage 11 output for every model and stack the results
read_all <- function(suffix) {
  map_dfr(names(models), function(m) {
    read.csv(paste0("data/processed/", models[[m]], suffix)) %>% mutate(model = m)
  })
}

# Validation metrics
model_metrics <- read_all("_metrics.csv") %>% select(model, everything())

comparison_metrics <- model_metrics %>%
  select(model, accuracy, precision, recall, f1, roc_auc, pr_auc,
         brier_score, calibration_intercept, calibration_slope)

write.csv(model_metrics, "data/processed/model_comparison_metrics.csv", row.names = FALSE)
write.csv(comparison_metrics, "data/processed/model_comparison_summary.csv", row.names = FALSE)

# Best model for each validation metric
metric_directions <- c(accuracy = "max", precision = "max", recall = "max", f1 = "max",
                       roc_auc = "max", pr_auc = "max", brier_score = "min",
                       calibration_intercept = "zero", calibration_slope = "one")

metric_winners <- map_dfr(names(metric_directions), function(metric) {
  values <- comparison_metrics[[metric]]
  
  score <- switch(metric_directions[[metric]],
                  max = -values, min = values, zero = abs(values), one = abs(values - 1))
  
  comparison_metrics[which(score == min(score, na.rm = TRUE)), ] %>%
    transmute(metric = metric, model = model, value = .data[[metric]])
})

write.csv(metric_winners, "data/processed/model_metric_winners.csv", row.names = FALSE)

# Confusion matrices and validation predictions
confusion_matrices <- read_all("_confusion_matrix.csv")

validation_predictions <- read_all("_validation_predictions.csv") %>%
  mutate(actual = as.numeric(actual), probability_b = as.numeric(probability_b))

write.csv(confusion_matrices, "data/processed/model_comparison_confusion_matrices.csv",
          row.names = FALSE)
write.csv(validation_predictions, "data/processed/model_comparison_predictions.csv",
          row.names = FALSE)

# ROC and precision-recall curve data
# Denominators are calculated before collapsing tied probabilities
curve_data <- validation_predictions %>%
  group_by(model) %>%
  arrange(desc(probability_b), .by_group = TRUE) %>%
  mutate(n_positive = sum(actual == 1), n_negative = sum(actual == 0),
         tp = cumsum(actual == 1), fp = cumsum(actual == 0)) %>%
  filter(!duplicated(probability_b, fromLast = TRUE)) %>%
  mutate(threshold = probability_b, tpr = tp / n_positive, fpr = fp / n_negative,
         precision = ifelse(tp + fp == 0, 0, tp / (tp + fp)), recall = tpr) %>%
  ungroup()

roc_curve_data <- curve_data %>% select(model, threshold, tpr, fpr)
pr_curve_data <- curve_data %>% select(model, threshold, precision, recall)

write.csv(roc_curve_data, "data/processed/model_roc_curve_data.csv", row.names = FALSE)
write.csv(pr_curve_data, "data/processed/model_pr_curve_data.csv", row.names = FALSE)

# Calibration curve data
calibration_data <- validation_predictions %>%
  mutate(probability_bin = cut(probability_b, breaks = seq(0, 1, by = 0.05),
                               include.lowest = TRUE)) %>%
  group_by(model, probability_bin) %>%
  summarise(mean_predicted = mean(probability_b), observed_rate = mean(actual),
            observations = n(), .groups = "drop")

write.csv(calibration_data, "data/processed/model_calibration_data.csv", row.names = FALSE)

# Feature importance
# Scaled to relative shares so the two tree models can be compared
feature_importance_comparison <- bind_rows(
  read.csv("data/processed/ranger_feature_importance.csv") %>%
    transmute(model = "Ranger", feature, importance),
  read.csv("data/processed/xgboost_feature_importance.csv") %>%
    transmute(model = "XGBoost", feature = Feature, importance = Gain)
) %>%
  group_by(model) %>%
  mutate(relative_importance = importance / sum(importance)) %>%
  ungroup()

write.csv(feature_importance_comparison,
          "data/processed/model_feature_importance_comparison.csv", row.names = FALSE)

# Plots
dir.create("outputs/model_comparison", recursive = TRUE, showWarnings = FALSE)
theme_set(theme_minimal(base_size = 12))

# ROC curve
roc_plot <- ggplot(roc_curve_data, aes(x = fpr, y = tpr, colour = model)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = "Validation ROC Curves", x = "False Positive Rate",
       y = "True Positive Rate", colour = "Model")

# Precision-recall curve
pr_plot <- ggplot(pr_curve_data, aes(x = recall, y = precision, colour = model)) +
  geom_line() +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = "Validation Precision-Recall Curves", x = "Recall",
       y = "Precision", colour = "Model")

# Calibration curve
calibration_plot <- ggplot(calibration_data,
                           aes(x = mean_predicted, y = observed_rate, colour = model)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line() +
  geom_point() +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = "Validation Calibration Curves", x = "Mean Predicted Probability",
       y = "Observed Choice Rate", colour = "Model")

ggsave("outputs/model_comparison/model_comparison_roc.png", roc_plot,
       width = 8, height = 5, dpi = 300)
ggsave("outputs/model_comparison/model_comparison_pr.png", pr_plot,
       width = 8, height = 5, dpi = 300)
ggsave("outputs/model_comparison/model_comparison_calibration.png", calibration_plot,
       width = 8, height = 5, dpi = 300)

# Candidate assessment
# Provides validation evidence without creating an arbitrary
# composite score across different evaluation criteria.
candidate_assessment <- comparison_metrics %>%
  mutate(roc_auc_rank = rank(-roc_auc, ties.method = "min"),
         pr_auc_rank = rank(-pr_auc, ties.method = "min"),
         brier_rank = rank(brier_score, ties.method = "min"),
         calibration_slope_distance = abs(calibration_slope - 1),
         calibration_intercept_distance = abs(calibration_intercept)) %>%
  arrange(roc_auc_rank)

write.csv(candidate_assessment, "data/processed/model_candidate_assessment.csv",
          row.names = FALSE)

cat("CPC18 model comparison completed.\n")
cat("Models compared:", nrow(comparison_metrics), "\n")
cat("Validation predictions:", nrow(validation_predictions), "\n")
cat("Test data was not used.\n")