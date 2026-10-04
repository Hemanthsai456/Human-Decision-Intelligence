# ============================================================
# Human Decision Intelligence
# 15_decision_intelligence.R
#
# Purpose:
#   Synthesize the completed statistical, behavioural, ML, and
#   explainability results into a concise decision-intelligence
#   layer for interpretation and the Shiny application.
# ============================================================

# Packages
library(dplyr)
library(ggplot2)

# Load completed analysis outputs
final_model_selection <- read.csv("data/processed/final_model_selection.csv")
final_test_metrics <- read.csv("data/processed/final_test_metrics.csv")
final_test_predictions <- read.csv("data/processed/final_test_predictions.csv")

shap_importance <- read.csv("data/processed/shap_global_importance.csv")
statistical_results <- read.csv("data/processed/statistical_model_results.csv")

cluster_sizes <- read.csv("data/processed/clustering_sizes.csv")
cluster_centers <- read.csv("data/processed/clustering_centers.csv")

final_model_name <- final_model_selection$final_model[1]

# Create output directory
dir.create("outputs/decision_intelligence", recursive = TRUE, showWarnings = FALSE)

# Final model summary
final_model_summary <- final_test_metrics %>%
  mutate(final_model = final_model_name) %>%
  select(final_model, dataset, accuracy, precision, recall, f1, roc_auc, pr_auc,
         brier_score, calibration_intercept, calibration_slope)

write.csv(final_model_summary, "data/processed/decision_intelligence_model_summary.csv",
          row.names = FALSE)

# Final prediction summary
prediction_summary <- final_test_predictions %>%
  summarise(observations = n(), actual_b_rate = mean(actual),
            predicted_b_rate = mean(predicted), mean_probability_b = mean(probability_b),
            correct_predictions = sum(actual == predicted), accuracy = mean(actual == predicted))

write.csv(prediction_summary, "data/processed/decision_intelligence_prediction_summary.csv",
          row.names = FALSE)

# Global decision-factor ranking from SHAP (shares of total importance)
decision_factor_summary <- shap_importance %>%
  arrange(desc(mean_abs_shap)) %>%
  mutate(rank = row_number(),
         share = mean_abs_shap / sum(mean_abs_shap),
         cumulative_share = cumsum(share),
         evidence = "SHAP global importance") %>%
  select(rank, feature, mean_abs_shap, share, cumulative_share, evidence)

write.csv(decision_factor_summary, "data/processed/decision_factor_summary.csv",
          row.names = FALSE)

# Statistical evidence from the main mixed-effects model (estimates are odds ratios)
statistical_evidence <- statistical_results %>%
  filter(model == "Main Model", term != "(Intercept)") %>%
  mutate(odds_ratio = estimate,
         ci_excludes_1 = conf.low > 1 | conf.high < 1,
         direction = case_when(!ci_excludes_1 ~ "No clear association",
                               odds_ratio > 1 ~ "Higher odds of choosing B",
                               TRUE ~ "Lower odds of choosing B")) %>%
  select(term, odds_ratio, conf.low, conf.high, std.error, statistic, p.value,
         ci_excludes_1, direction)

write.csv(statistical_evidence, "data/processed/decision_statistical_evidence.csv",
          row.names = FALSE)

# Behavioral profile summary from clustering
cluster_profile_summary <- cluster_sizes %>% left_join(cluster_centers, by = "cluster")

write.csv(cluster_profile_summary, "data/processed/decision_behavioral_profiles.csv",
          row.names = FALSE)

# Consolidated decision-intelligence summary
top_factors <- decision_factor_summary %>% slice_head(n = 5) %>% pull(feature)

decision_intelligence_summary <- data.frame(
  final_model = final_model_name,
  test_observations = prediction_summary$observations,
  test_accuracy = final_test_metrics$accuracy[1],
  test_roc_auc = final_test_metrics$roc_auc[1],
  test_pr_auc = final_test_metrics$pr_auc[1],
  test_brier_score = final_test_metrics$brier_score[1],
  actual_b_rate = prediction_summary$actual_b_rate,
  predicted_b_rate = prediction_summary$predicted_b_rate,
  mean_probability_b = prediction_summary$mean_probability_b,
  as.data.frame(as.list(setNames(top_factors, paste0("top_factor_", seq_along(top_factors)))))
)

write.csv(decision_intelligence_summary, "data/processed/decision_intelligence_summary.csv",
          row.names = FALSE)

# Plot the main decision factors
factor_plot <- decision_factor_summary %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(feature, mean_abs_shap), y = mean_abs_shap)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top Decision Factors in the Final XGBoost Model",
       x = "Feature", y = "Mean Absolute SHAP Value") +
  theme_minimal()

ggsave("outputs/decision_intelligence/top_decision_factors.png", factor_plot,
       width = 8, height = 6, dpi = 300)

cat("CPC18 decision intelligence synthesis completed.\n")
cat("Final model:", final_model_name, "\n")
cat("Test observations:", prediction_summary$observations, "\n")
cat("Top decision factor:", decision_factor_summary$feature[1], "\n")
cat("Behavioral profiles:", nrow(cluster_sizes), "\n")
cat("No new model was trained in Stage 15.\n")