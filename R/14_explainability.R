# ============================================================
# Human Decision Intelligence
# 14_explainability.R
#
# Purpose:
#   Explain the finalized XGBoost model using global feature
#   importance, SHAP explanations, and LIME local explanations.
# ============================================================

# Packages
library(dplyr)
library(ggplot2)
library(xgboost)
library(shapviz)
library(lime)

# Load final model and test data
final_model <- readRDS("data/processed/final_model.rds")
ml_test <- read.csv("data/processed/ml_test.csv")

ml_features <- setdiff(read.csv("data/processed/ml_feature_definition.csv")$feature,
                       "payoff_spread_diff")

# Prepare XGBoost predictors
xgb_features <- setdiff(ml_features, "payoff_correlation")

prepare_xgb <- function(data) {
  data %>%
    mutate(correlation_negative = as.integer(payoff_correlation == -1),
           correlation_positive = as.integer(payoff_correlation == 1)) %>%
    select(all_of(xgb_features), correlation_negative, correlation_positive) %>%
    as.matrix()
}

test_matrix <- prepare_xgb(ml_test)
feature_names <- colnames(test_matrix)

# Create output directory
dir.create("outputs/explainability", recursive = TRUE, showWarnings = FALSE)

# Global XGBoost feature importance
xgb_importance <- xgb.importance(feature_names = feature_names, model = final_model)

write.csv(xgb_importance, "data/processed/final_xgboost_feature_importance.csv",
          row.names = FALSE)

importance_plot <- xgb_importance %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col() +
  coord_flip() +
  labs(title = "Final XGBoost Feature Importance", x = "Feature", y = "Gain") +
  theme_minimal()

ggsave("outputs/explainability/xgboost_feature_importance.png", importance_plot,
       width = 8, height = 6, dpi = 300)

# SHAP values from XGBoost (log-odds scale)
shap_raw <- predict(final_model, test_matrix, type = "contrib")

if (ncol(shap_raw) != length(feature_names) + 1) stop("Unexpected SHAP output dimensions.")

shap_values <- shap_raw[, feature_names, drop = FALSE]

shap_object <- shapviz(shap_values, X = as.data.frame(test_matrix))

# Global SHAP importance
shap_importance <- data.frame(feature = colnames(shap_object$S),
                              mean_abs_shap = colMeans(abs(shap_object$S))) %>%
  arrange(desc(mean_abs_shap))

write.csv(shap_importance, "data/processed/shap_global_importance.csv", row.names = FALSE)

# Random subset of rows for the point-based plots
set.seed(42)
shap_sample <- shap_object[sample(nrow(test_matrix), min(5000, nrow(test_matrix)))]

# SHAP summary plot
shap_summary_plot <- sv_importance(shap_sample, kind = "beeswarm", max_display = 15)

ggsave("outputs/explainability/shap_summary.png", shap_summary_plot,
       width = 9, height = 7, dpi = 300)

# SHAP bar plot
shap_bar_plot <- sv_importance(shap_object, kind = "bar", max_display = 15)

ggsave("outputs/explainability/shap_global_importance.png", shap_bar_plot,
       width = 8, height = 6, dpi = 300)

# Top SHAP features
top_shap_features <- shap_importance %>% slice_head(n = 5) %>% pull(feature)

write.csv(data.frame(feature = top_shap_features), "data/processed/top_shap_features.csv",
          row.names = FALSE)

# SHAP dependence plots for the top features (kept in a named list)
shap_dependence_plots <- list()

for (feature in top_shap_features) {
  shap_dependence_plots[[feature]] <- sv_dependence(shap_sample, v = feature)
  
  ggsave(paste0("outputs/explainability/shap_dependence_",
                gsub("[^A-Za-z0-9_]+", "_", feature), ".png"),
         shap_dependence_plots[[feature]], width = 8, height = 6, dpi = 300)
}

# LIME model wrapper: lime finds the model type and predictions through S3 methods
lime_model <- structure(list(model = final_model), class = "cpc_xgb")

model_type.cpc_xgb <- function(x, ...) "classification"

predict_model.cpc_xgb <- function(x, newdata, type, ...) {
  probability <- predict(x$model, prepare_xgb(newdata))
  data.frame(`0` = 1 - probability, `1` = probability, check.names = FALSE)
}

# LIME preparation (payoff_correlation as a factor, since it has only three values)
lime_data <- ml_test %>%
  select(all_of(ml_features)) %>%
  mutate(payoff_correlation = factor(payoff_correlation, levels = c(-1, 0, 1)))

lime_explainer <- lime(x = lime_data, model = lime_model, bin_continuous = TRUE)

# Select random test observations
set.seed(42)
lime_indices <- sample(seq_len(nrow(lime_data)), size = min(5, nrow(lime_data)))
lime_cases <- lime_data %>% slice(lime_indices)

# Generate local explanations
lime_explanations <- explain(
  x = lime_cases,
  explainer = lime_explainer,
  labels = "1",
  n_features = 8,
  n_permutations = 1000,
  feature_select = "highest_weights"
)

# Convert LIME output to a CSV-safe table
lime_output <- lime_explanations %>%
  mutate(across(where(is.list), ~ sapply(.x, paste, collapse = "; ")))

write.csv(lime_output, "data/processed/lime_local_explanations.csv", row.names = FALSE)

# Local explanation plots (kept in a list, one per case)
lime_plots <- lapply(seq_along(lime_indices), function(i) {
  plot <- plot_features(lime_explanations %>% filter(case == i), ncol = 1)
  
  ggsave(paste0("outputs/explainability/lime_case_", i, ".png"), plot,
         width = 8, height = 6, dpi = 300)
  
  plot
})

cat("CPC18 explainability analysis completed.\n")
cat("Final model: XGBoost\n")
cat("SHAP features analyzed:", length(top_shap_features), "\n")
cat("LIME cases:", length(lime_indices), "\n")
cat("Explanations use test predictors only, not test outcomes or model training.\n")