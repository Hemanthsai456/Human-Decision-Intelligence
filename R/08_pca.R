# ============================================================
# Human Decision Intelligence
# 08_pca.R
#
# Purpose:
#   Reduce the dimensionality of key continuous decision
#   features and summarize their main variation.
# ============================================================

# Packages
library(dplyr)
library(ggplot2)

cpc18_features <- read.csv("data/processed/cpc18_features.csv")

pca_features <- c(
  "expected_payoff_diff",
  "probability_diff",
  "low_payoff_diff",
  "high_lottery_ev_diff",
  "payoff_spread_diff",
  "lottery_outcomes_diff"
)

pca_data <- cpc18_features %>%
  select(all_of(pca_features)) %>%
  scale()

pca_model <- prcomp(
  pca_data,
  center = FALSE,
  scale. = FALSE
)

explained_variance <- data.frame(
  component = paste0("PC", seq_along(pca_model$sdev)),
  variance = pca_model$sdev^2,
  proportion = pca_model$sdev^2 / sum(pca_model$sdev^2)
) %>%
  mutate(
    cumulative = cumsum(proportion)
  )

loadings <- as.data.frame(
  pca_model$rotation
)

loadings$feature <- rownames(loadings)
rownames(loadings) <- NULL

pca_scores <- as.data.frame(pca_model$x)

pca_scores <- bind_cols(
  cpc18_features %>%
    select(subject_id, game_id, chose_b),
  pca_scores
)

scree_plot <- ggplot(
  explained_variance,
  aes(x = component, y = proportion)
) +
  geom_col() +
  labs(
    x = "Principal Component",
    y = "Proportion of Variance",
    title = "PCA Explained Variance"
  )

write.csv(explained_variance, "data/processed/pca_explained_variance.csv",
  row.names = FALSE)

write.csv(loadings, "data/processed/pca_loadings.csv",
  row.names = FALSE)

data.table::fwrite(pca_scores, "data/processed/pca_scores.csv")

cat("PCA completed.\n")
cat("Features:", length(pca_features), "\n")
cat("Observations:", nrow(pca_scores), "\n")