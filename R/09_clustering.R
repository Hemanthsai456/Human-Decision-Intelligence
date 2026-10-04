# ============================================================
# Human Decision Intelligence
# 09_clustering.R
#
# Purpose:
#   Identify natural groups of decision problems using
#   the PCA representation from Stage 08.
# ============================================================

# Packages
library(dplyr)
library(ggplot2)
library(cluster)

pca_scores <- read.csv("data/processed/pca_scores.csv")

# PCA components used for clustering
pca_vars <- grep("^PC", names(pca_scores), value = TRUE)

pca_data <- pca_scores %>% select(all_of(pca_vars))


# Evaluate candidate cluster counts

set.seed(42)
k_values <- 2:6

silhouette_sample <- pca_data %>% slice_sample(n = min(10000, nrow(pca_data)))

silhouette_dist <- dist(silhouette_sample)

silhouette_results <- data.frame(
  k = k_values,
  silhouette = sapply(k_values, function(k) {
    model <- kmeans(silhouette_sample, centers = k, nstart = 25)
    mean(silhouette(model$cluster, silhouette_dist)[, 3])
  })
)

# Select k with highest average silhouette score
best_k <- silhouette_results$k[which.max(silhouette_results$silhouette)]


# Final K-means model

set.seed(42)

cluster_model <- kmeans(pca_data, centers = best_k, nstart = 50)

cluster_assignments <- pca_scores %>%
  select(subject_id, game_id) %>%
  mutate(cluster = cluster_model$cluster)

cluster_sizes <- cluster_assignments %>% count(cluster, name = "observations")

cluster_centers <- as.data.frame(cluster_model$centers)

cluster_centers$cluster <- seq_len(best_k)


# Plots
dir.create("outputs/clustering", recursive = TRUE, showWarnings = FALSE)
theme_set(theme_minimal(base_size = 12))

# Silhouette plot (selected k highlighted)
silhouette_plot <- ggplot(silhouette_results, aes(x = k, y = silhouette)) +
  geom_line(colour = "grey50") +
  geom_point(size = 2.5) +
  geom_point(
    data = filter(silhouette_results, k == best_k),
    colour = "firebrick", size = 4
  ) +
  scale_x_continuous(breaks = silhouette_results$k) +
  labs(
    x = "Number of clusters (k)",
    y = "Average silhouette score",
    title = "Silhouette Analysis",
    subtitle = paste0("Highest score at k = ", best_k, " (",
                      format(nrow(silhouette_sample), big.mark = ","),
                      "-observation sample)")
  )

ggsave("outputs/clustering/clustering_silhouette.png",
       silhouette_plot, width = 8, height = 5, dpi = 300)

# PCA cluster plot (random sample for readability, centroids labelled)
set.seed(42)
plot_sample <- pca_data %>%
  select(PC1, PC2) %>%
  mutate(cluster = factor(cluster_model$cluster)) %>%
  slice_sample(n = min(20000, nrow(pca_data)))

centroids <- plot_sample %>%
  group_by(cluster) %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")

cluster_plot <- ggplot(plot_sample, aes(x = PC1, y = PC2, colour = cluster)) +
  geom_point(alpha = 0.4, size = 0.7) +
  geom_label(
    data = centroids, aes(label = cluster),
    colour = "black", fill = "white", alpha = 0.85, size = 4,
    show.legend = FALSE
  ) +
  scale_colour_brewer(palette = "Dark2") +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(
    x = "PC1",
    y = "PC2",
    colour = "Cluster",
    title = "Clusters in PCA Space",
    subtitle = paste0(
      "Random sample of ", format(nrow(plot_sample), big.mark = ","), " of ",
      format(nrow(pca_data), big.mark = ","),
      " observations; labels mark cluster centroids"
    )
  )

ggsave("outputs/clustering/clustering_pca.png",
       cluster_plot, width = 8, height = 5, dpi = 300)


# Save outputs

write.csv(silhouette_results, "data/processed/clustering_silhouette.csv",
          row.names = FALSE)

write.csv(cluster_assignments, "data/processed/clustering_assignments.csv",
          row.names = FALSE)

write.csv(cluster_sizes, "data/processed/clustering_sizes.csv",
          row.names = FALSE)

write.csv(cluster_centers, "data/processed/clustering_centers.csv",
          row.names = FALSE)

cat("Clustering completed.\n")
cat("Candidate k values:", paste(k_values, collapse = ", "), "\n")
cat("Selected k:", best_k, "\n")
cat("Observations:", nrow(cluster_assignments), "\n")