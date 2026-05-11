# World Bank Analysis Accuracy & Evaluation Metrics

library(dplyr)

message("Computing accuracy and evaluation metrics...\n")

data      <- readRDS("data/cleaned_data.rds")
data_norm <- readRDS("data/cleaned_data_normalized.rds")
pca_result <- readRDS("output/pca_result.rds")

pca_cols <- c("log_gdp", "life_expectancy", "log_infant_mort", "literacy_rate",
              "internet_users", "electricity_access", "log_co2",
              "urban_population", "health_expenditure",
              "education_expenditure", "unemployment_rate", "gini_coefficient")

pca_input   <- as.matrix(data_norm %>% select(all_of(pca_cols)))

# ===== 1. PCA Variance Explained =====
message("1. PCA RECONSTRUCTION ACCURACY\n")
explained_var  <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_var <- cumsum(explained_var)

pca_df <- data.frame(
  component              = paste0("PC", 1:12),
  variance_pct           = round(explained_var * 100, 2),
  cumulative_pct         = round(cumulative_var * 100, 2),
  reconstruction_error   = round(1 - cumulative_var, 4)
)
print(pca_df[1:6, ])

n_components <- 2
reconstructed <- pca_result$x[, 1:n_components] %*% t(pca_result$rotation[, 1:n_components])
reconstruction_rmse <- sqrt(mean((pca_input - reconstructed)^2))
message("\nPCA Reconstruction RMSE (2 PCs): ", round(reconstruction_rmse, 4))
message("Variance captured by 2 PCs:       ", round(cumulative_var[2] * 100, 1), "%")

# ===== 2. Clustering Quality =====
message("\n2. CLUSTERING QUALITY METRICS\n")

set.seed(42)
km <- kmeans(pca_input, centers = 4, nstart = 30)
clusters <- km$cluster

silhouette_scores <- sapply(seq_len(nrow(pca_input)), function(i) {
  ci  <- clusters[i]
  mem <- which(clusters == ci)
  a_i <- if (length(mem) > 1)
    mean(sqrt(rowSums((pca_input[mem, ] -
      matrix(pca_input[i, ], nrow = length(mem), ncol = ncol(pca_input), byrow = TRUE))^2)))
  else 0
  other_cl <- unique(clusters[clusters != ci])
  b_i <- min(sapply(other_cl, function(j) {
    om <- which(clusters == j)
    mean(sqrt(rowSums((pca_input[om, ] -
      matrix(pca_input[i, ], nrow = length(om), ncol = ncol(pca_input), byrow = TRUE))^2)))
  }))
  (b_i - a_i) / max(a_i, b_i)
})

mean_sil <- mean(silhouette_scores)
message("Silhouette Score: ", round(mean_sil, 4),
        "  (", ifelse(mean_sil > 0.5, "GOOD", ifelse(mean_sil > 0.25, "FAIR", "POOR")), ")")

# Davies-Bouldin Index
db_index <- 0
for (i in 1:4) {
  mi <- which(clusters == i); ci <- colMeans(pca_input[mi, ])
  di <- mean(sqrt(rowSums((pca_input[mi, ] - matrix(ci, nrow=length(mi), ncol=ncol(pca_input), byrow=TRUE))^2)))
  max_r <- 0
  for (j in 1:4) {
    if (i != j) {
      mj <- which(clusters == j); cj <- colMeans(pca_input[mj, ])
      dj <- mean(sqrt(rowSums((pca_input[mj, ] - matrix(cj, nrow=length(mj), ncol=ncol(pca_input), byrow=TRUE))^2)))
      cd <- sqrt(sum((ci - cj)^2))
      max_r <- max(max_r, (di + dj) / max(cd, 0.0001))
    }
  }
  db_index <- db_index + max_r
}
db_index <- db_index / 4
message("Davies-Bouldin Index: ", round(db_index, 4), "  (lower = better separated)")

# ===== 3. Income Group Purity =====
message("\n3. CLUSTER PURITY vs INCOME GROUP CLASSIFICATION\n")
income_order <- c("Low income", "Lower middle income", "Upper middle income", "High income")
data$cluster <- as.factor(clusters)
purity_df <- data %>%
  group_by(cluster, income_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(cluster, desc(count))
print(purity_df)

purity_score <- sum(tapply(purity_df$count, purity_df$cluster, max)) / nrow(data)
message("\nIncome Group Purity Score: ", round(purity_score, 3),
        "  (fraction of countries in dominant income group per cluster)")

# ===== 4. Indicator Variance Contribution =====
message("\n4. FEATURE LOADINGS SUMMARY\n")
loadings <- read.csv("output/pca_loadings.csv")
print(loadings[, c("indicator", "PC1", "PC2", "importance")])

# ===== 5. Overall Report =====
report <- sprintf("
==========================================
WORLD BANK ANALYSIS - ACCURACY REPORT
==========================================

1. PCA MODEL ACCURACY
   PC1 variance explained  : %.2f%%
   PC1+PC2 combined        : %.2f%%
   Reconstruction RMSE (2PC): %.4f
   Status: %s

2. CLUSTERING QUALITY (K=4)
   Silhouette Score        : %.4f  (%s)
   Davies-Bouldin Index    : %.4f
   Income Group Purity     : %.2f%%
   Status: %s

3. DATA COVERAGE
   Total countries         : %d
   Indicators used         : %d
   Regions covered         : %d

4. KEY INSIGHT
   PC1 explains %.1f%% of variance — far higher than random data
   because development indicators are genuinely correlated.
   Clusters align %.0f%% with World Bank income groups.
==========================================
",
round(explained_var[1] * 100, 2),
round(cumulative_var[2] * 100, 2),
reconstruction_rmse,
ifelse(cumulative_var[2] > 0.5, "GOOD (>50%% in 2 PCs)", "MODERATE"),
mean_sil,
ifelse(mean_sil > 0.5, "GOOD", ifelse(mean_sil > 0.25, "FAIR", "POOR")),
db_index,
purity_score * 100,
ifelse(mean_sil > 0.4, "GOOD", "FAIR"),
nrow(data), length(pca_cols),
length(unique(data$region)),
explained_var[1] * 100,
purity_score * 100
)

write(report, "output/accuracy_report.txt")
message(report)

write.csv(pca_df,   "output/pca_variance_explained.csv", row.names = FALSE)
write.csv(purity_df, "output/cluster_purity.csv",        row.names = FALSE)

message("✓ Accuracy analysis complete")
message("  accuracy_report.txt, pca_variance_explained.csv, cluster_purity.csv")
