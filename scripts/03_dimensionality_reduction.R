# World Bank PCA - Dimensionality Reduction

library(dplyr)

data     <- readRDS("data/cleaned_data.rds")
data_norm <- readRDS("data/cleaned_data_normalized.rds")

pca_cols <- c("log_gdp", "life_expectancy", "log_infant_mort", "literacy_rate",
              "internet_users", "electricity_access", "log_co2",
              "urban_population", "health_expenditure",
              "education_expenditure", "unemployment_rate", "gini_coefficient")

pca_input <- data_norm %>% select(all_of(pca_cols))

message("Input data: ", nrow(data), " countries x ", length(pca_cols), " indicators")

pca_result <- prcomp(pca_input, center = FALSE, scale. = FALSE)

explained_var  <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_var <- cumsum(explained_var)

message("\nPCA Variance Explained:")
message("  PC1: ", round(explained_var[1] * 100, 1), "%  (Development axis)")
message("  PC2: ", round(explained_var[2] * 100, 1), "%  (CO2 / Industrial axis)")
message("  PC3: ", round(explained_var[3] * 100, 1), "%  (Social spending axis)")
message("  PC1+PC2: ", round(cumulative_var[2] * 100, 1), "% total")
message("  PC1+PC2+PC3: ", round(cumulative_var[3] * 100, 1), "% total")

# PCA scores with metadata
pca_scores <- as.data.frame(pca_result$x[, 1:4])
pca_scores <- cbind(data[, c("country", "region", "income_group")], pca_scores)

# Loadings table
loadings <- pca_result$rotation[, 1:3]
loadings_df <- data.frame(
  indicator = rownames(loadings),
  PC1 = round(loadings[, 1], 4),
  PC2 = round(loadings[, 2], 4),
  PC3 = round(loadings[, 3], 4)
) %>% mutate(importance = abs(PC1) + abs(PC2)) %>%
  arrange(desc(importance))

message("\nTop indicator contributions:")
print(loadings_df[, c("indicator", "PC1", "PC2", "importance")])

# Variance table
variance_df <- data.frame(
  component         = paste0("PC", 1:length(explained_var)),
  variance_explained = round(explained_var * 100, 2),
  cumulative_pct    = round(cumulative_var * 100, 2)
)

saveRDS(pca_result, "output/pca_result.rds")
saveRDS(pca_scores, "output/pca_scores.rds")
write.csv(pca_scores,   "output/pca_scores.csv",   row.names = FALSE)
write.csv(loadings_df,  "output/pca_loadings.csv",  row.names = FALSE)
write.csv(variance_df,  "output/pca_variance.csv",  row.names = FALSE)

message("\n✓ PCA complete")
message("  PC1 = Development axis (income, health, internet, electricity)")
message("  PC2 = CO2/Industrial axis (separates Gulf states, China)")
message("  Output: pca_scores.csv, pca_loadings.csv, pca_variance.csv")
