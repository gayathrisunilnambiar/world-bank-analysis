# World Bank Country Clustering

library(dplyr)
library(ggplot2)

data      <- readRDS("data/cleaned_data.rds")
data_norm <- readRDS("data/cleaned_data_normalized.rds")
pca_scores <- readRDS("output/pca_scores.rds")

pca_cols <- c("log_gdp", "life_expectancy", "log_infant_mort", "literacy_rate",
              "internet_users", "electricity_access", "log_co2",
              "urban_population", "health_expenditure",
              "education_expenditure", "unemployment_rate", "gini_coefficient")

pca_input <- data_norm %>% select(all_of(pca_cols))

message("Clustering countries by development indicators...")

# Elbow method
inertias <- sapply(1:8, function(k) kmeans(pca_input, centers = k, nstart = 15)$tot.withinss)

optimal_k <- 4  # matches World Bank income group structure
set.seed(42)
km <- kmeans(pca_input, centers = optimal_k, nstart = 30)

data$cluster       <- as.factor(km$cluster)
pca_scores$cluster <- as.factor(km$cluster)

message("✓ K-Means clustering with ", optimal_k, " clusters")

# Cluster profiles
indicator_cols <- c("gdp_per_capita", "life_expectancy", "infant_mortality",
                    "literacy_rate", "internet_users", "electricity_access",
                    "co2_emissions", "urban_population")

cluster_profiles <- data %>%
  group_by(cluster) %>%
  summarise(
    n_countries        = n(),
    avg_gdp            = round(mean(gdp_per_capita)),
    avg_life_exp       = round(mean(life_expectancy), 1),
    avg_infant_mort    = round(mean(infant_mortality), 1),
    avg_internet       = round(mean(internet_users), 1),
    avg_electricity    = round(mean(electricity_access), 1),
    dominant_income    = names(sort(table(income_group), decreasing = TRUE)[1]),
    dominant_region    = names(sort(table(region), decreasing = TRUE)[1])
  ) %>%
  arrange(desc(avg_gdp))

message("\nCluster Profiles (sorted by avg GDP):")
print(cluster_profiles)

# Assess how well clusters map to income groups
cross_tab <- table(data$cluster, data$income_group)
message("\nCluster vs Income Group cross-table:")
print(cross_tab)

# PCA scatter colored by cluster
p_cluster <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = cluster,
                                     shape = income_group, label = country)) +
  geom_point(alpha = 0.75, size = 3) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Country Clusters in PCA Space",
       subtitle = paste(optimal_k, "clusters via K-Means on development indicators"),
       x = "PC1 (Development)", y = "PC2 (CO2 / Industrial)",
       color = "Cluster", shape = "Income Group")

ggsave("output/07_country_clusters.png", p_cluster, width = 11, height = 7)

# Elbow plot
elbow_df <- data.frame(k = 1:8, inertia = inertias)
p_elbow  <- ggplot(elbow_df, aes(x = k, y = inertia)) +
  geom_line(color = "#4575b4") +
  geom_point(size = 3, color = "#4575b4") +
  geom_vline(xintercept = optimal_k, linetype = "dashed", color = "#d73027") +
  theme_minimal() +
  labs(title = "Elbow Method for Optimal K", x = "Number of Clusters (K)",
       y = "Total Within-Cluster Inertia")

ggsave("output/08_elbow_plot.png", p_elbow, width = 7, height = 5)

write.csv(cluster_profiles, "output/cluster_profiles.csv", row.names = FALSE)
write.csv(data %>% select(country, region, income_group, cluster,
                           gdp_per_capita, life_expectancy, internet_users),
          "output/countries_with_clusters.csv", row.names = FALSE)

message("\n✓ Clustering complete")
message("  07_country_clusters.png  - Clusters in PCA space")
message("  08_elbow_plot.png        - Elbow method chart")
message("  cluster_profiles.csv     - Cluster characteristics")
message("  countries_with_clusters.csv")
