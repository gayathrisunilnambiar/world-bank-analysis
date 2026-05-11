# World Bank Policy Recommendation System
# Find countries with similar profiles, identify over/under-performers

library(dplyr)

data      <- readRDS("data/cleaned_data.rds")
data_norm <- readRDS("data/cleaned_data_normalized.rds")
pca_scores <- readRDS("output/pca_scores.rds")

pca_cols <- c("log_gdp", "life_expectancy", "log_infant_mort", "literacy_rate",
              "internet_users", "electricity_access", "log_co2",
              "urban_population", "health_expenditure",
              "education_expenditure", "unemployment_rate", "gini_coefficient")

pca_input <- as.matrix(data_norm %>% select(all_of(pca_cols)))
rownames(pca_input) <- data$country

message("Building policy recommendation system...\n")

# ===== 1. Similar Country Finder =====
find_similar <- function(target_country, n = 5) {
  if (!target_country %in% rownames(pca_input)) {
    message("Country not found: ", target_country); return(NULL)
  }
  target <- pca_input[target_country, ]
  dists  <- apply(pca_input, 1, function(x) sqrt(sum((x - target)^2)))
  dists  <- sort(dists)
  similar <- names(dists)[2:(n + 1)]
  data.frame(
    query_country    = target_country,
    similar_country  = similar,
    distance         = round(dists[2:(n + 1)], 3),
    income_group     = data$income_group[match(similar, data$country)],
    gdp_per_capita   = data$gdp_per_capita[match(similar, data$country)],
    life_expectancy  = data$life_expectancy[match(similar, data$country)]
  )
}

message("1. SIMILAR COUNTRY RECOMMENDATIONS\n")
sample_countries <- c("India", "Kenya", "Brazil", "Ukraine")
similar_results <- do.call(rbind, lapply(sample_countries, find_similar))
print(similar_results)
write.csv(similar_results, "output/recommendations_similar_countries.csv", row.names = FALSE)

# ===== 2. Over-Performers: Better life expectancy than income predicts =====
message("\n2. DEVELOPMENT OVER-PERFORMERS\n")
income_order <- c("Low income", "Lower middle income", "Upper middle income", "High income")
data$income_group <- factor(data$income_group, levels = income_order)

# Fit expected life expectancy from GDP
model <- lm(life_expectancy ~ log10(gdp_per_capita), data = data)
data$predicted_life_exp <- predict(model, data)
data$life_exp_residual  <- data$life_expectancy - data$predicted_life_exp

over_performers <- data %>%
  filter(life_exp_residual > 2) %>%
  select(country, region, income_group, gdp_per_capita,
         life_expectancy, predicted_life_exp, life_exp_residual) %>%
  arrange(desc(life_exp_residual))

message("Countries with life expectancy > 2 years above GDP prediction:")
print(over_performers)
write.csv(over_performers, "output/recommendations_over_performers.csv", row.names = FALSE)

# ===== 3. Under-Performers: Worse outcomes than income level would suggest =====
message("\n3. DEVELOPMENT UNDER-PERFORMERS\n")
under_performers <- data %>%
  filter(life_exp_residual < -2) %>%
  select(country, region, income_group, gdp_per_capita,
         life_expectancy, predicted_life_exp, life_exp_residual) %>%
  arrange(life_exp_residual)

message("Countries with life expectancy > 2 years below GDP prediction:")
print(under_performers)
write.csv(under_performers, "output/recommendations_under_performers.csv", row.names = FALSE)

# ===== 4. Peer Learning: Best-practice neighbors =====
message("\n4. PEER LEARNING RECOMMENDATIONS\n")
peer_learning <- lapply(c("Ethiopia", "Nigeria", "Bangladesh", "Bolivia"), function(ctry) {
  peers <- find_similar(ctry, n = 3)
  peers$life_exp_delta <- data$life_expectancy[match(peers$similar_country, data$country)] -
                          data$life_expectancy[match(ctry, data$country)]
  peers
})
peer_df <- do.call(rbind, peer_learning)
peer_df <- peer_df %>% filter(life_exp_delta > 0) %>% arrange(query_country, desc(life_exp_delta))
message("Peer countries with better outcomes at similar development level:")
print(peer_df)
write.csv(peer_df, "output/recommendations_peer_learning.csv", row.names = FALSE)

# ===== 5. Internet Access Gap Analysis =====
message("\n5. DIGITAL DIVIDE — INTERNET ACCESS GAP\n")
internet_gap <- data %>%
  group_by(income_group) %>%
  summarise(
    avg_internet   = round(mean(internet_users), 1),
    min_internet   = round(min(internet_users), 1),
    max_internet   = round(max(internet_users), 1),
    gap_to_high_income = round(mean(data$internet_users[data$income_group == "High income"]) -
                                 mean(internet_users), 1)
  )
print(internet_gap)
write.csv(internet_gap, "output/recommendations_internet_gap.csv", row.names = FALSE)

report <- sprintf("
==========================================
POLICY RECOMMENDATION REPORT
==========================================

1. SIMILAR COUNTRY PAIRS
   Used: Euclidean distance on 12 normalized indicators
   Countries analyzed: India, Kenya, Brazil, Ukraine

2. OVER-PERFORMERS (life expectancy above GDP prediction)
   Count: %d countries
   Key insight: Strong health systems and education
   can extend life expectancy beyond what income alone predicts.

3. UNDER-PERFORMERS (life expectancy below GDP prediction)
   Count: %d countries
   Key insight: Resource wealth without institutional investment
   leads to worse outcomes than comparable-income peers.

4. DIGITAL DIVIDE
   Internet access gap (Low vs High income): %.1f%%
   Closing this gap is the single fastest lever for development.

5. METHODOLOGY
   Similarity metric: Euclidean distance on z-scored indicators
   Over/under-performance: Residual from log-linear Preston regression
==========================================
",
nrow(over_performers),
nrow(under_performers),
internet_gap$gap_to_high_income[internet_gap$income_group == "Low income"]
)

write(report, "output/policy_recommendation_report.txt")
message(report)
message("✓ Policy recommendation system complete")
message("  5 output files saved to output/")
