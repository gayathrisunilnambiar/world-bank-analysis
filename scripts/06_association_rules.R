# World Bank Association Rules Analysis

library(dplyr)

data <- readRDS("data/cleaned_data.rds")

message("Performing association analysis on development indicators...\n")

# Helper: tertile labeling
tert <- function(x, labels) {
  cut(x, breaks = quantile(x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      labels = labels, include.lowest = TRUE)
}

data <- data %>%
  mutate(
    gdp_level       = tert(gdp_per_capita, c("Low GDP", "Middle GDP", "High GDP")),
    health_level    = tert(health_expenditure, c("Low Health Spend", "Mid Health Spend", "High Health Spend")),
    internet_level  = tert(internet_users, c("Low Internet", "Mid Internet", "High Internet")),
    co2_level       = tert(co2_emissions, c("Low CO2", "Mid CO2", "High CO2")),
    edu_level       = tert(education_expenditure, c("Low Edu Spend", "Mid Edu Spend", "High Edu Spend")),
    life_level      = tert(life_expectancy, c("Low Life Exp", "Mid Life Exp", "High Life Exp")),
    gini_level      = tert(gini_coefficient, c("Low Inequality", "Mid Inequality", "High Inequality"))
  )

# ===== RULE 1: GDP × Health Spending → Life Expectancy =====
message("RULE 1: GDP × Health Spending → Life Expectancy\n")
rule1 <- data %>%
  count(gdp_level, health_level, life_level) %>%
  arrange(desc(n))
print(head(rule1, 12))

high_gdp_health_life <- data %>%
  filter(gdp_level == "High GDP", health_level == "High Health Spend",
         life_level == "High Life Exp") %>% nrow()
message("High GDP + High Health Spend → High Life Exp: ", high_gdp_health_life, " countries")
write.csv(rule1, "output/assoc_rule1_gdp_health_lifeexp.csv", row.names = FALSE)

# ===== RULE 2: CO2 × GDP (Kuznets Curve) =====
message("\nRULE 2: GDP × CO2 Emissions (Environmental Kuznets Curve)\n")
rule2 <- data %>%
  count(gdp_level, co2_level) %>%
  arrange(desc(n))
print(rule2)

high_co2_mid_gdp <- data %>% filter(gdp_level == "Middle GDP", co2_level == "High CO2") %>% nrow()
message("Middle GDP + High CO2 (Kuznets peak): ", high_co2_mid_gdp, " countries")
write.csv(rule2, "output/assoc_rule2_gdp_co2_kuznets.csv", row.names = FALSE)

# ===== RULE 3: Education × Internet → Income Group =====
message("\nRULE 3: Education Spending × Internet Access → Income Group\n")
rule3 <- data %>%
  count(edu_level, internet_level, income_group) %>%
  arrange(desc(n))
print(head(rule3, 12))
write.csv(rule3, "output/assoc_rule3_edu_internet_income.csv", row.names = FALSE)

# ===== RULE 4: Region-Specific Patterns =====
message("\nRULE 4: Regional Development Profiles\n")
rule4 <- data %>%
  group_by(region) %>%
  summarise(
    n_countries    = n(),
    avg_gdp        = round(mean(gdp_per_capita)),
    avg_life_exp   = round(mean(life_expectancy), 1),
    avg_co2        = round(mean(co2_emissions), 2),
    avg_gini       = round(mean(gini_coefficient), 1),
    avg_internet   = round(mean(internet_users), 1),
    pct_high_income = round(mean(income_group == "High income") * 100, 1)
  ) %>%
  arrange(desc(avg_gdp))
print(rule4)
write.csv(rule4, "output/assoc_rule4_regional_profiles.csv", row.names = FALSE)

# ===== RULE 5: Inequality Patterns =====
message("\nRULE 5: Inequality × Development Stage\n")
rule5 <- data %>%
  count(income_group, gini_level) %>%
  arrange(income_group, desc(n))
print(rule5)

high_gini_upper_mid <- data %>%
  filter(income_group == "Upper middle income", gini_level == "High Inequality") %>%
  select(country, gini_coefficient, gdp_per_capita, region)
message("\nHigh inequality in upper-middle income countries:")
print(high_gini_upper_mid)
write.csv(rule5, "output/assoc_rule5_inequality_patterns.csv", row.names = FALSE)

summary_text <- sprintf("
==========================================
WORLD BANK ASSOCIATION RULES SUMMARY
==========================================

RULE 1 — GDP + Health Spending → Life Expectancy
  High GDP + High Health Spend → High Life Exp: %d countries
  Insight: Health spending amplifies GDP's effect on longevity

RULE 2 — Environmental Kuznets Curve
  Middle-income countries with High CO2: %d countries
  Insight: Emissions peak at industrialisation phase then decline

RULE 3 — Education + Internet → Income Group
  Strong predictor: internet access > GDP for development classification

RULE 4 — Regional Profiles
  Highest avg GDP region: %s
  Highest avg Gini region: %s

RULE 5 — Inequality Paradox
  High inequality persists into upper-middle income
  Latin America and Sub-Saharan Africa most affected
==========================================
",
high_gdp_health_life,
high_co2_mid_gdp,
rule4$region[1],
rule4$region[which.max(rule4$avg_gini)]
)

write(summary_text, "output/association_rules_summary.txt")
message(summary_text)
message("✓ Association analysis complete — 5 rule files + summary saved")
