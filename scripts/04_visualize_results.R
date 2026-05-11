# World Bank Data Visualization

library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)

data       <- readRDS("data/cleaned_data.rds")
pca_scores <- readRDS("output/pca_scores.rds")
loadings   <- read.csv("output/pca_loadings.csv")

income_order <- c("Low income", "Lower middle income", "Upper middle income", "High income")
data$income_group <- factor(data$income_group, levels = income_order)

income_colors <- c(
  "Low income"            = "#d73027",
  "Lower middle income"   = "#fc8d59",
  "Upper middle income"   = "#4575b4",
  "High income"           = "#1a9641"
)

message("Creating visualizations...")

# ===== 1. Key Indicators by Income Group (box plots) =====
indicator_long <- data %>%
  select(income_group, life_expectancy, internet_users,
         electricity_access, literacy_rate) %>%
  pivot_longer(-income_group, names_to = "indicator", values_to = "value") %>%
  mutate(indicator = recode(indicator,
    life_expectancy   = "Life Expectancy (yrs)",
    internet_users    = "Internet Users (%)",
    electricity_access = "Electricity Access (%)",
    literacy_rate     = "Literacy Rate (%)"))

p1 <- ggplot(indicator_long, aes(x = income_group, y = value, fill = income_group)) +
  geom_boxplot(alpha = 0.8) +
  facet_wrap(~indicator, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = income_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "none") +
  labs(title = "Development Indicators by Income Group",
       x = "", y = "Value")

# ===== 2. Correlation Heatmap =====
num_cols <- c("log_gdp", "life_expectancy", "log_infant_mort", "literacy_rate",
              "internet_users", "electricity_access", "log_co2",
              "urban_population", "health_expenditure", "gini_coefficient")
cor_matrix  <- cor(data[, num_cols], use = "complete.obs")
cor_long    <- as.data.frame(as.table(cor_matrix))
nice_labels <- c(log_gdp = "GDP (log)", life_expectancy = "Life Exp",
                 log_infant_mort = "Infant Mort (log)", literacy_rate = "Literacy",
                 internet_users = "Internet", electricity_access = "Electricity",
                 log_co2 = "CO2 (log)", urban_population = "Urban Pop",
                 health_expenditure = "Health Exp", gini_coefficient = "Gini")
cor_long$Var1 <- recode(as.character(cor_long$Var1), !!!nice_labels)
cor_long$Var2 <- recode(as.character(cor_long$Var2), !!!nice_labels)

p2 <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 2.5) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a9641",
                       midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Indicator Correlation Matrix", x = "", y = "", fill = "r")

# ===== 3. PCA Biplot colored by Income Group =====
pca_scores$income_group <- factor(pca_scores$income_group, levels = income_order)
p3 <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = income_group, label = country)) +
  geom_point(alpha = 0.75, size = 2.5) +
  scale_color_manual(values = income_colors) +
  theme_minimal() +
  labs(title = "PCA Biplot: Countries by Development Level",
       subtitle = "PC1 = Development axis  |  PC2 = CO2 / Industrialisation axis",
       x = "PC1", y = "PC2", color = "Income Group") +
  theme(legend.position = "right")

# ===== 4. Feature Loadings on PC1 =====
p4 <- ggplot(loadings, aes(x = reorder(indicator, PC1), y = PC1, fill = PC1 > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#1a9641", "FALSE" = "#d73027"),
                    labels = c("TRUE" = "Positive", "FALSE" = "Negative")) +
  theme_minimal() +
  labs(title = "Indicator Loadings on PC1 (Development Axis)",
       x = "Indicator", y = "Loading Value", fill = "Direction") +
  theme(legend.position = "right")

# ===== 5. GDP vs Life Expectancy (Preston Curve) =====
p5 <- ggplot(data, aes(x = log10(gdp_per_capita), y = life_expectancy,
                       color = income_group, size = urban_population)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = income_colors) +
  scale_size_continuous(range = c(1.5, 6), name = "Urban Pop (%)") +
  theme_minimal() +
  labs(title = "Preston Curve: GDP vs Life Expectancy",
       subtitle = "Bubble size = urban population %",
       x = "GDP per Capita (log10, USD)", y = "Life Expectancy (years)",
       color = "Income Group")

# ===== 6. Regional Averages: Energy-Human Development =====
region_avg <- data %>%
  group_by(region) %>%
  summarise(
    avg_life_exp    = mean(life_expectancy),
    avg_internet    = mean(internet_users),
    avg_electricity = mean(electricity_access),
    n_countries     = n()
  )

p6 <- ggplot(region_avg, aes(x = avg_internet, y = avg_life_exp,
                              color = region, size = n_countries, label = region)) +
  geom_point(alpha = 0.9) +
  scale_size_continuous(range = c(4, 10)) +
  theme_minimal() +
  labs(title = "Internet Access vs Life Expectancy by Region",
       x = "Average Internet Users (%)", y = "Average Life Expectancy (years)",
       color = "Region", size = "Countries") +
  theme(legend.position = "right")

ggsave("output/01_indicators_by_income.png",    p1, width = 11, height = 7)
ggsave("output/02_correlation_heatmap.png",      p2, width = 10, height = 8)
ggsave("output/03_pca_biplot.png",               p3, width = 10, height = 6)
ggsave("output/04_pca_loadings.png",             p4, width = 9,  height = 6)
ggsave("output/05_preston_curve.png",            p5, width = 10, height = 6)
ggsave("output/06_region_internet_lifeexp.png",  p6, width = 10, height = 6)

message("✓ Visualizations saved to output/")
message("  01_indicators_by_income.png  - Box plots by income group")
message("  02_correlation_heatmap.png   - Indicator correlations")
message("  03_pca_biplot.png            - Country positions in PCA space")
message("  04_pca_loadings.png          - What drives PC1 (development axis)")
message("  05_preston_curve.png         - GDP vs Life Expectancy (Preston curve)")
message("  06_region_internet_lifeexp.png - Regional summary scatter")
