# World Bank Cartographic Visualization
# Choropleth maps of GDP, life expectancy, internet access, and income classification

library(dplyr)
library(ggplot2)
library(maps)

data <- readRDS("data/cleaned_data.rds")

# Align WDI country names to maps package names
name_map <- c(
  "United States"        = "USA",
  "United Kingdom"       = "UK",
  "Korea, Rep."          = "South Korea",
  "Russian Federation"   = "Russia",
  "Congo, Dem. Rep."     = "Democratic Republic of the Congo",
  "Congo, Rep."          = "Republic of Congo",
  "Egypt, Arab Rep."     = "Egypt",
  "Iran, Islamic Rep."   = "Iran",
  "Syrian Arab Republic" = "Syria",
  "Venezuela, RB"        = "Venezuela",
  "Yemen, Rep."          = "Yemen",
  "Lao PDR"              = "Laos",
  "Kyrgyz Republic"      = "Kyrgyzstan",
  "Slovak Republic"      = "Slovakia",
  "Czechia"              = "Czech Republic"
)

data <- data %>%
  mutate(map_name = ifelse(country %in% names(name_map), name_map[country], country))

world  <- map_data("world")
merged <- left_join(world, data, by = c("region" = "map_name"))
merged$income_group <- factor(merged$income_group,
  levels = c("Low income", "Lower middle income", "Upper middle income", "High income"))

theme_map <- theme_void() +
  theme(legend.position = "right",
        plot.title    = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "grey40"))

# 1. GDP per capita choropleth (log scale)
p_gdp <- ggplot(merged, aes(long, lat, group = group, fill = log10(gdp_per_capita))) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey85",
                       name = "GDP/capita\n(log₁₀ USD)") +
  theme_map +
  labs(title    = "World GDP per Capita (2021) — World Bank WDI",
       subtitle = paste0(nrow(data), " countries · Source: World Bank Development Indicators API"))
ggsave("output/11_world_gdp_choropleth.png", p_gdp, width = 14, height = 7, dpi = 150)

# 2. Life expectancy choropleth
p_life <- ggplot(merged, aes(long, lat, group = group, fill = life_expectancy)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_gradient2(low = "#d73027", mid = "#ffffbf", high = "#1a9641",
                       midpoint = 70, na.value = "grey85",
                       name = "Life Exp\n(years)") +
  theme_map +
  labs(title    = "Life Expectancy by Country (2021)",
       subtitle = "Midpoint = 70 years · Red < 70 · Green > 70")
ggsave("output/12_world_lifeexp_choropleth.png", p_life, width = 14, height = 7, dpi = 150)

# 3. Internet access choropleth
p_inet <- ggplot(merged, aes(long, lat, group = group, fill = internet_users)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "cividis", na.value = "grey85",
                       name = "Internet\nUsers (%)") +
  theme_map +
  labs(title    = "Internet Access by Country (2021)",
       subtitle = "High income avg ~87% vs Low income avg ~21% — 66 pp digital divide")
ggsave("output/13_world_internet_choropleth.png", p_inet, width = 14, height = 7, dpi = 150)

# 4. World Bank income classification map
p_inc <- ggplot(merged, aes(long, lat, group = group, fill = income_group)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  scale_fill_manual(
    values   = c("Low income"          = "#d73027",
                 "Lower middle income" = "#fc8d59",
                 "Upper middle income" = "#4575b4",
                 "High income"         = "#1a9641"),
    na.value = "grey85",
    name     = "Income Group") +
  theme_map +
  labs(title    = "World Bank Income Classification (2021)",
       subtitle = "4 income groups across 120 countries in 5 regions")
ggsave("output/14_world_income_groups.png", p_inc, width = 14, height = 7, dpi = 150)

# 5. Regional summary bar chart (GDP + internet color)
region_summary <- data %>%
  group_by(region) %>%
  summarise(avg_gdp      = mean(gdp_per_capita, na.rm = TRUE),
            avg_internet = mean(internet_users,  na.rm = TRUE),
            n_countries  = n(), .groups = "drop") %>%
  arrange(desc(avg_gdp))

p_bar <- ggplot(region_summary,
                aes(x = reorder(region, avg_gdp), y = avg_gdp, fill = avg_internet)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "cividis", name = "Avg Internet\nAccess (%)") +
  theme_minimal() +
  labs(title    = "Average GDP per Capita by Region (2021)",
       subtitle = "Bar color = average internet access (%)",
       x = "", y = "Average GDP per Capita (USD)")
ggsave("output/15_regional_gdp_bar.png", p_bar, width = 10, height = 5, dpi = 150)

message("✓ Cartographic visualizations complete — 5 maps saved to output/")
message("  11_world_gdp_choropleth.png    - GDP per capita (log scale)")
message("  12_world_lifeexp_choropleth.png - Life expectancy diverging palette")
message("  13_world_internet_choropleth.png - Internet access (digital divide)")
message("  14_world_income_groups.png     - World Bank 4-tier income classification")
message("  15_regional_gdp_bar.png        - Regional GDP summary")
message("\nData: ", nrow(data), " countries across ", length(unique(data$region)), " regions")
