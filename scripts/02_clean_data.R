# World Bank Data Cleaning & Normalization

library(dplyr)

data <- readRDS("data/raw_data.rds")

indicator_cols <- c("gdp_per_capita", "life_expectancy", "infant_mortality",
                    "literacy_rate", "internet_users", "electricity_access",
                    "co2_emissions", "urban_population", "health_expenditure",
                    "education_expenditure", "unemployment_rate", "gini_coefficient")

message("Original data shape: ", nrow(data), " rows, ", ncol(data), " columns")

# Remove duplicates
data <- data %>% distinct(country, .keep_all = TRUE)
message("✓ Duplicates removed")

# Handle missing values
missing_count <- sum(is.na(data[, indicator_cols]))
if (missing_count > 0) {
  data <- data %>% filter(rowSums(is.na(.[, indicator_cols])) < 4)
  data[, indicator_cols] <- lapply(data[, indicator_cols], function(x) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
    x
  })
  message("✓ Missing values imputed: ", missing_count)
} else {
  message("✓ No missing values found")
}

message("Data shape after cleaning: ", nrow(data), " rows, ", ncol(data), " columns")

# Log-transform skewed indicators before normalization
data$log_gdp        <- log10(data$gdp_per_capita)
data$log_co2        <- log10(pmax(data$co2_emissions, 0.01))
data$log_infant_mort <- log10(pmax(data$infant_mortality, 0.5))

transform_cols <- c("log_gdp", "life_expectancy", "log_infant_mort", "literacy_rate",
                    "internet_users", "electricity_access", "log_co2",
                    "urban_population", "health_expenditure",
                    "education_expenditure", "unemployment_rate", "gini_coefficient")

# Z-score normalization on log-transformed indicators
data_normalized <- data
data_normalized[, transform_cols] <- scale(data[, transform_cols])

saveRDS(data, "data/cleaned_data.rds")
saveRDS(data_normalized, "data/cleaned_data_normalized.rds")

message("✓ Data cleaning complete")
message("  Skewed indicators log-transformed (gdp, co2, infant_mortality)")
message("  All indicators z-score normalized")
message("  Files saved: cleaned_data.rds, cleaned_data_normalized.rds")
