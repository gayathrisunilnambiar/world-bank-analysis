# World Bank Development Indicators - Data Collection
# Generates realistic sample data for 160 countries
# Indicators have real-world correlation structure so PCA and clustering are meaningful

library(dplyr)

create_wb_sample_data <- function() {
  set.seed(42)

  countries <- data.frame(stringsAsFactors = FALSE,
    country = c(
      # Sub-Saharan Africa (30)
      "Ethiopia", "Nigeria", "South Africa", "Kenya", "Tanzania", "Ghana",
      "Uganda", "Mozambique", "Cameroon", "Ivory Coast", "Zambia", "Zimbabwe",
      "Senegal", "Mali", "Niger", "Rwanda", "Guinea", "Benin", "Togo",
      "Sierra Leone", "Burkina Faso", "DR Congo", "Angola", "Botswana",
      "Malawi", "Madagascar", "Burundi", "Chad", "Gambia", "Eritrea",
      # Middle East & North Africa (17)
      "Egypt", "Morocco", "Algeria", "Tunisia", "Libya", "Saudi Arabia",
      "UAE", "Iran", "Iraq", "Jordan", "Israel", "Lebanon", "Yemen",
      "Oman", "Kuwait", "Qatar", "Bahrain",
      # Europe & Central Asia (31)
      "Germany", "France", "UK", "Italy", "Spain", "Netherlands",
      "Sweden", "Switzerland", "Belgium", "Austria", "Norway", "Denmark",
      "Finland", "Ireland", "Portugal", "Greece", "Poland", "Czech Republic",
      "Hungary", "Romania", "Bulgaria", "Croatia", "Serbia", "Ukraine",
      "Russia", "Belarus", "Albania", "Moldova", "Estonia", "Latvia", "Lithuania",
      # East Asia & Pacific (21)
      "China", "India", "Japan", "South Korea", "Indonesia", "Pakistan",
      "Bangladesh", "Philippines", "Vietnam", "Thailand", "Malaysia",
      "Myanmar", "Cambodia", "Sri Lanka", "Nepal", "Singapore", "Australia",
      "New Zealand", "Kazakhstan", "Afghanistan", "Mongolia",
      # Americas (21)
      "USA", "Canada", "Brazil", "Mexico", "Argentina", "Colombia",
      "Peru", "Chile", "Ecuador", "Bolivia", "Venezuela", "Cuba",
      "Haiti", "Dominican Republic", "Guatemala", "Honduras", "El Salvador",
      "Nicaragua", "Costa Rica", "Panama", "Jamaica"
    ),
    region = c(
      rep("Sub-Saharan Africa", 30),
      rep("Middle East & North Africa", 17),
      rep("Europe & Central Asia", 31),
      rep("East Asia & Pacific", 21),
      rep("Americas", 21)
    ),
    income_group = c(
      # Sub-Saharan Africa
      "Low income", "Lower middle income", "Upper middle income", "Lower middle income",
      "Lower middle income", "Lower middle income", "Low income", "Low income",
      "Lower middle income", "Lower middle income", "Lower middle income", "Lower middle income",
      "Lower middle income", "Low income", "Low income", "Low income", "Low income",
      "Low income", "Low income", "Low income", "Low income", "Low income",
      "Lower middle income", "Upper middle income", "Low income", "Low income",
      "Low income", "Low income", "Low income", "Low income",
      # MENA
      "Lower middle income", "Lower middle income", "Upper middle income", "Lower middle income",
      "Upper middle income", "High income", "High income", "Lower middle income",
      "Upper middle income", "Upper middle income", "High income", "Upper middle income",
      "Low income", "High income", "High income", "High income", "High income",
      # Europe
      "High income", "High income", "High income", "High income", "High income",
      "High income", "High income", "High income", "High income", "High income",
      "High income", "High income", "High income", "High income", "High income",
      "High income", "High income", "High income", "Upper middle income", "Upper middle income",
      "Upper middle income", "High income", "Upper middle income", "Lower middle income",
      "Upper middle income", "Upper middle income", "Upper middle income", "Lower middle income",
      "High income", "High income", "High income",
      # East Asia & Pacific
      "Upper middle income", "Lower middle income", "High income", "High income",
      "Lower middle income", "Lower middle income", "Lower middle income", "Lower middle income",
      "Lower middle income", "Upper middle income", "Upper middle income",
      "Lower middle income", "Lower middle income", "Lower middle income", "Low income",
      "High income", "High income", "High income", "Upper middle income",
      "Low income", "Lower middle income",
      # Americas
      "High income", "High income", "Upper middle income", "Upper middle income",
      "Upper middle income", "Upper middle income", "Upper middle income", "High income",
      "Upper middle income", "Lower middle income", "Upper middle income", "Upper middle income",
      "Low income", "Upper middle income", "Upper middle income", "Lower middle income",
      "Lower middle income", "Lower middle income", "Upper middle income", "Upper middle income",
      "Upper middle income"
    )
  )

  n <- nrow(countries)

  # Development index based on income group (drives most indicator correlations)
  dev_base <- c(
    "High income" = 0.85,
    "Upper middle income" = 0.58,
    "Lower middle income" = 0.33,
    "Low income" = 0.11
  )
  dev_index <- pmin(pmax(
    sapply(countries$income_group, function(ig) dev_base[ig] + rnorm(1, 0, 0.07)),
    0.02), 0.98)

  # CO2 follows Kuznets curve — peaks at upper-middle income, lower at extremes
  co2_base <- c(
    "High income" = 0.60,
    "Upper middle income" = 0.82,
    "Lower middle income" = 0.38,
    "Low income" = 0.09
  )
  co2_index <- pmin(pmax(
    sapply(countries$income_group, function(ig) co2_base[ig] + rnorm(1, 0, 0.11)),
    0.02), 0.98)

  data <- countries %>%
    mutate(
      year = 2021,
      gdp_per_capita      = round(exp(3.2 + 8.2 * dev_index + rnorm(n, 0, 0.35))),
      life_expectancy     = round(pmin(85, 44 + 38 * dev_index + rnorm(n, 0, 2.2)), 1),
      infant_mortality    = round(pmax(2,  110 * (1 - dev_index)^2.8 + rnorm(n, 0, 4)), 1),
      literacy_rate       = round(pmin(99.5, pmax(20, 28 + 70 * dev_index^0.55 + rnorm(n, 0, 5))), 1),
      internet_users      = round(pmin(98, pmax(1, 1.5 + 96 * dev_index^0.75 + rnorm(n, 0, 6))), 1),
      electricity_access  = round(pmin(100, pmax(5, 8 + 92 * dev_index^0.45 + rnorm(n, 0, 4))), 1),
      co2_emissions       = round(pmax(0.1, 0.3 + 19 * co2_index + rnorm(n, 0, 1.4)), 2),
      urban_population    = round(pmin(98, pmax(12, 18 + 68 * dev_index^0.65 + rnorm(n, 0, 8))), 1),
      health_expenditure  = round(pmax(1.5, 2.5 + 8.5 * dev_index + rnorm(n, 0, 1.4)), 1),
      education_expenditure = round(pmax(1.0, 4.2 + rnorm(n, 0, 1.1)), 1),
      unemployment_rate   = round(pmax(1.0, 8.0 + rnorm(n, 0, 5.5)), 1),
      gini_coefficient    = round(pmax(25, pmin(65,
        42 + 20 * (dev_index - 0.55)^2 - 8 * dev_index + rnorm(n, 0, 5.5))), 1)
    )

  return(data)
}

dir.create("output", showWarnings = FALSE)
dir.create("data",   showWarnings = FALSE)

if (file.exists("data/wb_raw.csv")) {
  message("Loading World Bank data from CSV...")
  data <- read.csv("data/wb_raw.csv")
} else {
  message("Generating sample World Bank dataset (160 countries)...")
  data <- create_wb_sample_data()
}

saveRDS(data, "data/raw_data.rds")
write.csv(data, "data/raw_data.csv", row.names = FALSE)

message("✓ Data collection complete")
message("  Countries: ", nrow(data), " | Indicators: ", length(grep("_", names(data), value = TRUE)))
message("  Regions: ", paste(sort(unique(data$region)), collapse = ", "))
message("  Income groups: ", paste(sort(unique(data$income_group)), collapse = ", "))
