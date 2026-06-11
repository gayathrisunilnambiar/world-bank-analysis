# World Bank Development Indicators - Data Collection
# Fetches real data via the World Bank API (WDI package).

library(dplyr)

if (!requireNamespace("WDI", quietly = TRUE)) {
  message("Installing WDI package...")
  install.packages("WDI", repos = "https://cloud.r-project.org", quiet = TRUE)
}
library(WDI)

# World Bank indicator codes → local column names
WB_INDICATORS <- c(
  gdp_per_capita        = "NY.GDP.PCAP.CD",
  life_expectancy       = "SP.DYN.LE00.IN",
  infant_mortality      = "SP.DYN.IMRT.IN",
  literacy_rate         = "SE.ADT.LITR.ZS",
  internet_users        = "IT.NET.USER.ZS",
  electricity_access    = "EG.ELC.ACCS.ZS",
  co2_emissions         = "EN.ATM.CO2E.PC",
  urban_population      = "SP.URB.TOTL.IN.ZS",
  health_expenditure    = "SH.XPD.CHEX.GD.ZS",
  education_expenditure = "SE.XPD.TOTL.GD.ZS",
  unemployment_rate     = "SL.UEM.TOTL.ZS",
  gini_coefficient      = "SI.POV.GINI"
)

dir.create("output", showWarnings = FALSE)
dir.create("data",   showWarnings = FALSE)

message("Querying World Bank API...")
raw <- WDI(
  country   = "all",
  indicator = WB_INDICATORS,
  start     = 2016,
  end       = 2021,
  extra     = TRUE   # appends region, income group, iso codes
)

# Drop aggregate/regional rows supplied by the API
raw <- raw %>% filter(region != "Aggregates", !is.na(region), region != "")

# Only use indicator columns that the API actually returned
available_indicators <- intersect(names(WB_INDICATORS), names(raw))
missing_indicators   <- setdiff(names(WB_INDICATORS), names(raw))
if (length(missing_indicators) > 0)
  message("  Indicators unavailable from API: ", paste(missing_indicators, collapse = ", "))

# Per country: take the most recent non-NA value for each indicator
data <- raw %>%
  arrange(country, desc(year)) %>%
  group_by(country) %>%
  summarise(
    across(all_of(available_indicators), ~ first(na.omit(.x))),
    region       = first(region),
    income_group = first(income),
    year         = first(year[!is.na(gdp_per_capita)]),
    .groups      = "drop"
  ) %>%
  filter(!is.na(gdp_per_capita)) %>%
  mutate(income_group = case_when(
    grepl("High income", income_group)         ~ "High income",
    income_group == "Upper middle income"      ~ "Upper middle income",
    income_group == "Lower middle income"      ~ "Lower middle income",
    TRUE                                       ~ "Low income"
  ))

saveRDS(data, "data/raw_data.rds")
write.csv(data, "data/raw_data.csv", row.names = FALSE)

message("Data collection complete")
message("  Countries : ", nrow(data))
message("  Indicators: ", sum(names(data) %in% names(WB_INDICATORS)))
message("  Year range: ", min(data$year, na.rm = TRUE), " - ", max(data$year, na.rm = TRUE))
message("  Regions   : ", paste(sort(unique(data$region)), collapse = ", "))
message("  Income groups: ", paste(sort(unique(data$income_group)), collapse = ", "))
