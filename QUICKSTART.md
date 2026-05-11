# Quick Start Guide - World Bank Development Indicators Analysis

## 1. Run Full Analysis

```r
# In RStudio, open scripts/00_main.R and click "Source"
# OR in R console:
setwd("C:/Gayathri/wb-data-analysis")
source("scripts/00_main.R")
```

## 2. Find Your Results

```
output/
├── 01_indicators_by_income.png     ← Box plots by income group
├── 02_correlation_heatmap.png      ← Indicator correlations
├── 03_pca_biplot.png               ← Countries in PCA space ⭐
├── 04_pca_loadings.png             ← What drives the development axis
├── 05_preston_curve.png            ← GDP vs Life Expectancy
├── 06_region_internet_lifeexp.png  ← Regional summary
├── 07_country_clusters.png         ← 4 country clusters
├── 08_elbow_plot.png               ← Optimal K selection
├── 09_life_expectancy_trends.png   ← Time series by income group
├── 10_internet_adoption.png        ← S-curve internet diffusion
├── pca_scores.csv                  ← Country positions on PCs
├── cluster_profiles.csv            ← Cluster descriptions
├── accuracy_report.txt             ← Model evaluation
└── policy_recommendation_report.txt
```

## 3. Key Visualizations Explained

### PCA Biplot (03_pca_biplot.png) - Most Important
- **X-axis (PC1)**: Development axis — explains ~50-60% of variance
- **Y-axis (PC2)**: CO2 / Industrialisation axis
- **Colors**: Income groups (Red=Low, Orange=Lower-mid, Blue=Upper-mid, Green=High)
- Countries cluster by development level naturally

### Preston Curve (05_preston_curve.png)
- Classic economics result: diminishing returns on life expectancy as GDP rises
- Bubble size = urban population
- Points above the curve = over-performers (better health than income predicts)

### Internet Adoption (10_internet_adoption.png)
- Shows S-curve diffusion across income groups
- High income countries near saturation; others catching up

## 4. Why This Analysis is Meaningful

Unlike random simulated data, World Bank indicators have genuine correlations:
- GDP predicts life expectancy (r ≈ 0.8)
- Internet access and electricity are strongly co-linear
- CO2 follows the Environmental Kuznets Curve (peaks at middle income)

This means:
- PCA PC1 explains **50-60%** variance (vs ~13% for random data)
- Clusters clearly map to income groups (purity ~60-70%)
- Association rules capture real development economics

## 5. Key Numbers for Your Report

```r
# PCA variance explained
read.csv("output/pca_variance_explained.csv")

# Cluster profiles
read.csv("output/cluster_profiles.csv")

# Over-performing countries
read.csv("output/recommendations_over_performers.csv")

# Global trends 2000-2022
read.csv("output/ts_global_trends.csv")
```

## 6. Report Structure Suggestion

1. **Introduction** — Why measure development? What are WB indicators?
2. **Data** — 160 countries, 12 indicators, 23 years of panel data
3. **PCA Results** — PC1 as development axis (cite % variance explained)
4. **Clustering** — 4 country groups vs WB income classification
5. **Association Rules** — Kuznets curve, digital divide patterns
6. **Time Series** — Life expectancy convergence, internet diffusion
7. **Policy Implications** — Over/under-performers, peer learning
8. **Conclusion**

## 7. Troubleshooting

**Package not found** → `install.packages(c("dplyr","ggplot2","gridExtra","tidyr"))`
**Output folder empty** → Re-run `source("scripts/00_main.R")` from project root
