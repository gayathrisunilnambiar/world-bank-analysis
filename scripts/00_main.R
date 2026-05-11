# World Bank Development Indicators Analysis Pipeline
# Run this script to execute the entire analysis workflow

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

message("Starting World Bank Development Indicators Analysis Pipeline...\n")

message("========== STEP 1: COLLECT DATA ==========")
source("scripts/01_collect_data.R")

message("\n========== STEP 2: CLEAN DATA ==========")
source("scripts/02_clean_data.R")

message("\n========== STEP 3: DIMENSIONALITY REDUCTION (PCA) ==========")
source("scripts/03_dimensionality_reduction.R")

message("\n========== STEP 4: CREATE VISUALIZATIONS ==========")
source("scripts/04_visualize_results.R")

message("\n========== STEP 5: CLUSTERING & INSIGHTS ==========")
source("scripts/05_clustering_insights.R")

message("\n========== STEP 6: ASSOCIATION ANALYSIS ==========")
source("scripts/06_association_rules.R")

message("\n========== STEP 7: ACCURACY EVALUATION ==========")
source("scripts/07_accuracy_metrics.R")

message("\n========== STEP 8: POLICY RECOMMENDATIONS ==========")
source("scripts/08_policy_recommendations.R")

message("\n========== STEP 9: TIME SERIES FORECASTING ==========")
source("scripts/09_time_series_model.R")

message("\n", strrep("=", 55))
message("COMPLETE WORLD BANK ANALYSIS PIPELINE FINISHED!")
message(strrep("=", 55))
message("\n Output files created:")
message("   Visualizations: 6 PNG charts")
message("   Analysis Results: 20+ CSV files")
message("   Reports: 4 text summaries")
message("")
