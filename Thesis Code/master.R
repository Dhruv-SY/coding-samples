# ==============================================================================
# MASTER R SCRIPT
# Child Marriage, Legal Regimes, and Life Outcomes
# Dhruv Yadav and Pedro Silva
#
# This file runs the R robustness script used in the replication package.
# Set the working directory to the root of this replication package
# (the folder containing master.R) before running.
# ==============================================================================

# ------------------------------------------------------------------------------
# SET WORKING DIRECTORY
# Edit this line to point to your local copy of replication_clean/
# ------------------------------------------------------------------------------
# setwd("C:/path/to/replication_clean")

# ------------------------------------------------------------------------------
# CONFIRM DATA EXISTS
# ------------------------------------------------------------------------------
data_file <- "data/The_11_countries_union_migration_merged.dta"
if (!file.exists(data_file)) {
  stop(
    "Dataset not found: ", data_file, "\n",
    "Place The_11_countries_union_migration_merged.dta in the data/ subfolder."
  )
}

# ------------------------------------------------------------------------------
# RUN SCRIPTS
# ------------------------------------------------------------------------------

# Table 11: Conley spatial HAC SEs at 50, 100, and 200 km cutoffs
source("code/09_conley_se_robustness.R")

# Conley 50 km table and Lehner data-driven cutoff table
source("code/11_lehner_conley.R")

cat("\n")
cat("============================================================\n")
cat(" R scripts completed. Outputs in results/\n")
cat("============================================================\n")
