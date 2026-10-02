# ============================================================
# Human Decision Intelligence
# 01_data_loading.R
#
# Purpose:
#   Load the raw CPC18 dataset without
#   modifying the source files.
# ============================================================

# Packages
library(data.table)
library(janitor)


# Project paths
cpc18_path <- file.path("data", "raw", "CPC18.csv")


# Validate raw file
if (!file.exists(cpc18_path)) {
  stop(paste0("CPC18 raw file not found: ", cpc18_path))
}


# Load raw dataset
cpc18_raw <- fread(cpc18_path, na.strings = c("", "NA", "N/A"))


# Standardize column names for working copies
# The original CSV file remain completely unchanged.

cpc18_raw <- clean_names(cpc18_raw)


# Confirmation
print("Raw dataset loaded successfully.")

cat("CPC18:", nrow(cpc18_raw), "rows x", ncol(cpc18_raw), "columns\n")