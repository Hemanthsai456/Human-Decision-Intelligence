# ============================================================
# Human Decision Intelligence
# 01_data_loading.R
#
# Purpose:
#   Load the raw CPC18 and choices13k datasets without
#   modifying the source files.
# ============================================================

# Packages
library(tidyverse)
library(data.table)
library(janitor)


# Project paths
cpc18_path <- file.path("data", "raw", "CPC18.csv")
choices13k_path <- file.path("data", "raw", "c13k_selections.csv")


# Validate raw files
if (!file.exists(cpc18_path)) {
  stop(paste0("CPC18 raw file not found: ", cpc18_path))
}

if (!file.exists(choices13k_path)) {
  stop(paste0("choices13k raw file not found: ", choices13k_path))
}


# Load raw datasets
cpc18_raw <- fread(cpc18_path, na.strings = c("", "NA", "N/A"))

choices13k_raw <- fread(choices13k_path, na.strings = c("", "NA", "N/A"))


# Standardize column names for working copies
# The original CSV files remain completely unchanged.

cpc18_raw <- clean_names(cpc18_raw)
choices13k_raw <- clean_names(choices13k_raw)


# Confirmation
print("Raw datasets loaded successfully.")

cat("CPC18:", nrow(cpc18_raw), "rows x", ncol(cpc18_raw), "columns\n")

cat("choices13k:", nrow(choices13k_raw), "rows x",ncol(choices13k_raw), "columns\n")