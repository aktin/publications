# -------------------------------------------------------------------------
# Script Title: Supplementary Table 3 - Frequency Statistics of Query Data
# Author: Jonas Bienzeisler
# Email: jbienzeisler@ukaachen.de
# Affiliation: Institute of Medical Informatics, Medical Faculty of the RWTH Aachen University
#
# Description:
# This R script generates Supplementary Table 3 for the manuscript titled:
# "Pioneering Federated Data Access for a Learning Healthcare System: 
# Implementation Report of the Federated Data Access Authorization System 
# of the German National Emergency Department Data Registry"
#
# Tasks Performed:
# - Loads query broker metadata from "data_analysis.csv".
# - Cleans and transforms timestamp columns for analysis.
# - Calculates key performance indicators (KPIs) including:
#    1) Time until query completion (days).
#    2) Time until query rejection (days).
#    3) Processing time (seconds).
# - Differentiates between individual and periodic queries.
# - Produces frequency tables with descriptive statistics.
# - Outputs the results as both HTML and Word documents for reporting.
#
# Input Data:
# 1. "data_analysis.csv" - Contains broker query metadata.
#    Required Columns:
#       - year, last_status, automatic_rule, time_until_completed,
#         time_until_rejection, processing_time, retrieved, rejected, completed.
#
# Output:
# - Supplementary Table 3: Frequency statistics for individual and periodic queries.
# - HTML and Word files with summary statistics:
#    - "table2.html"
#    - "table2.doc"
#
# Notes:
# - `time_until_completed`, `time_until_rejection`: Measured in days.
# - `processing_time`: Measured in seconds.
# - Individual queries require manual authorization; periodic queries allow automatic authorization.
#
# Dependencies:
# - R libraries: lubridate, Hmisc, arsenal
#
# Last Modified: 2024-12-17
# -------------------------------------------------------------------------
library(lubridate)
library(Hmisc)
library(arsenal)

# ------------------------------------------------------------------
# Section: Data Loading
# Description: Load the data from data_analysis.csv.
# ------------------------------------------------------------------

# Read in the data from the CSV file
data_path <- "data_analysis.csv"
data <- read.csv(data_path, header = TRUE, sep = ",", na.strings = c("", "NA", "NULL"), stringsAsFactors = FALSE)

# ------------------------------------------------------------------
# Section: Data Transformation and Data Cleaning
# Description: Convert various timestamp columns from string format to datetime objects, and format factors and numbers.
# ------------------------------------------------------------------

# Ensure datetime conversion for all relevant columns
options(digits.secs = 3)
data$retrieved <- ymd_hms(data$retrieved)
data$queued <- ymd_hms(data$queued)
data$processing <- ymd_hms(data$processing)
data$interaction <- ymd_hms(data$interaction)
data$completed <- ymd_hms(data$completed)
data$failed <- ymd_hms(data$failed)
data$rejected <- ymd_hms(data$rejected)
data$deleted <- ymd_hms(data$deleted)
data$last_status <- factor(data$last_status)
data$year <- factor(data$year)


# Ensure all numeric columns are properly formatted
numeric_columns <- c("time_until_queued", "time_until_rejection", "processing_time", "time_until_completed")
data[numeric_columns] <- lapply(data[numeric_columns], as.numeric)

# ------------------------------------------------------------------
# Section: Additional Calculations
# Description: Calculate time until rejection and time until completed.
# ------------------------------------------------------------------

# Calculate Time Until Rejection and Time Until Completed
data$time_until_rejection <- round(as.numeric(difftime(data$rejected, data$retrieved, units = "days")), 1)
data$time_until_completed <- round(as.numeric(difftime(data$completed, data$retrieved, units = "days")), 1)

# ------------------------------------------------------------------
# Section: Frequency Analysis of KPIs
# Description: Calculate frequency statistics and save the results.
# ------------------------------------------------------------------

# Select relevant columns for frequency analysis
tabledata <- data[, c("year", "last_status", "automatic_rule", "time_until_completed", "time_until_rejection", "processing_time")]

# Rename columns for labeling
label(tabledata[["last_status"]]) <- "Last communicated Status"
label(tabledata[["automatic_rule"]]) <- "Request Type"
label(tabledata[["year"]]) <- "Year"
label(tabledata[["time_until_completed"]]) <- "Days until completed"
label(tabledata[["time_until_rejection"]]) <- "Days until rejected"
label(tabledata[["processing_time"]]) <- "Processing Time"

# Calculate Frequency Statistics
table <- tableby(automatic_rule ~ ., data = tabledata, numeric.stats = c("meansd", "medianrange", "q1q3", "iqr"))
table_summary <- summary(table, digits = 1)

# Print and save the table
print(table_summary)
write2html(table_summary, "table2.html")
write2word(table_summary, "table2.doc", title = "Table 2: Analysis of Key Performance Indicators derived from log files of the AKTIN Broker. Individual queries and periodically repeated queries are differentiated. For individual queries, data access has to be authorized individually. For periodic queries, participating emergency departments may omit future authorization and grant automatic data access authorization for future and past queries.")




