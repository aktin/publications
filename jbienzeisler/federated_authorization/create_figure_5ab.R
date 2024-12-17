# -------------------------------------------------------------------------
# Script Title: Figure 5a and 5b - Growth of the AKTIN Infrastructure
# Author: Jonas Bienzeisler
# Email: jbienzeisler@ukaachen.de
# Affiliation: Institute of Medical Informatics, Medical Faculty of the RWTH Aachen University
# 
# Description:
# This R script calculates and visualizes the data for Figure 5a and 5b 
# from the manuscript titled:
# "Pioneering Federated Data Access for a Learning Healthcare System: 
# Implementation Report of the Federated Data Access Authorization System 
# of the German National Emergency Department Data Registry"
#
# Tasks Performed:
# - Reads input dataset (CSV file) related to query broker data.
# - Prepares and summarizes data for connected nodes and accessible EHR records.
# - Manually integrates `total_ed_cases` for each year.
# - Generates visualizations for:
#    1) Number of active ED nodes connected to the AKTIN infrastructure (Figure 5a).
#    2) Total accessible EHR records (Figure 5b).
#
# Input Data:
# 1. "data_analysis.csv" - Contains query broker data.
#    Required Columns: 
#       - request_id (anonymized), node_id (anonymized), last_status, 
#         time_until_queued, time_until_rejection, processing_time, 
#         time_until_completed, year, automatic_rule
#
# Output:
# - Figure 5a: Active ED nodes per year.
# - Figure 5b: Accessible EHR records (in millions) per year.
#
# Notes:
# - `request_id` and `node_id` are anonymized as up-counting integers.
# - `time_until_rejection` and `time_until_completed` are in days.
# - `processing_time` is in seconds.
#
# Dependencies:
# - R libraries: ggplot2, ggtext, grid, patchwork
#
# Last Modified: 2024-12-17
# -------------------------------------------------------------------------

# Load necessary libraries for plotting and text formatting
library(ggplot2)
library(ggtext)
library(grid)
library(patchwork)

# -----------------------------------
# Load Data
# Tasks:
# - Load and prepare the data for plotting
# -----------------------------------

# Load the main dataset
file_path <- "data_analysis.csv"

# Use read.csv to read in the CSV file
request_node_status <- read.csv(file_path, stringsAsFactors = TRUE)

# -----------------------------------
# Calculate Summary Statistics for Query Broker Data
# -----------------------------------

# Summarize the data by year for active ED nodes and requests
query_summary <- aggregate(
  cbind(ED_nodes = as.integer(request_node_status$node_id), 
        requests = as.integer(request_node_status$request_id)) ~ year, 
  data = request_node_status, 
  FUN = function(x) length(unique(x))
)

# Add calculations for individual and periodic (automatic) queries
query_summary$Individual_requests <- aggregate(
  request_node_status$request_id ~ request_node_status$year, 
  data = request_node_status[!request_node_status$automatic_rule,], 
  FUN = function(x) length(unique(x))
)$`request_node_status$request_id`

query_summary$Serial_requests <- aggregate(
  request_node_status$request_id ~ request_node_status$year, 
  data = request_node_status[request_node_status$automatic_rule,], 
  FUN = function(x) length(unique(x))
)$`request_node_status$request_id`

# -----------------------------------
# Manually Set Total ED Cases
# -----------------------------------

# Create a new column for total ED cases and set values manually
query_summary$total_ed_cases <- NA  # Initialize column

# Assign manual values for total ED cases
query_summary$total_ed_cases[query_summary$year == "2017"] <- 425229
query_summary$total_ed_cases[query_summary$year == "2018"] <- 593097
query_summary$total_ed_cases[query_summary$year == "2019"] <- 825815
query_summary$total_ed_cases[query_summary$year == "2020"] <- 754537
query_summary$total_ed_cases[query_summary$year == "2021"] <- 1018809
query_summary$total_ed_cases[query_summary$year == "2022"] <- 1523266
query_summary$total_ed_cases[query_summary$year == "2023"] <- 1570503
query_summary$total_ed_cases[query_summary$year == "2024"] <- 1271382

# Filter the summary data for years starting from 2017
filtered_summary_ed_nodes <- query_summary[query_summary$year >= 2017, ]


# -----------------------------------
# Create Plots for Data Visualization for figure 3 in script
# -----------------------------------

# Define colors
BLUE <- "#5B9BB9"
RED <- "#5B9BB9"

# Create the first plot for connected ED Nodes (2017 onwards)
plot1 <- ggplot(filtered_summary_ed_nodes, aes(x = as.integer(as.character(year)), y = ED_nodes)) +
  geom_line(color = BLUE, size = 2.4) +
  geom_point(size = 5, pch = 21, color = "white", fill = BLUE, stroke = 1) +
  scale_color_manual(values = BLUE) +
  scale_fill_manual(values = BLUE) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(2016.5, 2024.5), expand = c(0, 0), breaks = seq(2017, 2024, 1)) +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, by = 5), expand = c(0, 0)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
        axis.ticks.length.y = unit(0, "mm"),
        axis.ticks.length.x = unit(2, "mm"),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "black"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "Econ Sans Cnd", size = 16)) +
  geom_text(data = data.frame(x = 2024, y = seq(0, 65, by = 5)),
            aes(x, y, label = y), hjust = 1, vjust = 0, nudge_y = 65 * 0.01, family = "Econ Sans Cnd", size = 6) +
  labs(title = "**a) Active ED Nodes,** per Year", subtitle = "") +
  theme(plot.title = element_markdown(family = "Econ Sans Cnd", size = 18))

# Create the second plot for total ED cases (2017 onwards)
plot2 <- ggplot(filtered_summary_ed_nodes, aes(x = as.integer(as.character(year)), y = total_ed_cases / 1e6)) +
  geom_line(color = RED, size = 2.4) +
  geom_point(size = 5, pch = 21, color = "white", fill = RED, stroke = 1) +
  scale_color_manual(values = RED) +
  scale_fill_manual(values = RED) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(2016.5, 2023.5), expand = c(0, 0), breaks = seq(2017, 2023, 1)) +
  scale_y_continuous(limits = c(0, 1.8), breaks = seq(0, 1.8, by = 0.2), expand = c(0, 0)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
        axis.ticks.length.y = unit(0, "mm"),
        axis.ticks.length.x = unit(2, "mm"),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "black"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = "Econ Sans Cnd", size = 16)) +
  geom_text(data = data.frame(x = 2023, y = seq(0, 1.6, by = 0.2)),
            aes(x, y, label = y), hjust = 1, vjust = 0, nudge_y = 1.6 * 0.01, family = "Econ Sans Cnd", size = 6) +
  labs(title = "**b) Accessible Records,** per Year in Millions*", subtitle = "") +
  theme(plot.title = element_markdown(family = "Econ Sans Cnd", size = 18))

# Adjust margins for both plots
plot1 <- plot1 + theme(plot.margin = margin(0, 0.05, 0, 0, "npc"))
plot2 <- plot2 + theme(plot.margin = margin(0, 0, 0.05, 0, "npc"))

# Combine the two plots using patchwork
combined_plot <- plot1 | plot2

# Add a title and subtitle to the combined plot
title_theme <- theme(
  plot.title = element_text(family = "Econ Sans Cnd", face = "bold", size = 22, margin = margin(1, 0, 1, 0, "cm")),
  plot.subtitle = element_text(family = "Econ Sans Cnd", size = 20, margin = margin(0.6, 0, 0.6, 0, "cm"))
)
combined_plot_with_title <- combined_plot + plot_annotation(
  title = "Growth of the AKTIN Infrastructure",
  theme = title_theme
)
combined_plot_with_title

# Add additional elements to the plot (line and rectangle on top)
grid.lines(x = c(0, 1), y = 1, gp = gpar(col = "#44748B", lwd = 4))
grid.rect(x = 0, y = 1, width = 0.05, height = 0.025, just = c("left", "top"), gp = gpar(fill = "#44748B", lwd = 0))

# Add third caption at the bottom-right
grid.text("*Based on Data voluntarily provided from 56 ED Nodes", x = 0.995, y = 0.005, just = c("right", "bottom"), 
          gp = gpar(col = "grey50", fontsize = 16, fontfamily = "Econ Sans Cnd"))

