# -------------------------------------------------------------------------
# Script Title: Figure 5c and 5d - Query Completion Time Analysis
# Author: Jonas Bienzeisler
# Email: jbienzeisler@ukaachen.de
# Affiliation: Institute of Medical Informatics, Medical Faculty of the RWTH Aachen University
# 
# Description:
# This R script generates Figure 5c and 5d for the manuscript titled:
# "Pioneering Federated Data Access for a Learning Healthcare System: 
# Implementation Report of the Federated Data Access Authorization System 
# of the German National Emergency Department Data Registry"
#
# Tasks Performed:
# - Loads query broker metadata.
# - Prepares data for Kaplan-Meier survival analysis.
# - Visualizes:
#    1) **Figure 5c**: Query completion time survival curves by year.
#    2) **Figure 5d**: Query response rate with hexbin density plots showing node activity.
# - Adds confidence intervals and legends for completion time analysis.
#
# Input Data:
# 1. "data_analysis.csv" - Contains broker query metadata.
#    Required Columns: 
#       - request_id (anonymized), node_id (anonymized), time_until_completed, 
#         year, automatic_rule
#
# Output:
# - Figure 5c: Query completion survival analysis by year.
# - Figure 5d: Hexbin density plot of unanswered query rates across nodes.
#
# Notes:
# - `request_id` and `node_id` are anonymized as up-counting integers.
# - `time_until_completed` is expressed in days.
# - The survival analysis uses Kaplan-Meier estimation.
#
# Dependencies:
# - R libraries: survival, survminer, ggplot2, patchwork, dplyr, ggtext
#
# Last Modified: 2024-12-17
# -------------------------------------------------------------------------
# Load necessary libraries
library(grid)
library(readr)
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(patchwork)

# ------------------------------------------------------------------
# Section: Data Loading
# Description: Load broker metadata from specified CSV files into R.
# - `file_path`:  Contains communication details between the broker and nodes.
# ------------------------------------------------------------------

# Define the path to the CSV file
file_path <- "data_analysis.csv"

# Read the CSV file with explicit column types to ensure data integrity
query_data <- readr::read_csv(file_path, col_types = cols(
  request_id = col_factor(),               # Assuming request_id is numeric
  node_id = col_factor(),                  # Assuming node_id is numeric
  time_until_completed = col_double(),
  year = col_factor(),                     # Year as factor
  automatic_rule = col_logical(),          # Logical, as defined earlier
))


# ------------------------------------------------------------------
# Section: Data Preparing
# Description: Append df to draw individual and cummulative survival curves
# Select Data for plotting
# Fit the Kaplan Meier Curves
# ------------------------------------------------------------------

# Add a new column 'time_until_event' with the same values as 'time_until_completed'
query_data <- query_data %>%
  dplyr::mutate(time_until_event = time_until_completed)

query_data <- query_data %>%
  dplyr::mutate(event = dplyr::if_else(is.na(time_until_event), 0, 1))

# Duplicate data for 'All Nodes' to append the original data set for plotting
nodes_data <- query_data %>%
  filter(automatic_rule == FALSE) %>%
  select(time_until_event, event) %>%
  mutate(node_id = "Cummulated for All Nodes", year = "Accumulated")

# Append the original data to draw overall years and nodes
appended_data <- query_data %>%
  filter(automatic_rule == FALSE) %>%
  select(node_id, time_until_event, event, year) %>%
  bind_rows(nodes_data)

# Ensure 'node_id' is a factor and includes the "All Nodes" level to plot data from all years
appended_data$node_id <- factor(appended_data$node_id, levels = unique(c(as.character(appended_data$node_id), "Cummulated for All Nodes")))




# Perform Kaplan-Meier survival analysis
surv_obj_nodes <- Surv(time = appended_data$time_until_event, event = appended_data$event)
fit_nodes <- survfit(surv_obj_nodes ~ node_id, data = appended_data)

# Extract the summary of the survival fit
fit_summary <- summary(fit_nodes)

# Create a data frame with the time, survival probability, and strata (node_id)
surv_data <- data.frame(
  time = fit_summary$time,
  surv = fit_summary$surv,
  strata = gsub("node_id=", "", fit_summary$strata)
)

# Filter out the data for all nodes
all_nodes_surv <- surv_data %>% filter(strata == "Cummulated for All Nodes")
individual_nodes_surv <- surv_data %>% filter(strata != "Cummulated for All Nodes")

# Ensure 'year' is a factor and includes the "Accumulated" level
appended_data$year <- factor(appended_data$year, levels = unique(c(as.character(appended_data$year), "Accumulated")))

# Filter data to include only the 'Accumulated' year
filtered_data <- appended_data %>% filter(year == "Accumulated")

# Perform Kaplan-Meier survival analysis on the filtered data
surv_obj_accumulated <- Surv(time = filtered_data$time_until_event, event = filtered_data$event)
fit_accumulated <- survfit(surv_obj_accumulated ~ 1, data = filtered_data)

# ------------------------------------------------------------------
# Section: Data Plotting
# ------------------------------------------------------------------

# Plotting only the 'Accumulated' survival curve in red
g_accumulated_completion <- ggsurvplot(
  fit_accumulated,
  data = filtered_data,
  conf.int = TRUE,
  ggtheme = theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),  # Add grid lines for y-axis ticks
      panel.grid.major.x = element_line(color = "lightgray", size = 0.1),  # Add grid lines for x-axis ticks
      axis.line.x.bottom = element_line(color = "black"),
      axis.line.y = element_line(color = "lightgray"),
      axis.text = element_text(size = 16, family = "Econ Sans Cnd"),
      axis.ticks.length.y = unit(0, "mm"),
      axis.ticks.length.x = unit(2, "mm"),
      axis.title = element_blank(),
      legend.position = "none",
      plot.margin = margin(0, 0.05, 0, 0, "npc"),
      panel.border = element_rect(color = "lightgray", fill = NA, size = 1)
    ),
  xlab = "Days Until Query Completion",
  ylab = "Probability of Query Uncompleted",
  size = 2.4,  # Line thickness for the survival curve
  color = "#74132d",
  break.x.by = 10,
  legend = "none"
) + labs(
  title = "Rate of answered Queries per day from retrieval for different Nodes",
  subtitle = ""
)

# Extract the survival plot without title (to match the hex plot)
surv_plot <- g_accumulated_completion$plot + theme(plot.title = element_blank())


# Create the hexagonal density plot without displaying it
hex_plot <- ggplot() +
  geom_hex(data = individual_nodes_surv, aes(x = time, y = surv), bins = 30) +
  scale_fill_gradient(low = "#f0f0f0", high = "#74132d") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
    axis.line.x.bottom = element_line(color = "black"),
    axis.line.y = element_line(color = "lightgray"),
    axis.text = element_text(size = 16, family = "Econ Sans Cnd"),
    axis.ticks.length.y = unit(0, "mm"),
    axis.ticks.length.x = unit(2, "mm"),
    axis.title = element_blank(),
    legend.position = "none",
    plot.margin = margin(0, 0.05, 0, 0, "npc"),
    panel.border = element_rect(color = "lightgray", fill = NA, size = 1)
  ) +
  xlab("Days Until Query Completion") +
  ylab("Probability of Query Uncompleted") +
  xlim(0, 60) + labs(
    title = "Rate of answered Queries, per day from retrieval for different nodes",
    subtitle = ""
  ) 

# Extract the survival plot data including the confidence intervals
surv_data <- g_accumulated_completion$plot$data

# Combine the hex plot with the survival curve and confidence intervals
combined_plot_hexagon <- ggplot() +
  geom_hex(data = individual_nodes_surv, aes(x = time, y = surv, fill = ..count..), bins = 30) +  # Include fill legend
  scale_fill_gradient(low = "#F0FAFF", high = "#44748B", name = "Density") +  # Continuous gradient for density
  # Continuous gradient for density
  geom_ribbon(data = surv_data, aes(x = time, ymin = lower, ymax = upper, fill = "Confidence Interval"), fill = "black", alpha = 0.2) +  # Confidence interval with legend
  geom_line(data = surv_data, aes(x = time, y = surv, color = "All (CI)"), size = 2) +  # Survival curve with legend
  scale_color_manual(name = "Completion rate", values = c("All (CI)" = "#74132d")) +  # Define line color for the legend
  scale_x_continuous(breaks = c(0, 20, 40, 60), labels = c("0", "20", "40", "60 Days"), limits = c(0, 60)) +  # Set specific x-axis ticks, labels, and limit to 60
  coord_cartesian(clip = 'off') +  # This keeps the scaling and avoids clipping
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),
    axis.line.x.bottom = element_line(color = "black"),
    axis.line.y = element_line(color = "lightgray"),
    axis.text = element_text(size = 16, family = "Econ Sans Cnd"),
    axis.ticks.x = element_line(color = "black"),  # Ensure x-axis ticks are visible
    axis.ticks.length.x = unit(2, "mm"),           # Set the length of x-axis ticks
    axis.ticks.length.y = unit(0, "mm"),           # Remove y-axis ticks
    axis.title = element_blank(),
    legend.position = c(0.85, 0.75),  # Position the legend inside the plot
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # Add a box around the legend
    legend.box = "vertical",  # Combine all legends into one box
    plot.margin = margin(0, 0.05, 0, 0, "npc"),
    panel.border = element_rect(color = "lightgray", fill = NA, size = 1)
  ) +
  labs(
    title = expression(bold("d) Rate of unanswered Queries") * ", Hexbin Density Distribution of Node Activity over days after retrieval"),
    subtitle = ""
  ) +
  guides(
    fill = guide_colorbar(order = 1, title.position = "top", title = expression(bold("Density"))),  # Continuous legend with bold title
    color = guide_legend(order = 2, title = expression(bold("Completion Rate")))  # Discrete legend with bold title
  )




# Extract the survival plot data including the confidence intervals
surv_data <- g_accumulated_completion$plot$data


# Combine the hex plot with the survival curve and confidence intervals
combined_plot_hexagon <- combined_plot_hexagon +
  geom_ribbon(data = surv_data, aes(x = time, ymin = lower, ymax = upper), fill = "black", alpha = 0.2) +  # Confidence interval
  geom_line(data = surv_data, aes(x = time, y = surv), color = "#74132d", size = 2) +  # Survival curve
  scale_x_continuous(breaks = c(0, 20, 40, 60), labels = c("0", "20", "40", "60 Days"), limits = c(0, 60)) +  # Set specific x-axis ticks, labels, and limit to 60
  coord_cartesian(clip = 'off')  # This keeps the scaling and avoids clipping




# Combined plot with hexagon density and survival curves by year
# Filter data for connected ED nodes from 2017 onwards
filtered_summary_ed_nodes <- appended_data %>%
  filter(as.integer(as.character(year)) >= 2017)

# Perform Kaplan-Meier survival analysis for years
surv_obj_combined <- Surv(time = appended_data$time_until_event, event = appended_data$event)
fit_combined <- survfit(surv_obj_combined ~ year, data = appended_data)

# Define colors
palette_colors <- c(grey.colors(length(levels(appended_data$year)) - 1), "#74132d")

# Plotting survival curves by year
g_year_completion <- ggsurvplot(
  fit_combined,
  data = appended_data,
  conf.int = FALSE,
  palette = palette_colors,
  ggtheme = theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(color = "#A8BAC4", size = 0.3),  # Add grid lines for x-axis ticks
      panel.grid.major.x = element_line(color = "lightgray", size = 0.1),  # Add grid lines for y-axis ticks
      axis.line.x.bottom = element_line(color = "black"),
      axis.line.y = element_line(color = "lightgray"),
      axis.text = element_text(size = 16, family = "Econ Sans Cnd"),
      axis.ticks.length.y = unit(0, "mm"),
      axis.ticks.length.x = unit(2, "mm"),
      axis.title = element_blank(),
      break.x.by = 10,
      legend.position = "none",
      plot.margin = margin(0, 0.05, 0, 0, "npc"),
      panel.border = element_rect(color = "lightgray", fill = NA, size = 1)
    ),
  xlab = "Days Until Query Completion",
  ylab = "Probability of Query Uncompleted",
  
  xlim = c(0, 60),
  size = 2.4,  # Increase line thickness to 2.4
  break.x.by = 20,
  legend = "none"
) + labs(
  title = expression(bold("c) Rate of unanswered queries by year") * ",  with 75% completion rate marked over day after retrieval"),
  subtitle = ""
) 

# Extract the survival plot for further customization
surv_plot <- g_year_completion$plot

# Modify the x-axis scale and labels
surv_plot <- surv_plot + 
  scale_x_continuous(breaks = c(0, 20, 40, 60), labels = c("0", "20", "40", "60 Days"))


# Hardcoded q3_values and initial label_positions
q3_values <- data.frame(
  year = factor(c("2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "Accumulated")),
  Q3 = c(9.43, 8, 3.98, 10.7, 12.2, 19.1, 14.6, 16.9, 15.4),
  surv = rep(0.25, 9)  # Set all survival probabilities to 0.25
)

# Define y-values ensuring at least 0.05 apart
label_positions_y <- seq(from = 0.25 + 0.05, by = 0.05, length.out = nrow(q3_values))

# Define x-values for labels
label_positions_x <- q3_values$Q3 + 5

label_positions <- data.frame(
  x = label_positions_x,
  y = label_positions_y
)

# Define custom labels with rounded Q3 values for the legend
custom_labels <- c(
  "2017: 9 Days",
  "2018: 8 Days",
  "2019: 4 Days",
  "2020: 11 Days",
  "2021: 12 Days",
  "2022: 19 Days",
  "2023: 15 Days",
  "2024: 17 Days",
  "All: 15 Days"
)

# Add round bullets at the Q3 for each line with corresponding color and include them in the legend
surv_plot <- surv_plot + 
  geom_point(data = q3_values, aes(x = Q3, y = surv, fill = year), shape = 21, size = 5, color = "white", stroke = 1, show.legend = TRUE) + 
  scale_fill_manual(values = palette_colors[1:9], labels = custom_labels) +  # Limit the palette to the first 9 elements and set custom labels
  guides(color = "none", linetype = "none", fill = guide_legend(override.aes = list(color = "white"), title = "75% Completion Rate")) +  # Remove legend for lines and retain for points, add title
  theme(
    legend.position = c(0.85, 0.75),  # Position the legend
    legend.box.background = element_rect(color = "black", fill = alpha("white", 0.75)),  # Add a background box to the legend
    legend.title = element_text(face = "bold", size = 10)  # Bold and size the legend title
  )





# Combine the plots using patchwork
combined_plot <- (surv_plot | combined_plot_hexagon) + plot_layout(ncol = 2, widths = c(1, 1))

# Title theme
title_theme <- theme(
  plot.title = element_text(
    family = "Econ Sans Cnd", 
    face = "bold",
    size = 22,
    margin = margin(1, 0, 1, 0, "cm")
  ),
  plot.subtitle = element_text(
    family = "Econ Sans Cnd",
    size = 20,
    margin = margin(0.6, 0, 0.6, 0, "cm")
  )
)

# Add main title and subtitle to the combined plot
combined_plot_with_title <- combined_plot + plot_annotation(
  title = "Query completion time from day of retrieval",
  theme = title_theme
)

# Display the combined plot with title
print(combined_plot_with_title)

# Add line on top of the chart
grid.lines(
  x = c(0, 1),
  y = 1,
  gp = gpar(col = "#44748B", lwd = 4)
)

# Add rectangle on top-left
grid.rect(
  x = 0,
  y = 1,
  width = 0.05,
  height = 0.025,
  just = c("left", "top"),
  gp = gpar(fill = "#44748B", lwd = 0)
)


