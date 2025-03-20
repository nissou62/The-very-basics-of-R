# Load necessary libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(janitor)
library(reshape2)
library(tm)
library(wordcloud)

> setwd("~/Documents/Butterflyproject/butterfly.R")
# Read the dataset
file_path <- "Consumer_Complaints 2.csv"
data <- read_csv(file_path)
# Generate Charts
# Bar chart of complaints by product
ggplot(data, aes(x = product)) +
  geom_bar(fill = "blue") +
  theme_minimal() +
  labs(title = "Number of Complaints by Product", x = "Product", y = "Count")

# Scatter plot of complaints by product and company
complaints_df <- data %>% count(product, company)

ggplot(complaints_df, aes(x = company, y = product, size = n, color = n)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Scatter Plot of Complaints by Product and Company", x = "Company", y = "Product", size = "Count")

# Line chart of complaints over time
data$date_received <- as.Date(data$date_received, format = "%m/%d/%Y")
data_time_series <- data %>% count(date_received)

ggplot(data_time_series, aes(x = date_received, y = n)) +
  geom_line(color = "red") +
  theme_minimal() +
  labs(title = "Complaints Over Time", x = "Date", y = "Count")
