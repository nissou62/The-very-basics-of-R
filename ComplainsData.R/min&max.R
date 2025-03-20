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
# Min-Max Scaling
data_scaled <- data %>% mutate(across(where(is.numeric), ~ (.-min(.))/(max(.)-min(.))))

# Deciles Calculation
data_deciles <- data %>% mutate(across(where(is.numeric), ~ ntile(., 10), .names = "decile_{col}"))

# Calculate Min and Max Values
min_values <- data %>% summarise(across(where(is.numeric), min, na.rm = TRUE))
max_values <- data %>% summarise(across(where(is.numeric), max, na.rm = TRUE))

print("Min values:")
print(min_values)
print("Max values:")
print(max_values)

