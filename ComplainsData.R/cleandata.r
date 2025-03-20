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

# View structure of dataset
str(data)

# Remove duplicate rows
data <- data %>% distinct()

# Handle missing values - removing rows with too many missing values
threshold <- 0.3 * ncol(data)  # If more than 30% missing, drop the row
data <- data %>% filter(rowSums(is.na(.)) <= threshold)

# Fill missing values with appropriate replacements
data <- data %>% mutate_all(~ ifelse(is.na(.), "Unknown", .))

# Standardize column names
data <- data %>% rename_all(tolower) %>% rename_all(~ gsub(" ", "_", .))

# Save cleaned dataset
write_csv(data, "Cleaned_Consumer_Complaints 2.csv")

# Print message
print("Data cleaning completed and saved as 'Cleaned_Consumer_Complaints 2.csv'")
