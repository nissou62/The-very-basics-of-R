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

# Create Pivot Table
pivot_table <- data %>%
  tabyl(product, company) %>%
  adorn_totals("row") %>%
  adorn_totals("col")
print(pivot_table)
