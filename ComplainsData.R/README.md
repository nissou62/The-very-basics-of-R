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
# Create Pivot Table
pivot_table <- data %>%
  tabyl(product, company) %>%
  adorn_totals("row") %>%
  adorn_totals("col")
print(pivot_table)

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

# Generate Word Cloud
complaints_text <- tolower(paste(data$consumer_complaint_narrative, collapse = " "))
complaints_corpus <- Corpus(VectorSource(complaints_text))
complaints_corpus <- tm_map(complaints_corpus, removePunctuation)
complaints_corpus <- tm_map(complaints_corpus, removeNumbers)
complaints_corpus <- tm_map(complaints_corpus, removeWords, stopwords("english"))
complaints_corpus <- tm_map(complaints_corpus, stripWhitespace)

dtm <- TermDocumentMatrix(complaints_corpus)
matrix <- as.matrix(dtm)
words_freq <- sort(rowSums(matrix), decreasing = TRUE)
data_frame <- data.frame(word = names(words_freq), freq = words_freq)

wordcloud(words = data_frame$word, freq = data_frame$freq, min.freq = 2, max.words = 100, colors = brewer.pal(8, "Dark2"))

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

