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
