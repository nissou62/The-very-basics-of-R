# What changes did I have to make?

To prepare the data, I first had to standardize the column names across all Excel files to ensure compatibility when merging. This included renaming columns like “mph” to “Speed”, I also  had to standardize all the column names across the different CSV and Excel files. This meant manually updating names in Excel and then using R to convert all column headers to lowercase, remove whitespace, and ensure consistency. In many files, dates and speed values were formatted inconsistently, so I had to clean those using R to ensure they were in the correct numeric or date format. I also saved space by uploading the cleaned datasets to my personal GitHub repository and then pulling the files directly from GitHub in R using read_csv().

# What challenges did I have in wrangling the data?
The biggest challenge was the inconsistency between the datasets. Each file had slightly different column names, some missing values, and formatting issues like merged cells or non-numeric speed entries. Handling Excel files and converting them properly to CSV also required care to avoid misaligned data. Once combined in R, I had to inspect the structure to make sure the speed column was numeric and didn't contain invalid values. Creating the histogram required binning the speed values and tuning the binwidth to show meaningful trends without too much noise.

# What charts did I have to redo?
After binding all the data together, I had to redo the histogram chart of car speeds. Each dataset may have had different speed scales or sampling methods, so once the datasets were unified, the histogram needed to reflect this broader and more diverse data collection. I used  R to recreate the histogram with frequency on the y-axis and speed on the x-axis, ensuring that the chart accurately visualized trends across all sources. and I ended up getting this chart 

<br>
<div align = "center">
<img src = "https://github.com/nissou62/The-very-basics-of-R/blob/main/speedof%20cars%20for%20data%20project.png" width = "450")>
</div>

# Reflection Paragraph:
This assignment taught me the importance of data consistency and cleanliness, especially when multiple people are collecting data separately. I learned how essential it is to have a shared structure and naming convention before beginning data collection. Even small inconsistencies like a tab in a column name or inconsistent capitalization can cause major issues when trying to combine data. The wrangling process in R helped reinforce skills with functions like rename(), and bind_rows(), which are crucial for working with messy real world datasets.

I also gained a deeper understanding of what it takes to collect quality data. It’s not just about recording observations, it's about ensuring the observations are recorded in a format that’s usable. Differences in time format, vehicle descriptions, and even missing values can complicate analysis. Collaborative data collection requires planning, standardization, and communication to be successful. This project gave me a taste of what it’s like to clean and combine data from multiple sources and showed me how vital this step is in the data science process.

# THE CODE I USED IN R: 

library(readr)
library(dplyr)

setwd("~/Downloads/carsdatagroups.R/datacars.R")
library(tidyverse)
library(readr)

# Read all CSVs into R
df1 <- read_csv("speed_counting_cars1.csv")            
df2 <- read_csv("Data_Counting_Cars.csv")             
df3 <- read_csv("Car Data Collection.csv")
df4 <- read_csv("counting_cars_final.csv")
df5 <- read_csv("Counting_Cars.csv")
df6 <- read_csv("file_2_Sheet1.csv")
df7 <- read_csv("file_1_Kritan.csv")
df8 <- read_csv("file_1_Abhib.csv")
df9 <- read_csv("file_1_Aashish.csv")

# Standardize column names across all
clean_names <- function(df) {
  names(df) <- tolower(gsub("\\s+", "_", trimws(names(df))))
  return(df)
}

df_list <- list(df1, df2, df3, df4, df5, df6, df7, df8, df9) %>% lapply(clean_names)
all_cars <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8, df9)
# Combine them all
all_cars <- bind_rows(df_list)


# View the structure of the combined dataset
glimpse(all_cars)
# Create histogram of Speed
# Convert Speed to numeric if necessary
all_cars$Speed <- as.numeric(all_cars$Speed)
ggplot(all_cars, aes(x = Speed)) +
  geom_histogram(binwidth = 5, fill = "#2C77B0", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Car Speeds",
    x = "Speed (mph)",
    y = "Frequency"
  ) +
  theme_minimal()
