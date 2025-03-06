# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Load data
course <- read_excel("Course.xlsx", sheet = "Course")
student <- read_excel("Student.xlsx", sheet = "Student")
registration <- read_excel("Registration 2.xlsx", sheet = "Registration")

# Merge datasets using left join
merged_data <- registration %>%
  left_join(student, by = "Student ID") %>%
  left_join(course, by = "Instance ID")

# Chart on the number of majors
ggplot(merged_data, aes(x = Title)) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Number of Students per Major", x = "Major", y = "Count")

# Chart on the birth year of students
merged_data$Birth_Year <- as.numeric(format(as.Date(merged_data$`Birth Date`), "%Y"))
ggplot(merged_data, aes(x = Birth_Year)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Distribution of Student Birth Years", x = "Year", y = "Count")

# Total cost per major, segmented by payment plan
total_cost <- merged_data %>%
  group_by(Title, `Payment Plan`) %>%
  summarise(Total_Cost = sum(`Total Cost`, na.rm = TRUE))

ggplot(total_cost, aes(x = Title, y = Total_Cost, fill = factor(`Payment Plan`))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Total Cost per Major by Payment Plan", x = "Major", y = "Total Cost", fill = "Payment Plan")

# Total balance due by major, segmented by payment plan
balance_due <- merged_data %>%
  group_by(Title, `Payment Plan`) %>%
  summarise(Total_Balance_Due = sum(`Balance Due`, na.rm = TRUE))

ggplot(balance_due, aes(x = Title, y = Total_Balance_Due, fill = factor(`Payment Plan`))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Total Balance Due per Major by Payment Plan", x = "Major", y = "Balance Due", fill = "Payment Plan")

