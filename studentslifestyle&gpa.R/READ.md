# 1. Load & Clean Data

dataset <- read_excel("student_lifestyle_dataset.xlsx")
colnames(dataset) <- make.names(colnames(dataset))# 1. Load & Clean Data

# 2. Auto Detect Column Names

sleep_col <- grep("Sleep", colnames(dataset), value = TRUE)
study_col <- grep("Study", colnames(dataset), value = TRUE)
gpa_col <- grep("GPA", colnames(dataset), value = TRUE)

# 3. Linear Regression Model

model <- lm(as.formula(paste(gpa_col, "~", sleep_col, "+", study_col)), data = dataset)
predicted_gpa <- predict(model, dataset)

# 4. UI Tab for Visualizations

tabPanel("First Visualization",
         plotOutput("sleep_gpa_plot")
)

# 5. Sleep vs GPA Plot

ggplot(dataset, aes_string(x = sleep_col, y = gpa_col)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# 6. Predicted vs Actual GPA

ggplot(data = dataset, aes(x = dataset[[gpa_col]], y = predicted_gpa)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red")

# 7 First Visualization
# Sleep Hours vs GPA Plot
  output$sleep_gpa_plot <- renderPlot({
    ggplot(dataset, aes_string(x = sleep_col, y = gpa_col)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "GPA vs Sleep Hours", x = sleep_col, y = gpa_col)
  })

# 8 Second Visualization
# Study Time Distribution Plot
  output$studytime_dist <- renderPlot({
    ggplot(dataset, aes_string(x = study_col)) +
      geom_histogram(bins = 10, fill = "skyblue", color = "black") +
      labs(title = "Study Time Distribution", x = study_col, y = "Frequency")
  })

# 9 predictive Model
  # Plot of actual vs predicted GPA
  output$predicted_vs_actual_plot <- renderPlot({
    ggplot(data = dataset, aes(x = dataset[[gpa_col]], y = predicted_gpa)) +
      geom_point(color = "blue") +
      geom_smooth(method = "lm", color = "red") +
      labs(title = "Actual vs Predicted GPA", x = "Actual GPA", y = "Predicted GPA")
  })

# 10 Geo Spatial 
We couldn't create a geospatial model for our student lifestyle and GPA project because the dataset lacked any geographic or location information. Geospatial modeling requires data points tied to specific places such as city, state, zip code, or GPS coordinates to analyze how location might influence outcomes. Since our dataset focused solely on individual lifestyle factors like sleep, study time, and GPA without including where students live or study, there was no spatial data to map or analyze.


