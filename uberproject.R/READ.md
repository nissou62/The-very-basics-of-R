library(rsconnect)
rsconnect::setAccountInfo(name='nisrineshinyapp',
                          token='6D9A9794617905ADEA48566D26C68A4B',
                          secret='YOUR_SECRET_HERE')  # Replace with your actual secret

> Deploy the app (set the correct folder path)
>  rsconnect::deployApp('path/to/UberApp') 

library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(viridis)

setwd("~/Documents/heatmap.R/heatmapproject.R")
> Load and prepare the data
> Sample office location data
data.location <- data.frame(
  place = c("Office A", "Office B"),
  address = c("123 Main St", "456 Elm St"),
  lat = c(40.7128, 40.706),
  long = c(-74.0060, -74.01),
  supervisor = c("John Doe", "Jane Smith"),
  student1 = c("Alice", "Charlie"),
  student2 = c("Bob", "Dave"),
  student3 = c("Eve", "Frank")
)

> Load and combine all six datasets
apr <- read.csv("uber-raw-data-apr14.csv")
may <- read.csv("uber-raw-data-may14.csv")
jun <- read.csv("uber-raw-data-jun14.csv")
jul <- read.csv("uber-raw-data-jul14.csv")
aug <- read.csv("uber-raw-data-aug14.csv")
sep <- read.csv("uber-raw-data-sep14.csv")

data <- rbind(apr, may, jun, jul, aug, sep)

> Convert and extract date-time components
data$Date.Time <- as.POSIXct(data$Date.Time, format="%m/%d/%Y %H:%M:%S")
data$Hour <- as.numeric(format(data$Date.Time, "%H"))
data$Day <- as.numeric(format(data$Date.Time, "%d"))
data$Month <- as.numeric(format(data$Date.Time, "%m"))
data$Weekday <- weekdays(data$Date.Time)
data$Week <- as.numeric(format(data$Date.Time, "%U"))

> UI

# 1 Pivot: Trips by Hour (Table)
What it shows: A table listing total Uber trips for each hour of the day (0 to 23).
Why it matters: Helps identify peak hours for Uber usage in NYC. Useful for operational planning and understanding customer demand patterns.

ui <- fluidPage(
  titlePanel("Uber NYC Data Analysis (2014)"),
  sidebarLayout(
    sidebarPanel(
      helpText("Visualizations: pivot tables, bar charts, heatmaps, and geospatial maps.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Pivot: Trips by Hour", tableOutput("tripsHourTable")),
        tabPanel("Chart: Trips by Hour and Month", plotOutput("tripsByHourMonth")),
        tabPanel("Chart: Trips Every Hour", plotOutput("tripsEveryHour")),
        tabPanel("Chart: Trips by Day of Month", plotOutput("tripsByDayOfMonth")),
        tabPanel("Table: Trips by Day (1-31)", tableOutput("tripsByDayTable")),
        tabPanel("Chart: Trips by Weekday and Month", plotOutput("tripsByDayMonth")),
        tabPanel("Chart: Trips by Month", plotOutput("tripsByMonth")),
        tabPanel("Chart: Trips by Base and Month", plotOutput("tripsByBaseMonth")),
        tabPanel("Heatmap: Hour vs Day", plotOutput("heatHourDay")),
        tabPanel("Heatmap: Month vs Day", plotOutput("heatMonthDay")),
        tabPanel("Heatmap: Month vs Week", plotOutput("heatMonthWeek")),
        tabPanel("Heatmap: Base vs Day of Week", plotOutput("heatBaseWeekday")),
        tabPanel("Map: Leaflet Geospatial Map", leafletOutput("map"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$tripsHourTable <- renderTable({
    data %>% group_by(Hour) %>% summarise(Total_Trips = n())
  })
 # 2 Chart: Trips by Hour and Month (Bar Chart)
What it shows: Compares Uber trip counts across hours of the day for each month using grouped bars.
Why it matters: Visualizes how ride-hailing demand changes by hour across different months, highlighting seasonal or monthly patterns in rider behavior. 

  output$tripsByHourMonth <- renderPlot({
    data %>% group_by(Hour, Month) %>% summarise(Total_Trips = n()) %>%
      ggplot(aes(x = Hour, y = Total_Trips, fill = factor(Month))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Trips by Hour and Month", x = "Hour", y = "Total Trips", fill = "Month") +
      theme_minimal()
  })
  
# 3 Chart: Trips Every Hour (Bar Chart)
What it shows: Total trips for each hour of the day, across all months.
Why it matters: Helps identify daily rush hours (typically morning and evening). Can inform decisions about driver availability and surge pricing. 
  output$tripsEveryHour <- renderPlot({
    data %>% group_by(Hour) %>% summarise(Total_Trips = n()) %>%
      ggplot(aes(x = Hour, y = Total_Trips)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Trips Every Hour", x = "Hour", y = "Number of Trips") +
      theme_minimal()
  })

  # 4 Chart: Trips by Day of Month (Bar Chart)
What it shows: Total Uber trips by calendar day (1 to 31).
Why it matters: Useful for spotting trends such as end-of-month or payday spikes in ride demand.

  output$tripsByDayOfMonth <- renderPlot({
    data %>% group_by(Day) %>% summarise(Total_Trips = n()) %>%
      ggplot(aes(x = Day, y = Total_Trips)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      labs(title = "Trips by Day of the Month", x = "Day (1–31)", y = "Total Trips") +
      theme_minimal()
  })
  # 5 Table: Trips by Day (1-31)
What it shows: A simple table showing trip counts for each day of the month.
Why it matters: Provides the same insight as Chart #4 in a numerical format for reference or precise value lookup.
  output$tripsByDayTable <- renderTable({
    data %>% group_by(Day) %>% summarise(Total_Trips = n()) %>% arrange(Day)
  })

  # 6 Chart: Trips by Day of Week and Month (Grouped Bar Chart)
What it shows: Compares number of trips for each weekday across all months.
Why it matters: Reveals which days of the week are busiest (often Friday and Saturday) and whether that varies by month. Helps with staffing and scheduling.

  output$tripsByDayMonth <- renderPlot({
    data %>% group_by(Month, Weekday) %>% summarise(Total_Trips = n()) %>%
      ggplot(aes(x = factor(Month), y = Total_Trips, fill = Weekday)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Trips by Day of Week and Month", x = "Month", y = "Total Trips", fill = "Weekday") +
      theme_minimal()
  })
  # 7 Chart: Trips by Month (Bar Chart)
What it shows: Total number of trips in each month from April to September 2014.
Why it matters: Shows seasonal growth or decline in demand. Can help understand external influences (weather, tourism season...).

  output$tripsByMonth <- renderPlot({
    data %>% group_by(Month) %>% summarise(Total_Trips = n()) %>%
      ggplot(aes(x = factor(Month), y = Total_Trips)) +
      geom_bar(stat = "identity", fill = "orange") +
      labs(title = "Trips by Month", x = "Month", y = "Total Trips") +
      theme_minimal()
  })

  # 8 Chart: Trips by Base and Month (Grouped Bar Chart)
What it shows: Number of trips per Uber base (dispatch hub) each month.
Why it matters: Highlights how different bases contribute to overall activity, and how their usage changes over time.
  output$tripsByBaseMonth <- renderPlot({
    data %>% group_by(Base, Month) %>% summarise(Total_Trips = n()) %>%
      ggplot(aes(x = Base, y = Total_Trips, fill = factor(Month))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Trips by Base and Month", x = "Base", y = "Total Trips", fill = "Month") +
      theme_minimal()
  })
  # 9 Heatmap: Hour vs Day
What it shows: A color-coded grid showing number of trips for each combination of hour and day of the month.
Why it matters: Allows quick identification of peak demand hours and days. Darker areas represent high activity.
  output$heatHourDay <- renderPlot({
    data %>% group_by(Hour, Day) %>% summarise(Trips = n()) %>%
      ggplot(aes(x = Hour, y = Day, fill = Trips)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(title = "Heatmap: Hour vs Day", x = "Hour", y = "Day", fill = "Trips") +
      theme_minimal()
  })
  # 10 Heatmap: Month vs Day
What it shows: A heatmap of total trips by day of the month across all months.
Why it matters: Provides an overview of high-traffic days across multiple months. Useful for anomaly detection or event analysis.
  output$heatMonthDay <- renderPlot({
    data %>% group_by(Month, Day) %>% summarise(Trips = n()) %>%
      ggplot(aes(x = Month, y = Day, fill = Trips)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(title = "Heatmap: Month vs Day", x = "Month", y = "Day", fill = "Trips") +
      theme_minimal()
  })

  # 11 Heatmap: Month vs Week
What it shows: Number of trips per week for each month (week 1 to ~week 40).
Why it matters: Shows weekly demand patterns across months, allowing for macro-level trend analysis.
  output$heatMonthWeek <- renderPlot({
    data %>% group_by(Month, Week) %>% summarise(Trips = n()) %>%
      ggplot(aes(x = Month, y = Week, fill = Trips)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(title = "Heatmap: Month vs Week", x = "Month", y = "Week", fill = "Trips") +
      theme_minimal()
  })
  # 12 Heatmap: Base vs Day of Week
What it shows: Trip volume per base (dispatch hub) by weekday.
Why it matters: Combines spatial and temporal data to see how each base operates during the week, aiding resource optimization.

  output$heatBaseWeekday <- renderPlot({
    data %>% group_by(Base, Weekday) %>% summarise(Trips = n()) %>%
      ggplot(aes(x = Base, y = Weekday, fill = Trips)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(title = "Heatmap: Base vs Day of Week", x = "Base", y = "Day of Week", fill = "Trips") +
      theme_minimal()
  })
  
  # 13. Map: Leaflet Geospatial Map
What it shows: An interactive map displaying office or dispatch hub locations with searchable popups.
Why it matters: Adds a spatial dimension to the analysis. You can see where key offices or students are located and measure distances. Useful for real-world deployment or logistics.

  output$map <- renderLeaflet({
    leaflet(data.location) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addMarkers(
        lng = ~long,
        lat = ~lat,
        popup = ~paste(
          paste('<b>Office:</b>', place),
          paste('<b>Address:</b>', address),
          paste('<b>Lat:</b>', lat),
          paste('<b>Long:</b>', long),
          paste('<b>Supervisor:</b>', supervisor),
          paste('<b>Student 1:</b>', student1),
          paste('<b>Student 2:</b>', student2),
          paste('<b>Student 3:</b>', student3),
          sep = '<br/>'
        ),
        label = ~place,
        group = 'data.location'
      ) %>%
      addResetMapButton() %>%
      addSearchFeatures(
        targetGroups = 'data.location',
        options = searchFeaturesOptions(
          zoom = 15,
          openPopup = TRUE,
          firstTipSubmit = TRUE,
          autoCollapse = TRUE,
          hideMarkerOnCollapse = TRUE
        )
      ) %>%
      addMeasure(
        position = 'bottomleft',
        primaryLengthUnit = 'meters',
        primaryAreaUnit = 'sqmeters',
        activeColor = '#3D535D',
        completedColor = '#7D4479'
      ) %>%
      addControl(
        html = "<P><b>Masterpiece Statistics 53</b><br/>Search for offices/industries<br/>in Java by name.</P>",
        position = 'topright'
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)

