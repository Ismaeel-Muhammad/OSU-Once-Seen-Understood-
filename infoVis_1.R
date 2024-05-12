# Set the working directory to the folder containing CSV files
setwd("D:\\Info Visualization")

# Get a list of all CSV files in the folder
csv_files <- list.files(pattern = "\\.csv$")
# Initialize an empty data frame to store the combined data
df <- data.frame()

# Loop through each CSV file and append it to the combined data frame
for (file in csv_files) {
  # Read the CSV file into a temporary data frame
  temp_df <- read.csv(file, header = TRUE)
  
  # Append the temporary data frame to the combined data frame
  df <- rbind(df, temp_df)
}
View(df)
summary(df)
sapply(df, function(x) sum(is.na(x)))
sapply(df, function(y) sum(duplicated(y)))

df$Date <- as.Date(df$Date)
sapply(df, class)


df_cleaned <- subset(df, Date >= as.Date("2002-01-01"))
df_cleaned <- df_cleaned[, !(names(df_cleaned) %in% c("Trades"))]
sapply(df_cleaned, function(x) sum(is.na(x)))
summary(df_cleaned)

library(tidyr)
df_cleaned <- df_cleaned %>% fill(Deliverable.Volume, .direction = "downup")
df_cleaned <- df_cleaned %>% fill(X.Deliverble, .direction = "downup")
summary(df_cleaned)
View(df_cleaned)

# Load the libraries
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(shinythemes)
library(bslib)
df_grouped <-df_cleaned %>% group_by(Symbol)
trimmed_data <- df_grouped %>%
  mutate(row_number = row_number()) %>%
  group_by(Symbol) %>%
  slice(seq(1, n(), by = round(n() * .01)))


result_df <- merge(trimmed_data, temp_df[c('Symbol', 'Industry')], by = 'Symbol', all.x = TRUE)
c_df <- na.omit(result_df)
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Interactive Dashboard"), div(
    id = "themeSwitcherContainer",
    style = "position: fixed; top: 0; right: 0; z-index: 1000;",
    actionButton("theme_switcher", "", class = "theme-switcher-btn", icon = icon("adjust"))
  ),
  dashboardSidebar(
    selectInput("category", "Select Category", 
                choices = c("All", unique(c_df$Symbol)),
                selected = "All"),
    checkboxGroupInput("categories", "Choose Categories for Comparison", choices = unique(c_df$Industry)),
    dateRangeInput("dateRange", "Select Date Range", start = min(c_df$Date), end = max(c_df$Date)),
    checkboxGroupInput("visualizations", "Choose Visualizations to Display",
                       choices = c("Line Chart", "Boxplot", "Candlestick Chart", "Scatter Plot", "Histogram", "Area Chart", "BarChart"),
                       selected = c("Line Chart", "Boxplot", "Candlestick Chart")),
    radioButtons("dataType", "Choose Data Type", choices = c("Daily", "Cumulative"), selected = "Daily"),
    selectInput("unit", "Choose Unit", choices = c("Percentage Change", "Absolute Values"), selected = "Absolute Values"),
    sliderInput("volumeFilter", "Filter by Volume", min = min(c_df$Volume), max = max(c_df$Volume),
                value = c(min(c_df$Volume), max(c_df$Volume)), step = 100000)
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Line Chart", plotlyOutput("lineChart")),
      tabPanel("Boxplot", plotlyOutput("boxplot")),
      tabPanel("Candlestick Chart", plotlyOutput("candlestickChart")),
      tabPanel("Scatter Plot", plotlyOutput("scatterPlot")),
      tabPanel("Histogram", plotlyOutput("histogram")),
      tabPanel("Area Chart", plotlyOutput("areaChart")),
      tabPanel("BarChart", plotlyOutput("BarChart"))
    ))
)

# Define server
server <- function(input, output, session) {
  observeEvent(input$dark_mode, {
    if (input$dark_mode == "dark") {
      showNotification("Welcome to the dark side!")
    }
  })  
  observe({
    if (input$category == "All") {
      filtered_data <- c_df %>%
        filter(Date >= input$dateRange[1], Date <= input$dateRange[2])
    } else {
      filtered_data <- c_df %>%
        filter(Industry %in% c(input$categories),
               Date >= input$dateRange[1], Date <= input$dateRange[2])
    }
    
    if (input$dataType == "Cumulative") {
      filtered_data <- filtered_data %>%
        group_by(Date) %>%
        summarise(Closeed = sum(c_df$Close))
    } 
    if (input$unit == "Percentage Change") {
      filtered_data$Close <- (filtered_data$Close - min(filtered_data$Close)) / (max(filtered_data$Close) - min(filtered_data$Close)) * 100
    }
    if ("Line Chart" %in% input$visualizations) {
      output$lineChart <- renderPlotly({
        p <- ggplot(filtered_data, aes(x = Date, y = Close, color = as.factor(Symbol))) +
          geom_line() +
          theme_minimal()
        ggplotly(p)
      })
    }
    
    if ("Boxplot" %in% input$visualizations) {
      output$boxplot <- renderPlotly({
        p <- ggplot(filtered_data, aes(x = factor(Symbol), y = Volume, fill = factor(Symbol))) +
          geom_boxplot() +
          theme_minimal()
        ggplotly(p)
      })
    }
    
    if ("Candlestick Chart" %in% input$visualizations) {
      output$candlestickChart <- renderPlotly({
        p <- plot_ly(filtered_data, type = "candlestick", x = ~Date, open = ~Open, close = ~Close, high = ~High, low = ~Low, group = ~Symbol)
        p
      })
    }
    
    if ("Scatter Plot" %in% input$visualizations) {
      output$scatterPlot <- renderPlotly({
        p <- ggplot(filtered_data, aes(x = Date, y = Close, color = as.factor(Symbol))) +
          geom_point() +
          theme_minimal()
        ggplotly(p)
      })
    }
    
    if ("Histogram" %in% input$visualizations) {
      output$histogram <- renderPlotly({
        p <- ggplot(filtered_data, aes(x = VWAP)) +
          geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
          theme_minimal()
        ggplotly(p)
      })
    }
    
    if ("Area Chart" %in% input$visualizations) {
      output$areaChart <- renderPlotly({
        df_volume <- filtered_data %>% select(Date, Symbol, Volume)
        p <- ggplot(df_volume, aes(x = Date, y = Volume, fill = as.factor(Symbol))) +
          geom_area() +
          theme_minimal()
        ggplotly(p)
      })
    }
    
    if ("Average Closing Prices" %in% input$visualizations) {
      avg_prices <- filtered_data %>%
        group_by(Symbol) %>%
        summarise(Avg_Close = mean(c_df$Close))
      p <- ggplot(avg_prices, aes(x = Symbol, y = Avg_Close, fill = Symbol)) +
        geom_bar(stat = "identity") +
        theme_minimal()
      ggplotly(p)
    }
  })
}

# Run the application
shinyApp(ui, server)

