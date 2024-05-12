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


install.packages("shinyDarkmode")

# Load the libraries
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(shinythemes)
library(shiny)
library(shinyDarkmode)

df_grouped <-df_cleaned %>% group_by(Symbol)
trimmed_data <- df_grouped %>%
  mutate(row_number = row_number()) %>%
  group_by(Symbol) %>%
  slice(seq(1, n(), by = round(n() * .01)))

result_df <- merge(trimmed_data, temp_df[c('Symbol', 'Industry')], by = 'Symbol', all.x = TRUE)
# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),  # You can change the theme as needed
  titlePanel("Interactive Dashboard"),
  uiOutput("style"),
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Select Category", 
                  choices = c("All", unique(result_df$Symbol)),
                  selected = "All"),  # "All" selected by default
      checkboxGroupInput("categories", "Choose Categories for Comparison", choices = unique(result_df$Industry)),
      dateRangeInput("dateRange", "Select Date Range", start = min(result_df$Date), end = max(result_df$Date)),
      checkboxGroupInput("visualizations", "Choose Visualizations to Display",
                         choices = c("Line Chart", "Boxplot", "Candlestick Chart", "Scatter Plot", "Histogram", "Area Chart", "BarChart"),
                         selected = c("Line Chart", "Boxplot", "Candlestick Chart")),
      radioButtons("dataType", "Choose Data Type", choices = c("Daily", "Cumulative"), selected = "Daily"),
      selectInput("unit", "Choose Unit", choices = c("Percentage Change", "Absolute Values"), selected = "Absolute Values"),
      sliderInput("volumeFilter", "Filter by Volume", min = min(result_df$Volume), max = max(result_df$Volume),
                  value = c(min(result_df$Volume), max(result_df$Volume)), step = 1000)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Line Chart", plotlyOutput("lineChart")),
        tabPanel("Boxplot", plotlyOutput("boxplot")),
        tabPanel("Candlestick Chart", plotlyOutput("candlestickChart")),
        tabPanel("Scatter Plot", plotlyOutput("scatterPlot")),
        tabPanel("Histogram", plotlyOutput("histogram")),
        tabPanel("Area Chart", plotlyOutput("areaChart")),
        tabPanel("BarChart", plotlyOutput("BarChart"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
 
  observe({
    if (input$category == "All") {
      filtered_data <- result_df %>%
        filter(Date >= input$dateRange[1], Date <= input$dateRange[2])
    } else {
      filtered_data <- result_df %>%
        filter(Industry %in% c(input$categories),
               Date >= input$dateRange[1], Date <= input$dateRange[2])
    }
    # Transform data based on user choice of daily or cumulative
    if (input$dataType == "Cumulative") {
      filtered_data <- filtered_data %>%
        group_by(Date) %>%
        summarise(Close = sum(Close))
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
        p <- ggplot(filtered_data, aes(x = Volume)) +
          geom_histogram(binwidth = 1, fill = "blue", color = "black") +
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
        summarise(Avg_Close = mean(Close))
      
      print(avg_prices)  # Add this line to check the data
      
      p <- ggplot(avg_prices, aes(x = Symbol, y = Avg_Close, fill = Symbol)) +
        geom_bar(stat = "identity") +
        theme_minimal()
      ggplotly(p)
    }
    
    
    
  })
}

# Run the application
shinyApp(ui, server)
