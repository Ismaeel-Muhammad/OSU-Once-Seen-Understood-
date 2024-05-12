# Load the libraries
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(shinythemes)
library(bslib)
library(tidyr)

setwd("D:\\Info Visualization")
csv_files <- list.files(pattern = "\\.csv$")
df <- data.frame()
for (file in csv_files) {
  temp_df <- read.csv(file, header = TRUE)
  df <- rbind(df, temp_df)
}
View(df)
summary(df)

#checking for nulls or duplicates
sapply(df, function(x) sum(is.na(x)))
sapply(df, function(y) sum(duplicated(y)))

#handling date datatype
df$Date <- as.Date(df$Date)
sapply(df, class)

#trimming data before this date because it is inconsistent
df_cleaned <- subset(df, Date >= as.Date("2002-01-01"))

#dropping this columns because it has more nulls than we could approve
df_cleaned <- df_cleaned[, !(names(df_cleaned) %in% c("Trades"))]
sapply(df_cleaned, function(x) sum(is.na(x)))
summary(df_cleaned)

#filling null with smoothing method 
df_cleaned <- df_cleaned %>% fill(Deliverable.Volume, .direction = "downup")
df_cleaned <- df_cleaned %>% fill(X.Deliverble, .direction = "downup")
summary(df_cleaned)
View(df_cleaned)

#reducing data size to reduce computational power and time
df_grouped <-df_cleaned %>% group_by(Symbol)
trimmed_data <- df_grouped %>%
  mutate(row_number = row_number()) %>%
  group_by(Symbol) %>%
  slice(seq(1, n(), by = round(n() * .01)))

#adding the label industry from the meta data file to filter with
result_df <- merge(trimmed_data, temp_df[c('Symbol', 'Industry')], by = 'Symbol', all.x = TRUE)

#making sure no nulls still in data
c_df <- na.omit(result_df)

# Defining UI
ui <- page_navbar(
  title = "Dashboard",
  nav_spacer(), 
  nav_panel("Page 1",dashboardPage(
    dashboardHeader(title = "Interactive Dashboard"),
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
                  value = c(min(c_df$Volume), max(c_df$Volume)), step = 1000)
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
      )
    )),
    nav_item(
      input_dark_mode(id = "dark_mode", mode = "light")
    )
  ))

# Defining server
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
        summarise(Close = sum(c_df$Close))
    }
    
    if ("Line Chart" %in% input$visualizations) {
      output$lineChart <- renderPlotly({
        p <- ggplot(filtered_data, aes(x = Date, y = VWAP, color = as.factor(Symbol))) +
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
        p <- ggplot(filtered_data, aes(x = Date, y = Turnover, color = as.factor(Symbol))) +
          geom_point() +
          theme_minimal()
        ggplotly(p)
      })
    }
    
    if ("Histogram" %in% input$visualizations) {
      output$histogram <- renderPlotly({
        p <- ggplot(filtered_data, aes(x = Deliverable.Volume)) +
          geom_histogram(binwidth = 1000, fill = "blue") +
          theme_minimal()
        ggplotly(p)
      })
    }
    
    if ("Area Chart" %in% input$visualizations) {
      output$areaChart <- renderPlotly({
        df_volume <- filtered_data %>% select(Date, Symbol, Volume)
        p <- ggplot(df_volume, aes(x = Date, y = X.Deliverble, fill = as.factor(Symbol))) +
          geom_area() +
          theme_minimal()
        ggplotly(p)
      })
    }
    
    if ("BarChart" %in% input$visualizations) {
      avg_prices <- filtered_data %>% select(Industry)
      summaryData <-  avg_prices %>%
        group_by(Industry) %>%
        summarise(count=n())
      output$BarChart <- renderPlotly({
        p <- ggplot(summaryData, aes(x = Industry, y = count, fill = as.factor(Industry))) +
          geom_bar(stat = "identity") +
          theme_minimal()
        ggplotly(p)
      })
    }
  })
}

# Running the application
shinyApp(ui, server)
