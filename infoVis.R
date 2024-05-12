# Set the working directory to the folder containing CSV files
setwd("D:\\Info Visualization")

# Get a list of all CSV files in the folder
csv_files <- list.files(pattern = "\\.csv$")
options(device = function(...) grDevices::X11())
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

df_grouped <-df_cleaned %>% group_by(Symbol)
trimmed_data <- df_grouped %>%
  mutate(row_number = row_number()) %>%
  group_by(Symbol) %>%
  slice(seq(1, n(), by = round(n() * .01)))

# Assuming the first dataset is stored in a variable named df_cleaned
# Replace "your_column_names" with the actual column names from your dataset

# Define UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Stock Data Dashboard"), 
  div(
    id = "themeSwitcherContainer",
    style = "position: fixed; top: 0; right: 0; z-index: 1000;",
    actionButton("theme_switcher", "", class = "theme-switcher-btn", icon = icon("adjust"))
  ),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Select Date Range", 
                     start = min(trimmed_data$Date), end = max(trimmed_data$Date), 
                     format = "yyyy-mm-dd"),
      selectInput("stock", "Select Stock", c("All", unique(trimmed_data$Symbol))),
      checkboxGroupInput("visualizations", "Choose Visualizations",
                         choices = c("Line Chart", "Bar Chart", "Box Plot", "Scatter Plot",
                                     "Histogram", "Multi-line Chart", "Stacked Area Chart"),
                         selected = c("Line Chart", "Bar Chart")),
      radioButtons("data_type", "Choose Data Type",
                   choices = c("Daily", "Cumulative"), selected = "Daily"),
      selectInput("unit", "Select Unit", choices = c("Absolute Values", "Percentage Change"), selected = "Absolute Values"),
      sliderInput("filter_criteria", "Filter Data based on Specific Criteria",
                  min = min(trimmed_data$Turnover), max = max(trimmed_data$Turnover),
                  value = c(min(trimmed_data$Turnover), max(trimmed_data$Turnover)))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Line Chart", plotOutput("lineChart")),
        tabPanel("Bar Chart", plotOutput("barChart")),
        tabPanel("Box Plot", plotOutput("boxPlot")),
        tabPanel("Scatter Plot", plotOutput("scatterPlot")),
        tabPanel("Histogram", plotOutput("histogram")),
        tabPanel("Multi-line Chart", plotOutput("multiLineChart")),
        tabPanel("Stacked Area Chart", plotOutput("stackedAreaChart"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {

  observeEvent(input$theme_switcher, {
    if (input$theme_switcher %% 2 == 1) {
      shinyjs::runjs("shinyjs.themeSwitcher('darkly')")
    } else {
      shinyjs::runjs("shinyjs.themeSwitcher('cerulean')")
    }
  })
  filtered_data <- reactive({
    if (input$stock == "All") {
      df_filtered <- trimmed_data[trimmed_data$Date >= input$date_range[1] & 
                           trimmed_data$Date <= input$date_range[2], ]
    } else {
      df_filtered <- trimmed_data[trimmed_data$Symbol == input$stock & 
                           trimmed_data$Date >= input$date_range[1] & 
                           trimmed_data$Date <= input$date_range[2], ]
    }
   # if (!is.null(input$categories) && length(input$categories) > 0) {
   #   df_filtered <- df_filtered[df_filtered$your_category_column %in% input$categories, ]
   # }
    df_filtered
  })
  
  plot_data <- reactive({
    switch(input$data_type,
           "Daily" = filtered_data(),
           "Cumulative" = trimmed_data[trimmed_data$Symbol == input$stock & trimmed_data$Date <= input$date_range[2], ]
    )
  })
  
  # Apply selected unit to the data
  plot_data_unit <- reactive({
    df_plot <- plot_data()
    if (input$unit == "Percentage Change") {
      df_plot$close <- df_plot$Turnover
    }
    df_plot
  })
  
  # Apply data filter based on specific criteria
  plot_data_filtered <- reactive({
    df_plot_unit <- plot_data_unit()
    df_plot_filtered <- df_plot_unit[df_plot_unit$Turnover >= input$filter_criteria[1] &
                                       df_plot_unit$Turnover <= input$filter_criteria[2], ]
    df_plot_filtered
  })
  
  # Function to generate plotly tooltips
  tooltip <- function(x) {
    paste("<b>Date:</b> ", format(x$Date, "%Y-%m-%d"), "<br>",
          "<b>Close:</b> ", sprintf("%.2f", x$Close), "<br>",
          "<b>Volume:</b> ", x$Volume, "<br>",
          "<b>Percent Change Price:</b> ", sprintf("%.2f", x$Turnover), "%<br>",
          "<b>Percent Change Volume:</b> ", sprintf("%.2f", x$Deliverable.Volume), "%")
  }
  
  # Line Chart
  output$lineChart <- renderPlotly({
    if ("Line Chart" %in% input$visualizations) {
      gg <- ggplot(plot_data_filtered(), aes(x = Date, y = Close)) +
        geom_line() +
        labs(title = "Closing Prices Over Time",
             x = "Date",
             y = if (input$unit == "Percentage Change") "Percent Change Price" else "Closing Price")
      ggplotly(gg, tooltip = "text") %>% layout(hovermode = "closest") %>% 
        style(hoverinfo = "text")
    }
    dev.off()
  })
  
  # Bar Chart
  output$barChart <- renderPlotly({
    if ("Bar Chart" %in% input$visualizations) {
      gg <- ggplot(plot_data_filtered(), aes(x = Date, y = Volume, fill = Symbol)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Volume Comparison",
             x = "Date",
             y = "Volume")
      ggplotly(gg, tooltip = "text") %>% layout(hovermode = "closest") %>% 
        style(hoverinfo = "text")
    }
    dev.off()
  })
  
  # Box Plot
  output$boxPlot <- renderPlotly({
    if ("Box Plot" %in% input$visualizations) {
      gg <- ggplot(plot_data_filtered(), aes(x = Symbol, y = Turnover)) +
        geom_boxplot() +
        labs(title = "Distribution of Percent Change in Price",
             x = "Stock",
             y = if (input$unit == "Percentage Change") "Percent Change in Price" else "Closing Price")
      ggplotly(gg, tooltip = "text") %>% layout(hovermode = "closest") %>% 
        style(hoverinfo = "text")
    }
    dev.off()
  })
  
  # Scatter Plot
  output$scatterPlot <- renderPlotly({
    if ("Scatter Plot" %in% input$visualizations) {
      gg <- ggplot(plot_data_filtered(), aes(x = Turnover, y = Deliverable.Volume)) +
        geom_point() +
        labs(title = "Relationship Between Percent Change in Price and Volume",
             x = if (input$unit == "Percentage Change") "Percent Change in Price" else "Closing Price",
             y = "Percent Change in Volume")
      ggplotly(gg, tooltip = "text") %>% layout(hovermode = "closest") %>% 
        style(hoverinfo = "text")
    }
    dev.off()
  })
  
  # Histogram
  output$histogram <- renderPlotly({
    if ("Histogram" %in% input$visualizations) {
      gg <- ggplot(plot_data_filtered(), aes(x = VWAP)) +
        geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = "Distribution of Days to Next Dividend",
             x = "Days to Next Dividend",
             y = "Frequency")
      ggplotly(gg, tooltip = "text") %>% layout(hovermode = "closest") %>% 
        style(hoverinfo = "text")
    }
    dev.off()
  })
  
  # Multi-line Chart
  output$multiLineChart <- renderPlotly({
    if ("Multi-line Chart" %in% input$visualizations) {
      gg <- ggplot(plot_data_filtered(), aes(x = Date, y = Close, color = Symbol)) +
        geom_line() +
        labs(title = "Closing Prices Over Time for Different Stocks",
             x = "Date",
             y = if (input$unit == "Percentage Change") "Percent Change Price" else "Closing Price")
      ggplotly(gg, tooltip = "text") %>% layout(hovermode = "closest") %>% 
        style(hoverinfo = "text")
      
    }
    dev.off()
  })
  
  # Stacked Area Chart
  output$stackedAreaChart <- renderPlotly({
    if ("Stacked Area Chart" %in% input$visualizations) {
      gg <-
        ggplot(plot_data_filtered(),
               aes(x = Date, y = X.Deliverble, fill = Symbol)) +
        geom_area() +
        labs(title = "Stacked Area Chart for Percent Return Next Dividend",
             x = "Date",
             y = "Percent Return Next Dividend")
      ggplotly(gg, tooltip = "text") %>% layout(hovermode = "closest") %>%
        style(hoverinfo = "text")
    }
    dev.off()
  })
  
}

# Run the app
shinyApp(ui,server)



ui_light <- fluidPage(
  theme = shinytheme("flatly"),  # Light theme, e.g., flatly
  titlePanel("Light Theme"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider", "Slider:", 1, 100, 50)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

ui_dark <- fluidPage(
  theme = shinytheme("darkly"),  # Dark theme, e.g., darkly
  titlePanel("Dark Theme"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider", "Slider:", 1, 100, 50)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    plot(1:input$slider, main = "Dynamic Plot")
  })
}

shinyApp(ui_light, server)
shinyApp(ui_dark, server)




