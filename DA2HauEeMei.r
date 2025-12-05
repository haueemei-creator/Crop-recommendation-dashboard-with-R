library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(ggplot2)

setwd("C:/Users/User/Downloads/r")
myData <- read.csv("Crop_recommendation.csv")
options(max.print = 1e7)

# User interface
ui <- dashboardPage(
  dashboardHeader(title = "Crop Recommendation Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Visit-us", icon = icon("send", lib = "glyphicon"),
               href = "https://www.kaggle.com/datasets/atharvaingle/crop-recommendation-dataset"),
      menuItem("Charts", tabName = "charts",
               menuSubItem("Nitrogen by temperature", tabName = "nitrogen_by_temperature"),
               menuSubItem("Phosphorus by pH value", tabName = "phosphorus_by_ph_value"),
               menuSubItem("Potassium by rainfall", tabName = "potassium_by_rainfall"),
               menuSubItem("pH value by Rainfall", tabName = "ph_value_by_rainfall"),
               menuSubItem("Amount of crop label by Humidity", tabName = "amount_of_crop_label_by_humidity")),
      menuItem("Data", tabName = "data",
               menuSubItem("First 5 Rows", tabName = "first_5_rows"),
               menuSubItem("Dataset Properties", tabName = "dataset_properties"),
               menuSubItem("Dataset Summary", tabName = "dataset_summary")),
      
      radioButtons("dvInput", "Columns:", 
                   c("N","P","K","temperature","humidity","ph","rainfall","label"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Crop Recommendation",
                    selectInput("N", "X Variable", choices = names(myData)[-5], selected = "N"),
                    selectInput("label", "Y Variable", choices = names(myData)[-5], selected = "label"),
                    plotOutput("bar_plot"),
                    background = "yellow"
                )
              ),
              box(title = "Crop Recommendation",
                  img(src = "croprecommendation.png", height = 350, width = 650)
              ),
              box(title = "Brief Explanation of dataset",
                  verbatimTextOutput("data_explanation"),
                  background = "teal"
              )
      ),
      
      # Chart tabs
      tabItem(tabName = "nitrogen_by_temperature",
              fluidRow(
                box(title = "Nitrogen by Temperature",
                    status = "primary",
                    solidHeader = TRUE,
                    plotOutput("nitrogen_temp_plot"),
                    width = 12)
              )
      ),
      
      tabItem(tabName = "phosphorus_by_ph_value",
              fluidRow(
                box(title = "Phosphorus by pH Value",
                    status = "primary",
                    solidHeader = TRUE,
                    plotOutput("phosphorus_ph_plot"),
                    width = 12)
              )
      ),
      
      tabItem(tabName = "potassium_by_rainfall",
              fluidRow(
                box(title = "Potassium by Rainfall",
                    status = "primary",
                    solidHeader = TRUE,
                    plotOutput("potassium_rainfall_plot"),
                    width = 12)
              )
      ),
      
      tabItem(tabName = "ph_value_by_rainfall",
              fluidRow(
                box(title = "pH Value by Rainfall",
                    status = "primary",
                    solidHeader = TRUE,
                    plotOutput("ph_rainfall_plot"),
                    width = 12)
              )
      ),
      
      tabItem(tabName = "amount_of_crop_label_by_humidity",
              fluidRow(
                box(title = "Crop Distribution by Humidity",
                    status = "primary",
                    solidHeader = TRUE,
                    plotOutput("crop_humidity_plot"),
                    width = 12)
              )
      ),
      
      # Data tabs
      tabItem(tabName = "first_5_rows",
              fluidRow(
                box(title = "First 5 Rows",
                    status = "primary",
                    background = "green",
                    verbatimTextOutput("header_rows"),
                    width = 12)
              )
      ),
      
      tabItem(tabName = "dataset_properties",
              fluidRow(
                box(title = "Dataset Properties",
                    status = "primary",
                    background = "red",
                    verbatimTextOutput("dataset_properties"),
                    width = 12)
              )
      ),
      
      tabItem(tabName = "dataset_summary",
              fluidRow(
                box(title = "Dataset Summary",
                    status = "primary",
                    background = "orange",
                    verbatimTextOutput("dataset_summary"),
                    width = 12)
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Display first 5 rows with header
  output$header_rows <- renderPrint({
    head(myData, 5)
  })
  
  # Display dataset properties 
  output$dataset_properties <- renderPrint({
    str(myData)
  })
  
  # Display summary of dataset
  output$dataset_summary <- renderPrint({
    summary(myData)
  })
  
  # Plot for nitrogen by temperature
  output$nitrogen_temp_plot <- renderPlot({
    ggplot(myData, aes(x = temperature, y = N)) +
      geom_point(alpha = 0.6, color = "blue") +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      labs(title = "Nitrogen by Temperature",
           x = "Temperature",
           y = "Nitrogen (N)") +
      theme_minimal()
  })
  
  # Plot for phosphorus by pH value
  output$phosphorus_ph_plot <- renderPlot({
    ggplot(myData, aes(x = ph, y = P)) +
      geom_point(alpha = 0.6, color = "green") +
      geom_smooth(method = "loess", color = "darkgreen", se = FALSE) +
      labs(title = "Phosphorus by pH Value",
           x = "pH Value",
           y = "Phosphorus (P)") +
      theme_minimal()
  })
  
  # Plot for potassium by rainfall
  output$potassium_rainfall_plot <- renderPlot({
    ggplot(myData, aes(x = rainfall, y = K)) +
      geom_point(alpha = 0.6, color = "orange") +
      geom_smooth(method = "lm", color = "brown", se = FALSE) +
      labs(title = "Potassium by Rainfall",
           x = "Rainfall",
           y = "Potassium (K)") +
      theme_minimal()
  })
  
  # Plot for pH value by rainfall
  output$ph_rainfall_plot <- renderPlot({
    ggplot(myData, aes(x = rainfall, y = ph)) +
      geom_point(alpha = 0.6, color = "purple") +
      geom_smooth(method = "loess", color = "darkviolet", se = FALSE) +
      labs(title = "pH Value by Rainfall",
           x = "Rainfall",
           y = "pH Value") +
      theme_minimal()
  })
  
  # Plot for crop distribution by humidity
  output$crop_humidity_plot <- renderPlot({
    ggplot(myData, aes(x = humidity, fill = label)) +
      geom_histogram(binwidth = 2, alpha = 0.7, position = "identity") +
      labs(title = "Crop Distribution by Humidity",
           x = "Humidity",
           y = "Count",
           fill = "Crop Type") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Bar plot for dashboard
  output$bar_plot <- renderPlot({
    req(input$N, input$label)
    
    # Create a bar plot based on selected variables
    if(input$label == "label") {
      # If label is selected as Y, show count of crops
      ggplot(myData, aes_string(x = input$N, fill = input$label)) +
        geom_histogram(bins = 30, alpha = 0.7) +
        labs(title = paste("Distribution of", input$N, "by Crop Type"),
             x = input$N,
             y = "Count",
             fill = "Crop Type") +
        theme_minimal()
    } else {
      # For other Y variables, use scatter plot
      ggplot(myData, aes_string(x = input$N, y = input$label)) +
        geom_point(alpha = 0.6, color = "steelblue") +
        geom_smooth(method = "lm", color = "red", se = FALSE) +
        labs(title = paste(input$label, "by", input$N),
             x = input$N,
             y = input$label) +
        theme_minimal()
    }
  })
  
  # Data explanation
  output$data_explanation <- renderPrint({
    cat("Crop Recommendation Dataset Explanation:\n")
    cat("========================================\n")
    cat("This dataset contains parameters for crop recommendation.\n")
    cat("Variables include:\n")
    cat("- N: Nitrogen content in soil\n")
    cat("- P: Phosphorus content in soil\n")
    cat("- K: Potassium content in soil\n")
    cat("- temperature: Temperature in Celsius\n")
    cat("- humidity: Humidity percentage\n")
    cat("- ph: Soil pH value\n")
    cat("- rainfall: Rainfall in mm\n")
    cat("- label: Crop type (target variable)\n")
    cat("\nThe dataset helps recommend suitable crops based on environmental\n")
    cat("and soil conditions using machine learning algorithms.")
  })
}

# Run the Shiny app
shinyApp(ui, server)