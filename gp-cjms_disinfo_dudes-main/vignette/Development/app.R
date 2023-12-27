# Load libraries
library(tidyverse)
library(shiny)
library(dplyr)
library(bslib)

# Load data
development_data <- read_csv("../../data/data_final.csv")

unique(development_data$`Country name`) ->
  country_list

names(development_data)[5:16] ->
  economic_variables

# Load functions
source("functions.R")

# Define UI for application that includes instruction, country comparison and development visualizer tabs. 
ui <- fluidPage(
  
  theme = bs_theme(version = 5, bootswatch = "sandstone"),
  
  # App title
  titlePanel("Development Differences Visualizer"),
  
  # Tabs
  tabsetPanel(
    tabPanel("Instructions",
             # Use HTML tags for content and structure
             tags$div(
               tags$h2("About the App"),
               tags$p('The "Development Differences Visualizer" R Shiny App allows users to explore and compare economic development indicators across countries worldwide. This tutorial will guide you through using the app to extract meaningful insights about global development.'),
               tags$h2("How to Use the App"),
               tags$p("Follow the steps below to learn how to interact with the app:"),
               
               # Tutorial steps
               tags$ol(
                 tags$li("Select the \"Country Comparison\" tab to compare two countries that have diverged over time, or the \"Development Visualizer\" tab to examine relationships between variables."),
                 tags$li("Use the dropdown menus to choose the countries and economic indicators."),
                 tags$li("Interpret the results using the provided statistics and visual aids below the plots.")
               ),
               # Images for tutorial
               tags$h3("Description of Variables"), 
               tags$p("The \"Development Differences Visualizer\" app processes a rich and diverse set of data, spanning multiple decades and covering a wide range of development indicators. The data is drawn from the Human Development Reports and the World Bank."),
               tags$img(src = "VariableTable.png", height = "600px")
               )
    ),
    
    # Tab 2: Country Comparison
    tabPanel("Country Comparison",
             sidebarLayout(
               sidebarPanel(
                 selectInput("country1", "Select country 1:",
                             choices = country_list,
                             selected = "Argentina"),
                 selectInput("country2", "Select country 2:",
                             choices = country_list,
                             selected = "Spain"),
                 uiOutput("suggested"),
                 selectizeInput("variables1", "Select 1-3 variables:",
                                choices = economic_variables, multiple = TRUE,
                                selected = "GDP per capita",
                                options = list(maxItems = 3)),
                 radioButtons("view_mode", "View mode:",
                              choices = c("Both Countries", "Difference")),
                 checkboxInput("normalize", "Normalize values to 100 in reference year", 
                               FALSE),
                 htmlOutput("ref_year")
               ),
               mainPanel(
                 plotOutput("timeSeriesPlot")
               )
             )
    ),
    
    # Tab 3: Development Visualizer
    tabPanel("Development Visualizer",
             sidebarLayout(
               sidebarPanel(
                 selectInput("year_filter", "Year", 
                             choices = unique(development_data$Year),
                             selected = 2022),
                 selectInput("region_filter", "Region:",
                             choices = c("All", unique(development_data$Region)),
                             selected = "All"),
                 selectInput("x_variable", "X Variable",
                             choices = economic_variables, 
                             selected = "Ages 15-64 proportion"),
                 checkboxInput("log_x_scale", "Use log scale for X", FALSE),
                 selectInput("y_variable", "Y variable",
                             choices = economic_variables,
                             selected = "GDP per capita"),
                 checkboxInput("log_y_scale", "Use log scale for Y", TRUE),
                 selectInput("color_variable", "Color by:",
                             choices = c("None", names(development_data)[-1]),
                             selected = "Region"),
                 selectInput("Size_variable", "Size by:",
                             choices = c("None", economic_variables),
                             selected = "GDP"),
                 checkboxInput("run_regression", "Run linear regression", TRUE)
               ),
               mainPanel(
                 # Output for new scatter plot
                 plotOutput("scatterPlot"),
                 htmlOutput("regressionSummaryTable"),
                 htmlOutput("regressionSummaryText")
               )
             )
    )
  )
)

# server

server <- function(input, output, session) {

variables <- reactive({
  validate(need(input$variables1, message = "Please select 1-3 variables."))
  for (i in seq_along(input$variables1)) {
    num_values_country1 <- sum(!is.na(development_data[[input$variables1[i]]][development_data[["Country name"]] == input$country1]))
    num_values_country2 <- sum(!is.na(development_data[[input$variables1[i]]][development_data[["Country name"]] == input$country2]))
    validate(
      need((num_values_country1 >= 2) | (num_values_country2 >= 2),
           message = str_c("Not enough data on ", input$variables1[i], 
                           " for ", input$country1, " or ", input$country2, 
                           ". Please choose another variable or other countries."))
    )
    validate(
      need(num_values_country1 >= 2,
           message = str_c("Not enough data on ", input$variables1[i], 
                           " for ", input$country1, 
                           ". Please choose another variable or another country."))
    )
    validate(
      need(num_values_country2 >= 2,
           message = str_c("Not enough data on ", input$variables1[i], 
                           " for ", input$country2, 
                           ". Please choose another variable or another country."))
    )
  }
  input$variables1
})
  
output$timeSeriesPlot <- renderPlot({
  plot_tab2(.data = development_data, 
            .countries = c(input$country1, input$country2), 
            .indicators = variables(),
            .view = input$view_mode,
            .year = input$ref_year,
            .normalize = input$normalize)
}, height = reactive({length(input$variables1) * 400}))

suggestions <- reactive({
  validate(need(input$variables1, "Suggested:"))
  get_divergence_examples(development_data, input$country1, input$variables1[1])
})

output$suggested <- renderUI({
  HTML(suggestions())
})

output$ref_year <- renderUI({
  if (input$normalize) {
    sliderInput("ref_year", "Reference year:", value = 2022, 
                min = 1960, max = 2022, sep = "")
  }
})

# Logic for Development Visualizer Plot
year_filter <- reactive({
  num_values_x_var <- development_data %>%
    filter(Year == input$year_filter) %>%
    select(input$x_variable) %>%
    drop_na %>%
    nrow
  num_values_y_var <- development_data %>%
    filter(Year == input$year_filter) %>%
    select(input$y_variable) %>%
    drop_na %>%
    nrow
  validate(need((num_values_x_var > 0) | (num_values_y_var > 0),
                str_c("No data on ", input$x_variable, " and ", input$y_variable,
                      " for ", input$year_filter,
                      ". Please choose another variable or another year.")))
  validate(need(num_values_x_var > 0,
                str_c("No data on ", input$x_variable, " for ", input$year_filter,
                      ". Please choose another variable or another year.")))
  validate(need(num_values_y_var > 0,
                str_c("No data on ", input$y_variable, " for ", input$year_filter,
                      ". Please choose another variable or another year.")))
  input$year_filter
})

output$scatterPlot <- renderPlot({
  # Ensure that X and Y variables are selected
  if (is.null(input$x_variable) || is.null(input$y_variable)) {
    return(NULL)
  }
  
    Dev_visualizer(data = development_data,
                   year_range = year_filter(),
                   region = input$region_filter,
                   x_var = input$x_variable, 
                   y_var = input$y_variable, 
                   color_var = input$color_variable, 
                   log_x = input$log_x_scale, 
                   log_y = input$log_y_scale,
                   size_var = input$Size_variable,
                   add_reg_line = input$run_regression)
  })

output$regressionSummaryTable <- renderTable({
  if (input$run_regression) {
    regressionSummary(input_data = development_data,
                      x_var = input$x_variable,
                      y_var = input$y_variable,
                      year = input$year_filter,
                      region = input$region_filter,
                      log_x = input$log_x_scale,
                      log_y = input$log_y_scale)[[1]]
  } else {
    return(NULL)}
})

output$regressionSummaryText <- renderText({
  if (input$run_regression) {
    regressionSummary(input_data = development_data,
                      x_var = input$x_variable,
                      y_var = input$y_variable,
                      year = input$year_filter,
                      region = input$region_filter,
                      log_x = input$log_x_scale,
                      log_y = input$log_y_scale)[[2]]
  } else {
    return(NULL)}
})
}

# Run the application 
shinyApp(ui = ui, server = server)
