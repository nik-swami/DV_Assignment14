library(shiny)
library(rworldmap)
library(ggplot2)
library(dplyr)
library(randomForest)

# Load dataset
tb_data <- read.csv("TB_Burden_Country_1.csv")
print(colnames(tb_data))

# Shiny app
ui <- fluidPage(
  titlePanel("Global Tuberculosis Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", 
                  choices = unique(tb_data$Country)),
      numericInput("future_year", "Predict for Year:", 
                   value = 2025, min = 2014, max = 2050)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Global Map", plotOutput("mapPlot")),
        tabPanel("Time Series", plotOutput("timeSeriesPlot")),
        tabPanel("ML Prediction", plotOutput("mlPredictionPlot"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Global Map
  output$mapPlot <- renderPlot({
    latest_year <- max(tb_data$Year)
    tb_latest <- subset(tb_data, Year == latest_year)
    
    worldMap <- joinCountryData2Map(tb_latest,
                                    joinCode = "ISO3",
                                    nameJoinColumn = "ISO.3.character.country.territory.code")
    
    green_colors <- colorRampPalette(c("white", "lightgreen", "green", "darkgreen"))(5)
    
    mapCountryData(worldMap, 
                   nameColumnToPlot = "Estimated.prevalence.of.TB..all.forms..per.100.000.population..high.bound", 
                   mapTitle = paste("Global TB Prevalence (per 100,000 people) in", latest_year),
                   colourPalette = green_colors,
                   catMethod = "fixedWidth",
                   addLegend = TRUE)
  })
  
  # Time Series Plot
  output$timeSeriesPlot <- renderPlot({
    country_data <- tb_data %>% 
      filter(Country == input$country) %>%
      select(Year, Estimated.prevalence.of.TB..all.forms..per.100.000.population..high.bound)
    
    ggplot(country_data, aes(x = Year, y = Estimated.prevalence.of.TB..all.forms..per.100.000.population..high.bound)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = paste("TB Prevalence Over Time in", input$country),
           x = "Year",
           y = "TB Prevalence (per 100,000 people)")
  })
  
  # ML Prediction Plot
  output$mlPredictionPlot <- renderPlot({
    country_data <- tb_data %>% 
      filter(Country == input$country) %>%
      select(Year, Estimated.prevalence.of.TB..all.forms..per.100.000.population..high.bound)
    
    # Train Random Forest model
    rf_model <- randomForest(Estimated.prevalence.of.TB..all.forms..per.100.000.population..high.bound ~ Year, 
                             data = country_data, 
                             ntree = 100)
    
    # Generate future years for prediction
    future_years <- data.frame(Year = seq(max(country_data$Year) + 1, input$future_year))
    
    # Make predictions
    predictions <- predict(rf_model, newdata = future_years)
    
    # Combine actual and predicted data
    plot_data <- rbind(
      data.frame(Year = country_data$Year, 
                 Prevalence = country_data$Estimated.prevalence.of.TB..all.forms..per.100.000.population..high.bound, 
                 Type = "Actual"),
      data.frame(Year = future_years$Year, 
                 Prevalence = predictions, 
                 Type = "Predicted")
    )
    
    ggplot(plot_data, aes(x = Year, y = Prevalence, color = Type)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = paste("TB Prevalence Prediction for", input$country),
           x = "Year",
           y = "TB Prevalence (per 100,000 people)")
  })
}

shinyApp(ui = ui, server = server)
