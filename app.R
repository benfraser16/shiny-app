library(shiny)
library(xgboost)
library(ggplot2)
library(dplyr)
library(caret)
library(readxl)
library(tidyr)
library(leaflet)

# Load the dataset
housing <- read.csv("https://raw.githubusercontent.com/benfraser16/shiny-app/refs/heads/main/shiny%20deployment%20set.csv")

housing_clean <- housing %>% drop_na()
# Remove rows with "#N/A" in any column
housing_clean <- housing_clean %>%
  filter_all(all_vars(. != "#N/A"))

housing_clean <- housing_clean %>%
  select(-longitude, -latitude)

str(housing_clean[, c("bedrooms", "bathrooms", "distanceToCoast", "LivingArea")])
housing_clean$bedrooms <- as.numeric(as.character(housing_clean$bedrooms))
housing_clean$LivingArea <- as.numeric(as.character(housing_clean$LivingArea))

# Perform train-test split
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(housing_clean$price, p = 0.8, list = FALSE)
train_data <- housing_clean[trainIndex, ]
test_data <- housing_clean[-trainIndex, ]

# Prepare training matrix
train_matrix <- xgb.DMatrix(data = as.matrix(train_data[, c("bedrooms", "bathrooms",
                                                            "distanceToCoast", "LivingArea")]),
                            label = train_data$price)

# Train the XGBoost model
xgb_model <- xgboost(data = train_matrix, objective = "reg:squarederror", nrounds = 100)



# Define UI for the application
ui <- fluidPage(
  titlePanel("Home Price Prediction"),
  
  # Use tabsetPanel for multiple tabs
  tabsetPanel(
    tabPanel("Price Prediction",
             sidebarLayout(
               sidebarPanel(
                 h4("Enter Home Characteristics"),
                 numericInput("bedrooms", "Bedrooms", value = 3, min = 0),
                 numericInput("bathrooms", "Bathrooms", value = 2, min = 0),
                 numericInput("distance_coast", "Distance to Coast (miles)", value = 10, min = 0),
                 numericInput("living_area", "Living Area (sqft)", value = 2000, min = 0),
                 actionButton("predictPrice", "Predict Price")
               ),
               
               mainPanel(
                 h3("Predicted Price"),
                 verbatimTextOutput("predictedPrice"),
                 
                 h3("Matching Homes Map"),
                 leafletOutput("housingMap")
               )
             )
    )
  )
)

# Define server logic for price prediction
server <- function(input, output) {
  
  # Create a reactive value to store the predicted price
  predicted_price_reactive <- reactiveVal(NULL)
  
  # Only update the predicted price when the "Predict Price" button is clicked
  observeEvent(input$predictPrice, {
    new_data <- data.frame(
      bedrooms = input$bedrooms,
      bathrooms = input$bathrooms,
      distanceToCoast = input$distance_coast,
      LivingArea = input$living_area
    )
    
    new_matrix <- xgb.DMatrix(data = as.matrix(new_data))
    predicted_price <- predict(xgb_model, new_matrix)
    predicted_price_reactive(round(predicted_price, 2))  # Store the predicted price in the reactive value
  })
  
  # Render the predicted price when it is updated
  output$predictedPrice <- renderPrint({
    req(predicted_price_reactive())  # Only render if the predicted price exists
    cat("Predicted Price: $", predicted_price_reactive())
  })
  
  # Filter matching homes based on bedrooms and bathrooms
  filtered_data <- eventReactive(input$predictPrice, {
    housing %>%
      filter(
        bedrooms == input$bedrooms,
        bathrooms == input$bathrooms
      )
  })
  
  # Render the map and display color-coded homes
  output$housingMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -119.4179, lat = 36.7783, zoom = 6) %>%
      addLegend(
        position = "bottomright",
        colors = c("green", "red"),
        labels = c("More Expensive", "Cheaper"),
        title = "Price Comparison"
      )
  })
  
  observeEvent(input$predictPrice, {
    data_to_show <- filtered_data()
    predicted_price <- predicted_price_reactive()  # Retrieve the predicted price from reactive value
    
    leafletProxy("housingMap", data = data_to_show) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        color = ~ifelse(price > predicted_price, "green", "red"),
        fillOpacity = 0.5, radius = 5,
        popup = ~paste(
          "<strong>Price:</strong>", price, "<br>",
          "<strong>Bedrooms:</strong>", bedrooms, "<br>",
          "<strong>Bathrooms:</strong>", bathrooms
        )
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)

