library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library("leaflet")

hdfc <- read.csv('file path', 
                 stringsAsFactors = FALSE, header = TRUE)

# Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "HDFC Co-ops for Sale March 2023")

# Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Currently for sale", icon = icon("send", lib = 'glyphicon'), 
             href = "https://streeteasy.com/for-sale/nyc/area:200,300,100,400,500%7Cincome_restricted:yes")
  )
)

frow1 <- fluidRow(
  valueBoxOutput("value1"),
  valueBoxOutput("value2"),
  valueBoxOutput("value3"),
  valueBoxOutput("value4"),
  valueBoxOutput("value5"),
  valueBoxOutput("value6"),
  valueBoxOutput("value7"),
  valueBoxOutput("value8"),
  valueBoxOutput("value9")
)

frow2 <- fluidRow( 
  box(
    title = "Map of Locations",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    leafletOutput("locationMap", height = "500px")
  ),
  box(
    title = "Number of Bedrooms",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    plotOutput("no_bedrooms", height = "500px")
  )
)

# Combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)

# Complete the UI part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin = "blue")

# Create the server functions for the dashboard  
server <- function(input, output) {
  
  # Data manipulation to derive the values of KPI boxes
  min_sale_price <- min(hdfc$Sale_price)
  max_sale_price <- max(hdfc$Sale_price)
  avg_maintenance_fee <- mean(hdfc$Monthly_maintenance_fee)
  sum_units <- count(hdfc)
  min_maintenance_fee <- min(hdfc$Monthly_maintenance_fee)
  max_maintenance_fee <- max(hdfc$Monthly_maintenance_fee)
  avg_max_ami <- mean(hdfc$AMI_max)
  min_ami <- min(hdfc$AMI_max)
  max_ami <- max(hdfc$AMI_max)
  
  
  # Creating the valueBoxOutput content
  # color options: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, 
  # lime, orange, fuchsia, purple, maroon, black.
  output$value1 <- renderValueBox({
    valueBox(
      formatC(sum_units, format = "d", big.mark = ','),
      "Number of Co-ops for Sale",
      icon = icon("home", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$value2 <- renderValueBox({
    valueBox(
      paste0("$", 
      formatC(min_sale_price, format = "d", big.mark = ',')),
      "Min. Sale Price",
      icon = icon("stats", lib = "glyphicon"),
      color = "teal"
    )
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      paste0("$",
      formatC(max_sale_price, format = "d", big.mark = ',')),
      "Max. Sale Price",
      icon = icon("stats", lib = "glyphicon"),
      color = "teal"
    )
  })
  
  
  output$value4 <- renderValueBox({
    valueBox(
      paste0("$",
      formatC(avg_maintenance_fee, format = "d", big.mark = ',')),
      "Average Monthly Maintenance Fee",
      icon = icon("usd", lib = "glyphicon"),
      color = "olive"
    )
  })
  
  output$value5 <- renderValueBox({
    valueBox(
      paste0("$",
      formatC(min_maintenance_fee, format = "d", big.mark = ',')),
      "Min. Monthly Maintenance Fee",
      icon = icon("usd", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
  output$value6 <- renderValueBox({
    valueBox(
      paste0("$",
      formatC(max_maintenance_fee, format = "d", big.mark = ',')),
      "Max. Monthly Maintenance Fee",
      icon = icon("usd", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
  output$value7 <- renderValueBox({
    valueBox(
      formatC(avg_max_ami, format = "d", big.mark = ','),
      "Average Max. Area Median Income % Allowed",
      # icon = icon("usd", lib = "glyphicon"),
      color = "navy"
    )
  })
  
  output$value8 <- renderValueBox({
    valueBox(
      formatC(min_ami, format = "d", big.mark = ','),
      "Lowest Max. Area Median Income % Allowed",
      #icon = icon("usd", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  output$value9 <- renderValueBox({
    valueBox(
      formatC(max_ami, format = "d", big.mark = ','),
      "Highest Max. Area Median Income % Allowed",
      #icon = icon("usd", lib = "glyphicon"),
      color = "purple"
    )
  })
  # Creating the plotOutput content
  
  output$locationMap <- renderLeaflet({
    leaflet(data = hdfc) %>%
      addTiles() %>%
      addMarkers(lng = ~lon, lat = ~lat, popup = paste0("Address: ", hdfc$Address, "<br>",
                                                        "Sale Price: $", formatC(hdfc$Sale_price, format = "d", big.mark = ","), "<br>",
                                                        "Monthly Maintenance Fee: $", hdfc$Monthly_maintenance_fee, "<br>",
                                                        "AMI Max: ", hdfc$AMI_max))
  })
  
  output$no_bedrooms <- renderPlot({
    ggplot(data = hdfc, aes(x = No_of_bedrooms)) +
      geom_bar(fill = "#6495ED") +
      scale_x_discrete(limits = 0:5) +  # Set the x-axis limits to 0 to 5
      scale_y_continuous(breaks = seq(0, max(hdfc$No_of_bedrooms), by = 10)) +  
      # Set the y-axis breaks every 10 values
      ylab("Count") +
      xlab("Number of Bedrooms") +
      theme(
        plot.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 10)
      ) +
      ggtitle("Number of Bedrooms")
  })
}

shinyApp(ui, server)
         
