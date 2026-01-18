library(shiny)
library(eurostat)
library(giscoR)


ui <- fluidPage(
  
    # app title  
    titlePanel("Old Faithful Geyser Data"),

    sidebarLayout(
      sidebarPanel(
        
        selectInput(
          inputId = "selected_nuts_level",
          label = "Select NUTS level:",
          choices = c("NUTS 1", "NUTS 2", "NUTS 3"),
          selected = "NUTS 1"
          ),
        
        selectInput(
          inputId = "selected_var",
          label = "Select a Eurostat dataset:",
          choices = NULL
        )
    ),
        
        # output
        mainPanel(
          "[plot placeholder]", plotOutput("map"),
          br(),
          br(),
          "[table placeholder]", tableOutput("descriptives")
        )
    )
)

server <- function(input, output) {}

# run the application 
shinyApp(ui = ui, server = server)
