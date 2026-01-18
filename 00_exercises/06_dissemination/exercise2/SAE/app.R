library(shiny)
library(tidyverse)
library(eurostat)

# # TEMPLATE -----

## UI -----

# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )

## SERVER -----

# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
#     })
# }

## APP -----

# # Run the application
# shinyApp(ui = ui, server = server)

# SAE APP -----

## UI -----

ui <- fluidPage(
  
  titlePanel("Small Area Estimation using Eurostat data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_nuts",
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
    
    mainPanel(
      verbatimTextOutput("selected_var_output")
    )
  )
)

  
  

## SERVER -----

# server <- function(input, output) {}

eurostat_database <- get_eurostat_toc() 

server <- function(input, output, session) {

  observeEvent(input$selected_nuts, {

    # filter datasets based on selected NUTS level
    filtered_datasets <- eurostat_database %>%
      filter(type %in% c("table", "dataset")) %>%
      filter(str_detect(title, regex(input$selected_nuts, ignore_case = TRUE))
      )

    choices <- filtered_datasets$title

    # update second dropdown
    updateSelectInput(
      session,
      inputId = "selected_var",
      choices = sort(choices),
      selected = ""
    )
  })

  output$selected_var_output <- renderPrint({
    list(
      dataset = input$selected_var
    )
  })
}


## APP -----

shinyApp(ui = ui, server = server)








