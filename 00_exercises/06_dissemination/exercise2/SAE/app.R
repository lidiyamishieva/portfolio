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

eurostat_database <- get_eurostat_toc() 
eurostat_database <- eurostat_database %>% 
  filter(type %in% c("table", "dataset") & str_detect(title, regex("nuts", ignore_case = TRUE)))

eurostat_datasets <- setNames(eurostat_database$code, eurostat_database$title)


ui <- fluidPage(
  
  titlePanel("Small Area Estimation using Eurostat data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_var",
        label = "Select a Eurostat dataset:",
        choices = eurostat_datasets,
        selected = "nama_10_gdp"
      )
    ),
    
    mainPanel(
      verbatimTextOutput("selected_var_output")
  )
  )
)
  
  

## SERVER -----

server <- function(input, output) {}

## APP -----

shinyApp(ui = ui, server = server)








