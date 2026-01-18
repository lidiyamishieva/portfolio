library(tidyverse)
library(shiny)
library(eurostat)
library(giscoR)


ui <- fluidPage(
  
    titlePanel("ADD TITLE"),

    sidebarLayout(
      
      # input
      sidebarPanel(
        # checkbox
        radioButtons(
          inputId = "selected_nuts_level",
          label = "Select NUTS level:",
          choices = c("NUTS 1", "NUTS 2", "NUTS 3")),
        # dropdown
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

eurostat_database <- get_eurostat_toc() 

server <- function(input, output, session) {
  
  # workflow to update the choice of data based on the selected nuts level
  observeEvent(input$selected_nuts_level, {
    
    filtered_datasets <- eurostat_database %>%
      filter(type %in% c("table", "dataset")) %>%
      filter(str_detect(title, regex(input$selected_nuts_level, ignore_case = TRUE))
      )
    
    choices <- filtered_datasets$title
    
    updateSelectInput(
      session,
      inputId = "selected_var",
      choices = sort(choices),
      selected = ""
    )
  })
}

shinyApp(ui = ui, server = server)
