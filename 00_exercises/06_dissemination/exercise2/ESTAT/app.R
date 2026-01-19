options(scipe=99999)

library(tidyverse)
library(shiny)
library(eurostat)
library(giscoR)
library(sf)

eurostat_database <- get_eurostat_toc() %>% filter(type %in% c("table", "dataset"))
nuts <- giscoR::gisco_get_nuts(resolution = "20")


ui <- fluidPage(
  
    titlePanel("Geography of EU data"),

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
        ),
        # dropdown
        selectInput(
          inputId = "selected_year",
          label = "Select a year:",
          choices = NULL
        )
    ),
        # output
        mainPanel(plotOutput("map"))
    )
)

server <- function(input, output, session) {

  # update dataset list based on NUTS level
  # show only datasets for selected NUTS level
  observeEvent(input$selected_nuts_level, {

    filtered_datasets <- eurostat_database %>%
      filter(str_detect(title, regex(input$selected_nuts_level, ignore_case = TRUE)))

    updateSelectInput(
      session,
      inputId = "selected_var",
      choices = sort(filtered_datasets$title),
      selected = NULL
    )
  })

  selected_dataset_id <- reactive({
    req(input$selected_var)

    eurostat_database %>%
      filter(title == input$selected_var) %>%
      pull(code) %>%
      first()
  })

  raw_data <- reactive({
    req(selected_dataset_id())

    get_eurostat(id = selected_dataset_id())
  })

  selected_levl_code <- reactive({
    req(input$selected_nuts_level)
    
    case_when(
      input$selected_nuts_level == "NUTS 1" ~ 1,
      input$selected_nuts_level == "NUTS 2" ~ 2,
      input$selected_nuts_level == "NUTS 3" ~ 3
    )
  })
  
  # update year selector from dataset
  observeEvent(raw_data(), {

    years <- raw_data() %>%
      mutate(YEAR = substr(TIME_PERIOD, 1, 4)) %>%
      pull(YEAR) %>%
      unique() %>%
      sort()

    updateSelectInput(
      session,
      inputId = "selected_year",
      choices = years)
  })
  

  dataset_prepared <- reactive({
    req(raw_data(), input$selected_year, selected_levl_code())
    
    raw_data() %>%
      mutate(YEAR = substr(TIME_PERIOD, 1, 4)) %>%
      left_join(nuts) %>%
      # st_as_sf() %>%
      filter(
        YEAR == input$selected_year,
        LEVL_CODE == selected_levl_code()
      )
  })
  
  nuts_filtered <- reactive({
    req(selected_levl_code())
    
    nuts %>% filter(LEVL_CODE == selected_levl_code())
  })
  
  map_data <- reactive({
    req(dataset_prepared(), nuts_filtered())
    
    left_join(
      nuts_filtered(),
      dataset_prepared()
    )
  })
  
  output$map <- renderPlot({
    req(map_data())
    
   plot(map_data()["values"], main="")
    
  })
  
}


shinyApp(ui = ui, server = server)
