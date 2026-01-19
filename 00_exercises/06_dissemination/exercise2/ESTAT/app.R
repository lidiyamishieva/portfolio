options(scipe=99999)

library(tidyverse)
library(shiny)
library(eurostat)
library(giscoR)
library(sf)
library(gtsummary)

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
        mainPanel(
          plotOutput("map"),
            br(),
            tableOutput("descriptives")
        )
    )
)


server <- function(input, output, session) {

  # update dataset list based on NUTS level
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
  
  # update year selector based on available data
  observeEvent(input$selected_var, {
    req(input$selected_var, input$selected_nuts_level) 
    
    filtered_datasets <- eurostat_database %>%
      filter(title==input$selected_var)

    start_year <- unique(as.numeric(substr(filtered_datasets$data.start, 1, 4)))
    end_year   <- unique(as.numeric(substr(filtered_datasets$data.end, 1, 4)))
    
    years <- seq(start_year, end_year, by = 1)

    updateSelectInput(
      session,
      inputId = "selected_year",
      choices = years,
      selected = NULL
    )
  })
  
  
  # get a dataset via the selected dataset name
  # we need this because we want to filter by year, but there are different
  # years avialable for each dataset
  # selected_dataset_id <- reactive({
  #   req(input$selected_var)
  # 
  #   eurostat_database %>%
  #     filter(title == input$selected_var) %>%
  #     pull(code) %>%
  #     first()
  # })

  # selected_levl_code <- reactive({
  #   req(input$selected_nuts_level)
  #   
  #   case_when(
  #     input$selected_nuts_level == "NUTS 1" ~ 1,
  #     input$selected_nuts_level == "NUTS 2" ~ 2,
  #     input$selected_nuts_level == "NUTS 3" ~ 3
  #   )
  # })
  # 
  # raw_data <- reactive({
  #   req(selected_dataset_id())
  #   get_eurostat(id = selected_dataset_id()) %>%
  #     mutate(YEAR = substr(TIME_PERIOD, 1, 4))
  # })
  # 
  # # update year selector from dataset
  # observeEvent(raw_data(), {
  # 
  #   years <- raw_data() %>%
  #     pull(YEAR) %>%
  #     unique() %>%
  #     sort()
  # 
  #   updateSelectInput(
  #     session,
  #     inputId = "selected_year",
  #     choices = years)
  # })
  # 
  # nuts_filtered <- reactive({
  #   req(selected_levl_code())
  #   
  #   nuts %>% filter(LEVL_CODE == selected_levl_code())
  # })
  # 
  # map_data <- reactive({
  #   req(raw_data(), input$selected_year, selected_levl_code())
  #   
  #   raw_data() %>%
  #     left_join(nuts_filtered(), by = c("geo" = "NUTS_ID")) %>%
  #     filter(
  #       YEAR == input$selected_year,
  #       LEVL_CODE == selected_levl_code()
  #     ) %>%
  #     st_as_sf()
  # })
  # 
  # output$map <- renderPlot({
  #   req(map_data())
  #   
  #  plot(map_data()["values"], main="")
  #   
  # })
  # 
  # output$descriptives <- renderTable({
  #   raw_data() %>%
  #    # st_drop_geometry() %>%
  #     select(values) %>%
  #     tbl_summary()
  # })
}

shinyApp(ui = ui, server = server)
