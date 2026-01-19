# notation of small numbers
options(scipe=99999)

# load libraries
library(tidyverse)
library(shiny)
library(eurostat)
library(giscoR)
library(sf)
library(gtsummary)

# load the meta data about the estat database
eurostat_database <- get_eurostat_toc() %>% filter(type %in% c("table", "dataset"))

# load nuts geometries
nuts <- giscoR::gisco_get_nuts(resolution = "20")

# define the user interface
ui <- fluidPage(
  
    # add a title of the app
    titlePanel("Geography of EU data"),

    sidebarLayout(
      # input
      sidebarPanel(
        # checkbox
        radioButtons(
          inputId = "selected_nuts_level",
          label = "Select NUTS level:",
          choices = c("Country", "NUTS 1")
          ),
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

# define the back-end computation
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
      filter(title==input$selected_var) %>%
      na.omit()

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
  
  # reactive variable for later data filter
  selected_levl_code <- reactive({
    req(input$selected_nuts_level)
    
    case_when(
      input$selected_nuts_level == "Country" ~ 0,
      input$selected_nuts_level == "NUTS 1" ~ 1
    )
    
  })
  
  # get a dataset id
  selected_dataset_id <- reactive({
    req(input$selected_var, input$selected_year)

    eurostat_database %>%
      filter(title == input$selected_var) %>%
      pull(code) %>%
      first()
  })
  
  # produce a reactive dataset for plotting and summarizinf
  map_data <- reactive({
    req(selected_dataset_id(), selected_levl_code())
    
    filtered_nuts <- nuts %>% filter(LEVL_CODE == selected_levl_code())
    
    get_eurostat(id = selected_dataset_id()) %>%
      select(c("geo", "TIME_PERIOD", "values")) %>%
      mutate(YEAR = substr(TIME_PERIOD, 1, 4)) %>%
      left_join(filtered_nuts, by = c("geo" = "NUTS_ID")) %>%
      filter(
        YEAR == input$selected_year,
        LEVL_CODE == selected_levl_code()
        ) %>%
      distinct(geo, .keep_all = TRUE) %>%
      st_as_sf()
  })

  # add a plot
  output$map <- renderPlot({
    req(map_data())
    
    plot(map_data()["values"], main="")

  })
  
  # add a table
  output$descriptives <- renderTable({
    map_data() %>%
      st_drop_geometry() %>%
      select(values) %>%
      tbl_summary(
        missing = "always",
        type = all_continuous() ~ "continuous2",
        statistic = list(
          all_continuous() ~ c("{mean} ({sd})", "{min} - {max}", "{median} ({p25} - {p75})"))
      ) %>%
      modify_header(label = "Descriptive Statistics") %>%
      modify_table_body(~ .x[-1, ])
  })
}

# run the app
shinyApp(ui = ui, server = server)
