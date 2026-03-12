library(shiny)
library(bslib)
library(sf)
library(tidyverse)
library(leaflet)

housing_data <- st_read("data/non-market-housing.geojson") |> 
  mutate(
    Adaptable  = rowSums(across(contains("Adaptable")), na.rm = TRUE),
    Accessible = rowSums(across(contains("Accessible")), na.rm = TRUE),
    Standard   = rowSums(across(contains("Standard")), na.rm = TRUE)
  ) |> 
  drop_na(name)

local_areas <- st_read("data/local-area-boundary.geojson") |>
  select(-geo_point_2d)

joined_data = st_join(housing_data, local_areas) |> 
  rename(name = name.x, local_area = name.y) |> 
  drop_na(local_area)

housing_wgs84 <- joined_data |> st_transform(crs = 4326)

ui <- page_sidebar(
  title = "Non-Market Housing Dashboard",
  sidebar = sidebar(
    selectizeInput(
      "select_local_area",
      "Local Area:",
      joined_data$local_area,
      multiple = TRUE
    )
  ),
  value_box(
    title = "Total Projects",
    value = textOutput("total_developments"),
    showcase = bsicons::bs_icon("building"),
    theme = "primary"
  ),
  leafletOutput(
    outputId = "housingMap", height = "85vh"
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    if (is.null(input$select_local_area) || length(input$select_local_area) == 0) {
      housing_wgs84
    } else {
      housing_wgs84 |> filter(local_area %in% input$select_local_area)
    }
  })
  
  output$total_developments <- renderText({
    scales::comma(nrow(filtered_data()))
  })
  
  output$housingMap <- renderLeaflet({
    leaflet(filtered_data()) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addMarkers(
        popup = ~paste0(
          "<strong>", name, "</strong><br>",
          address, "<br>",
          "<a href='", url, "' target='_blank'>View details</a>"
        )
      )
  })
}

shinyApp(ui = ui, server = server)