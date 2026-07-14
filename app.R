library(shiny)
library(tidyverse)
library(vroom)
library(maps)
library(ggrepel)
library(sf)
library(janitor)
library(leaflet)
library(bslib)
library(plotly)
library(shinyWidgets)
library(DT)

my_sf <- read_rds("data/my_sf.rds") 
data <- read_rds("data/collisions_data.rds") 

ui <- page_navbar(
  nav_panel("Home",
  div(
    style = "position: flex; justify-content: center; align-items: center; height: 100vh;",
    leafletOutput("map", height = "100%"),
    absolutePanel(
        top = "15%", right = "5%", width = "20%", height = "80%",
        style = "background: rgba(0,0,0,0.6); color: white; padding: 15px; border-radius: 8px;",
        h3("City-View Explorer"),
        selectInput("metric", "Metric:", choices = c("Collisions" = "count", 
                                                     "Injuries" = "injuries", 
                                                     "Casualties" = "deaths")),
        chooseSliderSkin(
        skin = "Square",
        color = "White"
        ),
        sliderInput("date_range", NULL, min = min(data$crash_date), max = max(data$crash_date), 
                    value = c(min(data$crash_date), max(data$crash_date))
                  ),
        DTOutput("boroughtable", height = "60%"))
  )),
  nav_panel("Compare"
  ),
  nav_panel("Data Finder"
  ),
  title = "NYC Collisions"
)
server <- function(input, output, session) {
  map_data <- reactive({
    data |> 
    filter(crash_date >= input$date_range[1],
           crash_date <= input$date_range[2]) |>
    group_by(crash_date, geometry, geoname, borough) |>
    summarize(count = n(),
    deaths = sum(number_of_persons_killed),
    injuries = sum(number_of_persons_injured),
    .groups = "drop") |> 
    group_by(geometry, geoname, borough) |>
    summarize(count = sum(count),
    deaths = sum(deaths),
    injuries = sum(injuries), 
    .groups = "drop") |> 
    st_as_sf()}
  )   
  output$map <- renderLeaflet({
    data <- map_data()
    metric <- input$metric
    data$value <- data[[metric]]
    pal <- colorNumeric(
    palette = "YlOrRd", 
    domain = data$value)
    
    leaflet(data, options = leafletOptions(minZoom = 10)) |>
    addTiles() |>
    addPolygons(
    fillColor = ~pal(value),
    fillOpacity = 0.8,
    color = "white",
    weight = 1,
    popup = ~paste0("Borough: ", borough, "<br>Neighborhood: ", geoname, "<br>Value: ", value)) |>
    setView( lng = -73.9570
           , lat = 40.708116
           , zoom = 11 ) |>
  setMaxBounds( lng1 = -75.9374
                , lat1 = 39.3682
                , lng2 = -71.7187
                , lat2 = 42.0329 )

  })
  output$boroughtable <- renderDT({
    metric <- input$metric

    map_data() |>
      st_drop_geometry() |>
      group_by(borough) |>
      summarize(total = sum(.data[[metric]]), .groups = "drop") |>
      rename(Borough = borough, Total = total) |>
      arrange(desc(Total)) |>
      datatable(options = list(dom = 't'), 
      caption = "Boroughs by Frequency" )
  })

}
shinyApp(ui, server)