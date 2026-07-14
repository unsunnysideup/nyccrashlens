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
library(lubridate)

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
                                                     "Fatalities" = "deaths")),
        chooseSliderSkin(
        skin = "Square",
        color = "White"
        ),
        sliderInput("date_range", NULL, min = min(data$crash_date), max = max(data$crash_date), 
                    value = c(min(data$crash_date), max(data$crash_date))
                  ),
        DTOutput("boroughtable", height = "60%"))
  )),
  nav_panel("Compare", 
  layout_sidebar(
    sidebar = sidebar(
      width = "30%",
      bg = "black",
      fg = "white",
      h3("Comparison Explorer"),
      p("How does the road safety of an area where I live compare to others?"),
      sliderInput("date_range", NULL, min = min(data$crash_date), max = max(data$crash_date), 
                  value = c(min(data$crash_date), max(data$crash_date))),
      selectInput("region_type", "Type of Region to Compare", choices = c("Borough" = "borough", "Neighborhood" = "geoname")),
      selectInput("region_1", "Select Region #1", choices = NULL),
      selectInput("region_2", "Select Region #2", choices = NULL)),
    layout_columns(
      col_widths = c(6, 6, 6, 6),
      card(
        plotlyOutput("hourPlot")),
      card( 
        plotlyOutput("timePlot")),
      card(
        plotlyOutput("injuryChart")),
      card( 
        plotlyOutput("fatalityChart")))
  )),
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
    leaflet_data <- map_data()
    metric <- input$metric
    leaflet_data$value <- leaflet_data[[metric]]
    pal <- colorNumeric(
    palette = "YlOrRd", 
    domain = leaflet_data$value)
    
    leaflet(leaflet_data, options = leafletOptions(minZoom = 10)) |>
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

  ## Page 2
  observeEvent(input$region_type, {
    choices <- if (input$region_type == "geoname") {
      sort(unique(data$geoname))
    } else {
      sort(unique(data$borough))
    }

  updateSelectInput(session, "region_1", choices = choices)
  updateSelectInput(session, "region_2", choices = choices)
  })
  comparison_data <- reactive({
    region_col <- if (input$region_type == "geoname") "geoname" else "borough"

    data |>
      filter(.data[[region_col]] %in% c(input$region_1, input$region_2)) |>
      filter(crash_date >= input$date_range[1], crash_date <= input$date_range[2]) 
  })
  output$hourPlot <- renderPlotly({
    hourData <- comparison_data()
    region_col <- if (input$region_type == "geoname") "geoname" else "borough"
    
    hourData |>
      mutate(crash_hour = hour(crash_time)) |>
      group_by(crash_hour, region = .data[[region_col]]) |>
      summarize(
        collisions = n(),
        .groups = "drop"
      ) |>
      plot_ly(x = ~crash_hour, y = ~collisions, color = ~region, 
        type = 'scatter', mode = 'lines') |>
      layout(title = 'Time Series Plot by Hour',legend=list(title=list(text='Region')))
  })
  output$timePlot <- renderPlotly({
    timeData <- comparison_data()
    region_col <- if (input$region_type == "geoname") "geoname" else "borough"

    timeData |>
      group_by(crash_date, region = .data[[region_col]]) |>
      summarize(collisions = n(), .groups = "drop") |>
      plot_ly(x = ~crash_date, y = ~collisions, color = ~region,
      type = 'scatter', mode = 'lines') |>
      layout(title = "Time Series Plot by Date", legend=list(title=list(text='Region')))

  })
  output$injuryChart <- renderPlotly({
    injuryData <- comparison_data() 
    region_col <- if (input$region_type == "geoname") "geoname" else "borough"

    injuryData |>
      group_by(region = .data[[region_col]]) |>
      summarize(pedestrians = sum(number_of_pedestrians_injured),
      cyclists = sum(number_of_cyclist_injured),
      motorists = sum(number_of_motorist_injured),
      .groups = "drop") |>
      pivot_longer(-region, names_to = "category", values_to = "injuries") |>
      plot_ly(x = ~category, y = ~injuries, color = ~region, type = "bar") |>
      layout(title = "Injury Breakdown Bar Chart", barmode = "group", legend=list(title=list(text='Region')))
  })
  output$fatalityChart <- renderPlotly({
    fatalityData <- comparison_data() 
    region_col <- if (input$region_type == "geoname") "geoname" else "borough"

    fatalityData |>
      group_by(region = .data[[region_col]]) |>
      summarize(pedestrians = sum(number_of_pedestrians_killed),
      cyclists = sum(number_of_cyclist_killed),
      motorists = sum(number_of_motorist_killed),
      .groups = "drop") |>
      pivot_longer(-region, names_to = "category", values_to = "deaths") |>
      plot_ly(x = ~category, y = ~deaths, color = ~region, type = "bar") |>
      layout(title = "Fatality Breakdown Bar Chart", barmode = "group", legend=list(title=list(text='Region')))
  })





}
shinyApp(ui, server)