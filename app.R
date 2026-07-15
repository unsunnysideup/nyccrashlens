library(shiny)
library(tidyverse)
library(vroom)
library(maps)
library(ggrepel)
library(sf)
library(leaflet)
library(bslib)
library(plotly)
library(shinyWidgets)
library(DT)
library(lubridate)
library(arrow)
library(leaflet.extras)

data <- read_parquet("data/collisions_data.parquet") 
my_sf <- read_rds("data/my_sf.rds")

# data for database
database <- data |>
      select(-c(location)) |>
      filter(!str_detect(contributing_factor_vehicle_1, "\\d")) |>
      mutate(
        geoname = as.factor(geoname),
        borough = as.factor(borough),
        contributing_factor_vehicle_1 = as.factor(contributing_factor_vehicle_1)
      ) |>
      rename(contributing_factor = contributing_factor_vehicle_1) |>
      select(collision_id, crash_date, crash_time, borough, geocode, geoname, longitude, latitude, contributing_factor, everything())

# UI

ui <- page_navbar(
  theme = bs_theme(
    bg = "#101010",
    fg = "#FFF",
    primary = "#000000",
    secondary = "#ffef5fff",
    success = "#95c297ff",
    base_font = font_google("Inconsolata"),
    code_font = font_collection("SFMono-Regular", "Consolas", "monospace"),
    bootswatch = "cyborg",
    font_scale = 1.2)
    |> bs_add_rules("
    .navbar { background-color: #000000ff !important; }
    .navbar { padding-top: 15px; padding-bottom: 15px;}
    .compare_card { background-color: white !important;}
    #compare_sidebar { padding-top: 5px; padding-left: 15px; padding-right: 15px}
    html::-webkit-scrollbar, body::-webkit-scrollbar { display: none; }
    html, body { -ms-overflow-style: none; scrollbar-width: none; }
    #attribution_link {color: #ffef5fff !important;}
  "),
  nav_panel("Home",
  div(
    style = "position: flex; justify-content: center; align-items: center; height: 100vh;",
    leafletOutput("map", height = "100%"),
    absolutePanel(
        top = "15%", right = "5%", width = "20%", height = "90%",
        style = "background: rgba(0,0,0,0.6); color: white; padding: 15px; border-radius: 8px;",
        h3("City-View Explorer"),
        selectInput("metric", "Metric:", choices = c("Collisions" = "count", 
                                                     "Injuries" = "injuries", 
                                                     "Fatalities" = "deaths")),
        chooseSliderSkin(
        skin = "Square",
        color = "White"
        ),
        sliderInput("map_date_range", NULL, min = min(data$crash_date), max = max(data$crash_date), 
                    value = c(min(data$crash_date), max(data$crash_date))
                  ),
        DTOutput("boroughtable", height = "60%"))
  )),
  nav_panel("Compare", 
  layout_sidebar(
    sidebar = sidebar(
      id = "compare_sidebar",
      width = "35%",
      bg = "black",
      fg = "white",
      h3("Comparative Tool"),
      p("Explore how your residing region differs from other regions of interests."),
      sliderInput("date_range", NULL, min = min(data$crash_date), max = max(data$crash_date), 
                  value = c(min(data$crash_date), max(data$crash_date))),
      selectInput("region_type", "Type of Region to Compare", choices = c("Borough" = "borough", "Neighborhood" = "geoname")),
      selectInput("region_1", "Select Region #1", choices = NULL),
      selectInput("region_2", "Select Region #2", choices = NULL)),
    layout_columns(
      col_widths = c(6, 6, 6, 6),
      card(
        class = "compare_card",
        plotlyOutput("hourPlot")),
      card( 
        class = "compare_card",
        plotlyOutput("timePlot")),
      card(
        class = "compare_card",
        plotlyOutput("injuryChart")),
      card( 
        class = "compare_card",
        plotlyOutput("fatalityChart")))
  )),
  nav_panel("Database",
  div(
    style = "background: black; color: #f0e15a; padding: 20px; text-align: center;",
    h3("New York City Collisions Database"),
    p("Data is available for export with filters applied.")
  ),
  card(
    DTOutput("database")
  ),
  div(
    style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 20px;",
    downloadButton("data_exporter", "Download for Export (.csv)"),
    span("Data source: ", a("NYC Open Data - Motor Vehicle Collisions", 
    href = "https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95",
    target = "_blank",
    id = "attribution_link"),
)

  )),
  title = "CrashLens NYC"
)

# Server
server <- function(input, output, session) {
  map_data <- reactive({
    counts <- data |> 
    filter(crash_date >= input$map_date_range[1],
           crash_date <= input$map_date_range[2]) |>
    group_by(crash_date, geoname, borough) |>
    summarize(count = n(),
    deaths = sum(number_of_persons_killed),
    injuries = sum(number_of_persons_injured),
    .groups = "drop") |> 
    group_by(geoname, borough) |>
    summarize(count = sum(count),
    deaths = sum(deaths),
    injuries = sum(injuries), 
    .groups = "drop") |>
    select(geoname, count, deaths, injuries) 
    
    left_join(my_sf, counts, by = "geoname")
  }) 
  lnglat_data <- reactive({
    data |>
      filter(crash_date >= input$map_date_range[1],
           crash_date <= input$map_date_range[2]) |>
      group_by(longitude, latitude) |>
      summarize(count = n(),
      deaths = sum(number_of_persons_killed),
      injuries = sum(number_of_persons_injured),
      .groups = "drop")
  })
  reasons <- reactive({
    data |> 
      filter(crash_date >= input$map_date_range[1],
      crash_date <= input$map_date_range[2], contributing_factor_vehicle_1 != "Unspecified") |>
      group_by(geoname, contributing_factor_vehicle_1) |>
      summarize(count = n(), .groups = "drop") |>
      arrange(desc(count))
  })

  output$map <- renderLeaflet({
    heatmap_data <- lnglat_data()
    leaflet_data <- map_data()
    factor_data <- reasons()

    metric <- input$metric
    leaflet_data$value <- leaflet_data[[metric]]
    heatmap_data$value <- heatmap_data[[metric]]

    pal <- colorNumeric(
    palette = "YlOrRd", 
    domain = leaflet_data$value)
    
    leaflet(leaflet_data, options = leafletOptions(minZoom = 10)) |>
    addTiles() |>
    addPolygons(
    group = "Choropleth",
    fillColor = ~pal(value),
    fillOpacity = 0.8,
    color = "white",
    weight = 1,
    popup = ~unname(mapply(function(b, g, v) {
      top_reasons <- factor_data |>
        filter(geoname == g) |>
        slice(1:3) |>
        pull(contributing_factor_vehicle_1)
      paste0("<b>Borough</b>: ", b, 
             "<br><b>Neighborhood</b>: ", g, 
             "<br><b>Occurrences</b>: ", v,
              "<br><b>Top Three Crash Factors</b>: ", paste(top_reasons, collapse = ", "))
    }, borough, geoname, value, SIMPLIFY = TRUE))) |>
    setView( lng = -73.9570
           , lat = 40.708116
           , zoom = 11 ) |>
  setMaxBounds( lng1 = -75.9374
                , lat1 = 39.3682
                , lng2 = -71.7187
                , lat2 = 42.0329 ) |>
  # Heatmap
  addHeatmap(
    data = heatmap_data,
    lng = ~longitude,
    lat = ~latitude,
    intensity = ~value,
    blur = 1,
    radius = 15,
    minOpacity = 0.35,
    max = max(heatmap_data$value),
    gradient = "Blues",
    group = "Heatmap" # Name this group
  ) |>
  # Layer Control
  addLayersControl(
    position = "topleft",
    overlayGroups = c("Choropleth", "Heatmap"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
      hideGroup("Heatmap") 

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
      st_drop_geometry() |>
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
      layout(title = 'Time Series Plot by Hour',
             legend=list(title=list(text='Region')),
             xaxis = list(title = "Hour of Crash (24-Hr Period)"),
             yaxis = list(title = "Collision Occurences"))
  })
  output$timePlot <- renderPlotly({
    timeData <- comparison_data()
    region_col <- if (input$region_type == "geoname") "geoname" else "borough"

    timeData |>
      group_by(crash_date, region = .data[[region_col]]) |>
      summarize(collisions = n(), .groups = "drop") |>
      plot_ly(x = ~crash_date, y = ~collisions, color = ~region,
      type = 'scatter', mode = 'lines') |>
      layout(title = "Time Series Plot by Date", 
      legend=list(title=list(text='Region')),
      xaxis = list(title = "Date of Crash"),
      yaxis = list(title = "Collision Occurrences"))

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
      layout(title = "Injury Breakdown Bar Chart", 
            barmode = "group", 
            legend=list(title=list(text='Region')),
            xaxis = list(title = "Category"),
            yaxis = list(title = "Injuries"))
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
      layout(title = "Fatality Breakdown Bar Chart", 
            barmode = "group", 
            legend=list(title=list(text='Region')), 
            xaxis = list(title = "Category"),
            yaxis = list(title = "Fatalities"))
  })

  
  output$database <- renderDT ({
    server = TRUE

    datatable(database,
    options = list(
      page_length = 5,
      searching = TRUE,
      ordering = TRUE,
      autoWidth = TRUE
    ),
    selection = 'multiple',
    filter = 'top',
    rownames = TRUE)
  })
  output$data_exporter <- downloadHandler(
    filename = function() {
      "nyc_collisions.csv"
    },
    content = function(file) {
      filtered <- input$database_rows_all
      write.csv(database[filtered, ], file, row.names = FALSE)
  })


}

shinyApp(ui, server)