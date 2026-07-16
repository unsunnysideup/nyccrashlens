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
library(waiter)

# loading all necessary datasets for the app
data <- read_parquet("data/collisions_data.parquet")
my_sf <- read_rds("data/my_sf.rds")

# UI
ui <- page_navbar(
  # loading screen
  header = tagList(
    useWaiter(),
    waiterShowOnLoad(html = spin_folding_cube(), color = "#000000")
  ),

  # customized cyborg theme + styling
  theme = bs_theme(
    bg = "#101010",
    fg = "#FFF",
    primary = "#000000",
    secondary = "#ffef5fff",
    success = "#95c297ff",
    base_font = font_google("Inconsolata"),
    code_font = font_collection("SFMono-Regular", "Consolas", "monospace"),
    bootswatch = "cyborg",
    font_scale = 1.2
  ) |>
    bs_add_rules(
      "
    .navbar { background-color: #000000ff !important; padding-top: 15px; padding-bottom: 15px;}
    .compare_card { background-color: white !important;}
    #compare_sidebar { padding-top: 5px; padding-left: 15px; padding-right: 15px}
    html::-webkit-scrollbar, body::-webkit-scrollbar { display: none; }
    html, body { -ms-overflow-style: none; scrollbar-width: none; }
    #attribution_link {color: #ffef5fff !important;}
    .button {display: flex; justify-content: center; align-items: center;}
  "
    ),

  # home panel for chloropleth + borough rank table
  nav_panel(
    "Home",
    div(
      style = "position: flex; justify-content: center; align-items: center; height: 100vh;",
      # leaflet output
      leafletOutput("map", height = "100%"),
      # side panel
      absolutePanel(
        top = "15%",
        right = "5%",
        width = "20%",
        height = "90%",
        style = "background: rgba(0,0,0,0.6); color: white; padding: 15px; border-radius: 8px;",
        h3("City-View Explorer"),
        selectInput(
          "metric",
          "Metric:",
          choices = c(
            "Collisions" = "count",
            "Injuries" = "injuries",
            "Fatalities" = "deaths"
          )
        ),
        chooseSliderSkin(
          skin = "Square",
          color = "White"
        ),
        sliderInput(
          "map_date_range",
          NULL,
          min = min(data$crash_date),
          max = max(data$crash_date),
          value = c(min(data$crash_date), max(data$crash_date))
        ),
        div(class = "button", actionButton("homeUpdate", "Update Me!")),
        div(
          style = "margin-top: 20px;",
          DTOutput("boroughtable", height = "60%")
        )
      )
    )
  ),

  # compare panel: sidebar + 4 visuals exploring temporal patterns and casualty breakdowns
  nav_panel(
    "Compare",
    layout_sidebar(
      # sidebar for time analysis and selection of two regions
      sidebar = sidebar(
        id = "compare_sidebar",
        width = "35%",
        bg = "black",
        fg = "white",
        h3("Comparative Tool"),
        p(
          "Explore how your residing region differs from other regions of interests."
        ),
        sliderInput(
          "date_range",
          NULL,
          min = min(data$crash_date),
          max = max(data$crash_date),
          value = c(min(data$crash_date), max(data$crash_date))
        ),
        selectInput(
          "region_type",
          "Type of Region to Compare",
          choices = c("Borough" = "borough", "Neighborhood" = "geoname")
        ),
        selectInput("region_1", "Select Region #1", choices = NULL),
        selectInput("region_2", "Select Region #2", choices = NULL),
        div(class = "button", actionButton("compareUpdate", "Update Me!"))
      ),

      # 4 cards, each to a visualization
      layout_columns(
        col_widths = c(6, 6, 6, 6),
        card(
          class = "compare_card",
          plotlyOutput("hourPlot")
        ),
        card(
          class = "compare_card",
          plotlyOutput("timePlot")
        ),
        card(
          class = "compare_card",
          plotlyOutput("injuryChart")
        ),
        card(
          class = "compare_card",
          plotlyOutput("fatalityChart")
        )
      )
    )
  ),

  # database panel: explore data + export with filters applied
  nav_panel(
    "Database",
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
      span(
        "Data source: ",
        a(
          "NYC Open Data - Motor Vehicle Collisions",
          href = "https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95",
          target = "_blank",
          id = "attribution_link"
        ),
      )
    )
  ),
  title = "NYC CrashLens"
)

# Server
server <- function(input, output, session) {
  # waiter for map rendering
  w <- Waiter$new(html = spin_folding_cube(), color = "#000000")

  # reactive data for chloropleth
  map_data <- eventReactive(
    input$homeUpdate,
    {
      w$show()
      counts <- data |>
        filter(
          crash_date >= input$map_date_range[1],
          crash_date <= input$map_date_range[2]
        ) |>
        group_by(crash_date, geoname, borough) |>
        summarize(
          count = n(),
          deaths = sum(number_of_persons_killed),
          injuries = sum(number_of_persons_injured),
          .groups = "drop"
        ) |>
        group_by(geoname, borough) |>
        summarize(
          count = sum(count),
          deaths = sum(deaths),
          injuries = sum(injuries),
          .groups = "drop"
        ) |>
        select(geoname, count, deaths, injuries)

      left_join(my_sf, counts, by = "geoname")
    },
    ignoreNULL = FALSE
  )

  # reactive data for popup crash factor info
  reasons <- eventReactive(
    input$homeUpdate,
    {
      data |>
        filter(
          crash_date >= input$map_date_range[1],
          crash_date <= input$map_date_range[2],
          contributing_factor_vehicle_1 != "Unspecified"
        ) |>
        group_by(geoname, contributing_factor_vehicle_1) |>
        summarize(count = n(), .groups = "drop") |>
        arrange(desc(count))
    },
    ignoreNULL = FALSE
  )

  # leaflet
  output$map <- renderLeaflet({
    req(map_data())
    req(reasons())
    leaflet_data <- map_data()
    factor_data <- reasons()

    # customizing map based on metric selected
    metric <- isolate(input$metric)
    leaflet_data$value <- leaflet_data[[metric]]

    # color scheme
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = leaflet_data$value
    )

    w$hide()

    leaflet(leaflet_data, options = leafletOptions(minZoom = 10)) |>
      addTiles() |>
      addPolygons(
        group = "Choropleth",
        fillColor = ~ pal(value),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        popup = ~ unname(mapply(
          function(b, g, v) {
            top_reasons <- factor_data |>
              filter(geoname == g) |>
              slice(1:3) |>
              pull(contributing_factor_vehicle_1)
            paste0(
              "<b>Borough</b>: ",
              b,
              "<br><b>Neighborhood</b>: ",
              g,
              "<br><b>Total Count</b>: ",
              v,
              "<br><b>Top Three Crash Factors</b>: ",
              paste(top_reasons, collapse = ", ")
            )
          },
          borough,
          geoname,
          value,
          SIMPLIFY = TRUE
        ))
      ) |>
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = leaflet_data$value,
        title = "Count Range",
        opacity = 1
      ) |>
      setView(lng = -73.889, lat = 40.7125, zoom = 11) |>
      setMaxBounds(
        lng1 = -75.9374,
        lat1 = 39.3682,
        lng2 = -71.7187,
        lat2 = 42.0329
      )
  })

  # ranked borough table
  output$boroughtable <- renderDT({
    metric <- isolate(input$metric)

    map_data() |>
      st_drop_geometry() |>
      group_by(borough) |>
      summarize(total = sum(.data[[metric]]), .groups = "drop") |>
      rename(Borough = borough, Total = total) |>
      arrange(desc(Total)) |>
      datatable(
        options = list(dom = 't'),
        caption = "Boroughs Ranked by Count"
      )
  })

  # dropdown options for borough and neighborhood selections
  observeEvent(input$region_type, {
    choices <- if (input$region_type == "geoname") {
      sort(unique(data$geoname))
    } else {
      sort(unique(data$borough))
    }

    updateSelectInput(session, "region_1", choices = choices)
    updateSelectInput(session, "region_2", choices = choices)
  })

  # reactive data for the 4 visualizations based on region selected
  comparison_data <- eventReactive(
    input$compareUpdate,
    {
      w$show()
      region_col <- if (input$region_type == "geoname") "geoname" else "borough"

      data |>
        st_drop_geometry() |>
        filter(.data[[region_col]] %in% c(input$region_1, input$region_2)) |>
        filter(
          crash_date >= input$date_range[1],
          crash_date <= input$date_range[2]
        )
    },
    ignoreNULL = FALSE
  )

  # time series plot by hr in 24 hr time frame
  output$hourPlot <- renderPlotly({
    req(comparison_data())
    hourData <- comparison_data()
    region_col <- isolate(
      if (input$region_type == "geoname") "geoname" else "borough"
    )

    w$hide()
    hourData |>
      mutate(crash_hour = hour(crash_time)) |>
      group_by(crash_hour, region = .data[[region_col]]) |>
      summarize(
        collisions = n(),
        .groups = "drop"
      ) |>
      plot_ly(
        x = ~crash_hour,
        y = ~collisions,
        color = ~region,
        type = 'scatter',
        mode = 'lines',
        text = ~ paste0("Time: ", crash_hour, ":00", "<br>Count: ", collisions),
        hoverinfo = "text"
      ) |>
      layout(
        title = 'Hourly Crash Trends',
        legend = list(title = list(text = 'Region')),
        xaxis = list(title = "Hour of Crash (24-Hr Period)"),
        yaxis = list(title = "Total Crash Count")
      )
  })

  # time series plot by date
  output$timePlot <- renderPlotly({
    req(comparison_data())
    timeData <- comparison_data()
    region_col <- isolate(
      if (input$region_type == "geoname") "geoname" else "borough"
    )

    timeData |>
      group_by(crash_date, region = .data[[region_col]]) |>
      summarize(collisions = n(), .groups = "drop") |>
      plot_ly(
        x = ~crash_date,
        y = ~collisions,
        color = ~region,
        type = 'scatter',
        mode = 'lines',
        text = ~ paste0("Date: ", crash_date, "<br>Count: ", collisions),
        hoverinfo = "text"
      ) |>
      layout(
        title = "Crashes over Time",
        legend = list(title = list(text = 'Region')),
        xaxis = list(title = "Date of Crash"),
        yaxis = list(title = "Total Crash Count")
      )
  })

  # injury barchart
  output$injuryChart <- renderPlotly({
    req(comparison_data())
    injuryData <- comparison_data()
    region_col <- isolate(
      if (input$region_type == "geoname") "geoname" else "borough"
    )

    injuryData |>
      group_by(region = .data[[region_col]]) |>
      summarize(
        pedestrians = sum(number_of_pedestrians_injured),
        cyclists = sum(number_of_cyclist_injured),
        motorists = sum(number_of_motorist_injured),
        .groups = "drop"
      ) |>
      pivot_longer(-region, names_to = "category", values_to = "injuries") |>
      plot_ly(
        x = ~category,
        y = ~injuries,
        color = ~region,
        type = "bar",
        text = ~ paste0("<br>Category: ", category, "<br>Injuries: ", injuries),
        hoverinfo = "text",
        textposition = "none"
      ) |>
      layout(
        title = "Total Injuries Breakdown",
        barmode = "group",
        legend = list(title = list(text = 'Region')),
        xaxis = list(title = "Category"),
        yaxis = list(title = "Injuries")
      )
  })

  # fatality barchart
  output$fatalityChart <- renderPlotly({
    req(comparison_data())
    fatalityData <- comparison_data()
    region_col <- isolate(
      if (input$region_type == "geoname") "geoname" else "borough"
    )

    fatalityData |>
      group_by(region = .data[[region_col]]) |>
      summarize(
        pedestrians = sum(number_of_pedestrians_killed),
        cyclists = sum(number_of_cyclist_killed),
        motorists = sum(number_of_motorist_killed),
        .groups = "drop"
      ) |>
      pivot_longer(-region, names_to = "category", values_to = "deaths") |>
      plot_ly(
        x = ~category,
        y = ~deaths,
        color = ~region,
        type = "bar",
        text = ~ paste0("<br>Category: ", category, "<br>Fatalities: ", deaths),
        hoverinfo = "text",
        textposition = "none"
      ) |>
      layout(
        title = "Total Fatalities Breakdown",
        barmode = "group",
        legend = list(title = list(text = 'Region')),
        xaxis = list(title = "Category"),
        yaxis = list(title = "Fatalities")
      )
  })

  # interactive database
  output$database <- renderDT(
    {
      w$show()

      database <- data |>
        select(-c(location)) |>
        filter(!str_detect(contributing_factor_vehicle_1, "\\d")) |>
        mutate(
          geoname = as.factor(geoname),
          borough = as.factor(borough),
          contributing_factor_vehicle_1 = as.factor(
            contributing_factor_vehicle_1
          )
        ) |>
        rename(contributing_factor = contributing_factor_vehicle_1) |>
        select(
          collision_id,
          crash_date,
          crash_time,
          borough,
          geocode,
          geoname,
          longitude,
          latitude,
          contributing_factor,
          everything()
        )

      w$hide()

      datatable(
        database,
        options = list(
          pageLength = 5,
          searching = TRUE,
          ordering = TRUE,
          autoWidth = TRUE
        ),
        selection = 'multiple',
        filter = 'top',
        rownames = TRUE
      )
    },
    server = TRUE
  )

  # export handler
  output$data_exporter <- downloadHandler(
    filename = function() {
      "nyc_collisions.csv"
    },
    content = function(file) {
      filtered <- input$database_rows_all
      write.csv(database[filtered, ], file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
