# Heatmap with leaflets.extra Code
lnglat_data <- reactive({
  data |>
    filter(
      crash_date >= input$map_date_range[1],
      crash_date <= input$map_date_range[2]
    ) |>
    group_by(longitude, latitude) |>
    summarize(
      count = n(),
      deaths = sum(number_of_persons_killed),
      injuries = sum(number_of_persons_injured),
      .groups = "drop"
    )
})

heatmap_data <- lnglat_data()
heatmap_data$value <- heatmap_data[[metric]]

# Noticing how it doesn't bring much insights, I decided to not implement this.
# Very visual noisy; Almost everywhere there's a crash.
addHeatmap(
  data = heatmap_data,
  lng = ~longitude,
  lat = ~latitude,
  blur = 1,
  radius = 5,
  minOpacity = 0.50,
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
