adj_editor <- function(shp, adj = geomander::adjacency(shp), opts = redistio_options()) {

  # defaults ----
  def_opts <- redistio_options()

  # run basic inputs ----
  if (missing(shp)) {
    stop('`shp` missing, but required.')
  }

  shp$redistio_id <- as.character(seq_len(length.out = nrow(shp)))

  edges_centers <- edge_center_df(shp, adj)

  # process shp components ----
  if (!sf::st_is_longlat(shp)) {
    shp <- sf::st_transform(shp, opts$crs %||% def_opts$crs)
  }

  shp <- shp |>
    sf::st_make_valid()

  # User Interface ----
  ui <- bslib::page_navbar(
    title = 'redistio',
    theme = bslib::bs_theme(preset = (opts$theme %||% def_opts$theme)),
    id = 'navbar',
    # editor panel ----
    bslib::nav_panel(
      title = ' adj editor',
      bslib::card( # interactive mapper
        full_screen = TRUE,
        leaflet::leafletOutput(
          outputId = 'map',
          height = opts$leaflet_height %||% def_opts$leaflet_height,
        )
      )
    )
  )

  # Server ----
  server <- function(input, output, session) {
    output$map <- leaflet::renderLeaflet({
      base_map <- leaflet::leaflet(data = shp) |>
        leaflet::addTiles() |>
        # leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          layerId = ~redistio_id,
          weight = 1,
          label = ~redistio_id,
          group = 'shapes'
        )
      base_map
    })
  }

  # run app ----
  shiny::shinyApp(ui = ui, server = server)
}
