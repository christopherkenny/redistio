#' Interactive Adjacency Graph Editing
#'
#' @param shp an `sf` tibble that you want to draw with
#' @param adj a zero-indexed adjacency graph
#' @param opts list of options. Default is `redistio_options()`
#'
#' @return Shiny app
#' @export
#'
#' @examples
#' if (interactive()) {
#'   adj_editor(dc, init_plan = dc$ward)
#' }
adj_editor <- function(
  shp,
  adj = geomander::adjacency(shp),
  init_plan,
  palette = NULL,
  opts = redistio_options()
) {
  # defaults ----
  def_opts <- redistio_options()

  # run basic inputs ----
  if (missing(shp)) {
    stop('`shp` missing, but required.')
  }

  shp$redistio_id <- as.character(seq_len(length.out = nrow(shp)))

  # process shp components ----
  shp <- prep_shp(shp, crs = opts$crs %||% def_opts$crs)$no_list_cols
  edges_centers <- edge_center_df(shp, adj)

  # handle colors ----
  if (missing(init_plan)) {
    init_plan <- rep(NA_integer_, nrow(shp))
  }
  if (is.character(init_plan)) {
    # TODO: think through allowing non 1, 2, ..., ndists inits
    init_plan <- as.integer(init_plan)
  }
  palette <- prep_palette(palette, length(unique(init_plan)))

  # User Interface ----
  leaf_tiles <- opts$map_tiles %||% def_opts$map_tiles

  ui <- bslib::page_navbar(
    title = 'redistio',
    theme = bslib::bs_theme(preset = (opts$theme %||% def_opts$theme)),
    id = 'navbar',
    # editor panel ----
    bslib::nav_panel(
      title = ' adj editor',
      bslib::card(
        # interactive mapper
        id = 'map-card',
        full_screen = TRUE,
        mapgl::maplibreOutput(
          outputId = 'map',
          height = opts$leaflet_height %||% def_opts$leaflet_height,
        )
      ),
    )
  )

  # Server ----
  server <- function(input, output, session) {
    output$map <- mapgl::renderMaplibre({
      base_map <- mapgl::maplibre(
        bounds = shp,
        style = leaf_tiles
      ) |>
        mapgl::add_source(
          id = 'redistio',
          data = shp,
          promoteId = 'redistio_id'
        ) |>
        mapgl::add_source(
          id = 'lines',
          data = edges_centers$nb,
          promoteId = 'redistio_lines'
        ) |>
        mapgl::add_fill_layer(
          source = 'redistio',
          id = 'precinct_fill',
          fill_color = discrete_palette(palette, init_plan),
          fill_opacity = 0.9,
          fill_outline_color = '#00000000'
        ) |>
        mapgl::add_line_layer(
          id = 'precinct_border',
          source = 'redistio',
          line_color = opts$border_color %||% def_opts$border_color,
          line_width = 0.1
        ) |>
        mapgl::add_line_layer(
          id = 'edges',
          source = 'lines',
          line_color = opts$border_color %||% def_opts$border_color,
          line_width = 1
        )

      base_map
    })
  }
  # run app ----
  shiny::shinyApp(ui = ui, server = server)
}
