#' Interactive Plan Drawing
#'
#' @param shp `sf` tibble that you want to draw with
#' @param init_plan Plan to initialize with.
#' @param ndists Number of districts to draw if `init_plan` is not supplied.
#' @param palette Color palette to fill shapes with. Default is Polychrome 36.
#' @param save_path Output path to save progress to.
#'
#' @return Shiny app
#' @export
#'
#' @examples
#' if (interactive()) {
#'   draw(dc, dc$ward)
#' }
#'
draw <- function(shp, init_plan, ndists, palette,
                 save_path = tempfile(fileext = '.baf')) {
  palette <- as.character(palette)

  if (missing(shp)) {
    stop('`shp` missing, but required.')
  }

  if (missing(init_plan)) {
    if (missing(ndists)) {
      stop('One of `init_plan` or `ndists` must be supplied.')
    } else {
      init_plan <- rep(NA_integer_, nrow(shp))
    }
  } else {
    ndists <- length(unique(init_plan))
  }

  if (missing(palette)) {
    palette <- grDevices::palette.colors(n = ndists, 'Polychrome 36')
  }

  shp$redistio_id <- seq_len(length.out = nrow(shp))
  shp$redistio_curr_plan <- init_plan

  ui <- shiny::fluidPage(
    title = 'redistio',
    theme = bslib::bs_theme(bootswatch = 'darkly'),

    shiny::fluidRow(
      shiny::column( # color selector
        1,
        shiny::radioButtons('district', 'Edit District:',
                            choiceNames = lapply(seq_len(ndists), function(x){
                              shiny::HTML("<p style='color:", palette[x], ";'> &#9632", x, "&#9632</p>")}),
                            choiceValues = seq_len(ndists))
      ),
      shiny::column( # interactive mapper
        8,
        leaflet::leafletOutput('map', height = '100vh')
      ),
      shiny::column( # details area
        3, shiny::tabsetPanel(
          shiny::tabPanel('Population', gt::gt_output('tab_pop')),
          shiny::tabPanel('Download', shiny::downloadButton('save_plan'))
        )
      )
    )

  )

  server <- function(input, output, session) {
    values <- shiny::reactiveValues(
      tab_pop = shp %>%
        dplyr::as_tibble() %>%
        dplyr::group_by(.data$redistio_curr_plan) %>%
        dplyr::summarize(pop = sum(.data$pop))
    )


    bbox <- unname(sf::st_bbox(shp))
    pal <- shiny::reactive({
      leaflet::colorFactor(
        palette = as.character(palette[seq_len(ndists)]),
        domain = seq_len(ndists))
    })

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(data = shp) %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(
          layerId = ~shp$redistio_id,
          # line colors
          stroke = TRUE, weight = 1, color = '#000000',
          # fill control
          fillOpacity = 0.95, fillColor = ~pal()(shp$redistio_curr_plan)
        ) #%>%
      #leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
    })

    shiny::observe({
      click <- input$map_shape_click
      if (is.null(click)) {
        return(NULL)
      }

      idx <- which(shp$redistio_id == click$id)
      shp$redistio_curr_plan[idx] <<- input$district

      values$tab_pop <- shp %>%
        dplyr::as_tibble() %>%
        dplyr::group_by(.data$redistio_curr_plan) %>%
        dplyr::summarize(pop = sum(.data$pop))

      leaflet::leafletProxy('map') %>%
        leaflet::clearShapes() %>%
        leaflet::addPolygons(
          data = shp, layerId = ~shp$redistio_id,
          # line colors
          stroke = TRUE, weight = 1, color = '#000000',
          # fill control
          fillOpacity = 0.95, fillColor = ~pal()(shp$redistio_curr_plan)
        )
    })

    shiny::observe({
      output$tab_pop <- gt::render_gt({
        values$tab_pop %>%
          gt::gt()  %>%
          gt::cols_label(
            redistio_curr_plan = ''
          )
      })
    })

    output$save_plan <- shiny::downloadHandler(
      filename = save_path,
      content = function(con) {
        writeLines(
          text = shp$redistio_curr_plan,
          con = con
        )
      }
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
