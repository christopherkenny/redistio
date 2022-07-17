#' Interactive Plan Drawing
#'
#' @param shp `sf` tibble that you want to draw with
#' @param init_plan Plan to initialize with.
#' @param ndists Number of districts to draw if `init_plan` is not supplied.
#' @param palette Color palette to fill shapes with. Default is Polychrome 36.
#' @param pop_tol the population tolerance.
#' @param opts list of options. Default is `redistio_options()`
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
draw <- function(shp, init_plan, ndists, palette, pop_tol = 0.05, opts = redistio_options(),
                 save_path = tempfile(fileext = '.baf')) {

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
  palette <- as.character(palette)

  shp$redistio_id <- seq_len(length.out = nrow(shp))
  shp$redistio_curr_plan <- init_plan

  tgt_pop <- sum(shp$pop) / ndists
  min_pop <- ceiling(tgt_pop * (1 - pop_tol))
  max_pop <- floor(tgt_pop * (1 + pop_tol))
  tgt_pop <- as.integer(round(tgt_pop))

  # User Interface ----
  ui <- shiny::fluidPage(
    title = 'redistio',
    theme = opts$theme,

    shiny::fluidRow(
      shiny::column( # color selector
        2,
        shinyWidgets::radioGroupButtons(inputId = 'district', label = '',
                                        choiceNames = lapply(seq_len(ndists), function(x){
                                          shiny::HTML("<p style='color:", palette[x], ";'> &#9632", x, "&#9632</p>")}),
                                        choiceValues = seq_len(ndists),
                                        direction = 'vertical', size = 'sm'),
        DT::DTOutput('district', width = '50px')

      ),
      shiny::column( # interactive mapper
        8,
        #the_javascripts,
        leaflet::leafletOutput('map', height = '100vh')
      ),
      shiny::column( # details area
        2, shiny::tabsetPanel(
          shiny::tabPanel('Population', gt::gt_output('tab_pop')),
          shiny::tabPanel('Precinct', gt::gt_output('hover')),
          shiny::tabPanel('Download', shiny::downloadButton('save_plan')),
          selected = 'Precinct'
        )
      )
    )

  )

  # Server ----
  server <- function(input, output, session) {

    tb_pop <- dplyr::tibble(
      district = seq_len(ndists),
      button = vapply(seq_len(ndists),
                      function(d) {as.character(shiny::radioButtons('radio_col', label = NULL, choices = d))},
                      FUN.VALUE = ''),
      Population = as.integer(tapply(shp$pop, init_plan, sum)),
      Deviation = as.integer(Population - tgt_pop)
    )

    tb_pop2 <- reactive({
      tb_pop$Population <- as.integer(tapply(shp$pop, shp$redistio_curr_plan, sum))
      tb_pop$Deviation <- as.integer(tb_pop$Population - tgt_pop)
      tb_pop
    })


    values <- shiny::reactiveValues(
      tab_pop = table_pop(shp, tgt_pop) %>%
        dplyr::as_tibble() %>%
        dplyr::rowwise() %>%
        dplyr::mutate(but = as.character(
          shiny::radioButtons(
            inputId = 'radio_col',
            label = NULL,
            choices = redistio_curr_plan)
        ),
        .before = dplyr::everything()) %>%
        dplyr::ungroup()
    )
    clicked <- shiny::reactiveValues(clickedMarker = NULL)
    checked <- shiny::reactive({
      input$tab_check
    })

    bbox <- unname(sf::st_bbox(shp))
    pal <- shiny::reactive({
      leaflet::colorFactor(
        palette = as.character(palette[seq_len(ndists)]),
        domain = seq_len(ndists))
    })
    #redistio_curr_plan <- shiny::reactive(shp$redistio_curr_plan)

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(data = shp) %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(
          layerId = ~redistio_id,
          # line colors
          stroke = TRUE, weight = 1, color = '#000000',
          # fill control
          fillOpacity = 0.95, fillColor = ~pal()(redistio_curr_plan),
          # label
          label = ~pop
        )
    })

    shiny::observeEvent(input$map_shape_click,{
      clicked$map_shape_click <- input$map_shape_click
    })

    shiny::observeEvent(eventExpr = clicked$map_shape_click,
                        handlerExpr = {
                          click <- clicked$map_shape_click
                          clicked$map_shape_click <- NULL
                          if (is.null(click)) {
                            return(NULL)
                          }

                          idx <- which(shp$redistio_id == click$id)
                          shp$redistio_curr_plan[idx] <<- input$district

                          values$tab_pop <- table_pop(shp, tgt_pop)

                          leaflet::leafletProxy('map', data = shp) %>%
                            leaflet::clearShapes() %>%
                            leaflet::addPolygons(
                              data = shp, layerId = ~redistio_id,
                              # line colors
                              stroke = TRUE, weight = 1, color = '#000000',
                              # fill control
                              fillOpacity = 0.95, fillColor = ~pal()(redistio_curr_plan),
                              # label
                              label = ~pop
                            )
                        })


    # district stats ----
    output$district <- DT::renderDT({
      shiny::isolate(tb_pop2()) %>%
        DT::datatable(
          options = list(
            autoWidth = TRUE, dom = 't', ordering = FALSE,
            preDrawCallback = DT::JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
            drawCallback = DT::JS('function() { Shiny.bindAll(this.api().table().node()); } ')
          ),
          rownames = FALSE, escape = FALSE,
          selection = 'none',
          colnames = c('', '', names(.)[-c(1:2)]),

        )
    })

    proxy <- DT::dataTableProxy('district')

    observe({
      DT::replaceData(proxy, tb_pop2(), rownames = FALSE,
                      resetPaging = FALSE, clearSelection = FALSE)
    })

    shiny::observe({
      output$tab_pop <- gt::render_gt({
        tb_pop2() %>%
          dplyr::select(-dplyr::any_of('button')) %>%
          gt::gt() %>%
          gt::tab_style(
            style = gt::cell_fill(color = 'red'),
            locations = gt::cells_body(
              rows = .data$Population > max_pop | .data$Population < min_pop
            )
          ) %>%
          gt::cols_label(
            district = ''
          ) %>%
          gt::tab_footnote(
            footnote = paste0('Population must be in [', min_pop, ', ', max_pop, '].')
          )
      })
    })

    # precinct stats ----
    shiny::observeEvent(input$map_shape_mouseover,{
      shiny::req(input$map_shape_mouseover)

      output$hover <- gt::render_gt({
        hover_precinct(shp, as.integer(input$map_shape_mouseover$id),
                       pop = dplyr::starts_with('pop'), vap = dplyr::starts_with('vap')) %>%
          format_alarm_names() %>%
          gt::gt() %>%
          gt::cols_label(V1 = '') %>%
          gt::tab_style(
            style = list(
              gt::cell_text(align = 'left')
            ),
            locations = gt::cells_stub(rows = TRUE)
          ) %>%
          gt::tab_options(
            data_row.padding = gt::px(0.5)
          )
      })
    })

    # downloader ----
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
