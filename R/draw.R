#' Interactive Plan Drawing
#'
#' @param shp `sf` tibble that you want to draw with
#' @param init_plan Plan to initialize with.
#' @param ndists Number of districts to draw if `init_plan` is not supplied.
#' @param palette Color palette to fill shapes with. Default is Polychrome 36.
#' @param pop_tol the population tolerance.
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
draw <- function(shp, init_plan, ndists, palette, pop_tol = 0.05,
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

  # User Interface ----
  ui <- shiny::fluidPage(
    title = 'redistio',
    theme = bslib::bs_theme(bootswatch = 'darkly'),

    shiny::fluidRow(
      shiny::column( # color selector
        1,
        shinyWidgets::radioGroupButtons(inputId = 'district', label = 'Edit District:',
                                        choiceNames = lapply(seq_len(ndists), function(x){
                                          shiny::HTML("<p style='color:", palette[x], ";'> &#9632", x, "&#9632</p>")}),
                                        choiceValues = seq_len(ndists),
                                        direction = 'vertical')
      ),
      shiny::column( # interactive mapper
        8,
        #the_javascripts,
        leaflet::leafletOutput('map', height = '100vh')
      ),
      shiny::column( # details area
        3, shiny::tabsetPanel(
          shiny::tabPanel('Population', gt::gt_output('tab_pop')),
          shiny::tabPanel('Precinct', gt::gt_output('hover')),
          shiny::tabPanel('test_dont_use', gt::gt_output('ex')),
          shiny::tabPanel('Download', shiny::downloadButton('save_plan')),
          # shiny::tabPanel('2', shiny::radioButtons(
          #   inputId = 'districtRadio',
          #   choices = 1:8,
          #   label = as.character(1:8)
          # )),
          selected = 'Precinct'
        )
      )
    )

  )

  # Server ----
  server <- function(input, output, session) {
    values <- shiny::reactiveValues(
      tab_pop = shp %>%
        dplyr::as_tibble() %>%
        dplyr::group_by(.data$redistio_curr_plan) %>%
        dplyr::summarize(pop = sum(.data$pop)) %>%
        dplyr::mutate(dev = .data$pop - round(tgt_pop))
    )
    clicked <- shiny::reactiveValues(clickedMarker = NULL)


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
        ) #%>%
      # leaflet::addLabelOnlyMarkers(
      # compute lat and long
      #   group = redistio_curr_plan,
      #   labelOptions = leaflet::labelOptions(noHide = TRUE,
      #                                        direction = 'top',
      #                                        textOnly = TRUE)
      # )
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

                          values$tab_pop <- shp %>%
                            dplyr::as_tibble() %>%
                            dplyr::group_by(.data$redistio_curr_plan) %>%
                            dplyr::summarize(pop = sum(.data$pop)) %>%
                            dplyr::mutate(dev = .data$pop - round(tgt_pop)) %>%
                            dplyr::arrange(as.integer(.data$redistio_curr_plan))

                          leaflet::leafletProxy('map', data = shp) %>%
                            # setShapeFillColor(
                            #   layerId = ~redistio_id, fillColor = ~pal()(redistio_curr_plan)
                            # ) #%>%
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

    shiny::observe({
      output$tab_pop <- gt::render_gt({
        values$tab_pop %>%
          gt::gt() %>%
          gt::tab_style(
            style = gt::cell_fill(color = 'red'),
            locations = gt::cells_body(
              rows = .data$pop > max_pop | .data$pop < min_pop
            )
          ) %>%
          gt::cols_label(
            redistio_curr_plan = ''
          ) %>%
          gt::tab_footnote(
            footnote = paste0('Population must be in [', min_pop, ', ', max_pop, '].')
          )
      })
    })
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
            #table.font.size = 12,
            data_row.padding = gt::px(0.5)
          )
      })
    })

    output$ex <- gt::render_gt(
      values$tab_pop %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(
          rn = seq_len(ndists),
          district = lapply(rn, function(x) gt::html(limited_button("radio", val = x))),
          color = lapply(rn, function(x) gt::html(paste0("<p style='color:", palette[x], ";", x, "'</p>")))
        ) %>%
        dplyr::select(.data$district, .data$color, .data$pop, .data$dev) %>%
        gt::gt() %>%
        #gt::cols_hide(.data$rn) %>%
        gt::cols_align(align = 'left', columns = c(.data$district, .data$color)) %>%
        gt::tab_style(
          style = gt::cell_fill(color = 'red'),
          locations = gt::cells_body(
            rows = .data$pop > max_pop | .data$pop < min_pop
          )
        ) %>%
        gt::cols_label(
          color = ''
        ) %>%
        gt::tab_footnote(
          footnote = paste0('Population must be in [', min_pop, ', ', max_pop, '].')
        )
    )

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

limited_button <- function(inputId, val) {
  stringr::str_glue('
<div id="{inputId}" class="form-group shiny-input-radiogroup shiny-input-container" role="radiogroup" aria-labelledby="{inputId}-label">
  <div class="shiny-options-group">
    <div class="radio">
      <label>
        <input type="radio" name="{inputId}" value="{val}"/>
        <span>{val}</span>
      </label>
    </div>
  </div>
</div>
  ')
}
