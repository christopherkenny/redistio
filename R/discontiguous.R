discontiguousUI <- function(id) {
  shiny::tagList(
    shiny::h4('Discontiguous Pieces'),
    # refresh button ~ don't want it to auto-refresh while edits are happening
    shiny::actionButton(
      inputId = shiny::NS(id, 'refresh'),
      label = 'Refresh',
      icon = shiny::icon('refresh')
    ),

    # print `x` pieces of `total` pieces
    shiny::textOutput(shiny::NS(id, 'status')),

    bslib::layout_column_wrap(
      width = 1 / 2,
      # previous button
      shiny::actionButton(
        inputId = shiny::NS(id, 'previousbutton'),
        label = NULL,
        icon = shiny::icon('arrow-left')
      ),
      # next button
      shiny::actionButton(
        inputId = shiny::NS(id, 'nextbutton'),
        label = NULL,
        icon = shiny::icon('arrow-right')
      )
    )
  )
}

discontiguousServer <- function(id, plan, adj, shp, map_reac) {
  stopifnot(shiny::is.reactivevalues(plan))
  stopifnot(shiny::is.reactive(map_reac))
  stopifnot(!shiny::is.reactive(adj))
  stopifnot(!shiny::is.reactive(shp))

  shiny::moduleServer(id, function(input, output, session) {

    cont <- shiny::reactiveVal(tibble::tibble())
    current <- shiny::reactiveVal(FALSE)

    # get contiguity on refresh only
    shiny::observeEvent(input$refresh, {
      plan2 <- plan$pl
      plan2[is.na(plan2)] <- max(plan$pl, na.rm = TRUE) + 1L
      long_pieces <- geomander::check_contiguity(adj, plan2) |>
        dplyr::mutate(redistio_id = dplyr::row_number()) |>
        dplyr::filter(!is.na(plan$pl)) |>
        dplyr::group_by(.data$group_number) |>
        dplyr::mutate(n = max(.data$component)) |>
        dplyr::ungroup() |>
        dplyr::filter(.data$n > 1) |>
        dplyr::group_by(.data$group_number, .data$component) |>
        dplyr::summarise(
          rows = list(.data$redistio_id)
        )
      cont(long_pieces)
      if (!is.logical(current())) {
        bb <- sf::st_bbox(shp)
        map_reac() |>
          leaflet::flyToBounds(
            lng1 = unname(bb['xmin']),
            lat1 = unname(bb['ymin']),
            lng2 = unname(bb['xmax']),
            lat2 = unname(bb['ymax'])
          )
      }
      current(FALSE)
    })


    # show status
    output$status <- shiny::renderText({
      paste0('Showing piece ',
             ifelse(is.logical(current()), 0L, ifelse(current() == 0L, nrow(cont()), current())),
             ' of ', nrow(cont()), ' discontiguities')
    })

    # handle zooming to things:
    shiny::observeEvent(input$nextbutton, {
      if (nrow(cont()) == 0L) return(NULL)
      current(current() %% nrow(cont()) + 1L)

      bb <- sf::st_bbox(shp[cont()$rows[[current()]], ])
      map_reac() |>
        leaflet::fitBounds(
          lng1 = unname(bb['xmin']),
          lat1 = unname(bb['ymin']),
          lng2 = unname(bb['xmax']),
          lat2 = unname(bb['ymax'])
        )
    })

    shiny::observeEvent(input$previousbutton, {
      if (nrow(cont()) == 0L) return(NULL)
      if (is.logical(current())) {
        current(nrow(cont()))
      } else {
        current((current() - 2L) %% nrow(cont()) + 1)
      }
      bb <- sf::st_bbox(shp[cont()$rows[[current()]], ])
      map_reac() |>
        leaflet::fitBounds(
          lng1 = unname(bb['xmin']),
          lat1 = unname(bb['ymin']),
          lng2 = unname(bb['xmax']),
          lat2 = unname(bb['ymax'])
        )
    })
  })
}
