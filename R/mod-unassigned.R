unassignedUI <- function(id) {
  shiny::tagList(
    shiny::h4('Unassigned Precincts'),
    # refresh button ~ don't want it to auto-refresh while edits are happening
    shiny::actionButton(
      inputId = shiny::NS(id, 'refresh'),
      label = 'Refresh',
      icon = shiny::icon('refresh')
    ),

    # print `x` pieces of `total` pieces
    shiny::textOutput(shiny::NS(id, 'status2')),
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

unassignedServer <- function(id, plan, shp, map_reac) {
  stopifnot(shiny::is.reactivevalues(plan))
  stopifnot(shiny::is.reactive(map_reac))
  stopifnot(!shiny::is.reactive(shp))

  shiny::moduleServer(id, function(input, output, session) {
    nas <- shiny::reactiveVal(NULL)
    current <- shiny::reactiveVal(FALSE)

    # get NAs on refresh only
    shiny::observeEvent(input$refresh, {
      nas(which(is.na(plan$pl)))
      if (!is.logical(current())) {
        bb <- sf::st_bbox(shp)
        map_reac() |>
          mapgl::fit_bounds(bb)
      }
      current(FALSE)
    })

    # show status
    output$status2 <- shiny::renderText({
      paste0(
        'Showing ',
        ifelse(is.logical(current()), 0L, ifelse(current() == 0L, length(nas()), current())),
        ' of ', length(nas()), ' unassigned precincts'
      )
    })

    # handle zooming to things:
    shiny::observeEvent(input$nextbutton, {
      if (length(nas()) == 0L) {
        return(NULL)
      }
      current(current() %% length(nas()) + 1L)

      bb <- sf::st_bbox(shp[nas()[current()], ])
      map_reac() |>
        mapgl::fit_bounds(
          bounds = c(unname(bb['xmin']), unname(bb['ymin']), unname(bb['xmax']), unname(bb['ymax']))
        )
    })

    shiny::observeEvent(input$previousbutton, {
      if (length(nas()) == 0L) {
        return(NULL)
      }
      if (is.logical(current())) {
        current(length(nas()))
      } else {
        current((current() - 2L) %% length(nas()) + 1)
      }
      bb <- sf::st_bbox(shp[nas()[current()], ])
      map_reac() |>
        mapgl::fit_bounds(
          bounds = c(unname(bb['xmin']), unname(bb['ymin']), unname(bb['xmax']), unname(bb['ymax']))
        )
    })
  })
}
