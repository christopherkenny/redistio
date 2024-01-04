discontiguousUI <- function(id) {
  shiny::tagList(
    # print `x` pieces of `total` pieces

    # refresh button ~ don't want it to auto-refresh while edits are happening
    shiny::actionButton(
      inputId = shiny::NS(id, 'refresh'),
      label = 'Refresh',
      icon = shiny::icon('refresh')
    ),
    # previous button
    shiny::actionButton(
      inputId = shiny::NS(id, 'previousbutton'),
      label = 'Previous',
      icon = shiny::icon('arrow-left')
    ),
    # next button
    shiny::actionButton(
      inputId = shiny::NS(id, 'nextbutton'),
      label = 'Next',
      icon = shiny::icon('arrow-right')
    )
  )
}

discontiguousServer <- function(id, plan, adj) {
  stopifnot(shiny::is.reactive(plan))
  stopifnot(!shiny::is.reactive(adj))

  shiny::moduleServer(id, function(input, output, session) {
    # get contiguity
    cont <- geomander::check_contiguity(adj, plan()) |>
      dplyr::group_by(.data$group_number) |>
      dplyr::mutate(n = max(.data$component)) |>
      dplyr::ungroup() |>
      dplyr::filter(n > 1) |>
      dplyr::group_by(.data$group_number, .data$component)

    # handle zooming to things:
    shiny::observeEvent(input$nextbutton, {
      cat('next')
    })

    shiny::observeEvent(input$previousbutton, {
      cat('previous')
    })


    # return reactive
    shiny::reactive({
      cont
    })
  }
  )
}
