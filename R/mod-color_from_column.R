color_from_columnUI <- function(id) {
  shiny::tagList(
    shiny::selectInput(
      inputId = shiny::NS(id, 'column'),
      label = 'Color by column',
      choices = NULL
    ),
    shiny::actionButton(
      inputId = shiny::NS(id, 'accept'),
      label = 'Accept column'
    )
  )
}

color_from_columnServer <- function(id, plan, shp, map_reac,
                                    i_fill_column, i_fill_opacity, i_precinct_border,
                                    i_precinct_linecolor,
                                    pal, undo_l, undo_log, val, tot_pop, ndists, tgt_pop) {
  stopifnot(shiny::is.reactivevalues(plan))
  stopifnot(shiny::is.reactive(map_reac))
  stopifnot(!shiny::is.reactive(shp))

  shiny::moduleServer(id, function(input, output, session) {
    # Find columns that can be used for coloring
    valid_cols <- shiny::reactive({
      n_unique <- sapply(shp, function(x) length(unique(x)))
      n_na <- sapply(shp, function(x) sum(is.na(x)) > 0)

      names(n_unique[n_unique == ndists | (n_unique == ndists + 1 & n_na)])
    })

    shiny::observe({
      shiny::updateSelectInput(session, 'column', choices = valid_cols())
    })

    shiny::observeEvent(input$accept, {
      shiny::req(input$column)

      plan$pl <- shp[[input$column]]

      undo_l(undo_log(undo_l(), plan$pl))

      new_tb_pop <- val()
      new_tb_pop$Population <- distr_pop(shp$pop, total = tot_pop, plan = plan$pl, ndists = ndists)
      new_tb_pop$Deviation <- as.integer(new_tb_pop$Population - c(0L, rep(tgt_pop, ndists)))
      val(new_tb_pop)

      map_reac() |>
        update_shape_style(
          i_fill_column, pal(), shp[[input$column]], shp,
          i_fill_opacity, i_precinct_border, i_precinct_linecolor
        )
    })
  })
}
