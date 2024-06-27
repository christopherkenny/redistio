color_from_fileUI <- function(id) {
  shiny::tagList(
    shiny::fileInput(
      inputId = shiny::NS(id, 'file'),
      label = 'Select a file',
      multiple = 'FALSE',
      accept = '.csv'
    ),
    shiny::textOutput(shiny::NS(id, 'status'))
  )
}

color_from_fileServer <- function(id, plan, shp, map_reac,
                                  i_fill_column, i_fill_opacity, i_precinct_border,
                                  pal, undo_l, undo_log, val, tot_pop, ndists, tgt_pop) {
  stopifnot(shiny::is.reactivevalues(plan))
  stopifnot(shiny::is.reactive(map_reac))
  stopifnot(!shiny::is.reactive(shp))

  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$file, {

      dat <- read.csv(file = input$file$datapath)
      intrscts <- intersect(names(shp), names(dat))

      if (ncol(dat) - length(intrscts) != 1) {
        output$status <- shiny::renderText({
          paste0(
            'Column overlap:', intrscts, '. Too many unmatched columns in input file.',
            ' Joining aborted. Please ensure only one column is unmatched.'
          )
        })
        return(NULL)
      } else {
        output$status <- shiny::renderText({
          paste0(
            'File ', input$file$name, ' uploaded. Column overlap: ',
            intrscts, '. Joining on overlap column(s).'
          )
        })
      }

      noms <- names(dat)
      noms[!noms %in% intrscts] <- '.redistio_from_file'
      names(dat) <- noms

      dat <- dplyr::left_join(shp, dat, by = intrscts)

      if (any(is.na(dat[['.redistio_from_file']]))) {
        output$status <- shiny::renderText({
          paste0(
            'Column ', intrscts, ' has NA values. Joining aborted.'
          )
        })
        return(NULL)
      }

      plan$pl <- dat[['.redistio_from_file']]

      undo_l(undo_log(undo_l(), plan$pl))

      new_tb_pop <- val()
      new_tb_pop$Population <- distr_pop(shp$pop, total = tot_pop, plan = plan$pl, ndists = ndists)
      new_tb_pop$Deviation <- as.integer(new_tb_pop$Population - c(0L, rep(tgt_pop, ndists)))
      val(new_tb_pop)

      map_reac() |>
        update_shape_style(i_fill_column, pal(), dat[['.redistio_from_file']], shp,
                           i_fill_opacity, i_precinct_border)
    })
  })
}
