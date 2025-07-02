planscoreUI <- function(id) {
  shiny::tagList(
    shiny::h4('Upload to Planscore'),
    # upload button
    shiny::textInput(
      inputId = shiny::NS(id, 'psdescription'),
      label = 'Description for plan for PlanScore',
      value = 'Plan from redistio'
    ),
    shiny::actionButton(
      inputId = shiny::NS(id, 'upload'),
      label = 'Upload',
      icon = shiny::icon('upload')
    ),
    shiny::uiOutput(shiny::NS(id, 'planscoreURLs')),
    shiny::uiOutput(shiny::NS(id, 'ingestBut')),
    shiny::uiOutput(shiny::NS(id, 'psSummary'))
  )
}

planscoreServer <- function(id, plan, shp) {
  stopifnot(shiny::is.reactivevalues(plan))
  stopifnot(!shiny::is.reactive(shp))

  shiny::moduleServer(id, function(input, output, session) {
    upload_time <- shiny::reactiveVal()
    ps_link <- shiny::reactiveVal()
    ps_data <- shiny::reactiveVal()


    shiny::observeEvent(input$upload, {
      f <- tempfile(fileext = '.geojson')

      shp |>
        tibble::as_tibble() |>
        sf::st_as_sf() |>
        dplyr::mutate(
          District = plan$pl
        ) |>
        dplyr::group_by(.data$District) |>
        dplyr::summarise() |>
        sf::st_write(f)

      planscorer::ps_upload_file(
        file = f, description = input$psdescription
      ) |>
        as.character() |>
        ps_link()
      # cat(ps_link(), '\n')
      upload_time(Sys.time())

      output$planscoreURLs <- shiny::renderUI({
        shiny::tagList(
          shiny::tags$h3('Upload successful'),
          shiny::tags$p(paste0('Uploaded at ', upload_time(), '.')),
          shiny::tags$p('Results are available at: '),
          shiny::tags$a(ps_link()[2], href = ps_link()[2], target = '_blank')
        )
      })

      output$ingestBut <- shiny::renderUI({
        shiny::actionButton(shiny::NS(id, 'ingest'), label = 'Summarize results')
      })
    })

    shiny::observeEvent(input$ingest, {
      ps_info <- NULL
      try(
        {
          ps_info <- planscorer::ps_ingest(ps_link()[1])
        },
        silent = TRUE
      )
      if (is.null(ps_info)) {
        shiny::showModal(
          shiny::modalDialog('Error ingesting results. Try again in a few seconds.')
        )
        message('PlanScore ingest failed at ', Sys.time(), '.\n')
        return()
      }

      ps_data(ps_info)

      output$psSummary <- shiny::renderUI({
        planscorer_summary(ps_info)
      })
    })
  })
}

color_bias <- function(x, domain = c(-0.3, 0.3),
                       oob = function(x) scales::oob_squish(x, range = domain),
                       reverse = FALSE) {
  pal <- ggredist::ggredist$partisan
  if (reverse) pal <- rev(pal)
  scales::col_numeric(
    palette = as.character(pal),
    domain = domain
  )(oob(x))
}

planscorer_summary <- function(ps_info) {
  vbs <- list(
    bslib::value_box(
      title = 'Efficiency gap',
      value = scales::label_percent(accuracy = 0.1, suffix = 'pp')(-1 * ps_info$efficiency_gap[1]),
      theme = bslib::value_box_theme(bg = color_bias(ps_info$efficiency_gap[1]))
    ),
    bslib::value_box(
      title = 'Mean median',
      value = scales::label_number(accuracy = 0.01)(ps_info$mean_median[1]),
      theme = bslib::value_box_theme(bg = color_bias(ps_info$mean_median[1]))
    ),
    bslib::value_box(
      title = 'Partisan bias (Symmetry)',
      value = scales::label_number(accuracy = 0.01)(ps_info$partisan_bias[1]),
      theme = bslib::value_box_theme(bg = color_bias(ps_info$partisan_bias[1]))
    ),
    bslib::value_box(
      title = 'Declination',
      value = scales::label_number(accuracy = 0.01)(ps_info$declination[1]),
      theme = bslib::value_box_theme(bg = color_bias(ps_info$declination[1]))
    )
  )

  bslib::layout_column_wrap(
    width = '250px',
    !!!vbs
  )
}
