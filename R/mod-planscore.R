planscoreUI <- function(id) {
  shiny::tagList(
    shiny::h4('Upload to Planscore'),
    # upload button
    shiny::actionButton(
      inputId = shiny::NS(id, 'upload'),
      label = 'Upload',
      icon = shiny::icon('upload')
    ),
    shiny::uiOutput(shiny::NS(id, 'planscoreURLs')),
    shiny::uiOutput(shiny::NS(id, 'ingestBut'))#,
    #shiny::uiOutput(shiny::NS(id, 'psSummary'))
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

      ps_link(as.character(planscorer::ps_upload_file(file = f)))
      cat(ps_link())
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
        shiny::actionButton(shiny::NS(id, 'ingest'), label = 'Download results')
      })
    })

    shiny::observeEvent(input$ingest, {
      ps_info <- NULL
      try({ps_info <- planscorer::ps_ingest(ps_link()[1])})
      if (is.null(ps_info)) {
        shiny::showNotification('Error ingesting results. Try again in a few seconds.')
        cat('PlanScore ingest failed at ', Sys.time(), '.\n')
        return()
      }

      ps_data(ps_info)

      # output$psSummary <- shiny::renderUI({
      #
      # })
    })
  })
}
