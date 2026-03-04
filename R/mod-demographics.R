#' Demographics Module
#'
#' @param id module id
#'
#' @return shiny UI
#' @noRd
demographicsUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = 'demographics',
    bslib::page_fillable(
      gt::gt_output(ns('demographics'))
    )
  )
}

#' Demographics Module Server
#'
#' @param id module id
#' @param shp sf object
#' @param redistio_curr_plan reactive plan
#'
#' @return shiny server
#' @noRd
demographicsServer <- function(id, shp, redistio_curr_plan) {
  shiny::moduleServer(id, function(input, output, session) {
    output$demographics <- gt::render_gt({
      list(
        rict::rict_population(shp, redistio_curr_plan$pl, as_gt = FALSE),
        rict::rict_demographics(shp, redistio_curr_plan$pl, normalize = TRUE, as_gt = FALSE)
      ) |>
        purrr::reduce(.f = dplyr::left_join, by = 'District') |>
        gt::gt() |>
        gt::fmt_number(columns = c('Population', 'deviation', 'vap'), decimals = 0) |>
        gt::fmt_percent(columns = 'pct_deviation', decimals = 1) |>
        gt::fmt_percent(columns = dplyr::starts_with(c('pop_', 'vap_')), decimals = 1) |>
        gt::cols_hide(columns = 'pop') |>
        gt::cols_label(
          deviation = 'People',
          pct_deviation = '%',
          vap = 'Total'
        ) |>
        gt::cols_label_with(
          columns = dplyr::starts_with(c('pop_', 'vap_')),
          fn = function(x) format_demog_string(stringr::word(x, 2, sep = '_'))
        ) |>
        gt::tab_spanner(label = 'Deviation', columns = c('deviation', 'pct_deviation')) |>
        gt::tab_spanner(label = 'Total Population', columns = dplyr::starts_with(c('pop_'))) |>
        gt::tab_spanner(label = 'Voting Age Population', columns = dplyr::starts_with(c('vap')))
    })
  })
}
