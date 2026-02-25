palette_pieces <- c('#98FB98', '#FBFB98')
palette_deviation <- c('#FFFFFF', '#301934')
palette_compactness <- c('#FBFB98', '#98FB98')
palette_splits <- c('#98FB98', '#FBFB98')

#' Integrity Module
#'
#' @param id module id
#'
#' @return shiny UI
#' @noRd
integrityUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = 'integrity',
    bslib::page_fillable(
      gt::gt_output(ns('integrity'))
    )
  )
}

#' Integrity Module Server
#'
#' @param id module id
#' @param shp sf object
#' @param redistio_curr_plan reactive plan
#' @param adj adjacency list
#' @param split_cols list of split columns
#'
#' @return shiny server
#' @noRd
integrityServer <- function(id, shp, redistio_curr_plan, adj, split_cols) {
  shiny::moduleServer(id, function(input, output, session) {
    output$integrity <- gt::render_gt({
      if (!any(is.na(redistio_curr_plan$pl))) {
        int_l <- list(
          rict::rict_population(shp, plan = redistio_curr_plan$pl, as_gt = FALSE),
          rict::rict_contiguity(shp, plan = redistio_curr_plan$pl, adj = adj, as_gt = FALSE),
          rict::rict_compactness(shp, plan = redistio_curr_plan$pl, as_gt = FALSE),
          rict::rict_splits(shp,
            plan = redistio_curr_plan$pl,
            admin = split_cols$admin, subadmin = split_cols$subadmin,
            multi = split_cols$multi, total = split_cols$total,
            as_gt = FALSE
          )
        )
      } else {
        int_l <- list(
          rict::rict_population(shp, redistio_curr_plan$pl, as_gt = FALSE),
          rict::rict_contiguity(shp, plan = redistio_curr_plan$pl, adj = adj, as_gt = FALSE)
        )
      }
      int_l |>
        purrr::reduce(.f = dplyr::left_join, by = 'District') |>
        gt::gt() |>
        gt::fmt_number(columns = c('Population', 'deviation'), decimals = 0) |>
        gt::fmt_percent(columns = 'pct_deviation', decimals = 1) |>
        gt::fmt_percent(columns = dplyr::starts_with('comp_'), decimals = 1) |>
        gt::tab_spanner(label = 'Deviation', columns = c('deviation', 'pct_deviation')) |>
        gt::tab_spanner(label = 'Contiguity', columns = dplyr::any_of('Pieces')) |>
        gt::tab_spanner(label = 'Compactness', columns = dplyr::starts_with('comp_')) |>
        gt::tab_spanner(
          label = 'Splits',
          columns = dplyr::starts_with(c('admin_', 'subadmin_'))
        ) |>
        gt::tab_spanner(label = 'Multi Splits', columns = dplyr::starts_with('multi_')) |>
        gt::tab_spanner(label = 'Total Splits', columns = dplyr::starts_with('total_')) |>
        gt::cols_label(
          dplyr::any_of('deviation') ~ 'People',
          dplyr::any_of('pct_deviation') ~ '%'
        ) |>
        gt::cols_label_with(
          columns = dplyr::starts_with('comp_'),
          fn = function(x) format_compactness(x)
        ) |>
        gt::cols_label_with(
          columns = dplyr::starts_with(c('admin_', 'subadmin_', 'multi_', 'total_')),
          fn = function(x) {
            x |>
              stringr::str_remove('^admin_|^subadmin_|^multi_|^total_')
          }
        ) |>
        gt::data_color(
          columns = dplyr::any_of('Pieces'),
          fn = scales::col_numeric(palette = palette_pieces, domain = NULL)
        ) |>
        gt::data_color(
          columns = dplyr::any_of(c('deviation', 'pct_deviation')),
          fn = function(x) {
            scales::col_numeric(palette = palette_deviation, domain = c(0, max(abs(x), na.rm = TRUE)))(abs(x))
          }
        ) |>
        gt::data_color(
          columns = dplyr::starts_with('comp_'),
          fn = scales::col_numeric(palette = palette_compactness, domain = c(0, 1))
        ) |>
        gt::data_color(
          columns = dplyr::starts_with(c('admin_', 'subadmin_', 'multi_', 'total_')),
          fn = scales::col_numeric(palette = palette_splits, domain = NULL)
        )
    })
  })
}
