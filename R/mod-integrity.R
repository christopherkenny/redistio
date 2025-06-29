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
#' @param curr_plan reactive plan
#' @param adj adjacency list
#' @param split_cols list of split columns
#'
#' @return shiny server
#' @noRd
integrityServer <- function(id, shp, curr_plan, adj, split_cols) {
  shiny::moduleServer(id, function(input, output, session) {
    output$integrity <- gt::render_gt({
      if (!any(is.na(curr_plan$pl))) {
          int_l <- list(
            rict_population(shp, plan = curr_plan$pl, as_gt = FALSE),
            rict_contiguity(shp, plan = curr_plan$pl, adj = adj, as_gt = FALSE),
            rict_compactness(shp, plan = curr_plan$pl, as_gt = FALSE),
            rict_splits(shp,
              plan = curr_plan$pl,
              admin = split_cols$admin, subadmin = split_cols$subadmin,
              multi = split_cols$multi, total = split_cols$total,
              as_gt = FALSE
            )
          )
      } else {
        int_l <- list(
          rict_population(shp, curr_plan$pl, as_gt = FALSE),
          rict_contiguity(shp, plan = curr_plan$pl, adj = adj, as_gt = FALSE)
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
        )
    })
  })
}
