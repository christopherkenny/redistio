#' Elections Module
#'
#' @param id module id
#'
#' @return shiny UI
#' @noRd
electionsUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = 'elections',
    bslib::page_fillable(
      gt::gt_output(ns('elections'))
    )
  )
}

#' Elections Module Server
#'
#' @param id module id
#' @param shp_tb tibble of shp
#' @param curr_plan reactive plan
#'
#' @return shiny server
#' @noRd
electionsServer <- function(id, shp_tb, curr_plan) {
  shiny::moduleServer(id, function(input, output, session) {
    output$elections <- gt::render_gt({
      rict_elections(shp_tb, plan = curr_plan$pl)
    })
  })
}
