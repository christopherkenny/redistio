test_that('`adj_editor()` works', {
  skip_on_cran()
  testthat::skip_if_not_installed('shinytest2')

  library(shinytest2)

  shiny_app <- adj_editor(dc)
  app <- NULL
  app <- try({
    AppDriver$new(shiny_app, name = 'adj_editor')
  })

  if (is.null(app)) {
    skip('Could not start chromote correctly.')
  }

  vals <- NULL
  vals <- try({
    app$get_values()$input
  })

  if (is.null(vals)) {
    skip('Could not retrieve input values from the app')
  }

  expect_true('edge_mode' %in% names(vals))
})
