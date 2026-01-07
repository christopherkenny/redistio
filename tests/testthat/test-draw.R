test_that('`draw()` works', {
  skip_on_cran()
  testthat::skip_if_not_installed('shinytest2')

  library(shinytest2)

  shiny_app <- draw(dc, dc$ward)
  app <- NULL
  app <- try({
    AppDriver$new(shiny_app, name = 'draw')
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

  expect_true('undo' %in% names(vals))
})
