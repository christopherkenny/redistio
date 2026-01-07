test_that('`adj_editor()` works', {
  skip_on_cran()
  testthat::skip_if_not_installed('shinytest2')

  library(shinytest2)

  shiny_app <- adj_editor(dc)
  app <- AppDriver$new(shiny_app, name = 'adj_editor')

  app$expect_values(screenshot_args = FALSE)
})
