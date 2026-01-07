test_that("`draw()` works", {
  skip_on_cran()
  testthat::skip_if_not_installed('shinytest2')

  library(shinytest2)

  shiny_app <- draw(dc, dc$ward)
  app <- AppDriver$new(shiny_app, name = "draw")

  app$expect_values()
})
