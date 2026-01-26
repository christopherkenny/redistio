test_that('`hover_precinct()` works', {
  hov <- hover_precinct(dc, 1, pop = dplyr::starts_with('pop'), vap = dplyr::starts_with('vap'))

  expect_type(hov, 'list')
  expect_equal(length(hov), 3)
  expect_named(hov, c('', 'pop', 'vap'))
})
