test_that('`redistio_options()` works', {
  expect_type(redistio_options(), 'list')
})

test_that('`redistio_options()` expands palette names', {
  opts <- redistio_options(palette_pop = 'Blues', palette_pct = 'RdYlGn')
  expect_length(opts$palette_pop, 3)
  expect_length(opts$palette_pct, 3)
})

test_that('`redistio_options()` validates debounce', {
  expect_error(redistio_options(debounce = -1))
  expect_error(redistio_options(debounce = 'abc'))
  expect_error(redistio_options(debounce = c(100, 200)))
})
