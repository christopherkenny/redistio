test_that('distr_pop computes district populations', {
  pop <- c(100, 200, 300, 400)
  plan <- c(1, 1, 2, 2)
  total <- sum(pop)

  result <- distr_pop(pop, total, plan, ndists = 2)
  expect_equal(result, c(0, 300, 700))
})

test_that('distr_pop handles unassigned precincts', {
  pop <- c(100, 200, 300, 400)
  plan <- c(1, NA, 2, 2)
  total <- sum(pop)

  result <- distr_pop(pop, total, plan, ndists = 2)
  expect_equal(result, c(200, 100, 700))
})

test_that('generate_lock_icons marks locked districts', {
  result <- generate_lock_icons(ndists = 3, locked_districts = c('1', '3'))
  expect_length(result, 4)
  expect_true(grepl('\U0001F512', result[2])) # district 1 locked
  expect_true(grepl('\U0001F513', result[3])) # district 2 unlocked
  expect_true(grepl('\U0001F512', result[4])) # district 3 locked
})

test_that('create_mapgl_source keeps only geometry and id', {
  shp <- dc
  shp$redistio_id <- seq_len(nrow(shp))
  result <- create_mapgl_source(shp, id_col = 'redistio_id')
  expect_equal(ncol(result), 2)
})
