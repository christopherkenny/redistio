test_that('undo_log adds and retrieves entries', {
  l <- undo_init()
  l <- undo_log(l, c(1L, 2L, 3L))
  expect_equal(l$undo_index, 1L)
  expect_equal(l$undo[[1]], c(1L, 2L, 3L))
})

test_that('undo_log wraps around at max_undo', {
  l <- undo_init(max_undo = 3L)
  for (i in 1:4) l <- undo_log(l, rep(as.integer(i), 3))

  expect_equal(l$undo_index, 3L)
  expect_equal(l$undo[[1]], c(2L, 2L, 2L))
  expect_equal(l$undo[[3]], c(4L, 4L, 4L))
})

test_that('undo_once decrements and floors at 0', {
  l <- undo_init()
  l <- undo_log(l, c(1L, 2L, 3L))
  l <- undo_once(l)
  expect_equal(l$undo_index, 0L)
  l <- undo_once(l)
  expect_equal(l$undo_index, 0L)
})
