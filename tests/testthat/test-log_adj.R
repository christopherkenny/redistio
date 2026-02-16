test_that('log_adj_update fills sequentially and expands when full', {
  l <- log_adj(len = 2L)
  l <- log_adj_update(l, act = 'add', p = c(1L, 2L), comment = 'test', len = 2L)
  l <- log_adj_update(l, act = 'remove', p = c(3L, 4L), len = 2L)

  expect_equal(l$action[1], 'add')
  expect_equal(l$action[2], 'remove')
  expect_equal(l$pair[1, ], c(1L, 2L))
  expect_equal(l$comment[1], 'test')

  # should expand when full

  l <- log_adj_update(l, act = 'add', p = c(5L, 6L), len = 2L)
  expect_equal(l$action[3], 'add')
  expect_equal(l$pair[3, ], c(5L, 6L))
})
