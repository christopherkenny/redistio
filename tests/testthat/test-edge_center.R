test_that('edge_center_df creates edges from adjacency', {
  adj <- geomander::adjacency(dc)
  result <- edge_center_df(dc, adj)

  expect_s3_class(result$nb, 'sf')
  expect_equal(nrow(result$centers), nrow(dc))
  expect_true(all(result$nb$i <= result$nb$j))
})

test_that('edge_center_df handles single row', {
  result <- edge_center_df(dc[1, ], list(integer(0)))
  expect_null(result$nb)
})

test_that('new_single_edge creates sf linestrings', {
  adj <- geomander::adjacency(dc)
  ec <- edge_center_df(dc, adj)
  result <- new_single_edge(ec$centers, c(1, 3), c(2, 4))
  expect_s3_class(result, 'sf')
  expect_equal(nrow(result), 2)
})
