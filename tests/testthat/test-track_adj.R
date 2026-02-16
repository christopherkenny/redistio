test_that('edge tracker add/remove/re-add cycle works', {
  edges_df <- tibble::tibble(i = c(1, 2), j = c(2, 3))
  tracker <- init_edge_tracker(edges_df)
  expect_equal(get_current_edge_ids(tracker), c('1-2', '2-3'))

  # add new edge
  tracker <- add_edge_to_tracker(tracker, 3, 4)
  expect_equal(nrow(tracker), 3)
  expect_false(tracker$original[3])

  # remove and re-add original edge
  tracker <- remove_edge_from_tracker(tracker, 1, 2)
  expect_equal(get_current_edge_ids(tracker), c('2-3', '3-4'))

  tracker <- add_edge_to_tracker(tracker, 1, 2)
  expect_true(tracker$shown[1])
  expect_equal(nrow(tracker), 3) # no new row
})

test_that('check_edge_state normalizes i/j order', {
  edges_df <- tibble::tibble(i = c(1, 2), j = c(2, 3))
  tracker <- init_edge_tracker(edges_df)

  expect_true(check_edge_state(tracker, 2, 1)$exists)
  expect_false(check_edge_state(tracker, 5, 6)$exists)
})
