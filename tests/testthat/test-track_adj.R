test_that('edge tracker add/remove/re-add cycle works', {
  edges_df <- tibble::tibble(i = c(1, 2), j = c(2, 3))
  tracker <- init_edge_tracker(edges_df)
  expect_length(get_hidden_original_edge_ids(tracker), 0L)

  tracker <- add_edge_to_tracker(tracker, 3, 4)
  expect_equal(nrow(tracker), 3)
  expect_identical(tracker$original[3], FALSE)

  expect_identical(
    check_edge_state(tracker, 4, 3),
    list(
      exists = TRUE,
      original = FALSE,
      shown = TRUE
    )
  )

  tracker <- remove_edge_from_tracker(tracker, 3, 4)
  expect_identical(
    check_edge_state(tracker, 4, 3),
    list(
      exists = TRUE,
      original = FALSE,
      shown = FALSE
    )
  )

  tracker <- remove_edge_from_tracker(tracker, 1, 2)
  expect_equal(get_hidden_original_edge_ids(tracker), '1-2')

  tracker <- add_edge_to_tracker(tracker, 1, 2)
  expect_identical(tracker$shown[1], TRUE)
  expect_equal(nrow(tracker), 3)
  expect_length(get_hidden_original_edge_ids(tracker), 0L)
})

test_that('edge visibility filter contains only hidden original edges', {
  edges_df <- tibble::tibble(i = c(1, 2), j = c(2, 3))
  tracker <- init_edge_tracker(edges_df) |>
    add_edge_to_tracker(3, 4) |>
    remove_edge_from_tracker(1, 2) |>
    remove_edge_from_tracker(3, 4)

  expect_equal(
    build_edge_visibility_filter(tracker),
    list(
      'match',
      list('get', 'line_id'),
      list('1-2'),
      FALSE,
      TRUE
    )
  )
})

test_that('edge visibility filter clears when all original edges are shown', {
  tracker <- init_edge_tracker(tibble::tibble(i = 1, j = 2))

  expect_null(build_edge_visibility_filter(tracker))
})

test_that('check_edge_state normalizes i/j order', {
  edges_df <- tibble::tibble(i = c(1, 2), j = c(2, 3))
  tracker <- init_edge_tracker(edges_df)

  expect_true(check_edge_state(tracker, 2, 1)$exists)
  expect_false(check_edge_state(tracker, 5, 6)$exists)
})
