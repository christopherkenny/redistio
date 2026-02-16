test_that('rict_population computes population by district', {
  plan <- dc$ward
  result <- rict_population(dc, plan, as_gt = FALSE)
  expect_equal(nrow(result), length(unique(plan)))
  expect_equal(sum(result$Population), sum(dc$pop))
})

test_that('rict_population returns gt', {
  expect_s3_class(rict_population(dc, dc$ward, as_gt = TRUE), 'gt_tbl')
})

test_that('rict_contiguity detects contiguity', {
  adj <- geomander::adjacency(dc)
  result <- rict_contiguity(dc, dc$ward, adj, as_gt = FALSE)
  expect_equal(nrow(result), length(unique(dc$ward)))
  expect_true('Pieces' %in% names(result))
})

test_that('rict_compactness values are in [0, 1]', {
  result <- rict_compactness(dc, dc$ward, as_gt = FALSE)
  comp_cols <- grep('^comp_', names(result), value = TRUE)
  for (col in comp_cols) {
    expect_true(all(result[[col]] >= 0 & result[[col]] <= 1))
  }
})

test_that('rict_elections computes election results by district', {
  result <- rict_elections(dc, dc$ward, as_gt = FALSE)
  expect_equal(nrow(result), length(unique(dc$ward)))
})

test_that('rict_splits computes splits', {
  admins <- guess_admins(dc)
  result <- rict_splits(
    dc, dc$ward,
    admin = admins$admin, subadmin = admins$subadmin,
    multi = admins$multi, total = admins$total,
    as_gt = FALSE
  )
  expect_equal(nrow(result), length(unique(dc$ward)))
})

test_that('tally_pop sums correctly and normalizes', {
  result <- tally_pop(dc, dc$ward)
  expect_equal(sum(result$pop), sum(dc$pop))

  result_norm <- tally_pop(dc, dc$ward, normalize = TRUE)
  pop_cols <- grep('^pop_', names(result_norm), value = TRUE)
  for (col in pop_cols) {
    expect_true(all(result_norm[[col]] >= 0 & result_norm[[col]] <= 1))
  }
})
