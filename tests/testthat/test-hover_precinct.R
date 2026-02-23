test_that('`hover_precinct()` works', {
  hov <- hover_precinct(dc, pop = dplyr::starts_with('pop'), vap = dplyr::starts_with('vap'))

  expect_s3_class(hov, 'tbl_df')
  expect_true(all(c('group', 'rowname') %in% names(hov)))
  expect_true(any(grepl('^V[0-9]+$', names(hov))))
  expect_true('Total Population' %in% hov$group)
  expect_true('Voting Age Population' %in% hov$group)
})
