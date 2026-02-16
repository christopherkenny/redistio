test_that('format_demog_string replaces abbreviations', {
  expect_equal(format_demog_string('white'), 'White')
  expect_equal(format_demog_string('hisp'), 'Hispanic')
  expect_equal(format_demog_string('unknown'), 'unknown')
})

test_that('format_compactness replaces abbreviations', {
  expect_equal(format_compactness('comp_polsby'), 'Polsby-Popper')
  expect_equal(format_compactness('comp_reock'), 'Reock')
})

test_that('format_election_names replaces abbreviations', {
  expect_equal(format_election_names('pre'), 'Pres')
  expect_equal(format_election_names('uss'), 'US Sen')
  expect_equal(format_election_names('xyz'), 'xyz')
})
