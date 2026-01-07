test_that('`guess_elections()` works', {
  el <- guess_elections(dc)
  expect_type(el, 'list')
  expect_length(el, 3)
  expect_named(el, c('pre_16', 'pre_20', 'uss_20'))
})

test_that('`guess_admins()` works', {
  ad <- guess_admins(dc)
  expect_type(ad, 'list')
  expect_length(ad, 4)
  expect_named(ad, c('admin', 'subadmin', 'multi', 'total'))
})

test_that('`guesstimate_demographics()` works', {
  dm <- guesstimate_demographics(dc)
  expect_s3_class(dm, 'data.frame')
  expect_true(all(substr(names(dm), start = 1, stop = 3) == 'pct'))
})
