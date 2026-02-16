test_that('discrete_palette builds match expression', {
  result <- discrete_palette(c('#FF0000', '#00FF00'), c(1, 2))
  expect_equal(result[[1]], 'match')
  expect_equal(result[[length(result)]], '#000000') # fallback
})

test_that('percent_palette builds continuous scale', {
  result <- percent_palette(c('#FF0000', '#FFFF00', '#00FF00'), column = 'x')
  expect_s3_class(result, 'mapgl_continuous_scale')
  expect_equal(result$breaks[1], 0)
  expect_equal(result$breaks[3], 1)
})
