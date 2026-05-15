test_that('discrete_palette builds match expression', {
  result <- discrete_palette(c('#FF0000', '#00FF00'), c(1, 2))

  expect_equal(result, list(
    'match',
    list('get', 'redistio_id'),
    '1', '#FF0000',
    '2', '#00FF00',
    '#000000'
  ))
})

test_that('discrete_palette skips unassigned precincts', {
  result <- discrete_palette(c('#FF0000', '#00FF00'), c(1, NA, 2))

  expect_equal(result, list(
    'match',
    list('get', 'redistio_id'),
    '1', '#FF0000',
    '3', '#00FF00',
    '#000000'
  ))
})

test_that('percent_palette builds continuous scale', {
  result <- percent_palette(c('#FF0000', '#FFFF00', '#00FF00'), column = 'x')
  expect_s3_class(result, 'mapgl_continuous_scale')
  expect_equal(result$breaks[1], 0)
  expect_equal(result$breaks[3], 1)
})
