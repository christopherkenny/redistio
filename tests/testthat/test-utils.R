test_that('prep_palette truncates, repeats, and errors appropriately', {
  expect_length(prep_palette(c('#FF0000', '#00FF00', '#0000FF', '#FFFF00'), ndists = 2), 2)
  expect_length(prep_palette(c('#FF0000'), ndists = 4), 4)
  expect_error(prep_palette(character(0), ndists = 3))
})

test_that('prep_palette uses default when NULL', {
  expect_length(prep_palette(NULL, ndists = 5), 5)
})

test_that('prep_shp returns valid sf without list columns', {
  result <- prep_shp(dc, crs = 4326)
  expect_s3_class(result$all_cols, 'sf')
  expect_s3_class(result$no_list_cols, 'sf')
  col_types <- vapply(result$no_list_cols, is.list, logical(1))
  expect_true(all(!col_types[names(col_types) != 'geometry']))
})

test_that('prep_layers converts character entries to sf', {
  result <- prep_layers(list('ward'), dc)
  expect_s3_class(result[[1]], 'sf')
  expect_equal(names(result), 'ward')
})

test_that('prep_layers handles NULL', {
  expect_null(prep_layers(NULL, dc))
})
