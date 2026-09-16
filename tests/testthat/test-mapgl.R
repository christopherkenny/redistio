test_that('discrete_palette builds match expression', {
  result <- discrete_palette(c('#FF0000', '#00FF00'), c(1, 2))

  expect_equal(
    result,
    list(
      'match',
      list('get', 'redistio_id'),
      '1',
      '#FF0000',
      '2',
      '#00FF00',
      '#000000'
    )
  )
})

test_that('discrete_palette skips unassigned precincts', {
  result <- discrete_palette(c('#FF0000', '#00FF00'), c(1, NA, 2))

  expect_equal(
    result,
    list(
      'match',
      list('get', 'redistio_id'),
      '1',
      '#FF0000',
      '3',
      '#00FF00',
      '#000000'
    )
  )
})

test_that('district_palette uses feature state with a source fallback', {
  result <- district_palette(c('#FF0000', '#00FF00'))

  expect_equal(
    result,
    list(
      'match',
      list(
        'coalesce',
        list('feature-state', 'district'),
        list('get', 'redistio_district')
      ),
      1L,
      '#FF0000',
      2L,
      '#00FF00',
      '#000000'
    )
  )
})

test_that('set_district_state sends compact feature updates', {
  sent <- NULL
  session <- new.env(parent = emptyenv())
  session$sendCustomMessage <- function(type, message) {
    sent <<- list(type = type, message = message)
  }
  map <- structure(
    list(id = 'map', session = session),
    class = 'maplibre_proxy'
  )

  set_district_state(map, c(2L, NA_integer_), feature_ids = c(4L, 9L))

  expect_equal(sent$type, 'redistio-set-district-state')
  expect_equal(sent$message$feature_ids, c(4L, 9L))
  expect_equal(sent$message$districts, c(2L, 0L))
})

test_that('enable_map_hover sends browser-side throttle settings', {
  sent <- NULL
  session <- new.env(parent = emptyenv())
  session$ns <- function(id) paste0('module-', id)
  session$sendCustomMessage <- function(type, message) {
    sent <<- list(type = type, message = message)
  }

  enable_map_hover(session, 'map', 'precinct_fill', 150)

  expect_equal(sent$type, 'redistio-enable-hover')
  expect_equal(sent$message$id, 'module-map')
  expect_equal(sent$message$layer_id, 'precinct_fill')
  expect_equal(sent$message$delay, 150)
})

test_that('percent_palette builds continuous scale', {
  result <- percent_palette(c('#FF0000', '#FFFF00', '#00FF00'), column = 'x')
  expect_s3_class(result, 'mapgl_continuous_scale')
  expect_equal(result$breaks[1], 0)
  expect_equal(result$breaks[3], 1)
})
