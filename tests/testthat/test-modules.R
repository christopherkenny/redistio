test_that('module UI functions return shiny tags', {
  expect_s3_class(demographicsUI('test'), 'shiny.tag')
  expect_s3_class(electionsUI('test'), 'shiny.tag')
  expect_s3_class(integrityUI('test'), 'shiny.tag')
  expect_s3_class(unassignedUI('test'), 'shiny.tag.list')
  expect_s3_class(discontiguousUI('test'), 'shiny.tag.list')
  expect_s3_class(color_from_fileUI('test'), 'shiny.tag.list')
  expect_s3_class(color_from_columnUI('test'), 'shiny.tag.list')
  expect_s3_class(planscoreUI('test'), 'shiny.tag.list')
  expect_s3_class(comparisonsUI('test'), 'shiny.tag')

  opts <- redistio_options()
  expect_s3_class(plansUI('test', opts, opts), 'shiny.tag')
})

test_that('module UI functions namespace correctly', {
  ui_html <- as.character(unassignedUI('myid'))
  expect_true(grepl('myid-refresh', ui_html))
  expect_true(grepl('myid-nextbutton', ui_html))
})
