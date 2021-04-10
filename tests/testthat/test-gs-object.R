test_that("can delete objects", {
  skip_if_no_auth()
  tb <- bq_table(bq_test_project(), "basedata", "mtcars")

  gs <- gs_test_object()
  expect_false(gs_object_exists(gs))

  bq_table_save(tb, gs)
  expect_true(gs_object_exists(gs))

  gs_object_delete(gs)
  expect_false(gs_object_exists(gs))
})

test_that("has useful print method", {
  gs <- gs_object("xxx", "yyy")
  expect_known_output(print(gs), test_path("gs-object-print.txt"))
})


test_that("coercing to character gives URI", {
  gs <- gs_object("xxx", "yyy")
  expect_equal(as.character(gs), "gs://xxx/yyy")
})
