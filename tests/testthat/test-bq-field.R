test_that("can create and coerce a single field", {
  x1 <- bq_field("x", "string")
  x2 <- as_bq_field(list(name = "x", type = "string"))

  expect_s3_class(x1, "bq_field")
  expect_equal(x1$name, "x")
  expect_equal(x1, x2)
})

test_that("can compute fields from data frame", {
  df <- data.frame(x = 1, y = "a")
  fs <- as_bq_fields(df)

  expect_length(fs, 2)
  expect_equal(fs[[1]], bq_field("x", "float"))
  expect_equal(fs[[2]], bq_field("y", "string"))
})

test_that("recursive printing of subfields", {
  x1 <- bq_field("x", "string")
  x2 <- bq_field("y", "integer")

  z1 <- bq_field("z1", "record", fields = list(x1, x2))
  z2 <- bq_field("z2", "record", fields = list(z1))
  z3 <- bq_field("z3", "record", fields = list(z2))

  expect_known_output({
    cat_line("Field:")
    print(z3)

    cat_line()
    cat_line("Fields: ")
    print(z3$fields)
  }, "bq-field-print.txt")
})
