test_that("startup message recommends install or update", {
  local_mocked_bindings(has_bigrquerystorage = function() TRUE)
  expect_silent(.onAttach())

  local_mocked_bindings(
    has_bigrquerystorage = function() FALSE,
    is_installed = function(...) FALSE
  )
  expect_snapshot(.onAttach())

  local_mocked_bindings(is_installed = function(...) TRUE)
  expect_snapshot(.onAttach())
})
