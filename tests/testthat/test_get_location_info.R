library(TripadvisoR)

context("Testing get_location_info")

api_key = Sys.getenv("api_key")

test_that("Function returns a data frame from valid location id", {
  result <- get_location_info(4065774)
  expect_is(result, "data.frame")
})

test_that("Function can handle invalid location id", {
  result <- get_location_info(1234567890123)
  expect_true(all(is.na(result)))
})

test_that("Function handles successful request to API", {
  result <- get_location_info(4065774)

  # check if fields are returned
  expect_true(all(is.na(result$Name) | !is.na(result$Name)))
  expect_true(all(is.na(result$Description) | !is.na(result$Description)))
  expect_true(all(is.na(result$Category) | !is.na(result$Category)))
})
