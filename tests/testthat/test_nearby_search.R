library(TripadvisoR)

context("Testing nearby_search")

api_key = Sys.getenv("api_key")

test_that("Function returns a data frame from valid location name", {
  result <- nearby_search(location_name = "Vancouver", category = "restaurants", api_key = api_key)
  expect_is(result, "data.frame")
})

test_that("Function can handle invalid category name", {
  result <- nearby_search(location_name = "Calgary", category = "bowling alley", api_key = api_key)
  expect_equal(nrow(result), 1)
})

test_that("Function handles successful request to API", {
  result <- nearby_search(location_name = "cartagena", category = "hotels", api_key = api_key)
  
  # check if fields are returned
  expect_true(all(is.na(result$location_id) | !is.na(result$location_id)))
  expect_true(all(is.na(result$name) | !is.na(result$name)))
  expect_true(all(is.na(result$distance) | !is.na(result$distance)))
  expect_true(all(is.na(result$bearing) | !is.na(result$bearing)))
})