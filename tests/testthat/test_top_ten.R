library(TripadvisoR)
library(dplyr)

context("Testing top_ten")

api_key <- Sys.getenv("api_key")

test_that("Function filters by city", {
  output <- top_ten("sushi", city = "kelowna")
  expect_true(all(output$country == "Canada"))
})

test_that("Function works without location filter", {
  output <- top_ten("sushi")
  expect_true(n_distinct(output$country) > 1)
})

test_that("Function filters by type", {
  output1 <- top_ten("sushi", type = "restaurants")
  output2 <- top_ten("sushi", type = "hotels")
  expect_true(all(output1 != output2))
})

test_that("Function handles invalid type", {
  check_string <- "Invalid business type input.+"
  expect_output(top_ten("test search string", "badtype"), regexp = check_string)
})

test_that("Function handles no search hits", {
  output <- top_ten("JmHguTQCAfmIWcsmLrwiMSnPHPtexvoeeBdrxOKoEwLprmlV")
  expect_equal(output, "No results!")
})
