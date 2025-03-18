library(TripadvisoR)

context("Testing get_city")

api_key <- Sys.getenv("API_KEY")

format <- "^\\d{1,3}\\.\\d{1,8}%2C-?\\d{1,3}\\.\\d{1,8}$"

test_that("Function returns correct format", {
  output <- get_city("kelowna")
  expect_match(output, regexp = format)
})

test_that("Function is consistent", {
  output1 <- get_city("penticton, bc")
  output2 <- get_city("penticton, british columbia")
  expect_equivalent(output1, output2)
})

test_that("Function doesn't break with numeric input", {
  output <- get_city(as.numeric(123.456))
  expect_match(output, regexp = format)
})

test_that("Function doesn't break with boolean input", {
  output <- get_city(FALSE)
  expect_match(output, regexp = format)
})

test_that("Function handles multiple input", {
  output <- get_city(c(1, 2))
  expect_match(output, regexp = "Invalid input.+")
})
