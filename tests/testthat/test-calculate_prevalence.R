library("testthat")
library("magrittr")
library("datateachr")

test_that("calculate_prevalence calculates prevalence correctly", {
  data <- data.frame(group_var = c("A", "B", "A", "B"),
                     count_var = c("G1", "G2", "G2", "G2"))
  result <- data %>% calculate_prevalence(group_var, count_var)
  expected <- data.frame(count_var = c("G1", "G2"), 
                         count = c(1, 2),
                         proportion = c(0.5, 1))
  expect_equal(result, expected)
})


test_that("test with real data", {
  prevalence <- vancouver_trees %>% calculate_prevalence(neighbourhood_name, genus_name)
  expect_s3_class(prevalence, "data.frame")
})


test_that("calculate_prevalence handles vector with NA's correctly", {
  data <- data.frame(group_var = c("A", "B", "A", NA),
                     count_var = c("G1", "G2", "G2", "G2"))
  result <- data %>% calculate_prevalence(group_var, count_var)
  expected <- data.frame(count_var = c("G1", "G2"), 
                         count = c(1, 2),
                         proportion = c(0.5, 1))
  expect_equal(result, expected)
})

test_that("calculate_prevalence handles vector of a different type correctly", {
  data <- data.frame(group_var = c(1, 2, 1, 2),
                     count_var = c("G1", "G2", "G2", "G2"))
  expect_error(data %>% calculate_prevalence(group_var, count_var))
})

test_that("calculate_prevalence handles vector of length 0 correctly", {
  data <- data.frame(group_var = character(0),
                     count_var = character(0))
  expect_error(data %>% calculate_prevalence(group_var, count_var))
})