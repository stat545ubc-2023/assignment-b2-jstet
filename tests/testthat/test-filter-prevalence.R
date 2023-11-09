library("testthat")
library("magrittr")
library("datateachr")

test_that("filter_by_prevalence filters prevalence correctly", {
  data <- data.frame(group_var = c("A", "B", "A", "B", "C"),
                     count_var = c("G1", "G2", "G2", "G2", "G2"))
  result <- data %>% filter_by_prevalence(group_var, count_var, limit = 0.5, at_least = FALSE)
    expected <- data.frame(group_var = c("A"),
                     count_var = c("G1"))
  expect_equal(result, expected)
})


test_that("test with real data", {
  result <- vancouver_trees %>% filter_by_prevalence(neighbourhood_name, genus_name, limit=0.3, at_least = TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("filter_by_prevalence handles vector with NA's correctly", {
  data <- data.frame(group_var = c("A", "B", "A", NA),
                     count_var = c("G1", "G2", "G2", "G2"))
  result <- data %>% filter_by_prevalence(group_var, count_var, limit = 0.5, at_least = FALSE)
  expected <- data.frame(group_var = character(),
                     count_var = character())
  expect_equal(result, expected)
})

test_that("filter_by_prevalence handles vector of a different type correctly", {
  data <- data.frame(group_var = c(1, 2, 1, 2),
                     count_var = c("G1", "G2", "G2", "G2"))
  expect_error(data %>% filter_by_prevalence(group_var, count_var, limit = 0.5, at_least = FALSE))
})


test_that("calculate_prevalence handles vector of length 0 correctly", {
  data <- data.frame(group_var = character(0),
                     count_var = character(0))
  expect_error(data %>% filter_by_prevalence(group_var, count_var))
})