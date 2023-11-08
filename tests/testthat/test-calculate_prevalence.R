library("testthat")


# Test if the function calculates prevalence correctly
test_that("calculate_prevalence calculates prevalence correctly", {
  data <- data.frame(group_var = c("A", "B", "A", "B"),
                     count_var = c("G1", "G2", "G2", "G2"))
  result <- calculate_prevalence(data, group_var = "group_var", count_var = "count_var")
  expected <- data.frame(count_var = c("G1", "G2"), 
                         count = c(1, 2),
                         proportion = c(0.5, 1))
  expect_equal(result, expected)
})


