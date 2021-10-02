require(dplyr)

## ---- Common Data Generation ----

data(depression)
data(social)

depression_tibble <- tibble(depression)


## ---- Testing ----

## ---- goldbricker & net_reduce ----

gb_empty <- goldbricker(depression)
badpairs_vector <- c("anhedonia", "sadness", "fatigue", "worthlessness")
badpairs_matrix <- matrix(badpairs_vector, ncol = 2, byrow = TRUE)
suggested_reductions <- c(0.2, 0.1)
names(suggested_reductions) <- c("anhedonia & sadness", "fatigue & worthlessness")
gb_with_badpairs <- gb_empty
gb_with_badpairs$suggested_reductions <- suggested_reductions

expected_reduced_colnames_PCA <- c("weight_change","sleep_disturbance","psychomotor_retardation",
                                   "concentration_problems", "suicidal_ideation",
                                   "PCA.anhedonia.sadness", "PCA.fatigue.worthlessness")
expected_reduced_colnames_bg <- c("weight_change","sleep_disturbance","psychomotor_retardation",
                                  "concentration_problems", "suicidal_ideation", "anhedonia",
                                  "worthlessness")

skip_on_cran()
test_that("goldbricker - default interface runs without errors", {
  expect_error(
    goldbricker(depression),
    NA
  )
})

skip_on_cran()
test_that("goldbricker - binary data does not cause an error", {
  expect_error(
    goldbricker(depression),
    NA
  )
})

skip_on_cran()
test_that("goldbricker - passing a tibble does not cause an error", {
  expect_error(
    goldbricker(depression_tibble),
    NA
  )
})

skip_on_cran()
test_that("net_reduce - default interface", {
  expect_error(
    net_reduce(depression, gb_with_badpairs),
    NA
  )
  expect_error(
    net_reduce(depression, gb_with_badpairs, method="best_goldbricker"),
    NA
  )
  expect_equal(
    colnames(net_reduce(depression, gb_with_badpairs)),
    expected_reduced_colnames_PCA
  )
  expect_equal(
    colnames(net_reduce(depression, gb_with_badpairs, method="best_goldbricker")),
    expected_reduced_colnames_bg
  )
})

skip_on_cran()
test_that("net_reduce - when supplied with badpairs as vector", {
  expect_equal(
    colnames(net_reduce(depression, badpairs_vector)),
    expected_reduced_colnames_PCA
  )
})

skip_on_cran()
test_that("net_reduce - when supplied with badpairs as matrix", {
  expect_equal(
    colnames(net_reduce(depression, badpairs_matrix)),
    expected_reduced_colnames_PCA
  )
})

skip_on_cran()
test_that("net_reduce - raises error when best_goldbricker called without gb object", {
  expect_error(
    net_reduce(depression, badpairs_vector, method="best_goldbricker"),
    'Argument badpairs must be of class goldbricker to use method best_goldbricker'
  )
  expect_error(
    net_reduce(depression, badpairs_matrix, method="best_goldbricker"),
    'Argument badpairs must be of class goldbricker to use method best_goldbricker'
  )
})

