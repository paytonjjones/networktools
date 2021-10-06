## ---- Common Data Generation ----

data(depression)
data(social)

dep_cor <- cor(depression)
dep_cor_g <- igraph::graph_from_adjacency_matrix(dep_cor, mode = "undirected")

depression_tibble <- dplyr::tibble(depression)


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



## ---- bridge ----

communities_vector <- c("Comm3","Comm3","Comm3", "Comm1", "Comm1", "Comm2", "Comm1", "Comm1", "Comm1")
communities_list <- list("Comm3" = c(1:3), "Comm2" = 6, "Comm1" = c(4:5, 7:9))
communities_list_missing_name <- list(c(1:3), "Comm2" = 6, "Comm3" = c(4:5, 7:9))
communities_vector_with_extra_val <- c("Comm3","Comm3","Comm3", "Comm1", "Comm1", "Comm2", "Comm1", "Comm1", "Comm1", "CommExtra")
communities_list_missing_slot <- list("Comm3" = c(1:3), "Comm2" = 6, "Comm1" = c(4:5, 7:8, 10))
communities_list_too_many <- list("Comm3" = c(1:3), "Comm2" = 6, "Comm1" = c(4:5, 7:10))
numeric_comm_vector <- c(3, 3, 3, 1, 1, 2, 1, 1, 1)
communities_igraph <- igraph::make_clusters(dep_cor_g, numeric_comm_vector)

skip_on_cran()
test_that("bridge - default interface runs without errors", {
  expect_error(
    bridge(dep_cor, communities = communities_vector),
    NA
  )
})

test_that("bridge - runs with communities as complex list", {
  expect_equal(
    bridge(dep_cor, communities = communities_list)$communities,
    communities_vector
  )
})

test_that("bridge - runs with igraph communities", {
  expect_equal(
    bridge(dep_cor, communities = communities_igraph)$communities,
    as.character(numeric_comm_vector)
  )
})

test_that("bridge - warns when communities list is missing names", {
  expect_warning(
    bridge(dep_cor, communities = communities_list_missing_name),
    "Check communities list: possible missing community names"
  )
})

test_that("bridge - fails when communities object does not match length of nodes", {
  expect_error(
    bridge(dep_cor, communities = communities_vector_with_extra_val),
    "Length of communities argument does not match number of nodes"
  )
  expect_error(
    bridge(dep_cor, communities = communities_vector[1:8]),
    "Length of communities argument does not match number of nodes"
  )
  expect_error(
    bridge(dep_cor, communities = communities_list_too_many),
    "Length of communities argument does not match number of nodes"
  )
})

test_that("bridge - fails when communities list skips over a value", {
  expect_error(
    bridge(dep_cor, communities = communities_list_missing_slot),
    "Invalid communities object"
  )
})
