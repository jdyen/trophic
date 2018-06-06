#' Example food web data
#'
#' Example food web data with 20 species
#'
#' @rdname example_datasets
#'
#' @docType data
#'
#' @usage data(food_web)
#'
#' @format A matrix with one row and column for each species. Values in cell {i, j} indicates that the species in row i eats the species in column j
#'
#' @keywords datasets
#'
#' @examples
#' library(trophic)
#' food_web_test <- build_food_web(food_web)
#' plot(food_web_test)
"food_web"

#' Example efficiency_matrix data
#'
#' Example efficiency matrix data with 20 species
#'
#' @rdname example_datasets
#'
#' @docType data
#'
#' @usage data(efficiency_mean)
#'
#' @format A matrix with one row and column for each species. Values in cell {i, j} indicates the relative efficiency with which the species in row i consumes the species in column j
#'
#' @keywords datasets
#'
#' @examples
#' library(trophic)
#' efficiency_matrix_test <- build_efficiency_matrix(efficiency_mean)
#' efficiency_matrix_test
"efficiency_mean"

#' Example dominance_matrix data
#'
#' Example dominance matrix data with 20 species
#'
#' @rdname example_datasets
#'
#' @docType data
#'
#' @usage data(food_web)
#'
#' @format A matrix with one row and column for each species. Values in cell {i, j} indicates the relative ability of the species in row i to feed on the species in column j
#'
#' @keywords datasets
#'
#' @examples
#' library(trophic)
#' dominance_matrix_test <- build_dominance_matrix(dominance_matrix)
#' dominance_matrix_test
"dominance_matrix"

#' Example production_mean data
#'
#' Example mean primary production data for seven primary sources
#'
#' @rdname example_datasets
#'
#' @docType data
#'
#' @usage data(production_mean)
#'
#' @format A vector with one value for each primary producer. Values are production in UNITS
#'
#' @keywords datasets
#'
#' @examples
#' library(trophic)
#' production_test <- build_primary_producers(production_mean, production_sd)
#' production_test
"production_mean"

#' Example production_sd data
#'
#' Example standard deviations of primary production data for seven primary sources
#'
#' @rdname example_datasets
#'
#' @docType data
#'
#' @usage data(production_sd)
#'
#' @format A vector with one value for each primary producer. Values are the standard deviation of production in UNITS
#'
#' @keywords datasets
#'
#' @examples
#' library(trophic)
#' production_test <- build_primary_producers(production_mean, production_sd)
#' production_test
"production_sd"

