#' Estimate production for each node in a food web
#'
#' @description Estimation updates replicate production values based on a trophic_dynamics object
#'
#' @rdname production_estimates
#'
#' @param trophic_dynamics a dynamics object - food web, trophic efficiency, trophic dominance, and P:B ratios
#' @param replicates number of estimates to generate
#' @param x a production_estimates object
#' @param ... further arguments passed to or from other methods
#'
#' @return An object of class \code{production_estimates}
#'
#' @export
#' 
#' @importFrom future plan multiprocess future values
#'
#' @examples
#'
#' library(trophic)
#' library(future)
#' plan(multiprocess)
#'
#' # Load pre-compiled food_web, efficiency_matrix, and dominance_matrix objects
#' 
#' food_web <- data(example_food_web)
#' dominance <- data(example_dominance_matrix)
#' efficiency_mean <- data(example_efficiency_matrix)
#' efficiency_sd <- data(example_efficiency_sd)
#' 
#' # Construct the component objects
#' test_fw <- build_food_web(interaction_matrix = food_web)
#' test_efficiency_matrix <- build_efficiency_matrix(efficiency_mean = efficiency_mean, efficiency_sd = efficiency_sd)
#' test_dominance <- build_dominance_matrix(dominance = dominance)
#' test_pb_ratio <- build_pb_ratio(pb_range = c(0.25, 5.75), pb_prob = c(5, 20, 10, 3, 1, 1, 1))
#' 
#' # Construct the trophic_dynamics object
#' test_trophic_dynamics <- build_trophic_dynamics(food_web = test_fw, efficiency_matrix = test_efficiency_matrix, dominance_matrix = test_dominance, pb_ratio = test_pb_ratio)
#'
#' # Estimate production values from constructed trophic_dynamics object
#' results <- estimate_production(test_trophic_dynamics, replicates = 2)

estimate_production <- function(trophic_dynamics, replicates = 1){
  
  production_estimates <- future::future_lapply(seq_len(replicates),
                                               FUN = estimate,
                                               trophic_dynamics = trophic_dynamics,
                                               future.seed = FALSE)
  
  as.production_estimates(production_estimates)

}

#' @rdname production_estimates
#'
#' @export
#' 
#' @examples
#'
#' # Test if object is of the type 'production_estimates'
#'   
#' is.production_estimates(x)

is.production_estimates <- function (x) {
  inherits(x, 'production_estimates')
}

#' @rdname production_estimates
#'
#' @export
#'
#' @examples
#' 
#' # Print information about the 'production_estimates' object
#'
#' print(x)

print.production_estimates <- function (x, ...) {
  cat("This is a production_estimates object")
}


# internal function: estimate one set of production values
estimate <- function(trophic_dynamics) {
  
  
  
}

# internal function: create trophic_dynamics object
as.production_estimates <- function (production_estimates) {
  as_class(production_estimates, name = "production_estimates", type = "list")
}
