#' Estimate biomass for each node in a food web
#'
#' @description Estimation converts production estimates to biomass estimates
#'
#' @rdname biomass_estimates
#'
#' @param production_estimates a production_estimates object calculated with \link[trophic]{production_estimates}
#' @param x a biomass_estimates object
#' @param ... further arguments passed to or from other methods
#'
#' @return An object of class \code{biomass_estimates}
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
#' test_primary_producers <- build_primary_producers(production_mean = c(1, 2), production_sd = c(0.5, 0.5))
#' 
#' # Construct the trophic_dynamics object
#' test_trophic_dynamics <- build_trophic_dynamics(food_web = test_fw, efficiency_matrix = test_efficiency_matrix, dominance_matrix = test_dominance)
#'
#' # Estimate production values from constructed trophic_dynamics object
#' production_estimates <- estimate_production(test_trophic_dynamics, test_primary_producers)
#' 
#' # Create a pb_ratio object
#' test_pb_ratio <- build_pb_ratio(range = c(0.25, 5.75), probs = c(5, 20, 10, 3, 1, 1, 1))
#' 
#' # Convert production to biomass estimates
#' biomass_estimates <- estimate_biomass(production_estimates, test_pb_ratio)

estimate_biomass <- function(production_estimates, pb_ratio) {
 
  # switch on type of pb_ratio object
  if (pb_ratio$type == "fixed") {
    
    biomass_estimates <- lapply(production_estimates, function(x) x * (10 / pb_ratio$values))
    
  }
  
  if (pb_ratio$type == "gradient") {
    
    biomass_estimates <- vector("list", length = length(pb_ratio$values))
    for (i in seq_along(pb_ratio$values)) {
      biomass_estimates <- lapply(production_estimates, function(x) x * (10 / pb_ratio$values[i]))
    }
    
  }
  
  if (pb_ratio$type == "stochastic") {

    stochastic_pb <- sample(pb_ratio$values,
                            size = ncol(production_estimates[[1]]),
                            replace = TRUE,
                            prob = pb_ratio$probs)    
    biomass_estimates <- lapply(production_estimates, function(x) sweep(x, 2, 10 / stochastic_pb), "*")
    
  }

  as.biomass_estimates(biomass_estimates)
  
}

#' @rdname biomass_estimates
#'
#' @export
#' 
#' @examples
#'
#' # Test if object is of the type 'biomass_estimates'
#'   
#' is.biomass_estimates(x)

is.biomass_estimates <- function (x) {
  inherits(x, 'biomass_estimates')
}

#' @rdname biomass_estimates
#'
#' @export
#'
#' @examples
#' 
#' # Print information about the 'biomass_estimates' object
#'
#' print(x)

print.biomass_estimates <- function (x, ...) {
  cat("This is a biomass_estimates object")
}


# internal function: create biomass_estimates object
as.biomass_estimates <- function (biomass_estimates) {
  as_class(biomass_estimates, name = "biomass_estimates", type = "list")
}
