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
#' test_primary_producers <- build_primary_producers(production_mean = c(1, 2), production_sd = c(0.5, 0.5))
#' 
#' # Construct the trophic_dynamics object
#' test_trophic_dynamics <- build_trophic_dynamics(food_web = test_fw, efficiency_matrix = test_efficiency_matrix, dominance_matrix = test_dominance)
#'
#' # Estimate production values from constructed trophic_dynamics object
#' production_estimates <- estimate_production(test_trophic_dynamics, test_primary_producers, replicates = 2)

estimate_production <- function(trophic_dynamics, primary_producers, nsim = 100) {
  
  # check if multiple food webs are included
  replicates <- 1
  if (is.multi_trophic) {
    replicates <- trophic_dynamics$ntrophic
  }
  
  # parallelise estimates for multiple food webs
  production_estimates <- future::future_lapply(seq_len(replicates),
                                                FUN = estimate,
                                                trophic_dynamics = trophic_dynamics,
                                                primary_producers = primary_producers,
                                                nsim = nsim,
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
estimate <- function(trophic_dynamics, primary_producers, nsim) {

  # unpack indices
  nsp <- trophic_dynamics$food_web$nsp
  
  # match primary producer names to food web
  spnames <- rownames(trophic_dynamics$food_web$interaction_matrix)
  primary_producer_id <- match(primary_producers$names, spnames)
  
  # create production output matrix
  production <- matrix(NA, nrow = nsp, ncol = nsim)
  rownames(production) <- spnames
  
  # initialise the primary producers/inputs
  production[primary_producer_id, ] <- matrix(rnorm(nsim,
                                                    mean = primary_producers$mean,
                                                    sd = primary_producers$sd),
                                              nrow = primary_producers$n)
  
  # remove negative values from production
  production <- ifelse(production < 0, 0, production)

  # calculate efficiency over all iterations
  efficiency <- array(rnorm((nsp * nsp * nsim),
                            mean = trophic_dynamics$efficiency_matrix$mean,
                            sd = trophic_dynamics$efficiency_matrix$sd),
                      dim = c(nsp, nsp, nsim))
  efficiency <- vector("list", length = trophic_dynamics$food_web$nsp)
  efficiency <- ifelse(efficiency < 0, 0, efficiency)

  # update depends on whether food web is fixed or stochastic
  if (trophic_dynamics$food_web$type == "fixed") {
    
    # calculate amount of biomass in each node (working from primary consumers up the food web)
    nodeorder <- seq_len(trophic_dynamics$food_web$nsp)
    nodeorder <- nodeorder[apply(trophic_dynamics$food_web$interaction_matrix, 1, sum) > 0]
    for (node in nodeorder) {
      production[node, ] <- production[node, ] +
        trophic_dynamics$dominance_matrix$dominance %*% (efficiency[, node, ] * production)
    }

  } else {
    
    
    
  }
   
  production

}

# internal function: create production_estimates object
as.production_estimates <- function (production_estimates) {
  as_class(production_estimates, name = "production_estimates", type = "list")
}
