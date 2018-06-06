#' Create an efficiency_matrix object to use in a trophic projection
#'
#' @description An 'efficiency_matrix' object stores the efficiency of energy conversion between a given pair of species.
#' It has two components: average values and standard deviations and is a sub-component of a \link[trophic]{trophic_dynamics} object
#' 
#' @rdname efficiency_matrix
#' 
#' @param efficiency_mean a scalar, vector, or matrix denoting average trophic efficiencies between species pairs
#' @param efficiency_sd a scalar, vector, or matrix denoting the standard deviation of trophic efficiencies
#' @param nsp a scalar denoting the number of species in the associated food_web object; only required if efficiency is a scalar
#' @param x an object to print or test as an efficiency object
#' @param ... further arguments passed to or from other methods
#'
#' @return An object of class \code{efficiency}
#' 
#' @note Scalar value efficiencies are assumed to be constant (on average) for all species. Vector efficiencies
#' are are assumed to apply to prey (not predators) under an assumption that conversion efficiencies are a
#' property of the food not the feeder. Matrix efficiencies are treated identically to food_web interaction matrices
#' but note that efficiency_matrix objects are not checked for loops or cannibalism (so any values on the diagonal
#' or upper triangular of the matrix are ignored).
#' 
#' @export
#'
#' @examples
#' 
#' library(trophic)
#' 
#' # Load a pre-compiled food_web object
#' 
#' # Scalar example
#' efficiency <- data(example_efficiency_scalar)
#' 
#' # Construct the efficiency object
#' test_efficiency_scalar <- build_efficiency_matrix(efficiency_mean = efficiency,
#'                                                   efficiency_sd = 0.1)
#' 
#' # Vector example
#' efficiency_vector <- data(example_efficiency_vector)
#' test_efficiency_vector <- build_efficiency_matrix(efficiency_mean = efficiency_vector,
#'                                                   efficiency_sd = 0.1)
#' 
#' # Matrix example 1: fixed standard deviation
#' efficiency_mean <- data(example_efficiency_matrix)
#' test_efficiency_matrix <- build_efficiency_matrix(efficiency_mean = efficiency_mean,
#'                                                   efficiency_sd = 0.1)
#' 
#' # Matrix example 2: changing standard deviation
#' efficiency_mean <- data(example_efficiency_matrix)
#' efficiency_sd <- data(example_efficiency_sd)
#' test_efficiency_matrix <- build_efficiency_matrix(efficiency_mean = efficiency_mean,
#'                                                   efficiency_sd = efficiency_sd)

build_efficiency_matrix <- function (efficiency_mean, efficiency_sd = NULL, nsp = NULL, ...) {
  
  # check whether scalar, vector, or matrix input
  input_type <- check_input_type(efficiency_mean, efficiency_sd)
  
  # create matrix from scalar average values
  if (input_type[1] == "scalar") {
    
    if (input_type[2] == "vector") {
      nsp <- length(efficiency_sd)
      efficiency_sd <- matrix(rep(efficiency_sd, each = nsp), ncol = nsp)
    }
    if (input_type[2] == "matrix") {
      nsp <- ncol(input_type[2])
      if (is.data.frame(efficiency_sd)) {
        efficiency_sd <- as.matrix(efficiency_sd)
      } else {
        efficiency_sd <- efficiency_sd
      }
    }
    
    if (is.null(nsp)) {
      stop("Cannot create an efficiency matrix from scalar values unless nsp is supplied")
    }
    
    efficiency_mean <- matrix(rep(efficiency_mean, times = (nsp * nsp)), ncol = nsp)

  }
  
  # create matrix from vector average values
  if (input_type[1] == "vector") {
    
    if (is.null(nsp)) {
      nsp <- length(efficiency_mean)
    }
    
    efficiency_mean <- matrix(rep(efficiency_mean, each = nsp), ncol = nsp)
    
    if (input_type[2] == "vector") {
      efficiency_sd <- matrix(rep(efficiency_sd, each = nsp), ncol = nsp)
    }
    if (input_type[2] == "scalar") {
      efficiency_sd <- matrix(rep(efficiency_sd, times = (nsp * nsp)), ncol = nsp)
    }
    
  }
  
  # create matrix from matrix average values
  if (input_type[1] == "matrix") {
    
    if (is.null(nsp)) {
      nsp <- ncol(efficiency_mean)
    }
    
    if (is.data.frame(efficiency_mean)) {
      efficiency_mean <- as.matrix(efficiency_mean)
    }
    
    if (input_type[2] == "vector") {
      efficiency_sd <- matrix(rep(efficiency_sd, each = nsp), ncol = nsp)
    }
    if (input_type[2] == "scalar") {
      efficiency_sd <- matrix(rep(efficiency_sd, times = (nsp * nsp)), ncol = nsp)
    }
    
  }

  efficiency_matrix <- list(mean = efficiency_mean,
                            sd = efficiency_sd,
                            type = paste(input_type, collapse = "/"))
  
  # return efficiency_matrix object with class definition
  as.efficiency_matrix(efficiency_matrix)
  
}

#' @rdname efficiency_matrix
#'
#' @export
#' 
#' @examples
#'
#' # Test if object is of the type 'efficiency_matrix'
#'   
#' is.efficiency_matrix(x)

is.efficiency_matrix <- function (x) {
  inherits(x, 'efficiency_matrix')
}

#' @rdname efficiency_matrix
#'
#' @export
#'
#' @examples
#' 
#' # Print information about the 'efficiency_matrix' object
#'
#' print(x)

print.efficiency_matrix <- function (x, ...) {
  cat(paste0("This is an efficiency_matrix object with ", x$type, " user-defined mean and standard deviation"))
}


# internal function: create efficiency_matrix object
as.efficiency_matrix <- function (efficiency_matrix) {
  as_class(efficiency_matrix, name = "efficiency_matrix", type = "list")
}
