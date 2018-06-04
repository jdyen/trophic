#' Create a primary_producers object to use in a trophic projection
#'
#' @description A 'primary_producers' object stores the primary production inputs for a set of nodes in a food web
#' It is a sub-component of a \link[trophic]{trophic_dynamics} object
#' 
#' @rdname primary_producers
#' 
#' @param production_mean a vector of production means
#' @param production_sd a vector of production standard deviations
#' @param x an object to print or test as a primary_producers object
#' @param ... further arguments passed to or from other methods
#'
#' @return An object of class \code{primary_producers}
#' 
#' @export
#'
#' @examples
#' 
#' library(trophic)
#' 
#' # Construct the primary_producers object
#' 
#' test_producers <- build_primary_producers(production_mean = c(1, 2, 3), production_sd = c(0.1, 0.5, 0.2))

build_primary_producers <- function (production_mean, production_sd, ...) {
  
  # add some checks
  
  # possibly add some names or IDs?? (could do same with food web objects (e.g. nsp) to speed up later calcs (and improve printing)
  # e.g. print "food web object with nsp species and x trophic levels and ??
  
  # create primary_producers object
  primary_producers <- list(mean = production_mean,
                            sd = production_sd)
  
  # return food_web object with class definition
  as.primary_producers(primary_producers)
  
}

#' @rdname primary_producers
#'
#' @export
#' 
#' @examples
#'
#' # Test if object is of the type 'primary_producers'
#'   
#' is.primary_producers(x)

is.primary_producers <- function (x) {
  inherits(x, 'primary_producers')
}

#' @rdname primary_producers
#'
#' @export
#'
#' @examples
#' 
#' # Print information about the 'primary_producers' object
#'
#' print(x)

print.primary_producers <- function (x, ...) {
  cat("This is a primary_producers object")
}


# internal function: create primary_producers object
as.primary_producers <- function (primary_producers) {
  as_class(primary_producers, name = "primary_producers", type = "list")
}
