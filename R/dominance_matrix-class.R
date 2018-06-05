#' Create a dominance_matrix object to use in a trophic projection
#'
#' @description A 'dominance matrix' object stores the relative dominance of a species over other species sharing the same resource.
#' It is a sub-component of a \link[trophic]{trophic_dynamics} object
#' 
#' @rdname dominance_matrix
#' 
#' @param dominance a matrix (or NULL object) identifying the relative dominance of a species over other species feeding off the same resource
#' @param x an object to print or test as a dominance_matrix object
#' @param ... further arguments passed to or from other methods
#'
#' @return An object of class \code{dominance_matrix}
#' 
#' @note Passing dominance = NULL will create a flat dominance matrix with all species assigned equal dominance
#' 
#' @export
#'
#' @examples
#' 
#' library(trophic)
#' 
#' # Load a pre-compiled dominance_matrix object
#' 
#' dominance <- data(example_dominance_matrix)
#' 
#' # Construct the dominance_matrix object
#' 
#' test_dominance <- build_dominance_matrix(dominance = dominance)

build_dominance_matrix <- function (dominance = NULL, nsp = NULL, ...) {
  
  # create flat dominance matrix
  if (is.null(dominance)) {
    
    if (is.null(nsp)) {
      stop("Number of species must be supplied if dominance is NULL")
    }
    
    dominance <- matrix(1, nrow = nsp, ncol = nsp)
    dominance <- dominance * lower.tri(dominance)
    
    type <- "default"

  } else {
    
    if (!is.matrix(dominance) & !is.data.frame(dominance)) {
      stop("dominance must be NULL or a nspecies-by-nspecies matrix")
    }
    
    type <- "user-defined"
    
  }
  
  # create dominance_matrix object
  dominance_matrix <- list(dominance = dominance,
                           type = type)
  
  # return dominance_matrix object with class definition
  as.dominance_matrix(dominance_matrix)
  
}

#' @rdname dominance_matrix
#'
#' @export
#' 
#' @examples
#'
#' # Test if object is of the type 'dominance_matrix'
#'   
#' is.dominance_matrix(x)

is.dominance_matrix <- function (x) {
  inherits(x, 'dominance_matrix')
}

#' @rdname dominance_matrix
#'
#' @export
#'
#' @examples
#' 
#' # Print information about the 'dominance_matrix' object
#'
#' print(x)

print.dominance_matrix <- function (x, ...) {
  cat(paste0("This is a ", x$type, " dominance_matrix object"))
}


# internal function: create dominance_matrix object
as.dominance_matrix <- function (dominance_matrix) {
  as_class(dominance_matrix, name = "dominance_matrix", type = "list")
}
