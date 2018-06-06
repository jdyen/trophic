#' Create a primary_producers object to use in a trophic projection
#'
#' @description A 'primary_producers' object stores the primary production inputs for a set of nodes in a food web
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
#' test_producers <- build_primary_producers(production_mean = c(1, 2, 3),
#'                                           production_sd = c(0.1, 0.5, 0.2))

build_primary_producers <- function (production_mean, production_sd, ...) {
  
  # check vectors match
  n_primary <- length(production_mean)
  if (length(production_sd) != n_primary) {
    stop(paste0("Length of production_mean and production_sd does not match; production_mean has ",
                n_primary, " elements and production_sd has ", length(production_sd), " values"),
         call. = FALSE)
  }
  
  # add some names to primary producers
  if (is.null(names(production_mean))) {
    if (is.null(names(production_sd))) {
      name_set <- letters[seq_len(n_primary)]
      name_type <- "default"
    } else {
      name_set <- names(production_sd)
      name_type <- "sd"
    }
  } else {
    name_set <- names(production_mean)
    name_type <- "mean"
  }
  
  # create primary_producers object
  primary_producers <- list(mean = production_mean,
                            sd = production_sd,
                            names = name_set,
                            name_type = name_type,
                            n = n_primary)
  
  # return primary_producers object with class definition
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
  cat(paste0("This is a primary_producers object with ",
             x$n,
             " primary producers: ",
             paste(x$names, collapse = ", "),
             " (", x$name_type, " names)"))
}


# internal function: create primary_producers object
as.primary_producers <- function (primary_producers) {
  as_class(primary_producers, name = "primary_producers", type = "list")
}
