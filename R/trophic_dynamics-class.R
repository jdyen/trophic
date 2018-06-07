#' Create a trophic_dynamics object to use in a trophic projection
#'
#' @description A 'trophic dynamics' object stores the trophic dynamics of a set of potentially interacting species.
#' 
#' @rdname trophic_dynamics
#' 
#' @param food_web a \link[trophic]{food_web} object identifying (weighted) trophic links between species pairs
#' @param efficiency_matrix an \link[trophic]{efficiency_matrix} object storing the efficiency of energy conversion between all pairs of species
#' @param dominance_matrix a \link[trophic]{dominance_matrix} object identifying the relative dominance of a species over other species feeding off the same resource
#' @param x an object to print or test as a trophic_dynamics object
#' @param ... further arguments passed to or from other methods
#'
#' @return An object of class \code{trophic_dynamics}
#' 
#' @export
#'
#' @importFrom graphics plot
#'
#' @examples
#' 
#' library(trophic)
#' 
#' # Construct the component objects
#' test_fw <- build_food_web(interaction_matrix = food_web)
#' test_efficiency_matrix <- build_efficiency_matrix(efficiency_mean = efficiency_mean,
#'                                                   efficiency_sd = 0.01)
#' test_dominance <- build_dominance_matrix(dominance = dominance_matrix)
#' 
#' # Construct the trophic_dynamics object
#' test_trophic_dynamics <- build_trophic_dynamics(food_web = test_fw,
#'                                                 efficiency_matrix = test_efficiency_matrix,
#'                                                 dominance_matrix = test_dominance)
#'
#' # Construct a trophic_dynamics object with two food webs
#' test_trophic_dynamics <- build_trophic_dynamics(food_web = list(test_fw, test_fw),
#'                                                 efficiency_matrix = test_efficiency_matrix,
#'                                                 dominance_matrix = test_dominance)

build_trophic_dynamics <- function (food_web, efficiency_matrix = NULL, dominance_matrix = NULL, ...) {

  # check input types on food web interaction matrix
  if (!is.food_web(food_web)) {
    if (is.list(food_web)) {
      if (!all(sapply(food_web, function(x) is.food_web(x)))) {
        stop("food_web object must be a single food_web object or list of food_web objects created using the build_food_web() function")
      }
      ntrophic <- length(food_web)
    } else {
      stop("food_web object must be a single food_web object or list of food_web objects created using the build_food_web() function")
    }
  } else {
    food_web <- list(food_web)
    ntrophic <- 1
  }
  
  # fill NULL efficiency matrix if required
  if (is.null(efficiency_matrix)) {
    efficiency_matrix <- build_efficiency_matrix(efficiency_mean = 0.1, efficiency_sd = 0.01,
                                                 nsp = food_web[[1]]$nsp)
    efficiency_matrix$type <- "default"
  }
  
  # fill NULL dominance matrix if required
  if (is.null(dominance_matrix)) {
    dominance_matrix <- build_dominance_matrix(dominance = NULL, nsp = food_web[[1]]$nsp)
    dominance_matrix$type <- "default"
  }
  
  # check input types on efficiency and dominance matrices
  if (!is.efficiency_matrix(efficiency_matrix)) {
    if (is.list(efficiency_matrix)) {
      if (!all(sapply(efficiency_matrix, function(x) is.efficiency_matrix(x)))) {
        stop("efficiency_matrix must be a single efficiency_matrix object or list of efficiency_matrix objects created using the build_efficiency_matrix() function")
      }
      nefficiency <- length(efficiency_matrix)
    } else {
      stop("efficiency_matrix must be a single efficiency_matrix object or list of efficiency_matrix objects created using the build_efficiency_matrix() function")
    }
  } else {
    efficiency_matrix <- list(efficiency_matrix)
    nefficiency <- 1
  }
  if (!is.dominance_matrix(dominance_matrix)) {
    if (is.list(dominance_matrix)) {
      if (!all(sapply(dominance_matrix, function(x) is.dominance_matrix(x)))) {
        stop("dominance_matrix must be a single dominance_matrix object or list of dominance_matrix objects created using the build_dominance_matrix() function")
      }
      ndominance <- length(dominance_matrix)
    } else {
      stop("dominance_matrix must be a single dominance_matrix object or list of dominance_matrix objects created using the build_dominance_matrix() function")
    }
  } else {
    dominance_matrix <- list(dominance_matrix)
    ndominance <- 1
  }

  # check that food_web, efficiency_matrix, and dominance_matrix have consistent lengths
  if (!all_equal_or_one(ntrophic, nefficiency, ndominance)) {
    stop(paste0("There are ", ntrophic, " food_web objects, ",
                nefficiency, " efficiency_matrix objects, and ",
                ndominance, " dominance_matrix objects but any values greater than one should be equal"))
  }
  
  # create trophic_dynamics object
  trophic_dynamics <- list(food_web = food_web,
                           efficiency_matrix = efficiency_matrix,
                           dominance_matrix = dominance_matrix,
                           ntrophic = ntrophic,
                           nefficiency = nefficiency,
                           ndominance = ndominance)
  
  # return trophic_dynamics object with class definition
  as.trophic_dynamics(trophic_dynamics)
  
}

#' @rdname trophic_dynamics
#'
#' @export
#' 
#' @examples
#'
#' # Test if object is of the type 'trophic_dynamics'
#'   
#' \dontrun{
#' is.trophic_dynamics(x)
#' }

is.trophic_dynamics <- function (x) {
  inherits(x, 'trophic_dynamics')
}

#' @rdname trophic_dynamics
#'
#' @export
#'
#' @examples
#' 
#' # Print information about the 'trophic_dynamics' object
#'
#' \dontrun{
#' print(x)
#' }

print.trophic_dynamics <- function (x, ...) {
  cat(paste0("This is a trophic_dynamics object with ", x$food_web[[1]]$nsp, " species, ",
             x$efficiency_matrix[[1]]$type, " efficiency matrix, and ",
             x$dominance_matrix[[1]]$type, " dominance matrix"))
}

#' @rdname trophic_dynamics
#'
#' @export
#'
#' @examples
#' 
#' # Plot a 'trophic_dynamics' object
#'
#' \dontrun{
#' plot(x)
#' }

plot.trophic_dynamics <- function (x, ...) {
  
  # create an igraph object from the adjacency matrix
  for (i in seq_along(x$food_web)) {
    plot(x$food_web[[i]])
  }
  
}

# internal function: create trophic_dynamics object
as.trophic_dynamics <- function (trophic_dynamics) {
  as_class(trophic_dynamics, name = "trophic_dynamics", type = "list")
}
