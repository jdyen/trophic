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
#' @examples
#' 
#' library(trophic)
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
#' 
#' # Construct the trophic_dynamics object
#' test_trophic_dynamics <- build_trophic_dynamics(food_web = test_fw, efficiency_matrix = test_efficiency_matrix, dominance_matrix = test_dominance)

build_trophic_dynamics <- function (food_web, efficiency_matrix, dominance_matrix, ...) {
  
  # check input types
  if (!is.food_web(food_web)) {
    stop("food_web object must be created using the build_food_web() function")
  }
  if (!is.efficiency_matrix(efficiency_matrix)) {
    stop("efficiency_matrix object must be created using the build_efficiency_matrix() function")
  }
  if (!is.dominance_matrix(dominance_matrix)) {
    stop("dominance_matrix object must be created using the build_dominance_matrix() function")
  }
  
  # create trophic_dynamics object
  trophic_dynamics <- list(food_web = food_web,
                           efficiency_matrix = efficiency_matrix,
                           dominance_matrix = dominance_matrix)
  
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
#' is.trophic_dynamics(x)

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
#' print(x)

print.trophic_dynamics <- function (x, ...) {
  cat("This is a trophic_dynamics object")
}

#' @rdname trophic_dynamics
#'
#' @export
#'
#' @examples
#' 
#' # Plot a 'trophic_dynamics' object
#'
#' plot(x)

plot.trophic_dynamics <- function (x, ...) {
  
  # create an igraph object from the adjacency matrix
  fw_graph <- igraph::graph_from_adjacency_matrix(x$food_web$interaction_matrix,
                                                  weighted = TRUE,
                                                  mode = "directed")
  
  # set sizes of vertices based on dominance
  vertex_size <- rep(20, ncol(x$food_web$interaction_matrix))

  # set width of edges based on probability of interactions
  edge_width <- 3 * igraph::E(fw_graph)$weight
  
  # plot the adjacency matrix
  plot(fw_graph,
       layout = layout_fw,
       vertex.size = vertex_size,
       edge.width = edge_width)
  
}

# internal function: create trophic_dynamics object
as.trophic_dynamics <- function (trophic_dynamics) {
  as_class(trophic_dynamics, name = "trophic_dynamics", type = "list")
}
