#' Estimate biomass for each node in a food web
#'
#' @description Estimation converts production estimates to biomass estimates
#'
#' @rdname biomass_estimates
#'
#' @param production_estimates a production_estimates object calculated with \link[trophic]{production_estimates}
#' @param pb_ratio a production:biomass ratio object created with \link[trophic]{pb_ratio}
#' @param x a biomass_estimates object
#' @param nodes integer, character, integer vector, or character vector of nodes to plot (indexed by name or food_web row)
#' @param settings plot settings passed directly to \link[graphics]{plot}
#' @param FUN function used to summarise information extracted from a biomass_estimates object
#' @param ... further arguments passed to or from other methods
#'
#' @return An object of class \code{biomass_estimates}
#'
#' @export
#' 
#' @importFrom stats quantile
#' @importFrom graphics lines plot points axis mtext par
#' 
#' @examples
#'
#' library(trophic)
#' library(future)
#' plan(multiprocess)
#'
#' # Construct the component objects
#' test_fw <- build_food_web(interaction_matrix = food_web)
#' test_efficiency_matrix <- build_efficiency_matrix(efficiency_mean = efficiency_mean,
#'                                                   efficiency_sd = 0.01)
#' test_dominance <- build_dominance_matrix(dominance = dominance_matrix)
#' test_primary_producers <- build_primary_producers(production_mean = c(1, 2),
#'                                                   production_sd = c(0.5, 0.5))
#' 
#' # Construct the trophic_dynamics object
#' test_trophic_dynamics <- build_trophic_dynamics(food_web = test_fw,
#'                                                 efficiency_matrix = test_efficiency_matrix,
#'                                                 dominance_matrix = test_dominance)
#'
#' # Estimate production values from constructed trophic_dynamics object
#' production_estimates <- estimate_production(test_trophic_dynamics,
#'                                             test_primary_producers)
#' 
#' # Create a pb_ratio object
#' test_pb_ratio <- build_pb_ratio(range = c(0.25, 5.75),
#'                                 probs = c(5, 20, 10, 3, 1, 1, 1))
#' 
#' # Convert production to biomass estimates
#' biomass_estimates <- estimate_biomass(production_estimates, test_pb_ratio)

estimate_biomass <- function(production_estimates, pb_ratio) {
 
  # switch on type of pb_ratio object
  if (pb_ratio$type == "fixed") {
    
    biomass <- lapply(production_estimates$production,
                      function(x) x * (10 / pb_ratio$values))
    
  } 
  
  if (pb_ratio$type == "gradient") {
    
    biomass <- vector("list", length = length(pb_ratio$values))
    for (i in seq_along(pb_ratio$values)) {
      biomass <- lapply(production_estimates$production,
                        function(x) x * (10 / pb_ratio$values[i]))
    } 
    
  } 
  
  if (pb_ratio$type == "stochastic") {
     
    stochastic_pb <- sample(pb_ratio$values,
                            size = ncol(production_estimates$production[[1]]),
                            replace = TRUE,
                            prob = pb_ratio$probs)    
    biomass <- lapply(production_estimates$production,
                      function(x) sweep(x, 2, 10 / stochastic_pb, "*")) 
    
  }
   
  biomass_estimates <- list(biomass = biomass,
                            replicates = production_estimates$replicates)
  
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
#' \dontrun{
#' is.biomass_estimates(x)
#' }

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
#' \dontrun{
#' print(x)
#' }

print.biomass_estimates <- function (x, ...) {
  cat(paste0("This is a biomass_estimates object with ", x$replicates, " replicates"))
}

#' @rdname biomass_estimates
#'
#' @export
#'
#' @examples
#' 
#' # Plot a 'biomass_estimates' object
#'
#' \dontrun{
#' plot(x)
#' }

plot.biomass_estimates <- function (x, nodes = NULL, settings = list(), ...) {
  
  nplot <- x$replicates
  
  plot_set <- list(pch = 16,
                   las = 1,
                   bty = "l",
                   col = "black",
                   barwidth = c(1, 1.5, 2.6),
                   mar = c(5.1, 10.1, 2.1, 1.1))
  plot_set[names(settings)] <- settings
  
  for (i in seq_len(nplot)) {
    
    if (is.null(nodes)) {
      node_set <- seq_len(nrow(x$biomass[[i]]))
    } else {
      if (is.character(nodes)) {
        node_set <- match(nodes, rownames(x$biomass[[i]]))
        if (any(is.na(node_set))) {
          warning(paste0("some nodes were not found in node names and have been removed: ",
                         node_set[is.na(node_set)]))
          node_set <- node_set[!is.na(node_set)]
        }
        if (!length(node_set)) {
          stop("there are no matches between nodes and node names")
        }
      } else {
        if (is.integer(nodes)) {
          node_set <- nodes
        } else {
          stop("nodes must be a character or integer vector")
        }
      }
    }
    
    if (length(node_set) == 1) {
      to_plot <- quantile(x$biomass[[i]],
                          p = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975))
      to_plot <- matrix(to_plot, ncol = 1)
      colnames(to_plot) <- rownames(x$biomass[[i]])[node_set]
    } else {
      to_plot <- apply(x$biomass[[i]][node_set, ], 1, quantile,
                       p = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975))
    }
    
    old_mar <- par()$mar
    par(mar = plot_set$mar)
    
    plot(to_plot[4, ], seq_along(node_set),
         type = "n",
         xaxt = "n", yaxt = "n",
         xlab = "", ylab = "",
         xlim = range(to_plot),
         las = plot_set$las,
         bty = plot_set$bty,
         ...)
    axis(1,
         las = plot_set$las)
    mtext("Estimated biomass",
          side = 1, adj = 0.5, line = 3.1)
    axis(2, at = seq_along(node_set), labels = colnames(to_plot),
         las = plot_set$las)
    mtext("Node",
          side = 2, adj = 0.5, line = 9.2)
    
    for (k in seq_len(3)) {
      for (j in seq_along(node_set)) {
        lines(c(to_plot[k, j], to_plot[(8 - k), j]),
              c(j, j),
              lwd = plot_set$barwidth[k],
              col = plot_set$col)  
      }
    }
    
    points(to_plot[4, ], seq_along(node_set),
           pch = plot_set$pch,
           col = plot_set$col)
    
    par(mar = old_mar)
    
  }
  
}

#' @rdname biomass_estimates
#'
#' @export
#'
#' @examples
#' 
#' # Extract information on one or several nodes from a 'biomass_estimates' object
#'
#' \dontrun{
#' extract_nodes(x, nodes = c(5:7), FUN = mean)
#' }

extract_nodes <- function (x, nodes = NULL, FUN = summary, ...) {

  nbiomass <- x$replicates
  
  out <- vector("list", length = nbiomass)
  for (i in seq_len(nbiomass)) {

    if (is.null(nodes)) {
      node_set <- seq_len(nrow(x$biomass[[i]]))
    } else {
      if (is.character(nodes)) {
        node_set <- match(nodes, rownames(x$biomass[[i]]))
        if (any(is.na(node_set))) {
          warning(paste0("some nodes were not found in node names and have been removed: ",
                         node_set[is.na(node_set)]))
          node_set <- node_set[!is.na(node_set)]
        }
        if (!length(node_set)) {
          stop("there are no matches between nodes and node names")
        }
      } else {
        if (is.integer(nodes)) {
          node_set <- nodes
        } else {
          stop("nodes must be a character or integer vector")
        }
      }
    }
    
    if (length(node_set) == 1) {
      out[[i]] <- FUN(x$biomass[[i]],
                      ...)
      out[[i]] <- matrix(out[[i]], ncol = 1)
      colnames(out[[i]]) <- rownames(x$biomass[[i]])[node_set]
    } else {
      out[[i]] <- apply(x$biomass[[i]][node_set, ], 1, FUN, ...)
    }
      
  }
  
  out
  
}


# internal function: create biomass_estimates object
as.biomass_estimates <- function (biomass_estimates) {
  as_class(biomass_estimates, name = "biomass_estimates", type = "list")
}
