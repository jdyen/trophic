#' Estimate production for each node in a food web
#'
#' @description Estimation updates replicate production values based on a trophic_dynamics object
#'
#' @rdname production_estimates
#'
#' @param trophic_dynamics a dynamics object - food web, trophic efficiency, trophic dominance, and P:B ratios
#' @param primary_producers a primary_producers object containing estimates of primary production
#' @param stochastic a character vector naming the stochastic elements of the trophic projection; can be any combination of "food_web", "efficiency", and "primary_production"
#' @param nsim number of estimates to generate
#' @param x a production_estimates object
#' @param nodes integer or integer vector of nodes to plot (indexed by food_web row)
#' @param settings plot settings passed directly to \link[graphics]{plot}
#' @param ... further arguments passed to or from other methods
#'
#' @return An object of class \code{production_estimates}
#'
#' @export
#' 
#' @importFrom future plan multiprocess future values
#' @importFrom future.apply future_lapply
#' @importFrom truncnorm rtruncnorm
#' @importFrom stats rbinom quantile
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
#' production_estimates <- estimate_production(test_trophic_dynamics, test_primary_producers)

estimate_production <- function(trophic_dynamics,
                                primary_producers,
                                stochastic = c("food_web", "efficiency", "primary_production"),
                                nsim = 100) {
  
  # check if multiple food webs are included
  replicates <- max(c(trophic_dynamics$ntrophic, trophic_dynamics$nefficiency, trophic_dynamics$ndominance))
  
  # set nsim = 1 if all components are deterministic
  if (!length(stochastic)) {
    cat("All variables are deterministic so nsim has been reduced from ", nsim, " to 1\n")
    nsim <- 1
  }

  # parallelise estimates for multiple food webs
  production <- future.apply::future_lapply(seq_len(replicates),
                                            FUN = estimate,
                                            trophic_dynamics = trophic_dynamics,
                                            primary_producers = primary_producers,
                                            stochastic = stochastic,
                                            nsim = nsim,
                                            future.seed = FALSE)
  
  production_estimates <- list(production = production,
                               replicates = replicates)
  
  as.production_estimates(production_estimates)
   
}

#' @rdname production_estimates
#'
#' @export
#' 
#' @examples
#'
#' # Test if object is of the type 'production_estimates'
#' \dontrun{
#' is.production_estimates(x)
#' }

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
  cat(paste0("This is a production_estimates object with ", x$replicates, " replicates"))
}

#' @rdname production_estimates
#'
#' @export
#'
#' @examples
#' 
#' # Plot a 'production_estimates' object
#'
#' \dontrun{
#' plot(x)
#' }

plot.production_estimates <- function (x, nodes = NULL, settings = list(), ...) {

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
      node_set <- seq_len(nrow(x$production[[i]]))
    } else {
      node_set <- nodes
    }
    
    if ((length(node_set) == 1) & (ncol(x$production[[i]]) > 1)) {
      to_plot <- quantile(x$production[[i]],
                          p = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975))
      to_plot <- matrix(to_plot, ncol = 1)
      colnames(to_plot) <- rownames(x$production[[i]])[node_set]
    } else {
      if (ncol(x$production[[i]]) == 1) {
        to_plot <- matrix(rep(x$production[[i]], times = 7), 
                          nrow = 7, byrow = TRUE)
      } else {
        to_plot <- apply(x$production[[i]][node_set, ], 1, quantile,
                         p = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975))
      }
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
    mtext("Estimated production",
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

# internal function: estimate one set of production values
estimate <- function(i,
                     trophic_dynamics,
                     primary_producers,
                     stochastic,
                     nsim) {

  # unpack indices and food webs
  nsp <- trophic_dynamics$food_web[[i]]$nsp
  if (trophic_dynamics$ntrophic > 1) {
    food_web <- trophic_dynamics$food_web[[i]]
  } else {
    food_web <- trophic_dynamics$food_web[[1]]
  }
  if (trophic_dynamics$nefficiency > 1) {
    efficiency_matrix <- trophic_dynamics$efficiency_matrix[[i]]
  } else {
    efficiency_matrix <- trophic_dynamics$efficiency_matrix[[1]]
  }
  if (trophic_dynamics$ndominance > 1) {
    dominance_matrix <- trophic_dynamics$dominance_matrix[[i]]
  } else {
    dominance_matrix <- trophic_dynamics$dominance_matrix[[1]]
  }

  # match primary producer names to food web
  spnames <- rownames(food_web$interaction_matrix)
  primary_producer_id <- match(primary_producers$names, spnames)
  if (any(is.na(primary_producer_id))) {
    primary_producer_id <- seq_len(primary_producers$n)
    warning(paste0("names of primary_producers do not match nodes in food_web; primary_producers are assumed to be the first ",
                   primary_producers$n,
                   " nodes of food_web"))
  }
  
  # create production output matrix
  production <- matrix(0, nrow = nsp, ncol = nsim)
  rownames(production) <- spnames
  
  # initialise the primary producers/inputs
  if ("primary_production" %in% stochastic) {
    
    production[primary_producer_id, ] <- matrix(truncnorm::rtruncnorm((nsim * primary_producers$n),
                                                                      a = 0,
                                                                      mean = primary_producers$mean,
                                                                      sd = primary_producers$sd),
                                                nrow = primary_producers$n)
    
  } else {
    
    production[primary_producer_id, ] <- matrix(rep(unlist(primary_producers$mean),
                                                    times = nsim),
                                                nrow = primary_producers$n)
    
  }
  
  # calculate dominance weights
  n_split <- apply(dominance_matrix$dominance, 2, sum)
  if (any(n_split == 0)) {
    n_split[n_split == 0] <- 1
  }
  dominance_weights <- sweep(dominance_matrix$dominance,
                             2,
                             n_split,
                             "/")
  dominance_weights <- as.matrix(dominance_weights)
  
  # calculate efficiency over all iterations
  if ("efficiency" %in% stochastic) {
    efficiency <- array(truncnorm::rtruncnorm((nsp * nsp * nsim),
                                              a = 0,
                                              b = 1,
                                              mean = efficiency_matrix$mean,
                                              sd = efficiency_matrix$sd),
                        dim = c(nsp, nsp, nsim))
  } else {
    efficiency <- array(rep(efficiency_matrix$mean, times = nsim),
                        dim = c(nsp, nsp, nsim))
  }

  # update depends on whether food web is fixed or stochastic
  if ((food_web$type == "fixed") | !("food_web" %in% stochastic)) {
    
    # calculate amount of biomass in each node (working from primary consumers up the food web)
    nodeorder <- seq_len(food_web$nsp)
    nodeorder <- nodeorder[apply(food_web$interaction_matrix, 1, sum) > 0]
    for (node in nodeorder) {
      production[node, ] <- production[node, ] +
        dominance_weights[node, ] %*% (efficiency[node, , ] * production)
    }

  } else {
    
    for (i in seq_len(nsim)) {
      
      # simulate a single food web based on the probabilities in the interaction_matrix
      temp_fw <- matrix(rbinom(n = length(food_web$interaction_matrix),
                               size = 1,
                               prob = c(food_web$interaction_matrix)))
      
      # calculate amount of biomass in each node (working from primary consumers up the food web)
      nodeorder <- seq_len(food_web$nsp)
      nodeorder <- nodeorder[apply(food_web$interaction_matrix, 1, sum) > 0]
      for (node in nodeorder) {
        production[node, i] <- production[node, i] +
          dominance_weights[node, ] %*% (efficiency[node, , i] * production[, i])
      }
      
    }
    
  }
   
  production

}

# internal function: create production_estimates object
as.production_estimates <- function (production_estimates) {
  as_class(production_estimates, name = "production_estimates", type = "list")
}
