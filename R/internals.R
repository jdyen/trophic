# internal helper functions for trophic R package

# set an object class
as_class <- function (object, name, type = c("function", "list")) {
  
  type <- match.arg(type)
  stopifnot(inherits(object, type))
  class(object) <- c(name, class(object))
  
  object
  
}

# test if values are equal to zero with a non-zero tolerance
almost_equal <- function(x, y, tolerance = 1e-10) {
  
  diff <- abs(x - y)
  mag <- pmax(abs(x), abs(y))
  
  ifelse(mag > tolerance, (diff / mag) <= tolerance, diff <= tolerance)
  
}

# test if multiple values are all equal
all_equal_or_one <- function(..., tolerance = 1e-10) {
  
  test <- list(...)
  
  test <- test[test > 1]
  
  if (length(test) > 1) {
    out <- rep(NA, length(test))
    for (i in seq_along(test)) {
      out[i] <- almost_equal(test[[i]], test[[1]], tolerance = tolerance)
    }
  } else {
    out <- TRUE
  }
  
  all(out)
  
}


# test input type to functions (scalar, vector, matrix)
check_input_type <- function(...) {
  
  test_vals <- list(...)
  
  out <- sapply(test_vals, check_input_internal)
  
  if ("unknown" %in% out)
    stop("One or more input types is unidentifiable; all inputs should be scalar, vector, or matrix valued")
  
  out
  
}

# internal function to check input type
check_input_internal <- function(x) {
  
  if (length(x) > 1) {
    if (is.matrix(x) | is.data.frame(x)) {
      
      if (ncol(x) == nrow(x)) {
        type <- "matrix"
      } else {
        if ((ncol(x) == 1) | nrow(x) == 1) {
          type <- "vector"
        } else {
          type = "unknown"
        }
      }
      
    } else {
      type <- "vector"
    }
    
  } else {
    if (is.null(x)) {
      type <- "missing"
    } else {
      type <- "scalar"
    }
  }
  
  type
  
}

# foodweb layout function for plotting (adapted from https://gist.github.com/tpoisot/2984097)
layout_fw <- function(w, tlfunc = min, ...) {

  # remove disconnected nodes
  igraph::V(w)$comp <- igraph::components(w)$membership
  w <- igraph::induced_subgraph(w, igraph::V(w)$comp == 1)

  # identify primary producers
  primary_producers <- almost_equal(igraph::degree(w, mode = "out"), 0)
  
  # calculate trophic levels
  full_sp <- igraph::shortest.paths(w)
  short_path <- igraph::shortest.paths(w,
                                       igraph::V(w)[names(primary_producers[!primary_producers])],
                                       igraph::V(w)[names(primary_producers[primary_producers])])
  igraph::V(w)[names(primary_producers[primary_producers])]$y <- 0
  le <- apply(short_path, 1, tlfunc, ...)
  for(i in seq_len(length(le))){
    igraph::V(w)[names(le)[i]]$y <- le[i]
  }

  # randomise species horizontal plot positions
  igraph::V(w)$x <- sample(seq(from = -1, to = 1, length = length(igraph::V(w))))

  # calculate horizontal plot positions
  for(i in seq_len(5)) {
    for(j in sample(seq_len(length(igraph::V(w))))) {
      igraph::V(w)[j]$x <- mean(igraph::V(w)[almost_equal(full_sp[j, ], 1)]$x)
    }
    for(j in unique(igraph::V(w)$y)) {
      accepted_nodes <- igraph::V(w)[almost_equal(igraph::V(w)$y, j)]$x
      n_accepted <- length(accepted_nodes)
      igraph::V(w)[almost_equal(igraph::V(w)$y, j)]$x <-
        seq(from = -n_accepted, to = n_accepted, length = n_accepted)[rank(accepted_nodes, ties.method = 'random')]
    }
  }
  
  # return outputs
  cbind(igraph::V(w)$x, rev(igraph::V(w)$y))
  
}
