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
    if (is.matrix(x)) {
      
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
    type <- "scalar"
  }
  
  type
  
}

# foodweb layout function for plotting (adapted from https://gist.github.com/tpoisot/2984097)
layout_fw <- function(w, tlfunc = min, ...) {

  # identify primary producers
  primary_producers <- almost_equal(igraph::degree(w, mode = "out"), 0)
  
  # calculate vertex sequence
  vert_seq <- igraph::V(w)
  
  # calculate trophic levels
  full_sp <- igraph::shortest.paths(w)
  short_path <- igraph::shortest.paths(w,
                                       vert_seq[names(primary_producers[!primary_producers])],
                                       vert_seq[names(primary_producers[primary_producers])])
  vert_seq[names(primary_producers[primary_producers])]$y <- 0
  long_edge <- apply(short_path, 1, tlfunc, ...)
  for(i in seq_len(length(long_edge))){
    vert_seq[names(long_edge)[i]]$y <- long_edge[i]
  }

  # randomise species horizontal plot positions
  vert_seq$x <- sample(seq(from = -1, to = 1, length = length(vert_seq)))

  # calculate horizontal plot positions
  for(i in seq_len(5)) {
    for(j in sample(seq_len(length(vert_seq)))) {
      vert_seq[j]$x <- mean(vert_seq[almost_equal(full_sp[j, ], 1)]$x)
    }
    for(j in unique(vert_seq$y)) {
      accepted_nodes <- vert_seq[almost_equal(vert_seq$y, j)]$x
      n_accepted <- length(accepted_nodes)
      vert_seq[almost_equal(vert_seq$y, uy)]$x <-
        seq(from = -n_accepted, to = n_accepted, length = n_accepted)[rank(accepted_nodes, ties.method = 'random')]
    }
  }
  
  # return outputs
  cbind(vert_seq$x, vert_seq$y)
  
}
