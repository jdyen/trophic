#' Create a pb_ratio object to use in a trophic projection
#'
#' @description A 'pb ratio' object stores the potentially stochastic ratio of production to standing biomass
#' 
#' @rdname pb_ratio
#' 
#' @param range a range of possible pb values
#' @param length length of pb value sequence
#' @param probs relative probabilities of pb values in pb_range
#' @param type one of 'fixed', 'gradient', or 'stochastic' that determines how pb_values are treated
#' @param x an object to print or test as a pb_ratio object
#' @param ... further arguments passed to or from other methods
#'
#' @return An object of class \code{pb_ratio}
#' 
#' @export
#'
#' @examples
#' 
#' library(trophic)
#' 
#' # Construct the pb_ratio object
#' 
#' test_pb <- build_pb_ratio(range = c(0.25, 5.75), probs = c(5, 20, 10, 3, 1, 1, 1))

build_pb_ratio <- function (range, length = NULL, probs = NULL, type = "stochastic", ...) {

  # create sequence of pb_values
  if (type == "fixed") {
    values <- mean(range)
    if (!is.null(length) | !is.null(probs)) {
      warning("length and probs are ignored if type = 'stochastic'")
    }
  }
  if (type == "gradient") {
    if (is.null(length)) {
      stop("Cannont set gradient pb_ratio if length is NULL")
    }
    values <- seq(range[1], range[2], length = length)
  }
  if (type == "stochastic") {
    if (is.null(probs)) {
      stop("Cannot set stochastic pb_ratio if probs is NULL")
    }
    values <- seq(range[1], range[2], length = length(probs))
  }
  
  # standardise pb_prob
  probs <- probs / sum(probs)
  
  # create food_web object
  pb_ratio <- list(values = values,
                   probs = probs,
                   type = type)
  
  # return food_web object with class definition
  as.pb_ratio(pb_ratio)
  
}

#' @rdname pb_ratio
#'
#' @export
#' 
#' @examples
#'
#' # Test if object is of the type 'pb_ratio'
#'   
#' is.pb_ratio(x)

is.pb_ratio <- function (x) {
  inherits(x, 'pb_ratio')
}

#' @rdname pb_ratio
#'
#' @export
#'
#' @examples
#' 
#' # Print information about the 'pb_ratio' object
#'
#' print(x)

print.pb_ratio <- function (x, ...) {
  cat(paste0("This is a ", x$type, " pb_ratio object"))
}

#' @rdname pb_ratio
#'
#' @export
#'
#' @examples
#' 
#' # Plot a 'pb_ratio' object
#'
#' plot(x)

plot.pb_ratio <- function (x, ...) {
  
  # plot distribution of pb_ratio values
  plot(x$probs ~ x$values,
       bty = "l", las = 1,
       type = "n",
       xlab = "P:B ratio", ylab = "Probability")
  lines(x$probs ~ x$values,
        lty = 1, lwd = 2,
        col = "gray50")
  points(x$probs ~ x$values,
         pch = 16, cex = 1.2,
         col = "black")
  
}

# internal function: create food_web object
as.pb_ratio <- function (pb_ratio) {
  as_class(pb_ratio, name = "pb_ratio", type = "list")
}
