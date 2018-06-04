#' Create a pb_ratio object to use in a trophic projection
#'
#' @description A 'pb ratio' object stores the potentially stochastic ratio of production to standing biomass
#' It is a sub-component of a \link[trophic]{trophic_dynamics} object
#' 
#' @rdname pb_ratio
#' 
#' @param pb_range a range of possible pb values
#' @param pb_prob relative probabilities of pb values in pb_range
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
#' test_pb <- build_pb_ratio(pb_range = c(0.25, 5.75), pb_prob = c(5, 20, 10, 3, 1, 1, 1))

build_pb_ratio <- function (pb_range, pb_prob, ...) {

  # create sequence of pb_values
  pb_values <- seq(pb_range[1], pb_range[2], length = length(pb_prob))
  
  # standardise pb_prob
  pb_prob <- pb_prob / sum(pb_prob)
  
  # create food_web object
  pb_ratio <- list(values = pb_values,
                   prob = pb_prob)
  
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
  cat("This is a pb_ratio object")
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
  plot(x$prob ~ x$values,
       bty = "l", las = 1,
       type = "n",
       xlab = "P:B ratio", ylab = "Probability")
  lines(x$prob ~ x$values,
        lty = 1, lwd = 2,
        col = "gray50")
  points(x$prob ~ x$values,
         pch = 16, cex = 1.2,
         col = "black")
  
}

# internal function: create food_web object
as.pb_ratio <- function (pb_ratio) {
  as_class(pb_ratio, name = "pb_ratio", type = "list")
}
