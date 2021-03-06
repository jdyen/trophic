% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/primary_producers-class.R
\name{build_primary_producers}
\alias{build_primary_producers}
\alias{is.primary_producers}
\alias{print.primary_producers}
\title{Create a primary_producers object to use in a trophic projection}
\usage{
build_primary_producers(production_mean, production_sd, ...)

is.primary_producers(x)

\method{print}{primary_producers}(x, ...)
}
\arguments{
\item{production_mean}{a vector of production means}

\item{production_sd}{a vector of production standard deviations}

\item{...}{further arguments passed to or from other methods}

\item{x}{an object to print or test as a primary_producers object}
}
\value{
An object of class \code{primary_producers}
}
\description{
A 'primary_producers' object stores the primary production inputs for a set of nodes in a food web
}
\examples{

library(trophic)

# Construct the primary_producers object

test_producers <- build_primary_producers(production_mean = c(1, 2, 3),
                                          production_sd = c(0.1, 0.5, 0.2))

# Test if object is of the type 'primary_producers'
  
\dontrun{
is.primary_producers(x)
}

# Print information about the 'primary_producers' object

\dontrun{
print(x)
}
}
