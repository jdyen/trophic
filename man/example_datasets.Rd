% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example-datasets.R
\docType{data}
\name{food_web}
\alias{food_web}
\alias{efficiency_mean}
\alias{dominance_matrix}
\alias{production_mean}
\alias{production_sd}
\title{Example food web data}
\format{A matrix with one row and column for each species. Values in cell {i, j} indicates that the species in row i eats the species in column j}
\usage{
data(food_web)

data(efficiency_mean)

data(food_web)

data(production_mean)

data(production_sd)
}
\description{
Example food web data with 20 species

Example efficiency matrix data with 20 species

Example dominance matrix data with 20 species

Example mean primary production data for seven primary sources

Example standard deviations of primary production data for seven primary sources
}
\examples{
library(trophic)
food_web_test <- build_food_web(food_web)
plot(food_web_test)
library(trophic)
efficiency_matrix_test <- build_efficiency_matrix(efficiency_mean)
efficiency_matrix_test
library(trophic)
dominance_matrix_test <- build_dominance_matrix(dominance_matrix)
dominance_matrix_test
library(trophic)
production_test <- build_primary_producers(production_mean, production_sd)
production_test
library(trophic)
production_test <- build_primary_producers(production_mean, production_sd)
production_test
}
\keyword{datasets}
