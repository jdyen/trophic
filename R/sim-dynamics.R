# main functions in trophic R package

sim_dynamics <- function(foodweb,
                         primary = NULL,
                         efficiencies = NULL,
                         leakage = NULL,
                         settings = list()) {

  # check food web for errors


  # check efficiencies for errors and fill if NULL


  # check leakage for errors and fill if NULL


  # check inits for errors and fill if NULL


  # unpack settings
  sim_set <- list(n = 1000,
                  tol = 1e-5,
                  pb_ratio = )
  sim_set[names(settings)] <- settings

  # simulate


  # tidy outputs
  out <- NULL

  # return outputs
  out

}

