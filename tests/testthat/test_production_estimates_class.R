context('production_estimates class')

test_that('estimate matches expected values in fixed case', {
  
  source('helpers.R')
  
  test_fw <- build_food_web(interaction_matrix = food_web)
  test_efficiency_matrix <- build_efficiency_matrix(efficiency_mean = efficiency_mean,
                                                    efficiency_sd = 0.01)
  test_dominance <- build_dominance_matrix(dominance = dominance_matrix)
  test_primary_producers <- build_primary_producers(production_mean = production_mean,
                                                    production_sd = production_sd)
  test_trophic_dynamics <- build_trophic_dynamics(food_web = test_fw,
                                                  efficiency_matrix = test_efficiency_matrix,
                                                  dominance_matrix = test_dominance)
   
  # estimate production values from constructed trophic_dynamics object
  production_estimates <- estimate_production(test_trophic_dynamics, test_primary_producers,
                                              stochastic = c("bananas"), nsim = 2)

  # estimate production values with fixed model
  production_fixed <- fixed_projection(test_trophic_dynamics, test_primary_producers)
  
  # test within and among method equality
  expect_equal(production_estimates$production[[1]][, 1], production_estimates$production[[1]][, 2])
  expect_equal(production_estimates$production[[1]][, 1], production_fixed[, 1])
  expect_equal(production_fixed[, 1], production_fixed[, 2])
  
})

test_that('estimate production works', {
  
  test_fw <- build_food_web(interaction_matrix = food_web)
  test_efficiency_matrix <- build_efficiency_matrix(efficiency_mean = efficiency_mean,
                                                    efficiency_sd = 0.01)
  test_dominance <- build_dominance_matrix(dominance = dominance_matrix)
  test_primary_producers <- build_primary_producers(production_mean = production_mean,
                                                    production_sd = production_sd)
  test_trophic_dynamics <- build_trophic_dynamics(food_web = test_fw,
                                                  efficiency_matrix = test_efficiency_matrix,
                                                  dominance_matrix = test_dominance)
  
  # expect nsim = 1 and output if stochastic = NULL
  expect_output(estimate_production(test_trophic_dynamics, test_primary_producers,
                                    stochastic = NULL, nsim = 2),
                "All variables are deterministic")
  
  # expect nsim to not be reduced if stochastic is random string
  expect_silent(estimate_production(test_trophic_dynamics, test_primary_producers,
                                    stochastic = c("bananas"), nsim = 2))
  
  # but expect all production estimates to be equal in this case
  prod_est <- estimate_production(test_trophic_dynamics, test_primary_producers,
                                  stochastic = c("bananas"), nsim = 2)
  expect_equal(prod_est$production[[1]][, 1], prod_est$production[[1]][, 2])

  # expect silent if nsim > 1 and any combo of inputs stochastic
  expect_silent(estimate_production(test_trophic_dynamics, test_primary_producers,
                                    stochastic = c("food_web"), nsim = 3))
  expect_silent(estimate_production(test_trophic_dynamics, test_primary_producers,
                                    stochastic = c("efficiency"), nsim = 3))
  expect_silent(estimate_production(test_trophic_dynamics, test_primary_producers,
                                    stochastic = c("primary_production"), nsim = 3))
  expect_silent(estimate_production(test_trophic_dynamics, test_primary_producers,
                                    stochastic = c("food_web", "efficiency"), nsim = 3))
  expect_silent(estimate_production(test_trophic_dynamics, test_primary_producers,
                                    stochastic = c("food_web", "primary_production"), nsim = 3))
  expect_silent(estimate_production(test_trophic_dynamics, test_primary_producers,
                                    stochastic = c("efficiency", "primary_production"), nsim = 3))
  expect_silent(estimate_production(test_trophic_dynamics, test_primary_producers,
                                    stochastic = c("food_web", "efficiency", "primary_production"), nsim = 3))
  
  # expect silent if multiple food webs, efficiencies, or dominances provided
  test_trophic_dynamics2 <- build_trophic_dynamics(food_web = list(test_fw, test_fw),
                                                   efficiency_matrix = test_efficiency_matrix,
                                                   dominance_matrix = test_dominance)
  expect_silent(estimate_production(test_trophic_dynamics2, test_primary_producers,
                                    stochastic = c("efficiency"), nsim = 3))
  test_trophic_dynamics2 <- build_trophic_dynamics(food_web = test_fw,
                                                   efficiency_matrix = list(test_efficiency_matrix, test_efficiency_matrix),
                                                   dominance_matrix = test_dominance)
  expect_silent(estimate_production(test_trophic_dynamics2, test_primary_producers,
                                    stochastic = c("efficiency"), nsim = 3))
  test_trophic_dynamics2 <- build_trophic_dynamics(food_web = test_fw,
                                                   efficiency_matrix = test_efficiency_matrix,
                                                   dominance_matrix = list(test_dominance, test_dominance))
  expect_silent(estimate_production(test_trophic_dynamics2, test_primary_producers,
                                    stochastic = c("efficiency"), nsim = 3))
  test_trophic_dynamics2 <- build_trophic_dynamics(food_web = list(test_fw, test_fw),
                                                   efficiency_matrix = test_efficiency_matrix,
                                                   dominance_matrix = list(test_dominance, test_dominance))
  expect_silent(estimate_production(test_trophic_dynamics2, test_primary_producers,
                                    stochastic = c("efficiency"), nsim = 3))
  test_trophic_dynamics2 <- build_trophic_dynamics(food_web = list(test_fw, test_fw),
                                                   efficiency_matrix = list(test_efficiency_matrix, test_efficiency_matrix),
                                                   dominance_matrix = list(test_dominance, test_dominance))
  expect_silent(estimate_production(test_trophic_dynamics2, test_primary_producers,
                                    stochastic = c("efficiency"), nsim = 3))
  
  # expect warning if names of primary producers do not match node names
  new_producers <- test_primary_producers
  names(new_producers) <- NULL
  expect_error(estimate_production(test_trophic_dynamics, new_producers,
                                   stochastic = NULL, nsim = 2))
  
  
})

test_that('print works', {
  
  test_fw <- build_food_web(interaction_matrix = food_web)
  test_efficiency_matrix <- build_efficiency_matrix(efficiency_mean = efficiency_mean,
                                                    efficiency_sd = 0.01)
  test_dominance <- build_dominance_matrix(dominance = dominance_matrix)
  test_primary_producers <- build_primary_producers(production_mean = production_mean,
                                                    production_sd = production_sd)
  test_trophic_dynamics <- build_trophic_dynamics(food_web = test_fw,
                                                  efficiency_matrix = test_efficiency_matrix,
                                                  dominance_matrix = test_dominance)

  production_estimates <- estimate_production(test_trophic_dynamics, test_primary_producers,
                                              stochastic = c("efficiency"), nsim = 2)
  
  # print method
  expect_output(print(production_estimates), "This is a production_estimates object")
  
})

test_that('is.food_web works', {
  
  test_fw <- build_food_web(interaction_matrix = food_web)
  test_efficiency_matrix <- build_efficiency_matrix(efficiency_mean = efficiency_mean,
                                                    efficiency_sd = 0.01)
  test_dominance <- build_dominance_matrix(dominance = dominance_matrix)
  test_primary_producers <- build_primary_producers(production_mean = production_mean,
                                                    production_sd = production_sd)
  test_trophic_dynamics <- build_trophic_dynamics(food_web = test_fw,
                                                  efficiency_matrix = test_efficiency_matrix,
                                                  dominance_matrix = test_dominance)

  production_estimates <- estimate_production(test_trophic_dynamics, test_primary_producers,
                                              stochastic = c("efficiency"), nsim = 2)
  
  # is.production_estimates expect TRUE
  expect_true(is.production_estimates(production_estimates))
  
  # is.production_estimates expect FALSE
  expect_false(is.production_estimates(rnorm(100)))
  
  # errors with no argument
  expect_error(is.production_estimates())
  
})

test_that('plot works', {

  test_fw <- build_food_web(interaction_matrix = food_web)
  test_efficiency_matrix <- build_efficiency_matrix(efficiency_mean = efficiency_mean,
                                                    efficiency_sd = 0.01)
  test_dominance <- build_dominance_matrix(dominance = dominance_matrix)
  test_primary_producers <- build_primary_producers(production_mean = production_mean,
                                                    production_sd = production_sd)
  test_trophic_dynamics <- build_trophic_dynamics(food_web = test_fw,
                                                  efficiency_matrix = test_efficiency_matrix,
                                                  dominance_matrix = test_dominance)
  
  production_estimates <- estimate_production(test_trophic_dynamics, test_primary_producers,
                                              stochastic = c("efficiency"), nsim = 2)
  expect_silent(plot(production_estimates))
  expect_silent(plot(production_estimates, nodes = c(15:18)))
  expect_silent(plot(production_estimates, nodes = c(18)))
  expect_silent(plot(production_estimates, nodes = c(15:18),
                     settings = list(pch = 15)))
  expect_silent(plot(production_estimates, nodes = c(16:18), log = "x"))
  production_estimates <- estimate_production(test_trophic_dynamics, test_primary_producers,
                                              stochastic = NULL, nsim = 100)
  expect_silent(plot(production_estimates))
  
})



