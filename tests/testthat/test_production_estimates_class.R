context('production_estimates class')

capture.output(
  test_fw <- build_food_web(interaction_matrix = food_web),
  test_efficiency_matrix <- build_efficiency_matrix(efficiency_mean = efficiency_mean,
                                                    efficiency_sd = 0.01),
  test_dominance <- build_dominance_matrix(dominance = dominance_matrix),
  test_primary_producers <- build_primary_producers(production_mean = production_mean,
                                                    production_sd = production_sd),
  test_trophic_dynamics <- build_trophic_dynamics(food_web = test_fw,
                                                  efficiency_matrix = test_efficiency_matrix,
                                                  dominance_matrix = test_dominance)
)

test_that('estimate matches expected values in fixed case', {
  
  source('helpers.R')

  # estimate production values from constructed trophic_dynamics object
  production_estimates <- estimate_production(test_trophic_dynamics, test_primary_producers,
                                              stochastic = c("bananas"), nsim = 2)

  # estimate production values with fixed model
  production_fixed <- fixed_projection(test_trophic_dynamics, test_primary_producers)
  
  # test within and among method equality
  expect_equal(production_estimates$production[[1]], production_estimates$production[[2]])
  expect_equal(production_estimates$production[[1]], production_fixed$production[[1]])
  expect_equal(production_fixed$production[[1]], production_fixed$production[[2]])
  
})

test_that('estimate production works', {
  
  # expect nsim = 1 and output if stochastic = NULL
  
})

