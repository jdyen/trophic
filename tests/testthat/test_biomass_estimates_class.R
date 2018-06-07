context('biomass_estimates class')

capture.output(
  test_fw <- build_food_web(interaction_matrix = food_web),
  test_efficiency_matrix <- build_efficiency_matrix(efficiency_mean = efficiency_mean,
                                                    efficiency_sd = 0.01),
  test_dominance <- build_dominance_matrix(dominance = dominance_matrix),
  test_primary_producers <- build_primary_producers(production_mean = production_mean,
                                                    production_sd = production_sd),
  test_trophic_dynamics <- build_trophic_dynamics(food_web = test_fw,
                                                  efficiency_matrix = test_efficiency_matrix,
                                                  dominance_matrix = test_dominance),
  production_estimates <- estimate_production(test_trophic_dynamics,
                                              test_primary_producers),
  test_pb_ratio <- build_pb_ratio(range = c(0.25, 5.75),
                                  probs = c(5, 20, 10, 3, 1, 1, 1))
)

test_that('estimate_biomass works', {
  
  expect_silent(estimate_biomass(production_estimates = production_estimates,
                                 pb_ratio = test_pb_ratio))
  
  test_pb <- build_pb_ratio(range = c(0.25, 5.75), type = "fixed")
  expect_silent(estimate_biomass(production_estimates = production_estimates,
                                 pb_ratio = test_pb))
  
  test_pb <- build_pb_ratio(range = c(0.25, 5.75), length = 3)
  expect_silent(estimate_biomass(production_estimates = production_estimates,
                                 pb_ratio = test_pb))
  
})

test_that('is.biomass_estimates works', {

  estimates <- estimate_biomass(production_estimates = production_estimates,
                                pb_ratio = test_pb_ratio)
  expect_true(is.biomass_estimates(estimates))
  expect_false(is.biomass_estimates(rnorm(100)))
  
})

test_that('print.biomass_estimates works', {
  
  estimates <- estimate_biomass(production_estimates = production_estimates,
                                pb_ratio = test_pb_ratio)
  expect_output(print(estimates),
                "This is a biomass_estimates object")
  
})

test_that('plot.biomass_estimates works', {
  
  estimates <- estimate_biomass(production_estimates = production_estimates,
                                pb_ratio = test_pb_ratio)
  expect_silent(plot(estimates))
  expect_silent(plot(estimates, nodes = c(15:19)))
  expect_silent(plot(estimates, nodes = c("carp", "small.fish")))
  
})

test_that('extract_nodes works', {
  
  estimates <- estimate_biomass(production_estimates = production_estimates,
                                pb_ratio = test_pb_ratio)
  expect_silent(extract_nodes(estimates, nodes = "small.fish"))
  expect_silent(extract_nodes(estimates, nodes = c(15:18)))
  expect_silent(extract_nodes(estimates, nodes = c("small.fish", "large.bodied.native.fish")))
  expect_error(extract_nodes(estimates, nodes = 15))
  expect_error(extract_nodes(estimates, nodes = c(5.1525, 2.25151)))
  
})
