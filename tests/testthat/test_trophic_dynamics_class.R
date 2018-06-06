context('trophic_dynamics class')

capture.output(
  test_fw <- build_food_web(interaction_matrix = food_web),
  test_efficiency_matrix <- build_efficiency_matrix(efficiency_mean = efficiency_mean,
                                                    efficiency_sd = 0.01),
  test_dominance <- build_dominance_matrix(dominance = dominance_matrix)
)

test_that('build_trophic_dynamics works', {
  
  expect_silent(build_trophic_dynamics(food_web = test_fw,
                                       efficiency_matrix = test_efficiency_matrix,
                                       dominance_matrix = test_dominance))
  expect_silent(build_trophic_dynamics(food_web = list(test_fw, test_fw),
                                       efficiency_matrix = test_efficiency_matrix,
                                       dominance_matrix = test_dominance))
  expect_silent(build_trophic_dynamics(food_web = test_fw,
                                       efficiency_matrix = list(test_efficiency_matrix,
                                                                test_efficiency_matrix),
                                       dominance_matrix = test_dominance))
  expect_silent(build_trophic_dynamics(food_web = test_fw,
                                       efficiency_matrix = test_efficiency_matrix,
                                       dominance_matrix = list(test_dominance, test_dominance)))
  expect_silent(build_trophic_dynamics(food_web = test_fw,
                                       efficiency_matrix = test_efficiency_matrix,
                                       dominance_matrix = NULL))
  expect_silent(build_trophic_dynamics(food_web = test_fw,
                                       efficiency_matrix = NULL,
                                       dominance_matrix = test_dominance))
  expect_silent(build_trophic_dynamics(food_web = test_fw,
                                       efficiency_matrix = NULL,
                                       dominance_matrix = NULL))
  

})

test_that('print works', {
  
  new_dynamics <- build_trophic_dynamics(food_web = test_fw,
                                         efficiency_matrix = test_efficiency_matrix,
                                         dominance_matrix = test_dominance)  
  
  # print method
  expect_output(print(new_dynamics), "This is a trophic_dynamics object")
  
})

test_that('is.trophic_dynamics works', {
  
  new_dynamics <- build_trophic_dynamics(food_web = test_fw,
                                         efficiency_matrix = test_efficiency_matrix,
                                         dominance_matrix = test_dominance)  
  
  # is.food_web expect TRUE
  expect_true(is.trophic_dynamics(new_dynamics))
  
  # is.food_web expect FALSE
  expect_false(is.trophic_dynamics(rnorm(100)))
  
  # errors with no argument
  expect_error(is.trophic_dynamics())
  
})

test_that('plot works', {
  
  new_dynamics <- build_trophic_dynamics(food_web = test_fw,
                                         efficiency_matrix = test_efficiency_matrix,
                                         dominance_matrix = test_dominance)
  expect_silent(plot(new_dynamics))
  
})

test_that('errors work if build_trophic_dynamics inputs are wrong', {
  
  expect_output(build_trophic_dynamics(food_web = rnorm(100),
                                       efficiency_matrix = test_efficiency_matrix,
                                       dominance_matrix = test_dominance),
                "food_web object must be a single food_web object")
  expect_output(build_trophic_dynamics(food_web = list(rnorm(100), test_fw),
                                       efficiency_matrix = test_efficiency_matrix,
                                       dominance_matrix = test_dominance),
                "food_web object must be a single food_web object")
  expect_output(build_trophic_dynamics(food_web = test_fw,
                                       efficiency_matrix = rnorm(100),
                                       dominance_matrix = test_dominance),
                "efficiency_matrix must be a single efficiency_matrix object")
  expect_output(build_trophic_dynamics(food_web = test_fw,
                                       efficiency_matrix = list(rnorm(100), test_efficiency_matrix),
                                       dominance_matrix = test_dominance),
                "efficiency_matrix must be a single efficiency_matrix object")
  expect_output(build_trophic_dynamics(food_web = test_fw,
                                       efficiency_matrix = test_efficiency_matrix,
                                       dominance_matrix = rnorm(100)),
                "dominance_matrix must be a single dominance_matrix object")
  expect_output(build_trophic_dynamics(food_web = test_fw,
                                       efficiency_matrix = test_efficiency_matrix,
                                       dominance_matrix = list(rnorm(100), test_dominance)),
                "dominance_matrix must be a single dominance_matrix object")
  expect_output(build_trophic_dynamics(food_web = list(test_fw, test_fw, test_fw),
                                       efficiency_matrix = list(test_efficiency_matrix,
                                                                test_efficiency_matrix),
                                       dominance_matrix = test_dominance),
                "any values greater than one should be equal")
  
})
