context('food_web class')

test_that('print works', {

  source('helpers.R')

  test_fw <- matrix(rbinom(16, size = 1, p = 0.5), ncol = 4)
  test_fw <- test_fw * lower.tri(test_fw)
  
  # print method
  expected_output <- "This is a fixed food_web object with 4 species"
  expect_identical(print(test_fw), expected_output)

})

test_that('as.food_web and is.food_web work', {

  source('helpers.R')

  # as.food_web (### IS THIS EXPORTED?)
  
  # is.food_web

})

test_that('food_web errors are correct', {
  
  source('helpers.R')
  
  
})


