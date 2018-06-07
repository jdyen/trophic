context('pb_ratio class')

test_that('build_pb_ratio works', {
  
  expect_silent(build_pb_ratio(range = c(0.25, 5.75),
                               probs = c(5, 20, 10, 3, 1, 1, 1)))
  expect_silent(build_pb_ratio(range = c(0.25, 5.75), type = "fixed"))
  expect_silent(build_pb_ratio(range = c(0.25, 5.75), length = 3, type = "gradient"))

})

test_that('is.pb_ratio works', {
  
  expect_true(is.pb_ratio(build_pb_ratio(range = c(0.25, 5.75),
                                                  probs = c(5, 20, 10, 3, 1, 1, 1))))
  expect_false(is.pb_ratio(rnorm(100)))
  
})

test_that('print.pb_ratio works', {
  
  expect_output(print(build_pb_ratio(range = c(0.25, 5.75),
                                     probs = c(5, 20, 10, 3, 1, 1, 1))),
                "pb_ratio object")
  
})

test_that('plot.pb_ratio works', {
  
  expect_silent(plot(build_pb_ratio(range = c(0.25, 5.75),
                                    probs = c(5, 20, 10, 3, 1, 1, 1))))
  expect_error(plot(build_pb_ratio(range = c(0.25, 5.75),
                                   length = 3)))
  expect_error(plot(build_pb_ratio(range = c(0.25, 5.75),
                                   probs = c(5, 20, 10, 3, 1, 1, 1),
                                   type = "fixed")))
  
})

test_that('errors as expected', {
  
  expect_warning(build_pb_ratio(range = c(0.5, 5), length = 3, type = "fixed"))
  expect_warning(build_pb_ratio(range = c(0.5, 5), type = "gradient"))
  expect_warning(build_pb_ratio(range = c(0.5, 5), length = 3))
  expect_warning(build_pb_ratio(range = c(0.5, 5), probs = c(0.5, 0.25, 0.2, 0.05), type = "gradient"))
  expect_warning(build_pb_ratio(range = c(0.5, 5), type = "stochastic"))
  expect_warning(build_pb_ratio(range = c(0.5, 5), length = 5, type = "stochastic"))
  
})
