# test functions

quietly <- function (expr)
  invisible(capture.output(expr))

expect_ok <- function (expr)
  expect_error(expr, NA)

# simulate a simple food web without stochasticity to test calculation
