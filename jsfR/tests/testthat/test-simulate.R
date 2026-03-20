# Unit tests for jsfR simulation wrappers
# Skip all tests if Python jsf is not available
skip_if_not(
  reticulate::py_module_available("jsf"),
  message = "Python 'jsf' module not available"
)

test_that("jsf_available() returns TRUE when jsf is installed", {
  expect_true(jsf_available())
})

test_that("simulate_jsf() returns a valid list with expected components", {
  rates_fn <- function(x, t) c(1.0 * x[[1]], 0.5 * x[[1]])
  stoich <- list(
    nu         = list(c(1L), c(-1L)),
    DoDisc     = c(1L),
    nuReactant = list(c(1L), c(1L)),
    nuProduct  = list(c(2L), c(0L))
  )

  result <- simulate_jsf(
    x0    = 5L,
    rates = rates_fn,
    stoich = stoich,
    t_max  = 2.0,
    seed   = 42L
  )

  expect_type(result, "list")
  expect_named(result, c("compartments", "times"))
  expect_type(result$compartments, "list")
  expect_length(result$compartments, 1)

  # First value should be the initial condition
  expect_equal(result$compartments[[1]][1], 5)

  # Times should be monotonically non-decreasing
  expect_true(all(diff(result$times) >= 0))

  # Final time should be approximately t_max
  expect_true(tail(result$times, 1) >= 2.0 - 0.02)
})

test_that("simulate_birth_death_jsf() produces valid output", {
  result <- simulate_birth_death_jsf(
    lambda = 1.0, mu = 0.5, x0 = 10L,
    t_max  = 3.0, seed = 123L
  )

  expect_type(result, "list")
  expect_equal(result$compartments[[1]][1], 10)
  expect_true(length(result$times) > 1)
})

test_that("simulate_jsf() rejects bad inputs", {
  rates_fn <- function(x, t) c(1.0 * x[[1]])
  stoich <- list(
    nu = list(c(1L)), DoDisc = c(1L),
    nuReactant = list(c(1L)), nuProduct = list(c(2L))
  )

  # Negative t_max
  expect_error(
    simulate_jsf(5L, rates_fn, stoich, t_max = -1),
    "t_max > 0"
  )

  # Invalid method
  expect_error(
    simulate_jsf(5L, rates_fn, stoich, t_max = 1, method = "bogus")
  )
})

test_that("mean of many JSF birth-death runs approximates E[X(t)]", {
  # Parameters
  lam   <- 1.0
  mu    <- 0.5
  x0    <- 5L
  t_max <- 4.0
  n_rep <- 80

  finals <- vapply(seq_len(n_rep), function(i) {
    res <- simulate_birth_death_jsf(
      lambda = lam, mu = mu, x0 = x0,
      t_max  = t_max, seed = as.integer(i * 997)
    )
    tail(res$compartments[[1]], 1)
  }, numeric(1))

  observed_mean <- mean(finals)
  expected_mean <- x0 * exp((lam - mu) * t_max)
  se            <- sd(finals) / sqrt(n_rep)

  # Two-sigma test
  expect_lt(abs(observed_mean - expected_mean), 3 * se)
})
