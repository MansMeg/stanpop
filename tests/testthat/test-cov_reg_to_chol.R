# Run the line below to run different test suites locally
# See documentation for details.
# stanpop:::set_test_stan_basic_on_local(TRUE)
# stanpop:::set_test_stan_full_on_local(TRUE)
# options(mc.cores = parallel::detectCores())
if(FALSE){ # For debugging
  library(testthat)
  library(stanpop)
  library(rstan)
}

test_that("Test cov_reg_to_chol", {
  test_stan_full_on_local <- get_internal("test_stan_full_on_local")
  skip_if_not(test_stan_full_on_local())

  set.seed(4711)
  n = 1000
  p = 5

  Sigma = solve(stats::toeplitz(c(2,-1,rep(0,p-2))))
  Y = matrix(0, nrow = n, ncol= p)

  L <- t(chol(Sigma))
  for(i in 1:n){
    Y[i,] <- L%*%rnorm(p,1)
  }
  stan_data = list(Y=Y, N=n, P=p)

  test_model_path <- "tests/testthat/stan_code/cov_reg_to_chol.stan"
  fit <- rstan::stan(file = test_model_path,
              data    = stan_data,
              warmup  = 500,
              iter    = 1000,
              chains  = 2,
              cores   = 2,
              thin    = 1)

  Sigma_hat <- extract(fit,'Q')$Q[1,,]
  expect_true(all(abs(Sigma_hat - Sigma) < 0.2))

})

