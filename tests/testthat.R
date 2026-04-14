library(testthat)
library(stanpop)

if (requireNamespace("rstan", quietly = TRUE)) {
  rstan::rstan_options(auto_write = TRUE)
}

test_check("stanpop")
