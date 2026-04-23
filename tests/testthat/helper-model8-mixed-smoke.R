make_model8_mixed_smoke_case <- function(npolls = 40) {
  data("x_test")
  txdf <- as.data.frame(x_test[3:4])
  colnames(txdf) <- paste0("x", 3:length(x_test))
  data("pd_test")

  time_scale <- "week"
  parties <- c("x3", "x4")
  set.seed(4711)
  true_idx <- c(44, 72)
  known_state <- tibble::tibble(
    date = as.Date("2010-01-01") + lubridate::weeks(true_idx - 1)
  )
  known_state <- cbind(known_state, txdf[true_idx, ])

  polls_data <- simulate_polls(
    x = txdf,
    pd = pd_test,
    npolls = npolls,
    time_scale = time_scale,
    start_date = "2010-01-01"
  )

  list(
    polls_data = polls_data,
    known_state = known_state,
    parties = parties,
    time_scale = time_scale,
    time_scale_overrides = tibble::tibble(
      from = as.Date("2010-05-05"),
      to = as.Date("2010-05-10"),
      time_scale = "day"
    )
  )
}

# Assert that one fitted in-range known-state time point is fixed while the
# adjacent latent time points still have posterior uncertainty.
#
# This helper is intentionally defined on the latent index `known_t` rather
# than a calendar date. That keeps the assertion focused on the reconstructed
# latent-state draw array itself, independent of how dates map onto the mixed
# latent grid. The known-state reference values are taken from
# `pop$stan_data$stan_data$x_known_t` and `x_known`, so only known states that
# were actually included in the fitted model are considered. This avoids false
# failures when `pop$known_state` contains rows outside the model time range.
#
# Args:
#   pop: A fitted `poll_of_polls` object.
#   known_t: One latent time index that must appear in `x_known_t`.
#   parties: Character vector of party names to check. Defaults to all modeled
#     parties in `pop$y`.
#   tolerance: Numerical tolerance for equality at the known state. Kept
#     slightly above machine-noise to allow backend-specific floating-point
#     differences in reconstructed latent-state draws.
#
# Returns:
#   Invisibly returns the checked `known_t` and its neighboring latent indices.
expect_known_t_fixed_and_neighbors_vary <- function(pop,
                                                    known_t,
                                                    parties = pop$y,
                                                    tolerance = 1e-8) {
  checkmate::assert_class(pop, "poll_of_polls")
  checkmate::assert_integerish(known_t, len = 1L)
  checkmate::assert_character(parties, any.missing = FALSE, min.len = 1L)
  checkmate::assert_true(all(parties %in% pop$y))
  checkmate::assert_number(tolerance, lower = 0)

  ls <- latent_state(pop)
  known_t <- as.integer(known_t)
  sd <- pop$stan_data$stan_data
  known_idx <- match(known_t, sd$x_known_t)
  neighbor_t <- known_t + c(-1L, 1L)

  testthat::expect_false(
    is.na(known_idx),
    info = paste0("t = ", known_t, " should correspond to an in-range known state.")
  )
  testthat::expect_true(
    all(neighbor_t >= 1L & neighbor_t <= dim(ls$latent_state)[2]),
    info = paste0("Neighbor t values around known t = ", known_t, " should be inside the latent grid.")
  )

  for(party in parties) {
    party_idx <- match(party, pop$y)
    known_value <- sd$x_known[known_idx, party_idx]
    party_draws <- as.numeric(ls$latent_state[, known_t, party])

    testthat::expect_equal(
      party_draws,
      rep(known_value, length(party_draws)),
      tolerance = tolerance,
      info = paste0("Known state should be fixed for party '", party, "'.")
    )
  }

  for(i in seq_along(neighbor_t)) {
    neighbor_draws <- ls$latent_state[, neighbor_t[i], parties, drop = FALSE]
    neighbor_sds <- apply(neighbor_draws, 3, stats::sd)

    testthat::expect_true(
      all(is.finite(neighbor_sds)),
      info = paste0("Neighbor t = ", neighbor_t[i], " should have finite posterior variation.")
    )
    testthat::expect_true(
      all(neighbor_sds > 0),
      info = paste0("Neighbor t = ", neighbor_t[i], " should not collapse to a known state.")
    )
  }

  invisible(list(
    known_t = known_t,
    neighbor_t = neighbor_t
  ))
}
