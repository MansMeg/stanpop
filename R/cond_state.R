#' Validate a conditional state object
#'
#' Checks that a conditional state contains the required hyperparameters
#' and active houses for supported conditional models.
#'
#' @param cond_state A list representation of a conditional state of the
#'   process, containing the hyperparameters and the active houses.
#' @param model Character scalar giving the model name. Currently only
#'   `"model8k6"` is supported.
#'
#' @return Invisibly returns `TRUE`. Throws an error if `cond_state` does not
#'   contain the required entries.
#' @keywords internal
assert_cond_state <- function(cond_state, model){
  # Takes in  conditional state object and asserts that all hyperparameter are present 
  if (model %in% c("model8k6")){
  keys <- c("eta_1_mu_hyper", "eta_1_sigma_hyper", "kappa_1_mu_hyper", "kappa_1_sigma_hyper", "beta_sigma_1_mu_hyper", "beta_sigma_1_sigma_hyper", "beta_mu_1_mu_hyper", "beta_mu_1_sigma_hyper")
  checkmate::assert(length(cond_state$houses)> 0)  
  }
  state <- cond_state$hyperparameters
  checkmate::assert_names(names(state),  permutation.of = keys)
  checkmate::assert(all(lengths(state) > 0))
}

#' Extract conditional hyperparameters
#'
#' Builds the list of conditional hyperparameters to append to the Stan data
#' for supported conditional models.
#'
#' @param cond_state A list representation of a conditional state of the
#'   process, containing the hyperparameters and the active houses.
#' @param poll_data The current poll data.
#' @param model Character scalar giving the model name. Currently only
#'   `"model8k6"` is supported.
#' 
#' @return A named list of hyperparameters formatted for Stan input.
#' @keywords internal
get_cond_hyperparams <- function(cond_state, poll_data, model){
     # Takes in poll_data and a list representation of a conditional state of the process
     # Returns a list of prior hyperparameters for the conditional Ada model
  
     state <- cond_state$hyperparameters
     houses_cond <- cond_state$houses
    
     # Aligning levels of the houses with the houses used to produce the conditional state
     house_levels <- levels(poll_data$poll_info$.house)
     checkmate::assert_subset(house_levels, houses_cond)
     houses_relevel <- factor(poll_data$poll_info$.house, levels = houses_cond)
     houses_key <- as.integer(factor(house_levels, levels = houses_cond))
     
     # Extracting hyperparameters
     cond_hyperparams <- list(use_conditional_model = 1,
                             H_cond = length(houses_cond),
                             houses_key = houses_key,
                             eta_1_mu_hyper = state$eta_1_mu_hyper,
                             eta_1_sigma_hyper = state$eta_1_sigma_hyper,
                             kappa_1_mu_hyper = state$kappa_1_mu_hyper,
                             kappa_1_sigma_hyper = state$kappa_1_sigma_hyper,
                             beta_sigma_1_mu_hyper = state$beta_sigma_1_mu_hyper[houses_key], # Selecting active houses only
                             beta_sigma_1_sigma_hyper = state$beta_sigma_1_sigma_hyper[houses_key],
                             beta_mu_1_mu_hyper = do.call(rbind, state$beta_mu_1_mu_hyper),
                             beta_mu_1_sigma_hyper = do.call(rbind, state$beta_mu_1_sigma_hyper)
                             )
     return(cond_hyperparams)
}
