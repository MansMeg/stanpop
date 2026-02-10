#' Create a map between constrained parameter names and unconstrained parameter indecies
#'
#' @param x a poll of polls object
#' @param return_first return the first character element (the unconstrained,
#' non-transformed parameter)
#'
#' @description
#' Go through all unconstrained indecies and return all parameters
#' that changes due to the constrained parameter.
#'
#' Note that in a state space model a unconstrated parameter can change
#' a whole series due to the model formulation.
#'
#'
#' @export
create_constrained_names_to_unconstrained_idx_map <- function(x, return_first = FALSE){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_flag(return_first)

  # Recompile model without generated quantities
  mc <- x$stan_fit@stanmodel@model_code
  mc <- utils::capture.output(cat(mc))
  gqidx <- which(grepl(x = mc, pattern = "generated quantities\\{"))
  if(length(gqidx)!=1) stop("Cannot remove 'generated quantities{'")
  mc <- paste(mc[1:(gqidx-1)], collapse = "\n")
  message("Recompiling model and data")
  out <- utils::capture.output(
    srm <- rstan::stan(model_code = mc,
                       iter = 1, warmup = 0, chains = 1, data = x$stan_data$stan_data)
  )
  message("Running through all unconstrained parameters")
  # srm <- recompile_stanfit(x)
  nupars <- rstan::get_num_upars(srm)
  pars_values <- rep(0.0, nupars)
  base_comparison <- rstan::constrain_pars(srm, pars_values)
  results <- list()
  for(i in 1:nupars){
    vals <- pars_values
    vals[i] <- 1.0
    comp <- rstan::constrain_pars(srm, vals)
    diff_names <- return_diff_names(x = base_comparison, y = comp)
    results[[i]] <- unname(diff_names)
  }
  if(return_first){
    unlist(lapply(results, function(x) x[1]))
  } else {
    return(results)
  }

}

return_diff_names <- function(x, y){
  checkmate::assert_list(x)
  checkmate::assert_list(y)
  checkmate::assert_true(all(names(x) == names(y)))

  ns <- names(x)
  results <- list();results <- results[1:length(ns)]
  names(results) <- ns
  for(i in seq_along(names(x))){
    if(is.null(dim(x[[i]]))){
      dim(x[[i]]) <- dim(y[[i]]) <- 1L
    }
    res <- which(!x[[i]] == y[[i]], arr.ind = TRUE, useNames = TRUE)
    if(nrow(res) > 0){
      res_char <- character(length = nrow(res))
      for(j in 1:nrow(res)){
        res_char[j] <- paste0(names(x)[i],"[", paste0(res[j,], collapse = ","),"]")
      }
      results[[i]] <- res_char
    }
  }
  return(unlist(results))
}

# The two functions should work in crosschecks.
# res1 <- create_constrained_names_to_unconstrained_idx_map(pop, TRUE)
# res2 <- create_unpar_one_to_one_map(pop)
# all(res == res2)

#' Semi-automatic construction of parameters for specific models
#'
#' @param x a poll of polls object
#'
#' @details
#' Create the names of the unconstrained parameters from specific models
#'
#' @export
create_unpar_one_to_one_map <- function(x){
  checkmate::assert_class(x, "poll_of_polls")
  checkmate::assert_true(x$model %in% c("model8k2"))

  no_pars <- length(parameter_names(x))
  map <- character(no_pars)
  idx <- 1L

  # matrix[use_softmax ? T - T_known : 0, use_softmax ? P : 0] eta_z_unknown; // unknown states (proportions)
  checkmate::assert_true(x$stan_data$stan_data$use_softmax == 1L)
  no_eta_unknown_t <- x$stan_data$stan_data$T - x$stan_data$stan_data$T_known
  P <- x$stan_data$stan_data$P
  for(j in 1:P){
    for(i in 1:no_eta_unknown_t){
      map[idx] <- paste0("eta_z_unknown[",i ,",",j,"]")
      idx <- idx + 1L
    }
  }

  # vector<lower=0>[P] sigma_x; // dynamic movement
  for(i in 1:P){
    map[idx] <- paste0("sigma_x[",i ,"]")
    idx <- idx + 1L
  }

  # matrix[use_industry_bias ? no_unknown_kappa : 0, use_industry_bias ? P : 0] kappa_raw; // Industry bias
  checkmate::assert_true(x$stan_data$stan_data$use_industry_bias == 1L)
  no_unknown_kappa <-
    x$stan_data$stan_data$estimate_kappa_next +
    x$stan_data$stan_data$T_known
  for(j in 1:P){
    for(i in 1:no_unknown_kappa){
      map[idx] <- paste0("kappa_raw[",i ,",",j,"]")
      idx <- idx + 1L
    }
  }

  # vector<lower=0>[use_industry_bias ? P : 0] sigma_kappa; // Industry bias effect
  checkmate::assert_true(x$stan_data$stan_data$use_industry_bias == 1L)
  for(i in 1:P){
    map[idx] <- paste0("sigma_kappa[",i ,"]")
    idx <- idx + 1L
  }

  # real beta_mu[use_house_bias ? S : 0, use_house_bias ? H : 0, use_house_bias ? P : 0];
  checkmate::assert_true(x$stan_data$stan_data$use_house_bias == 1L)
  S <- x$stan_data$stan_data$S
  H <- x$stan_data$stan_data$H
  for(k in 1:S){
    for(j in 1:H){
      for(i in 1:P){
        map[idx] <- paste0("beta_mu[",k ,",",j,",",i,"]")
        idx <- idx + 1L
      }
    }
  }

  # real<lower=0> sigma_beta_mu[use_house_bias ? 1 : 0];
  map[idx] <- paste0("sigma_beta_mu[1]")
  idx <- idx + 1L

  #   real beta_sigma[use_design_effects ? S : 0, use_design_effects ? H : 0];
  checkmate::assert_true(x$stan_data$stan_data$use_design_effects == 1L)
  for(j in 1:S){
    for(i in 1:H){
      map[idx] <- paste0("beta_sigma[",j ,",",i,"]")
      idx <- idx + 1L
    }
  }

  # real<lower=0> sigma_beta_mu[use_house_bias ? 1 : 0];
  map[idx] <- paste0("sigma_beta_sigma[1]")
  idx <- idx + 1L

  # real<lower=-1,upper=1> alpha_kappa_unknown[estimate_alpha_kappa ? 1 : 0];
  checkmate::assert_true(x$stan_data$stan_data$estimate_alpha_kappa == 1L)
  map[idx] <- paste0("alpha_kappa_unknown[1]")
  idx <- idx + 1L

  # real<lower=-1,upper=1> alpha_beta_mu_unknown[estimate_alpha_beta_mu ? 1 : 0];
  checkmate::assert_true(x$stan_data$stan_data$estimate_alpha_beta_mu == 0L)
  # map[idx] <- paste0("alpha_beta_mu_unknown[1]")
  # idx <- idx + 1L

  # real<lower=-1,upper=1> alpha_beta_sigma_unknown[estimate_alpha_beta_sigma ? 1 : 0];
  checkmate::assert_true(x$stan_data$stan_data$estimate_alpha_beta_sigma == 0L)

  # vector<lower=0>[use_t_dist_industry_bias ? 1 : 0] nu_kappa_raw;
  checkmate::assert_true(x$stan_data$stan_data$use_t_dist_industry_bias == 1L)
  map[idx] <- paste0("nu_kappa_raw[1]")
  idx <- idx + 1L

  # vector<lower=0>[use_t_dist_industry_bias ? 1 : 0] v_kappa;
  map[idx] <- paste0("v_kappa[1]")
  idx <- idx + 1L


  # matrix<lower=0>[use_jump_process ? T : 0, use_jump_process ? P : 0] V_noise;
  # vector<lower=2,upper=4>[use_jump_process ? P : 0] alpha_V; //shape of jumps
  # vector<lower=0,upper=1>[use_jump_process ? P : 0] ar_V; // AR component for jump
  # vector<lower=0,upper=1>[use_jump_process ? P : 0] theta_x; // proportion of jump vs Gauss

  # cholesky_factor_corr [use_cholesky_factor_corr ? P : 0] L_Omega_x[use_cholesky_factor_corr ? no_Omega : 0]; // correlation matrix
  checkmate::assert_true(x$stan_data$stan_data$use_multivariate_version < 3L)
  checkmate::assert_true(x$stan_data$stan_data$use_multivariate_version > 1L)
  no_Omega <- S
  for(k in 1:no_Omega){
    for(j in 1:P){
      for(i in 1:P){
        if(i >= j) next
        map[idx] <- paste0("L_Omega_x[",k ,",",j,",",i,"]")
        idx <- idx + 1L
      }
    }
  }
  # matrix[use_cov_reg ? no_Omega : 0, use_cov_reg ? Pp : 0] psi; // psi params for constructing covariance
  # real<lower=0> sigma_psi[use_sigma_psi ? 1 : 0];
  return(map[1:(idx-1)])
}
