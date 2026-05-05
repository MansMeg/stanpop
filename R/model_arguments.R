#' Set default values and assert arguments for a model configuration
#'
#' @details
#' With model arguments, the arguments that are set by the
#' user for each poll_of_polls stan model. This differs between
#' models. See [model_arguments()] for the model arguments
#' used by each model.
#'
#' @param model the model for which to set the model arguments
#' @param x a list with model arguments (the model config) set by the user
#' @param stan_data a list with the stan data used for the model
#'
#' @export
model_config <- function(model, x = NULL, stan_data = NULL){
  checkmate::assert_choice(model, supported_pop_models())
  if (is.null(x)) x <- list()
  checkmate::assert_list(x, null.ok = FALSE)
  mp <- model_arguments(model)
  mc <- as.character(names(x))
  checkmate::assert_subset(mc, mp)
  is_missing <- !(mp %in% mc)

  # Set default values
  if(any(is_missing)){
    message("Default value(s) set:")
  }
  for(i in seq_along(mp)){
    if(is_missing[i]){
      x[[mp[i]]] <- set_default_model_argument_value(model, mp[i], x, stan_data)
      message(mp[i], " = ", paste(x[[mp[i]]], collapse = ", "))
    }
  }
  # Assert model config
  for(i in seq_along(mp)){
    assert_model_argument_value(model, mp[i], value = x[[mp[i]]], x, stan_data)
  }
  x <- x[mp]
  class(x) <- c("pop_model_config", "list")
  x
}

#' @rdname model_config
#' @export
model_arguments <- function(model){
  checkmate::assert_choice(model, supported_pop_models())
  if(model %in% c("model6b")){
    return(character(0))
  } else if(model %in% c("model8a")) {
    return(c("sigma_kappa_hyper"))
  } else if(model %in% c("model8b")) {
    return(c("beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper"))
  } else if(model %in% c("model8c")) {
    return(c("sigma_beta_sigma_sigma_hyper", "beta_sigma_sigma_hyper"))
  } else if(model %in% c("model8d")) {
    return(c("sigma_kappa_hyper",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "sigma_beta_sigma_sigma_hyper", "beta_sigma_sigma_hyper"))
  } else if(model %in% c("model8d2")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa", "use_random_walk_kappa",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "sigma_beta_sigma_sigma_hyper", "beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper"))
  } else if(model %in% c("model8d3")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "sigma_kappa_gamma_a_hyper", "sigma_kappa_gamma_b_hyper",
             "g_scale",
             "use_sigma_kappa_gamma_prior",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa", "use_random_walk_kappa",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "sigma_beta_sigma_sigma_hyper", "beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper"))
  } else if(model %in% c("model8e", "model11b")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa",
             "use_ar_kappa",
             "use_latent_state_version",
             "estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma",
             "alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "beta_sigma_1_sigma_hyper", "sigma_beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper"))
  } else if(model %in% c("model8e2")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa",
             "use_ar_kappa",
             "use_latent_state_version",
             "estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma",
             "alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "beta_sigma_1_sigma_hyper", "sigma_beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper",
             "estimate_kappa_next"))
  } else if(model %in% c("model8f", "model8f1")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa",
             "use_ar_kappa",
             "use_latent_state_version",
             "estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma",
             "alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "beta_sigma_1_sigma_hyper", "sigma_beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper",
             "estimate_kappa_next"))
  } else if(model %in% c("model8f2")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa",
             "use_ar_kappa",
             "use_latent_state_version",
             "use_t_dist_industry_bias",
             "estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma",
             "alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "beta_sigma_1_sigma_hyper", "sigma_beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper",
             "estimate_kappa_next",
             "nu_kappa_raw_alpha", "nu_kappa_raw_beta"))
  } else if(model %in% c("model8f3")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa",
             "use_ar_kappa",
             "use_latent_state_version",
             "use_t_dist_industry_bias",
             "estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma",
             "alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "beta_sigma_1_sigma_hyper", "sigma_beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper",
             "estimate_kappa_next",
             "nu_kappa_raw_alpha", "nu_kappa_raw_beta",
             "alpha_kappa_mean", "alpha_kappa_sd",
             "alpha_beta_mu_mean", "alpha_beta_mu_sd",
             "alpha_beta_sigma_mean", "alpha_beta_sigma_sd"))
  } else if(model %in% c("model8g")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa",
             "use_ar_kappa",
             "use_latent_state_version",
             "use_t_dist_industry_bias",
             "use_multivariate_version",
             "estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma",
             "alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "beta_sigma_1_sigma_hyper", "sigma_beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper",
             "estimate_kappa_next",
             "nu_kappa_raw_alpha", "nu_kappa_raw_beta",
             "alpha_kappa_mean", "alpha_kappa_sd",
             "alpha_beta_mu_mean", "alpha_beta_mu_sd",
             "alpha_beta_sigma_mean", "alpha_beta_sigma_sd",
             "nu_lkj"))
  } else if(model %in% c("model8g1", "model8g2", "model8g3")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa",
             "use_ar_kappa",
             "use_latent_state_version",
             "use_t_dist_industry_bias",
             "use_multivariate_version",
             "use_softmax",
             "estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma",
             "alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "beta_sigma_1_sigma_hyper", "sigma_beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper",
             "estimate_kappa_next",
             "nu_kappa_raw_alpha", "nu_kappa_raw_beta",
             "alpha_kappa_mean", "alpha_kappa_sd",
             "alpha_beta_mu_mean", "alpha_beta_mu_sd",
             "alpha_beta_sigma_mean", "alpha_beta_sigma_sd",
             "nu_lkj"))
  } else if(model %in% c("model8h2", "model8h3", "model8h4")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa",
             "use_ar_kappa",
             "use_latent_state_version",
             "use_t_dist_industry_bias",
             "use_multivariate_version",
             "use_softmax",
             "estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma",
             "alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "beta_sigma_1_sigma_hyper", "sigma_beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper",
             "estimate_kappa_next",
             "nu_kappa_raw_alpha", "nu_kappa_raw_beta",
             "alpha_kappa_mean", "alpha_kappa_sd",
             "alpha_beta_mu_mean", "alpha_beta_mu_sd",
             "alpha_beta_sigma_mean", "alpha_beta_sigma_sd",
             "nu_lkj",
             "x1_prior_p", "x1_prior_alpha0"))
  } else if(grepl(model, pattern = "^model8i[0-9]+$")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa",
             "use_ar_kappa",
             "use_latent_state_version",
             "use_t_dist_industry_bias",
             "use_multivariate_version",
             "use_softmax",
             "estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma",
             "alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "beta_sigma_1_sigma_hyper", "sigma_beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper",
             "estimate_kappa_next",
             "nu_kappa_raw_alpha", "nu_kappa_raw_beta",
             "alpha_kappa_mean", "alpha_kappa_sd",
             "alpha_beta_mu_mean", "alpha_beta_mu_sd",
             "alpha_beta_sigma_mean", "alpha_beta_sigma_sd",
             "nu_lkj",
             "x1_prior_p", "x1_prior_alpha0",
             "psi_sigma_hyper"))
  } else if(grepl(model, pattern = "^model8j[0-9]+$")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_multiplicative_industry_bias",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa",
             "use_ar_kappa",
             "use_latent_state_version",
             "use_t_dist_industry_bias",
             "use_multivariate_version",
             "use_softmax",
             "estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma",
             "alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "beta_sigma_1_sigma_hyper", "sigma_beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper",
             "estimate_kappa_next",
             "nu_kappa_raw_alpha", "nu_kappa_raw_beta",
             "alpha_kappa_mean", "alpha_kappa_sd",
             "alpha_beta_mu_mean", "alpha_beta_mu_sd",
             "alpha_beta_sigma_mean", "alpha_beta_sigma_sd",
             "nu_lkj",
             "x1_prior_p", "x1_prior_alpha0",
             "psi_sigma_hyper"))
  } else if(grepl(model, pattern = "^model8k[0-9]+$")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_multiplicative_industry_bias",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa",
             "use_ar_kappa",
             "use_latent_state_version",
             "use_t_dist_industry_bias",
             "use_multivariate_version",
             "use_softmax",
             "estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma",
             "alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "beta_sigma_1_sigma_hyper", "sigma_beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper",
             "estimate_kappa_next",
             "nu_kappa_raw_alpha", "nu_kappa_raw_beta",
             "alpha_kappa_mean", "alpha_kappa_sd",
             "alpha_beta_mu_mean", "alpha_beta_mu_sd",
             "alpha_beta_sigma_mean", "alpha_beta_sigma_sd",
             "nu_lkj",
             "x1_prior_p", "x1_prior_alpha0",
             "psi_sigma_hyper",
             "use_constrained_party_kappa_pred",
             "use_obs_of_x", "R", "obs_of_x_t", "obs_of_x_p", "obs_of_x_mu", "obs_of_x_sigma", "obs_of_x_nu",
             "eta_1_mu_hyper", "eta_1_sigma_hyper",
             "use_conditional_model",
             "H_cond",
             "houses_key",
             "kappa_1_mu_hyper", 
             "beta_sigma_1_mu_hyper",
             "beta_mu_1_mu_hyper" 
             ))
  } else if(grepl(model, pattern = "^model8l[0-9]+$")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_multiplicative_industry_bias",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa",
             "use_ar_kappa",
             "use_latent_state_version",
             "use_t_dist_industry_bias",
             "use_multivariate_version",
             "use_softmax",
             "estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma",
             "alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "beta_sigma_1_sigma_hyper", "sigma_beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper",
             "estimate_kappa_next",
             "nu_kappa_raw_alpha", "nu_kappa_raw_beta",
             "alpha_kappa_mean", "alpha_kappa_sd",
             "alpha_beta_mu_mean", "alpha_beta_mu_sd",
             "alpha_beta_sigma_mean", "alpha_beta_sigma_sd",
             "nu_lkj",
             "x1_prior_p", "x1_prior_alpha0",
             "psi_sigma_hyper",
             "use_constrained_party_kappa_pred",
             "use_obs_of_x", "R", "obs_of_x_t", "obs_of_x_p", "obs_of_x_mu", "obs_of_x_sigma", "obs_of_x_nu",
             "use_sigma_ep", "election_period", "sigma_ep_mean", "sigma_ep_sd"
    ))
  } else if(grepl(model, pattern = "^model8m1")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_multiplicative_industry_bias",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa",
             "use_ar_kappa",
             "use_latent_state_version",
             "use_t_dist_industry_bias",
             "use_multivariate_version",
             "use_softmax",
             "estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma",
             "alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "beta_sigma_1_sigma_hyper", "sigma_beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper",
             "estimate_kappa_next",
             "nu_kappa_raw_alpha", "nu_kappa_raw_beta",
             "alpha_kappa_mean", "alpha_kappa_sd",
             "alpha_beta_mu_mean", "alpha_beta_mu_sd",
             "alpha_beta_sigma_mean", "alpha_beta_sigma_sd",
             "nu_lkj",
             "x1_prior_p", "x1_prior_alpha0",
             "psi_sigma_hyper",
             "use_constrained_party_kappa_pred",
             "use_obs_of_x", "R", "obs_of_x_t", "obs_of_x_p", "obs_of_x_mu", "obs_of_x_sigma", "obs_of_x_nu",
             "use_sigma_ep", "election_period", "sigma_ep_mean", "sigma_ep_sd",
             "EP", "ep_inv_x"
    ))
  } else if(grepl(model, pattern = "^model8m2$")) {
    return(c("sigma_kappa_hyper_sd", "sigma_kappa_hyper_mean", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_multiplicative_industry_bias",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa",
             "use_ar_kappa",
             "use_latent_state_version",
             "use_t_dist_industry_bias",
             "use_multivariate_version",
             "use_softmax",
             "estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma",
             "alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "beta_sigma_1_sigma_hyper", "sigma_beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper",
             "estimate_kappa_next",
             "nu_kappa_raw_alpha", "nu_kappa_raw_beta",
             "alpha_kappa_mean", "alpha_kappa_sd",
             "alpha_beta_mu_mean", "alpha_beta_mu_sd",
             "alpha_beta_sigma_mean", "alpha_beta_sigma_sd",
             "nu_lkj",
             "x1_prior_p", "x1_prior_alpha0",
             "psi_sigma_hyper",
             "use_constrained_party_kappa_pred",
             "use_obs_of_x", "R", "obs_of_x_t", "obs_of_x_p", "obs_of_x_mu", "obs_of_x_sigma", "obs_of_x_nu",
             "use_sigma_ep", "election_period", "sigma_ep_mean", "sigma_ep_sd",
             "EP", "ep_inv_x"
    ))
  } else if(grepl(model, pattern = "^model8m[3-9]$")) {
    return(c("sigma_kappa_hyper_sd", "sigma_kappa_hyper_mean", "kappa_1_sigma_hyper",
             "g_scale",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_multiplicative_industry_bias",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa",
             "use_ar_kappa",
             "use_latent_state_version",
             "use_t_dist_industry_bias",
             "use_multivariate_version",
             "use_softmax",
             "estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma",
             "alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "beta_sigma_1_sigma_hyper", "sigma_beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper",
             "estimate_kappa_next",
             "nu_kappa_raw_alpha", "nu_kappa_raw_beta",
             "alpha_kappa_mean", "alpha_kappa_sd",
             "alpha_beta_mu_mean", "alpha_beta_mu_sd",
             "alpha_beta_sigma_mean", "alpha_beta_sigma_sd",
             "nu_lkj",
             "x1_prior_p", "x1_prior_alpha0",
             "psi_sigma_hyper",
             "use_constrained_party_kappa_pred",
             "use_obs_of_x", "R", "obs_of_x_t", "obs_of_x_p", "obs_of_x_mu", "obs_of_x_sigma", "obs_of_x_nu",
             "use_sigma_ep", "election_period", "sigma_ep_mean", "sigma_ep_sd", "sigma_ep_mean_vector", "sigma_ep_sd_vector",
             "EP", "ep_inv_x"
    ))
  } else if(model %in% c("model10d", "model10e")) {
    return(c("sigma_kappa_hyper", "kappa_1_sigma_hyper",
             "sigma_kappa_gamma_a_hyper", "sigma_kappa_gamma_b_hyper",
             "g_scale",
             "use_sigma_kappa_gamma_prior",
             "use_industry_bias", "use_house_bias", "use_design_effects",
             "use_constrained_party_house_bias", "use_constrained_house_house_bias", "use_constrained_party_kappa", "use_random_walk_kappa",
             "beta_mu_1_sigma_hyper", "sigma_beta_mu_sigma_hyper",
             "sigma_beta_sigma_sigma_hyper", "beta_sigma_sigma_hyper",
             "kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper"))
  } else {
    stop("Model arguments not implemented for ", model, call. = FALSE)
  }

}


supported_model_arguments <- function(){
  spm <- supported_pop_models()
  mps <- list()
  for(i in seq_along(spm)){
    res <- try(model_arguments(spm[i]), silent = TRUE)
    if(!inherits(res, "try-error")){
      mps[[spm[i]]] <- res
    }
  }
  unname(unique(unlist(mps)))
}


set_default_model_argument_value <- function(model, arg, x = NULL, stan_data = NULL){
  checkmate::assert_choice(arg, supported_model_arguments())
  checkmate::assert_list(x)
  if(length(x) > 0){
    checkmate::assert_names(names(x), subset.of = supported_model_arguments())
  }

  if(arg %in% c("sigma_kappa_hyper", "sigma_kappa_hyper_sd")){
    return(0.005)
  } else if (arg %in% c("sigma_kappa_hyper_mean")) {
    return(0.0)
  } else if (arg %in% c("use_industry_bias",
                        "use_house_bias",
                        "use_design_effects")) {
    return(0L)
  } else if (arg %in% c("use_multiplicative_industry_bias")) {
    return(0L)
  } else if (arg %in% c("use_softmax")) {
    return(1L)
  } else if (arg %in% c("use_softmax")) {
    return(1L)
  } else if (arg %in% c("use_constrained_party_house_bias",
                        "use_constrained_house_house_bias",
                        "use_constrained_party_kappa",
                        "use_random_walk_kappa",
                        "use_sigma_kappa_gamma_prior")) {
    return(0L)
  } else if (arg %in% c("beta_mu_1_sigma_hyper")) {
    if (model %in% c("model8k6")){
      return(matrix(0.02, nrow = 1, ncol = 1))
    } else {
    return(0.02)
    } 
  } else if (arg %in% c("kappa_1_sigma_hyper")) {
     if (model %in% c("model8k6")){
      return(c(0.02))
     } else {
        return(0.02)
     }  
  } 
    else if (arg %in% c("nu_lkj")) {
    return(1.0)
  } else if (arg %in% c("sigma_beta_mu_sigma_hyper")) {
    return(0.01)
  } else if (arg %in% c("kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper")) {
    return(0.01)
  } else if (arg %in% c("sigma_beta_sigma_sigma_hyper",
                        "beta_sigma_sigma_hyper")) {
    return(1.0)
  } else if (arg %in% c("sigma_kappa_gamma_a_hyper")) {
    res <- compute_gamma_parameters(x$sigma_kappa_hyper)
    return(round(res$a, 3))
  } else if (arg %in% c("sigma_kappa_gamma_b_hyper")) {
    res <- compute_gamma_parameters(x$sigma_kappa_hyper)
    return(round(res$b, 1))
  } else if (arg %in% c("g_scale")) {
    if(is.null(stan_data)){
      message("Using default value for g_scale = 1, as stan_data is not provided.")
      res <- 1.0
    } else {
      res <- compute_g_scale_default(stan_data)
    }
    return(res)
  } else if (arg %in% c("use_ar_kappa")) {
    return(0L)
  } else if (arg %in% c("use_t_dist_industry_bias")) {
    return(0L)
  } else if (arg %in% c("use_latent_state_version")) {
    return(0L)
  } else if (arg %in% c("use_multivariate_version")) {
    return(0L)
  } else if (arg %in% c("estimate_alpha_kappa", "estimate_alpha_beta_mu", "estimate_alpha_beta_sigma")) {
    return(0L)
  } else if (arg %in% c("alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known")) {
    return(1.0)
  } else if (arg %in% c("nu_kappa_raw_alpha")) {
    return(6.5)
  } else if (arg %in% c("nu_kappa_raw_beta")) {
    return(1.0)
  } else if (arg %in% c("estimate_kappa_next")) {
    return(1L)
  } else if (arg %in% c("alpha_kappa_mean", "alpha_beta_mu_mean", "alpha_beta_sigma_mean")) {
    return(0.0)
  } else if (arg %in% c("alpha_kappa_sd", "alpha_beta_mu_sd", "alpha_beta_sigma_sd")) {
    return(1.0)
  } else if (arg %in% c("x1_prior_alpha0")) {
    return(100.0)
  } else if (arg %in% c("x1_prior_p")) {
    if(is.null(stan_data$tw_i)){
      message("Using default value for prior_p = 1, as stan_data is not provided.")
      prior_p <- 1.0
    } else {
      prior_p <- get_first_poll_as_simplex(stan_data)
    }
    return(prior_p)
  } else if (arg %in% c("psi_sigma_hyper")) {
    return(1.0)
  } else if (arg %in% c("use_constrained_party_kappa_pred")) {
    return(0L)
  } else if (arg %in% c("use_obs_of_x")) {
    return(0L)
  } else if (arg %in% c("R")) {
    return(0L)
  } else if (arg %in% c("obs_of_x_t", "obs_of_x_p")) {
    return(as.array(integer(0L)))
  } else if (arg %in% c("obs_of_x_mu", "obs_of_x_sigma", "obs_of_x_nu")) {
    return(as.array(numeric(0L)))
  } else if (arg %in% c("use_sigma_ep")) {
    return(0L)
  } else if (arg %in% c("election_period")) {
    return(0L)
  } else if (arg %in% c("sigma_ep_mean")) {
    return(1.0)
  } else if (arg %in% c("sigma_ep_sd")) {
    return(1.0)
  } else if (arg %in% c("sigma_ep_mean_vector")) {
    if(is.null(stan_data)){
      message("Using default value for P = 1 (in sigma_ep_mean_vector), as stan_data is not provided.")
      return(rep(1.0, 1L))
    } else {
      return(rep(1.0, stan_data$P))
    }
  } else if (arg %in% c("sigma_ep_sd_vector")) {
    if(is.null(stan_data)){
      message("Using default value for P = 1 (in sigma_ep_sd_vector), as stan_data is not provided.")
      return(rep(1.0, 1L))
    } else {
      return(rep(1.0, stan_data$P))
    }
  } else if (arg %in% c("EP")) {
    return(0L)
  } else if (arg %in% c("ep_inv_x")) {
    return(list())
  }
    else if (arg %in% c("use_conditional_model")) {
    return(0L)
  }
    else if (arg %in% c("eta_1_mu_hyper", "kappa_1_mu_hyper", "beta_sigma_1_mu_hyper" )){
      return(c(0.0))
  } else if (arg %in% c("beta_mu_1_mu_hyper")){
       if (model %in% c("model8k6")){
        return(matrix(0.0, nrow = 1, ncol = 1))
      } else {
        return(0.0)
      }
  } else if (arg %in% c("beta_sigma_1_sigma_hyper")) {
     if (model %in% c("model8k6")){
        return(c(1.0))
      } else {
        return(1.0)
      }
  }
    else if (arg %in% c("eta_1_sigma_hyper")){
      return(c(0.01))
  }
    else if (arg %in% c("H_cond")){
       return(1L)
  } else if (arg %in% c("houses_key")){
       return(integer(0))
  }
   else
  stop(arg, " is not implemented.")
}

assert_model_argument_value <- function(model, arg, value, x, stan_data){
  checkmate::assert_choice(arg, supported_model_arguments())
  if(arg %in% c("sigma_kappa_hyper", "sigma_kappa_hyper_sd", "sigma_kappa_hyper_mean",
                "sigma_beta_mu_sigma_hyper",
                "sigma_beta_sigma_sigma_hyper",
                "beta_sigma_sigma_hyper",
                "g_scale")){
    checkmate::assert_number(value, lower = 0, .var.name = arg)
  } else if (arg %in% c("kappa_sum_sigma_hyper", "beta_mu_sum_party_sigma_hyper", "beta_mu_sum_house_sigma_hyper")) {
    checkmate::assert_number(value, lower = 0, .var.name = arg)
  } else if (arg %in% c("sigma_kappa_gamma_a_hyper", "sigma_kappa_gamma_b_hyper")) {
    checkmate::assert_number(value, lower = 0, .var.name = arg)
  } else if (arg %in% c("use_industry_bias",
                        "use_house_bias",
                        "use_design_effects")) {
    checkmate::assert_integerish(value, lower = 0, upper = 1, .var.name = arg)
  } else if (arg %in% c("use_multiplicative_industry_bias")) {
    checkmate::assert_integerish(value, lower = 0, upper = 1, .var.name = arg)
  } else if (arg %in% c("use_constrained_party_house_bias",
                        "use_constrained_house_house_bias")) {
    checkmate::assert_integerish(value, lower = 0, upper = 1, .var.name = arg)
    if(value == 1){
      checkmate::assert_true(x$use_house_bias == 1, .var.name = paste0("use_house_bias == 1 : ", arg))
    }
  } else if (arg %in% c("use_softmax")) {
    checkmate::assert_integerish(value, lower = 0, upper = 1, .var.name = arg)
  } else if (arg %in% c("use_constrained_party_kappa",
                        "use_random_walk_kappa")) {
    checkmate::assert_integerish(value, lower = 0, upper = 1, .var.name = arg)
    if(value == 1){
      checkmate::assert_true(x$use_industry_bias == 1, .var.name = paste0("use_industry_bias == 1 : ", arg))
    }
  } else if (arg %in% c("use_sigma_kappa_gamma_prior")) {
    checkmate::assert_integerish(value, lower = 0, upper = 1, .var.name = arg)
  } else if (arg %in% c("estimate_kappa_next")) {
    if(value == 0){
      checkmate::assert_true(x$use_industry_bias == 1, .var.name = paste0("use_industry_bias == 1 : ", arg))
    }
    checkmate::assert_integerish(value, lower = 0, upper = 1, .var.name = arg)
  } else if (arg %in% c("use_ar_kappa")) {
    checkmate::assert_integerish(value, lower = 0, upper = 1, .var.name = arg)
    if(value == 1){
      checkmate::assert_true(x$use_industry_bias == 1, .var.name = paste0("use_industry_bias == 1 : ", arg))
    }
  } else if (arg %in% c("use_latent_state_version")) {
    checkmate::assert_integerish(value, lower = 0, .var.name = arg)
  } else if (arg %in% c("use_multivariate_version")) {
    checkmate::assert_integerish(value, lower = 0, .var.name = arg)
  } else if (arg %in% c("estimate_alpha_kappa")) {
    checkmate::assert_integerish(value, lower = 0, upper = 1, .var.name = arg)
    if(value == 1){
      checkmate::assert_true(x$use_industry_bias == 1, .var.name = paste0("use_industry_bias == 1 : ", arg))
      checkmate::assert_true(x$use_ar_kappa == 1, .var.name = paste0("use_ar_kappa == 1 : ", arg))
    }
  } else if (arg %in% c("estimate_alpha_beta_mu")) {
    checkmate::assert_integerish(value, lower = 0, upper = 1, .var.name = arg)
    if(value == 1){
      checkmate::assert_true(x$use_house_bias == 1, .var.name = paste0("use_house_bias == 1 : ", arg))
    }
  } else if (arg %in% c("estimate_alpha_beta_sigma")) {
    checkmate::assert_integerish(value, lower = 0, upper = 1, .var.name = arg)
    if(value == 1){
      checkmate::assert_true(x$use_design_effects == 1, .var.name = paste0("use_design_effects == 1 : ", arg))
    }
  } else if (arg %in% c("alpha_kappa_known", "alpha_beta_mu_known", "alpha_beta_sigma_known")) {
    checkmate::assert_number(value, lower = -1, upper = 1, .var.name = arg)
  } else if (arg %in% c("nu_lkj")) {
    checkmate::assert_number(value, lower = 0, .var.name = arg)
  } else if (arg %in% c("nu_kappa_raw_alpha",
                        "nu_kappa_raw_beta")) {
    checkmate::assert_number(value, lower = 0, .var.name = arg)
  } else if (arg %in% c("use_t_dist_industry_bias")) {
    checkmate::assert_integerish(value, lower = 0, upper = 1, .var.name = arg)
    if(value == 1){
      checkmate::assert_true(x$use_industry_bias == 1, .var.name = paste0("use_industry_bias == 1 : ", arg))
    }
  } else if (arg %in% c("alpha_kappa_mean", "alpha_beta_mu_mean", "alpha_beta_sigma_mean")) {
    checkmate::assert_number(value, .var.name = arg)
  } else if (arg %in% c("alpha_kappa_sd", "alpha_beta_mu_sd", "alpha_beta_sigma_sd")) {
    checkmate::assert_number(value, lower = 0, .var.name = arg)
  } else if (arg %in% c("x1_prior_alpha0")) {
    checkmate::assert_number(value, lower = 0, .var.name = arg)
  } else if (arg %in% c("x1_prior_p")) {
    assert_simplex(value)
  } else if (arg %in% c("psi_sigma_hyper")) {
    checkmate::assert_number(value, lower = 0, .var.name = arg)
  } else if (arg %in% c("use_constrained_party_kappa_pred")) {
    checkmate::assert_integerish(value, lower = 0, upper = 1, .var.name = arg)
  } else if (arg %in% c("use_obs_of_x")) {
    checkmate::assert_integerish(value, lower = 0, upper = 1, .var.name = arg)
  } else if (arg %in% c("R")) {
    checkmate::assert_integerish(value, lower = 0, .var.name = arg)
    if(value > 0){
      checkmate::assert_true(x$use_obs_of_x == 1, .var.name = paste0("use_obs_of_x == 1 : ", arg))
    }
  } else if (arg %in% c("obs_of_x_t")) {
    checkmate::assert_integerish(value, lower = 1, len = x$R, .var.name = arg)
  } else if (arg %in% c("obs_of_x_p")) {
    checkmate::assert_integerish(value, lower = 1, len = x$R, .var.name = arg)
  } else if (arg %in% c("obs_of_x_mu")) {
    checkmate::assert_numeric(value, len = x$R, .var.name = arg)
  } else if (arg %in% c("obs_of_x_sigma")) {
    checkmate::assert_numeric(value, lower = 0, len = x$R, .var.name = arg)
  } else if (arg %in% c("obs_of_x_nu")) {
    checkmate::assert_numeric(value, lower = 2, len = x$R, .var.name = arg)
  } else if (arg %in% c("use_sigma_ep")) {
    checkmate::assert_int(value, lower = 0, .var.name = arg)
  } else if (arg %in% c("election_period")) {
    checkmate::assert_integer(value, lower = 0L, len = x$T, .var.name = arg)
  } else if (arg %in% c("sigma_ep_mean")) {
    checkmate::assert_number(value, .var.name = arg)
  } else if (arg %in% c("sigma_ep_sd")) {
    checkmate::assert_number(value, lower = 0, .var.name = arg)
  } else if (arg %in% c("sigma_ep_mean_vector")) {
    checkmate::assert_numeric(value, len = x$P, .var.name = arg)
  } else if (arg %in% c("sigma_ep_sd_vector")) {
    checkmate::assert_numeric(value, lower = 0, len = x$P, .var.name = arg)
  } else if (arg %in% c("EP")) {
    checkmate::assert_int(value, lower = 0, .var.name = arg)
  } else if (arg %in% c("ep_inv_x")) {
    checkmate::assert_list(value, .var.name = arg, len = x$EP)
    for(i in seq_along(value)){
      checkmate::assert_numeric(value[[i]], lower = 0, .var.name = arg, len = x$P)
    }
  } else if (arg %in% c("use_conditional_model")) {
    checkmate::assert_integerish(value, lower = 0, upper = 1, .var.name = arg)
  }
  else if (arg %in% c("H_cond")) {
    checkmate::assert_int(value, lower = 0, .var.name = arg)
  }
    else if (arg %in% c("eta_1_mu_hyper", "kappa_1_mu_hyper")){
      if (isTRUE(x$use_conditional_model == 1)){
      checkmate::assert_numeric(value, len = stan_data$P, .var.name = arg)
      } else {
        checkmate::assert_numeric(value, len = 1, .var.name = arg)
      }
  } 
    else if (arg %in% c("eta_1_sigma_hyper", "kappa_1_sigma_hyper")){
      if (isTRUE(x$use_conditional_model == 1)){
        checkmate::assert_numeric(value, lower = 0, len = stan_data$P, .var.name = arg)
      } else {
         checkmate::assert_numeric(value, lower = 0, len = 1, .var.name = arg)
      }
  }
    else if (arg %in% c("beta_sigma_1_mu_hyper")){
      if (isTRUE(x$use_conditional_model == 1)){
        checkmate::assert_numeric(value, len = stan_data$H, .var.name = arg)
      } else {
        checkmate::assert_numeric(value, len = 1, .var.name = arg)
      }
  } else if (arg %in% c("beta_sigma_1_sigma_hyper")){
    if (isTRUE(x$use_conditional_model == 1)){
        checkmate::assert_numeric(value, lower = 0, len = stan_data$H, .var.name = arg)
      } else {
        checkmate::assert_numeric(value, lower = 0, len = 1, .var.name = arg)
      }
     
  } else if (arg %in% c("beta_mu_1_mu_hyper")) {
      if (model %in% c("model8k6")){
        if (isTRUE(x$use_conditional_model == 1)){
          checkmate::assertMatrix(value, nrows = stan_data$H_cond, ncols = stan_data$P, mode = "numeric") 
        } else {
       checkmate::assertMatrix(value, nrows = 1, ncols = 1, mode = "numeric") 
        }
      }
  } else if (arg %in% c("beta_mu_1_sigma_hyper")) {
      if (model %in% c("model8k6")){
        if (isTRUE(x$use_conditional_model == 1)){
       checkmate::assertMatrix(value, nrows = stan_data$H_cond, ncols = stan_data$P, mode = "numeric") }
       else{
          checkmate::assertMatrix(value, nrows = 1, ncols = 1, mode = "numeric")
       }
      } else {
      checkmate::assert_number(value, lower = 0, .var.name = arg)
      }
  } else if (arg %in% c("houses_key")) {
    if (isTRUE(x$use_conditional_model == 1)){
      checkmate::assert_integerish(value, .var.name = arg)
    } else {
       checkmate::assert_integerish(value,  .var.name = arg, len = 0)
    }
    
  }
    else {
    stop(arg, " is not implemented.")
  }
}

assert_model_arguments <- function(x){
  checkmate::assert_list(x)
  spm <- supported_model_arguments()
  nms <- names(x)
  to_check <- nms %in% spm

  for(i in which(to_check)){
    assert_model_argument_value(nms[i], value = x[[nms[i]]], x = x, stan_data = x)
  }
}

#' Compute the default value of g_scale
#'
#' @param x a list with g and next_known_state_index
#'
compute_g_scale_default <-function(x){
  checkmate::assert_class(x, "list")
  if(is.null(x$g)){
    g <- x$g_i
    f <- x$next_known_state_poll_index
  } else {
    g <- x$g
    f <- x$next_known_state_index
  }
  mean(unlist(lapply(split(g, f = f), max)))
}


#' Compute gamma parameters with same expectation and variances
#' as a half-normal with parameter sigma
#'
#' @param sigma the sigma parameter in a half-formal
#' @param n how many samples to approximate the half-normal
#'
#' @return a list with parameters a and b
#'
compute_gamma_parameters <- function(sigma, n = 1000000){
  x <- rnorm(n, 0, sigma)
  x <- x[x>0]
  mx <- mean(x)
  vx <- stats::var(x)
  b = mx/vx
  a = b * mx
  list(a=a, b=b)
}


#' The first poll is the poll with the earliest starting date
#' @param x a stan_data object
get_first_poll_as_simplex <- function(x){
  checkmate::assert_integerish(x$tw_i, lower = 1)
  checkmate::assert_integerish(x$tw_t, lower = 1)
  checkmate::assert_matrix(x$y)
  first_poll_id <- x$tw_i[which.min(x$tw_t)]
  smplx <- unname(c(x$y[first_poll_id,]))
  smplx[!(smplx > 0)] <- NA # NA values are set to 0
  smplx <- c(smplx, 1 - sum(smplx))

  # Handle NAs
  is_na_values <- is.na(smplx)
  if(any(is_na_values)){
    na_total_prop <- 1 - sum(smplx, na.rm = TRUE)
    smplx[is_na_values] <- na_total_prop/sum(is_na_values)
  }
  assert_simplex(smplx)
  smplx
}

assert_simplex <- function(x, tol = 6){
  checkmate::assert_numeric(x, lower = 0, upper = 1, any.missing = FALSE)
  checkmate::assert_true(min(x) > 0)
  checkmate::assert_number(round(sum(x), digits = tol), lower = 1, upper = 1)
}
