#' Supported Stan backends
#'
#' @keywords internal
supported_pop_backends <- function() {
  c("rstan", "cmdstanr")
}

#' Assert supported backend value
#'
#' @keywords internal
assert_pop_backend <- function(backend){
  checkmate::assert_choice(backend, choices = supported_pop_backends())
}

backend_default <- function(x, default) {
  if(is.null(x)) default else x
}

assert_cmdstanr_available <- function() {
  if(!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop(
      "Package 'cmdstanr' must be installed to use backend = 'cmdstanr'.",
      call. = FALSE
    )
  }
}

#' Run a Stan fit using the selected backend
#'
#' @keywords internal
backend_sample <- function(backend,
                           sample_arguments,
                           stan_file = NULL,
                           model_name = NULL,
                           compile_arguments = NULL){
  checkmate::assert_list(sample_arguments)
  checkmate::assert_list(compile_arguments, null.ok = TRUE)
  assert_pop_backend(backend)
  if(backend == "rstan"){
    return(backend_sample_rstan(
      sample_arguments = sample_arguments,
      stan_file = stan_file,
      model_name = model_name
    ))
  }
  if(backend == "cmdstanr"){
    return(backend_sample_cmdstanr(
      sample_arguments = sample_arguments,
      stan_file = stan_file,
      compile_arguments = compile_arguments
    ))
  }
  stop("Unknown backend '", backend, "'.", call. = FALSE)
}

#' @keywords internal
backend_sample_rstan <- function(sample_arguments,
                                 stan_file = NULL,
                                 model_name = NULL){
  rstan_arguments <- sample_arguments
  if(is.null(rstan_arguments$file)) rstan_arguments$file <- stan_file
  if(is.null(rstan_arguments$model_name)) rstan_arguments$model_name <- model_name
  do.call(rstan::stan, rstan_arguments)
}

#' @keywords internal
backend_sample_cmdstanr <- function(sample_arguments,
                                    stan_file,
                                    compile_arguments = NULL){
  assert_cmdstanr_available()
  checkmate::assert_file_exists(stan_file)
  compile_arguments <- backend_default(compile_arguments, list())
  checkmate::assert_list(compile_arguments)
  checkmate::assert_list(sample_arguments)

  reserved_compile_args <- c("stan_file", "exe_file", "compile")
  if(any(names(compile_arguments) %in% reserved_compile_args)){
    warning(
      "Ignoring reserved cmdstanr compile_args: ",
      paste0(intersect(names(compile_arguments), reserved_compile_args), collapse = ", "),
      call. = FALSE
    )
    compile_arguments[intersect(names(compile_arguments), reserved_compile_args)] <- NULL
  }

  if(is.null(sample_arguments$adapt_engaged) &&
     identical(sample_arguments$iter_warmup, 0L) &&
     !isTRUE(sample_arguments$fixed_param)) {
    sample_arguments$adapt_engaged <- FALSE
  }

  model <- do.call(
    cmdstanr::cmdstan_model,
    c(
      list(
        stan_file = stan_file,
        compile = TRUE
      ),
      compile_arguments
    )
  )
  do.call(model$sample, sample_arguments)
}

#' Compute diagnostics for a backend fit object
#'
#' @keywords internal
backend_compute_diagnostics <- function(backend, fit) {
  assert_pop_backend(backend)
  if(backend == "rstan"){
    fit_summary <- rstan::summary(fit)
    return(list(
      n_eff = fit_summary$summary[, "n_eff"],
      Rhat = fit_summary$summary[, "Rhat"]
    ))
  }
  if(backend == "cmdstanr"){
    fit_summary <- fit$summary()
    n_eff <- fit_summary$ess_bulk
    names(n_eff) <- fit_summary$variable
    rhat <- fit_summary$rhat
    names(rhat) <- fit_summary$variable
    return(list(n_eff = n_eff, Rhat = rhat))
  }
  stop("Unknown backend '", backend, "'.", call. = FALSE)
}

#' Backend sampler state for warm starts
#'
#' @description
#' Extract backend-specific sampler state that Stan uses on the unconstrained
#' parameter space, notably the per-chain step size and inverse metric. This is
#' complementary to [backend_get_last_draws_for_init()], which returns
#' constrained parameter values for Stan's `init` argument. Warm-started refits
#' therefore combine constrained draws for initialization with unconstrained
#' sampler state for HMC tuning.
#'
#' @keywords internal
backend_get_sampler_state <- function(backend, fit, ...) {
  assert_pop_backend(backend)
  if(backend == "rstan"){
    return(backend_get_sampler_state_rstan(fit, ...))
  }
  if(backend == "cmdstanr"){
    return(backend_get_sampler_state_cmdstanr(fit, ...))
  }
  stop("Unknown backend '", backend, "'.", call. = FALSE)
}

#' @keywords internal
backend_get_sampler_state_rstan <- function(fit, ...) {
  ai <- rstan::get_adaptation_info(fit, ...)
  lapply(ai, function(chain_info){
    parsed <- parse_adaption_information(chain_info)
      list(
        adaption_terminated = parsed$adaption_terminated,
        step_size = parsed$step_size,
        inv_metric = parsed$inv_metric,
        metric_type = parsed$metric_type
      )
    })
}

#' @keywords internal
backend_get_sampler_state_cmdstanr <- function(fit, ...) {
  inv_metric <- fit$inv_metric(matrix = FALSE)
  step_size <- backend_get_cmdstanr_step_size(fit)
  if(length(step_size) != length(inv_metric)){
    step_size <- rep(NA_real_, length(inv_metric))
  }

  lapply(seq_along(inv_metric), function(i){
      list(
        adaption_terminated = NA,
        step_size = step_size[i],
        inv_metric = inv_metric[[i]],
        metric_type = backend_metric_type_from_inv_metric(inv_metric[[i]])
      )
    })
}

#' @keywords internal
backend_get_cmdstanr_step_size <- function(fit) {
  sampler_diagnostics <- as.array(
    fit$sampler_diagnostics(inc_warmup = FALSE, format = "draws_array")
  )
  variable_names <- dimnames(sampler_diagnostics)[[3]]
  step_idx <- which(variable_names == "stepsize__")
  if(length(step_idx) != 1L){
    return(rep(NA_real_, dim(sampler_diagnostics)[2]))
  }
  apply(
    sampler_diagnostics[, , step_idx, drop = FALSE],
    2,
    function(x) as.numeric(x[1])
  ) |> unname()
}

#' @keywords internal
backend_metric_type_from_inv_metric <- function(inv_metric) {
  if(is.matrix(inv_metric)) {
    return("dense_e")
  }
  "diag_e"
}

#' Backend last draws formatted for init
#'
#' @description
#' Extract the final post-warmup draw from each chain and return it in the
#' shape expected by Stan's `init` argument. The intention is to support
#' warm-started refits: after changing the data or sampler settings, we can
#' reuse the last constrained-parameter draw from an existing fit as the
#' starting point for a new fit. This helper intentionally works on the
#' constrained parameter space because Stan's `init` interface expects
#' constrained values. In contrast, inverse metrics and step sizes are supplied
#' on the unconstrained space via [backend_get_sampler_state()]. The
#' backend-specific helpers hide the differences between RStan and CmdStanR
#' while returning a common per-chain init representation.
#'
#' @keywords internal
backend_get_last_draws_for_init <- function(backend, fit, ...) {
  assert_pop_backend(backend)
  if(backend == "rstan"){
    return(backend_get_last_draws_for_init_rstan(fit, ...))
  }
  if(backend == "cmdstanr"){
    return(backend_get_last_draws_for_init_cmdstanr(fit, ...))
  }
  stop("Unknown backend '", backend, "'.", call. = FALSE)
}

#' @keywords internal
backend_get_last_draws_for_init_rstan <- function(fit, ...) {
  skeleton <- backend_get_rstan_init_skeleton(fit)
  draws <- as.array(fit)
  if(dim(draws)[1] < 1L){
    stop("RStan fit does not contain post-warmup draws.", call. = FALSE)
  }

  last_iter <- dim(draws)[1]
  variable_names <- dimnames(draws)[[3]]
  lapply(seq_len(dim(draws)[2]), function(chain_id){
    chain_draw <- as.numeric(draws[last_iter, chain_id, ])
    names(chain_draw) <- variable_names
    backend_relist_flat_draw_to_init(chain_draw, skeleton[[chain_id]])
  })
}

#' Build a backend-neutral init skeleton
#'
#' @description
#' Return the parameter-shaped skeleton needed to validate or relist Stan init
#' values across backends. The result is a list with one skeleton per chain,
#' where each skeleton is a named list of parameters in the structure expected
#' by Stan's `init` argument.
#'
#' For RStan, the skeleton comes from the stored init objects or from
#' parameter-dimension metadata. For CmdStanR, the skeleton is reconstructed
#' from the saved draw variable names and repeated once per chain.
#'
#' @param backend Stan backend used by `fit`.
#' @param fit A fitted backend object, either an `rstan::stanfit` or a
#'   `cmdstanr` fit object.
#' @param ... Reserved for backend-specific extensions.
#'
#' @return A list with one parameter-shaped init skeleton per chain.
#'
#' @keywords internal
backend_get_init_skeleton <- function(backend, fit, ...) {
  assert_pop_backend(backend)
  if(backend == "rstan") {
    return(backend_get_rstan_init_skeleton(fit, ...))
  }
  if(backend == "cmdstanr") {
    draws <- as.array(fit$draws(inc_warmup = FALSE, format = "draws_array"))
    variable_names <- dimnames(draws)[[3]]
    skeleton <- backend_build_init_skeleton_from_variable_names(
      variable_names,
      parameter_roots = backend_get_cmdstanr_parameter_roots(fit, variable_names = variable_names)
    )
    return(rep(list(skeleton), dim(draws)[2]))
  }
  stop("Unknown backend '", backend, "'.", call. = FALSE)
}

#' @keywords internal
backend_get_last_draws_for_init_cmdstanr <- function(fit, ...) {
  draws <- as.array(fit$draws(inc_warmup = FALSE, format = "draws_array"))
  if(dim(draws)[1] < 1L){
    stop("CmdStanR fit does not contain post-warmup draws.", call. = FALSE)
  }

  last_iter <- dim(draws)[1]
  variable_names <- dimnames(draws)[[3]]
  skeleton <- backend_build_init_skeleton_from_variable_names(
    variable_names,
    parameter_roots = backend_get_cmdstanr_parameter_roots(fit, variable_names = variable_names)
  )
  lapply(seq_len(dim(draws)[2]), function(chain_id){
    chain_draw <- as.numeric(draws[last_iter, chain_id, ])
    names(chain_draw) <- variable_names
    backend_relist_flat_draw_to_init(chain_draw, skeleton)
  })
}

#' Build a CmdStanR init skeleton from draw names
#'
#' @description
#' Reconstruct a parameter-shaped init skeleton from the flat variable names in
#' a CmdStanR draws array. CmdStanR does not always retain user init files, so
#' this helper infers the parameter roots and array dimensions directly from
#' names like `alpha`, `beta[1]`, or `gamma[2,1]`. Sampler method variables
#' such as `lp__` and `stepsize__` are removed before building the skeleton.
#'
#' @param variable_names A character vector of variable names from a CmdStanR
#'   draws object. This may include Stan sampler method variables, which are
#'   ignored when constructing the parameter skeleton.
#' @param parameter_roots Optional character vector restricting the skeleton to
#'   specific parameter roots, for example the names declared in the Stan
#'   `parameters` block.
#'
#' @keywords internal
backend_build_init_skeleton_from_variable_names <- function(variable_names,
                                                            parameter_roots = NULL) {
  checkmate::assert_character(variable_names, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_character(parameter_roots, any.missing = FALSE, unique = TRUE, null.ok = TRUE)

  cmdstan_method_variables <- c(
    "lp__", "accept_stat__", "stepsize__", "treedepth__",
    "n_leapfrog__", "divergent__", "energy__"
  )
  variable_names <- variable_names[!variable_names %in% cmdstan_method_variables]
  draw_roots <- unique(sub("\\[.*$", "", variable_names))
  if(is.null(parameter_roots)) {
    parameter_roots <- draw_roots
  } else {
    parameter_roots <- intersect(parameter_roots, draw_roots)
  }

  out <- lapply(parameter_roots, function(root) {
    indexed_names <- variable_names[grepl(paste0("^", root, "\\["), variable_names)]
    if(length(indexed_names) == 0L) {
      return(NA_real_)
    }

    idx_strings <- sub(paste0("^", root, "\\["), "", indexed_names)
    idx_strings <- sub("\\]$", "", idx_strings)
    idx_list <- strsplit(idx_strings, ",", fixed = TRUE)
    max_dims <- vapply(seq_len(max(lengths(idx_list))), function(i) {
      max(vapply(idx_list, function(idx) as.integer(idx[[i]]), integer(1)))
    }, integer(1))
    array(NA_real_, dim = max_dims)
  })

  names(out) <- parameter_roots
  out
}

#' Extract CmdStanR parameter roots for init reconstruction
#'
#' @description
#' Resolve the parameter roots that should be used when rebuilding CmdStanR
#' init values from saved draws. The helper prefers the Stan source file when
#' available so only parameters declared in the `parameters` block are used,
#' then falls back to CmdStanR metadata, and finally to the draw variable roots.
#'
#' @param fit A `cmdstanr` fit object.
#' @param variable_names Optional draw variable names used as a final fallback.
#'
#' @keywords internal
backend_get_cmdstanr_parameter_roots <- function(fit, variable_names = NULL) {
  checkmate::assert_character(variable_names, any.missing = FALSE, null.ok = TRUE)
  # Prefer cmdstanr metadata when it is available, but keep a fallback list so
  # mock fits and older metadata shapes still drop sampler method variables.
  cmdstan_method_variables <- c(
    "lp__", "accept_stat__", "stepsize__", "treedepth__",
    "n_leapfrog__", "divergent__", "energy__"
  )

  runset <- try(fit$runset, silent = TRUE)
  if(!inherits(runset, "try-error") && !is.null(runset)) {
    stan_code <- try(runset$stan_code(), silent = TRUE)
    if(!inherits(stan_code, "try-error") && length(stan_code) > 0L) {
      parameter_roots <- backend_extract_parameter_roots_from_stan_code(
        paste(stan_code, collapse = "\n")
      )
      if(length(parameter_roots) > 0L) {
        return(parameter_roots)
      }
    }
  }

  metadata <- try(fit$metadata(), silent = TRUE)
  if(!inherits(metadata, "try-error")) {
    if(!is.null(metadata$sampler_diagnostics) && length(metadata$sampler_diagnostics) > 0L) {
      cmdstan_method_variables <- unique(c("lp__", metadata$sampler_diagnostics))
    }

    if(!is.null(metadata$stan_file) &&
       checkmate::test_file_exists(metadata$stan_file, extension = "stan")) {
      stan_code <- paste(readLines(metadata$stan_file, warn = FALSE), collapse = "\n")
      parameter_roots <- backend_extract_parameter_roots_from_stan_code(stan_code)
      if(length(parameter_roots) > 0L) {
        return(parameter_roots)
      }
    }

    if(!is.null(metadata$model_params) && length(metadata$model_params) > 0L) {
      metadata_roots <- unique(sub("\\[.*$", "", metadata$model_params))
      metadata_roots <- metadata_roots[!metadata_roots %in% cmdstan_method_variables]
      return(metadata_roots)
    }
  }

  if(is.null(variable_names)) {
    return(character())
  }
  variable_roots <- unique(sub("\\[.*$", "", variable_names))
  variable_roots[!variable_roots %in% cmdstan_method_variables]
}

#' Build an RStan init skeleton
#'
#' @description
#' Build a parameter-shaped skeleton for relisting RStan draws back into the
#' constrained structure expected by Stan's `init` argument. When RStan stores
#' original initial values in `fit@inits`, those objects already provide the
#' right structure and can be reused directly. Otherwise we reconstruct the
#' skeleton from `fit@par_dims` so the last post-warmup draw can still be
#' relisted into a per-chain init object.
#'
#' @param fit An `rstan::stanfit` object whose parameter dimensions are used to
#'   recover the structure expected by Stan's `init` argument.
#'
#' @keywords internal
backend_get_rstan_init_skeleton <- function(fit) {
  parameter_roots <- backend_extract_parameter_roots_from_stan_code(
    fit@stanmodel@model_code
  )
  if(length(parameter_roots) == 0L) {
    parameter_roots <- fit@model_pars
  }
  if(length(fit@inits) > 0L){
    return(lapply(fit@inits, function(chain_init) {
      chain_init[intersect(names(chain_init), parameter_roots)]
    }))
  }

  n_chains <- length(fit@sim$samples)
  par_dims <- fit@par_dims[intersect(names(fit@par_dims), parameter_roots)]
  lapply(seq_len(n_chains), function(chain_id){
    lapply(par_dims, function(parameter_dims){
      if(length(parameter_dims) == 0L){
        return(NA_real_)
      }
      array(NA_real_, dim = parameter_dims)
    })
  })
}

#' @keywords internal
backend_extract_parameter_roots_from_stan_code <- function(stan_code) {
  checkmate::assert_string(stan_code)

  lines <- strsplit(stan_code, "\n", fixed = TRUE)[[1]]
  # Strip line comments and surrounding whitespace before locating the parameters block.
  normalized_lines <- trimws(sub("//.*$", "", lines))

  # Find the opening line of the parameters block.
  start_idx <- which(grepl("^parameters\\s*\\{$", normalized_lines))
  if(length(start_idx) == 0L) {
    return(character())
  }

  normalized_lines <- normalized_lines[(start_idx[[1]] + 1L):length(normalized_lines)]

  # Stop at the first matching block terminator after `parameters {`.
  end_idx <- which(normalized_lines == "}")
  if(length(end_idx) == 0L) {
    return(character())
  }

  lines <- normalized_lines[seq_len(end_idx[[1]] - 1L)]
  lines <- lines[nzchar(lines)]

  declarations <- character()
  current <- character()

  # Accumulate declarations until `;` so multi-line parameter declarations are handled.
  for(line in lines) {
    current <- c(current, line)
    if(grepl(";\\s*$", line)) {
      declarations <- c(declarations, paste(current, collapse = " "))
      current <- character()
    }
  }

  # Extract the final declared identifier from each parameter declaration.
  roots <- vapply(declarations, function(declaration) {
    match <- regmatches(
      declaration,
      regexec("([A-Za-z][A-Za-z0-9_]*)\\s*(?:\\[[^;]*\\])?\\s*;\\s*$", declaration, perl = TRUE)
    )[[1]]
    if(length(match) < 2L) {
      return(NA_character_)
    }
    match[[2]]
  }, character(1))

  unique(roots[!is.na(roots)])
}

#' Relist a flat Stan draw into an init object
#'
#' @description
#' Convert a named flat draw vector, such as the per-chain output returned by
#' Stan with variable names like `alpha`, `beta[1]`, or `gamma[2,1]`, back into
#' the nested parameter structure expected by Stan's `init` argument. The
#' `skeleton` supplies the target parameter names and dimensions, and this
#' helper fills that structure with the corresponding values from `draw` while
#' preserving scalar, vector, and array shapes. It is used by the backend-
#' specific last-draw extractors when turning saved Stan draws into reusable
#' per-chain init objects.
#'
#' @param draw A named numeric vector containing one chain's saved draw on the
#'   constrained parameter scale. Names are expected to follow Stan's usual
#'   parameter naming convention, for example `alpha`, `beta[1]`, or
#'   `gamma[2,1]`.
#' @param skeleton A named list giving the target parameter structure for the
#'   returned init object. Each element provides the parameter name and shape to
#'   fill, typically from a previous init object or from parameter dimensions
#'   reconstructed from the fitted model.
#'
#' @keywords internal
backend_relist_flat_draw_to_init <- function(draw, skeleton) {
  checkmate::assert_numeric(draw, null.ok = FALSE)
  checkmate::assert_list(skeleton, names = "named")

  # Parameter roots are the base Stan names before indexing, e.g. beta for beta[1] and beta[2].
  parameter_roots <- names(skeleton)
  # Ignore saved extras such as lp__ and keep only entries that belong to init parameters.
  relevant_idx <- vapply(
    names(draw),
    function(name) any(vapply(
      parameter_roots,
      function(root) grepl(paste0("^", root, "(\\[|$)"), name),
      logical(1)
    )),
    logical(1)
  )
  draw <- draw[relevant_idx]
  checkmate::assert_numeric(draw, null.ok = FALSE)

  out <- skeleton
  omitted_roots <- character()
  for(i in seq_along(parameter_roots)){
    root <- parameter_roots[[i]]
    template <- skeleton[[root]]
    idx <- grepl(paste0("^", root, "(\\[|$)"), names(draw))
    values <- unname(draw[idx])

    if(length(template) == 0L){
      # Zero-size parameters (for example matrix[0, 0]) cannot be serialized as
      # useful init values for CmdStan, so omit them and let Stan rebuild them
      # from the model dimensions.
      out[[root]] <- NULL
      next
    }
    # Some RStan fits do not retain reusable values for every parameter root,
    # e.g. beta for beta[1] and beta[2]; omit those roots and let Stan fall
    # back to its default init behavior.
    if(length(values) == 0L || all(is.na(values))) {
      out[[root]] <- NULL
      omitted_roots <- c(omitted_roots, root)
      next
    }
    if(anyNA(values)) {
      stop(
        "Cannot reconstruct init for parameter '", root,
        "' because the saved draws contain missing values.",
        call. = FALSE
      )
    }
    if(length(values) != length(template)){
      stop(
        "Cannot reconstruct init for parameter '", root,
        "' from the saved draws. Expected ", length(template),
        " value(s) but found ", length(values), ".",
        call. = FALSE
      )
    }

    if(is.null(dim(template)) && length(template) == 1L){
      out[[root]] <- as.numeric(values[[1]])
    } else {
      out[[root]] <- template
      out[[root]][] <- as.numeric(values)
    }
  }

  if(length(omitted_roots) > 0L) {
    warning(
      "Missing saved draw values were found for parameter root(s): ",
      paste0(omitted_roots, collapse = ", "),
      ". These roots are omitted from the returned init, so Stan will use its default initialization for them.",
      call. = FALSE
    )
  }

  out
}

#' Backend adaptation info
#'
#' @keywords internal
backend_get_adaptation_info <- function(backend, fit, ...) {
  state <- backend_get_sampler_state(backend, fit, ...)
  lapply(state, function(chain_state){
    diag_inv_mass_matrix <- as.numeric(chain_state$inv_metric)
    if(is.matrix(chain_state$inv_metric)){
      diag_inv_mass_matrix <- diag(chain_state$inv_metric)
    }
    list(
      adaption_terminated = chain_state$adaption_terminated,
      step_size = chain_state$step_size,
      diag_inv_mass_matrix = diag_inv_mass_matrix
    )
  })
}

#' Backend number of unconstrained parameters
#'
#' @keywords internal
backend_get_num_upars <- function(backend, fit, ...) {
  assert_pop_backend(backend)
  if(backend == "rstan"){
    num_upars <- try(rstan::get_num_upars(fit, ...), silent = TRUE)
    if(!inherits(num_upars, "try-error")) {
      return(num_upars)
    }

    sampler_state <- try(backend_get_sampler_state(backend, fit), silent = TRUE)
    if(!inherits(sampler_state, "try-error") &&
       length(sampler_state) > 0L &&
       !is.null(sampler_state[[1]]$inv_metric)) {
      if(is.matrix(sampler_state[[1]]$inv_metric)) {
        return(nrow(sampler_state[[1]]$inv_metric))
      }
      return(length(sampler_state[[1]]$inv_metric))
    }

    stop(
      "Unable to recover the number of unconstrained parameters from the stored rstan fit. ",
      "This can happen after deserializing an rstan-backed poll_of_polls object ",
      "whose stanfit model object is no longer valid. If you are refitting with ",
      "refit_poll_of_polls(), disable warm-start reuse that depends on the old fit, ",
      "for example warm_start = list(init = NULL, inv_metric = NULL, step_size = NULL).",
      call. = FALSE
    )
  }
  if(backend == "cmdstanr"){
    sampler_state <- backend_get_sampler_state(backend, fit)
    if(is.matrix(sampler_state[[1]]$inv_metric)){
      return(nrow(sampler_state[[1]]$inv_metric))
    }
    return(length(sampler_state[[1]]$inv_metric))
  }
  stop("Unknown backend '", backend, "'.", call. = FALSE)
}

#' Capture reusable warm-start state from a fitted backend object
#'
#' @description
#' Extract the compact warm-start payload needed for future refits and store it
#' independently of the backend fit object itself. The payload contains the
#' final constrained draw for `init`, the unconstrained sampler state, the
#' expected init skeleton, and the number of unconstrained parameters.
#' If the stored fit does not contain a complete last draw for all init-able
#' parameter roots, automatic `init` reuse is disabled while the remaining
#' sampler state is still cached for future refits.
#'
#' @param backend Stan backend used by `fit`.
#' @param fit A fitted backend object.
#' @param ... Reserved for backend-specific extensions.
#'
#' @return A named list with elements `init`, `init_complete`, `sampler_state`,
#'   `init_skeleton`, and `num_upars`.
#'
#' @keywords internal
backend_capture_warm_start_state <- function(backend, fit, ...) {
  assert_pop_backend(backend)

  sampler_state <- backend_get_sampler_state(backend, fit, ...)
  init_skeleton <- backend_get_init_skeleton(backend, fit, ...)
  expected_init_skeleton <- backend_nonempty_init_skeleton(init_skeleton)
  last_draws <- try(backend_get_last_draws_for_init(backend, fit, ...), silent = TRUE)
  init_complete <- FALSE
  init <- NULL

  if(inherits(last_draws, "try-error")) {
    warning(
      "The stored fit does not contain a reusable complete last draw for automatic init reuse. ",
      "This can happen, for example, when a restrictive 'pars' argument was used. ",
      "Automatic init reuse will be disabled for this object, but sampler_state and num_upars are still cached. ",
      "Original error: ",
      conditionMessage(attr(last_draws, "condition")),
      call. = FALSE
    )
  } else if(backend_init_matches_skeleton(last_draws, expected_init_skeleton)) {
    init_complete <- TRUE
    init <- last_draws
  } else {
    warning(
      "The stored fit does not contain a complete last draw for all init-able parameters. ",
      "This can happen, for example, when a restrictive 'pars' argument was used. ",
      "Automatic init reuse will be disabled for this object, but sampler_state and num_upars are still cached.",
      call. = FALSE
    )
  }

  list(
    init = init,
    init_complete = init_complete,
    sampler_state = sampler_state,
    init_skeleton = init_skeleton,
    num_upars = as.integer(backend_get_num_upars(backend, fit, ...))
  )
}

#' Filter zero-length roots from an init skeleton
#'
#' @description
#' Drop parameter roots with zero length from a per-chain init skeleton before
#' checking whether saved draws are complete enough to be reused as Stan
#' `init` values.
#'
#' @param skeleton Per-chain init skeleton.
#'
#' @return The filtered per-chain init skeleton.
#'
#' @keywords internal
backend_nonempty_init_skeleton <- function(skeleton) {
  checkmate::assert_list(skeleton)
  lapply(skeleton, function(chain_expected) {
    chain_expected[vapply(chain_expected, length, integer(1)) > 0L]
  })
}

#' Test whether init values cover an expected skeleton
#'
#' @description
#' Check whether each chain in a per-chain `init` list contains all parameter
#' roots expected by the corresponding init skeleton.
#'
#' @param init Per-chain init values.
#' @param expected Per-chain init skeleton.
#'
#' @return Logical scalar indicating whether the init values are complete.
#'
#' @keywords internal
backend_init_matches_skeleton <- function(init, expected) {
  checkmate::assert_list(init)
  checkmate::assert_list(expected)

  if(length(init) != length(expected)) {
    return(FALSE)
  }

  all(vapply(seq_along(expected), function(chain_id) {
    chain_init <- init[[chain_id]]
    chain_expected <- expected[[chain_id]]
    is.list(chain_init) &&
      !is.null(names(chain_init)) &&
      all(names(chain_init) != "") &&
      setequal(names(chain_init), names(chain_expected))
  }, logical(1)))
}

#' Backend log probability evaluation
#'
#' @keywords internal
backend_log_prob <- function(backend, fit, ...) {
  assert_pop_backend(backend)
  dots <- list(...)
  if(backend == "rstan"){
    return(do.call(rstan::log_prob, c(list(fit), dots)))
  }
  if(backend == "cmdstanr"){
    fit$init_model_methods()
    if(length(dots) == 0L){
      stop("cmdstanr log_prob requires unconstrained variables.", call. = FALSE)
    }
    if(length(dots) > 0L && is.null(names(dots)[1])){
      dots$unconstrained_variables <- dots[[1]]
      dots[[1]] <- NULL
    }
    return(do.call(fit$log_prob, dots))
  }
  stop("Unknown backend '", backend, "'.", call. = FALSE)
}

#' Backend sampler diagnostics
#'
#' @keywords internal
backend_get_sampler_params <- function(backend, fit, inc_warmup = FALSE, ...) {
  assert_pop_backend(backend)
  if(backend == "rstan"){
    return(rstan::get_sampler_params(fit, inc_warmup = inc_warmup, ...))
  }
  if(backend == "cmdstanr"){
    sd <- as.array(fit$sampler_diagnostics(inc_warmup = inc_warmup, format = "draws_array"))
    variable_names <- dimnames(sd)[[3]]
    return(lapply(seq_len(dim(sd)[2]), function(chain_id){
      mat <- matrix(sd[, chain_id, ], nrow = dim(sd)[1], ncol = dim(sd)[3])
      colnames(mat) <- variable_names
      mat
    }))
  }
  stop("Unknown backend '", backend, "'.", call. = FALSE)
}

#' Backend parameter names
#'
#' @keywords internal
backend_parameter_names <- function(backend, fit) {
  assert_pop_backend(backend)
  if(backend == "rstan"){
    return(names(fit))
  }
  if(backend == "cmdstanr"){
    cmdstan_method_variables <- c(
      "accept_stat__", "stepsize__", "treedepth__",
      "n_leapfrog__", "divergent__", "energy__"
    )
    draws <- try(fit$draws(inc_warmup = FALSE, format = "draws_array"), silent = TRUE)
    if(!inherits(draws, "try-error")) {
      draw_variables <- dimnames(as.array(draws))[[3]]
      if(!is.null(draw_variables) && length(draw_variables) > 0) {
        model_variables <- draw_variables[!draw_variables %in% cmdstan_method_variables]
        if("lp__" %in% model_variables) {
          model_variables <- c(model_variables[model_variables != "lp__"], "lp__")
        }
        return(model_variables)
      }
    }

    metadata <- try(fit$metadata(), silent = TRUE)
    if(!inherits(metadata, "try-error") &&
       !is.null(metadata$model_params) &&
       length(metadata$model_params) > 0) {
      model_variables <- metadata$model_params[!metadata$model_params %in% cmdstan_method_variables]
      if("lp__" %in% model_variables) {
        model_variables <- c(model_variables[model_variables != "lp__"], "lp__")
      }
      return(model_variables)
    }

    variables <- fit$summary()$variable
    model_variables <- variables[!variables %in% cmdstan_method_variables]
    if("lp__" %in% model_variables) {
      model_variables <- c(model_variables[model_variables != "lp__"], "lp__")
    }
    return(model_variables)
  }
  stop("Unknown backend '", backend, "'.", call. = FALSE)
}

#' Backend number of posterior draws
#'
#' @keywords internal
backend_get_ndraws <- function(backend, fit) {
  assert_pop_backend(backend)
  if(backend == "rstan"){
    return(sum(unlist(lapply(fit@stan_args, function(x) {x$iter - x$warmup}))))
  }
  if(backend == "cmdstanr"){
    dr <- as.array(fit$draws(inc_warmup = FALSE, format = "draws_array"))
    return(dim(dr)[1] * dim(dr)[2])
  }
  stop("Unknown backend '", backend, "'.", call. = FALSE)
}

#' Backend draw extraction
#'
#' @keywords internal
backend_extract <- function(backend, fit, pars = NULL, ...) {
  assert_pop_backend(backend)
  if(backend == "rstan"){
    return(rstan::extract(fit, pars = pars, ...))
  }
  if(backend == "cmdstanr"){
    variables <- pars
    if(is.null(variables)){
      variables <- unique(sub("\\[.*$", "", fit$summary()$variable))
    }
    res <- list()
    for(i in seq_along(variables)){
      res[[i]] <- backend_extract_variable_cmdstanr(fit, variables[i])
    }
    names(res) <- variables
    return(res)
  }
  stop("Unknown backend '", backend, "'.", call. = FALSE)
}

#' @keywords internal
backend_extract_variable_cmdstanr <- function(fit, variable) {
  draws <- as.array(fit$draws(variables = variable, inc_warmup = FALSE, format = "draws_array"))
  variable_names <- dimnames(draws)[[3]]
  merged <- matrix(draws, nrow = dim(draws)[1] * dim(draws)[2], ncol = dim(draws)[3])

  if(length(variable_names) == 1L && identical(variable_names, variable)){
    return(merged[, 1])
  }

  idx_strings <- sub(paste0("^", variable, "\\["), "", variable_names)
  idx_strings <- sub("\\]$", "", idx_strings)
  idx_list <- strsplit(idx_strings, ",", fixed = TRUE)
  max_dims <- vapply(seq_len(max(lengths(idx_list))), function(i){
    max(vapply(idx_list, function(idx) as.integer(idx[i]), integer(1)))
  }, integer(1))
  out <- array(NA_real_, dim = c(nrow(merged), max_dims))

  for(col in seq_along(variable_names)){
    idx <- as.integer(idx_list[[col]])
    target <- cbind(seq_len(nrow(merged)), matrix(rep(idx, each = nrow(merged)), ncol = length(idx)))
    out[target] <- merged[, col]
  }

  out
}
