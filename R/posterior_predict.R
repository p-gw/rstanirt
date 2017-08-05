#' @title
#' Predicting observations from posterior predictive distribution
#'
#' @description
#' Given a model, a data set and (optional) additional parameters, 
#' this function calculates observations from the posterior predictive distribution.
#'
#' @param   fit         a stanfit object containing an item response model
#' @param   data        a dataset in stan_list format
#' @param   new_theta   draw new person parameters from the prior (suggested)
#' @param   draws       number of draws from the posterior distribution
#' @param   seed        set a seed for the random number generator
#'
posterior_predict <- function(fit, data, new_theta = NULL, draws = NULL, seed = NULL) {
  if (!is.null(seed)) { set.seed(seed) }
  # extracting posterior distribution if necessary
  if (is(fit, "stanfit")) {
    pars <- c("lp__", "mu_beta", "sigma_beta", if (!is.null(new_theta)) "theta")
    fit <- extract(fit, pars, include = FALSE)
  }

  n_iter <- nrow(fit$beta)
  if (!is.null(draws)) {
    if (draws > n_iter) {
      msg <- paste0("number of draws must be <= ", n_iter, "!")
      stop(msg) 
    }
    samp <- sample(n_iter, draws)
  } else {
    samp <- 1:n_iter
  }

  if (is.null(new_theta)) {
    # calculate y_hat normally
    pp <- pp_eta(
      alpha = ifelse(!("alpha" %in% names(fit)), 1, fit$alpha[samp, data$k]),
      beta  = fit$beta[samp, data$k],
      theta = fit$theta[samp, data$j]
    )
  } else {
    # calculate y_hat with new_theta
    # TODO: eval different possibilities in new_theta
    # for now sample from prior
    theta <- matrix(rnorm(length(samp)*data$J, 0, 1), nrow = length(samp))
    theta <- theta[ , data$j]

    pp <- pp_eta(
      alpha = ifelse(!("alpha" %in% names(fit)), 1, fit$alpha[samp, data$k]),
      beta  = fit$beta[samp, data$k],
      theta = theta             
    )
  }

  pp <- as.numeric(pp)
  pp <- rbinom(length(pp), 1, pp)
  pp <- matrix(pp, nrow = length(samp))
    
  return(pp)
}

pp_eta <- function(alpha, beta, theta) {
  plogis(alpha*theta - beta)
}
