#' @title
#' Fitting function for the 1PL-IRT Model
#'
#' @description
#' Fits the 1PL Item Response Model 
#'
#' @param     data    A named list containing all relevant variables (see details)
#'
#' @details
#' The named list must contain the following variables:
#' 
stan_1pl <- function(data, ...) {
  stan_fit <- sampling(object = stanmodels$'1pl', data = data, ...)
  return(stan_fit)
  # structure(stan_fit, class = c("rstanirt", "1pl"))
}

stan_2pl <- function(data, ...) {
  stan_fit <- sampling(object = stanmodels$'2pl', data = data, ...)
  return(stan_fit)
}

stan_3pl <- function(...) {
  print("3PL NOT IMPLEMENTED YET")
}
