#' @title
#' Internal characteristic curve function
#' 
#' @description
#' plots characteristic and information curves
#' 
#' @param   item   item indicator (an integer)
#' @param   fit    posterior samples from an IRT model
#' @param   range  x axis limits
#' @param   draws  number of random parameter draws from the posterior    
#' @param   seed   sets the seed for random draws of the posterior replications
#' @param   FUN    declares the function to be used to calculate curves (see details)
#'
#' @details
#' Supported functions are (1) icf, which plots item/test characteristic functions, and 
#' (2) iif, which plots item/test information curves.   
#'
characteristic_curve <- function(item, fit, range, draws, seed, FUN) {
  if (!is.null(seed)) { set.seed(seed) }

  if (is(fit, "stanfit")) {
    cat("Extracting posterior replications...\n")
    fit <- extract(fit, pars = c("theta", "lp__"), include = FALSE)
  }

  if (any(item > ncol(fit$beta))) {
    msg <- paste0("Argument 'item' must be between 1 and ", ncol(fit$beta))
    stop(msg)
  }
 
  d <- data.table("x" = seq(-range, range, length.out = 200))

  # calculate mean characteristic curve
  d[, "y" := 0]

  for (i in item) {
    set(d, NULL, "y", d[, y] + FUN(ifelse("alpha" %in% names(fit), mean(fit$alpha[, i]), 1), mean(fit$beta[, i]), d[, x])) 
  }

  if (!is.null(draws)) {
    if (draws > nrow(fit$beta)) {
      msg <- paste0("Argument 'draws' must be >= ", nrow(fit$beta), "! Maximum number of draws used.")
      warning(msg, call. = FALSE, immediate. = TRUE)
      draws <- nrow(fit$beta)
      samp  <- 1:draws 
    } else {
      samp <- sample(nrow(fit$beta), draws)
    }

    # calculate characteristic curves by iteration
    v <- paste0("v", samp)
    d[, eval(v) := 0]

    for (i in item) {
      for (j in 1:draws) {
        set(d, NULL, v[j], d[, get(v[j])] + FUN(ifelse("alpha" %in% names(fit), fit$alpha[samp[j], i], 1), fit$beta[samp[j], i], d[, x]))
      }
    }
  }

  d <- melt(d, id.vars = "x")
  d[, "ind" := ifelse(variable == "y", FALSE, TRUE)]
  return(d)
}

#' @title
#' Item characteristic curve function
#' 
#' @description
#' plots item characteristic curves
#' 
#' @param   item   item indicator (an integer)
#' @param   fit    posterior samples from an IRT model
#' @param   range  x axis limits
#' @param   draws  number of random parameter draws from the posterior    
#' @param   seed   sets the seed for random draws of posterior replications
#'
#' @details
#' High level wrapper for \code{characteristic_curve} used for plotting item characteristic 
#' curves. 
#' 
icc <- function(item, fit, range = 3, draws = NULL, seed = NULL, alpha = 0.1) {
  if (length(item) > 1) { stop("Only single items are supported for use with icc()!") }

  d <- characteristic_curve(item, fit, range, draws, seed, FUN = icf)  

  col <- c("#039BE5", "#81D4FA") 

  ggplot(d, aes(x, value, group = variable)) +
    geom_path(colour = col[2], alpha = alpha) +
    geom_path(aes(colour = ind), size = 1) +
    scale_colour_manual(values = c(col[1], "transparent"), guide = "none") +
    scale_x_continuous(limits = c(-range, range), breaks = c(-range, 0, range), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), expand = c(0, 0)) + 
    xlab(expression(theta)) + 
    ylab("P(Y = 1)") + 
    theme_minimal()  
}

#' @title
#' Test characteristic curve function
#' 
#' @description
#' plots test characteristic curves
#' 
#' @param   fit    posterior samples from an IRT model
#' @param   range  x axis limits
#' @param   draws  number of random parameter draws from the posterior    
#' @param   seed   sets the seed for random draws of posterior replications
#'
#' @details
#' High level wrapper for \code{characteristic_curve} used for plotting test characteristic 
#' curves. 
#' 
tcc <- function(fit, range = 3, draws = NULL, seed = NULL, alpha = 0.1) {

  nitems <- ifelse(is(fit, "stanfit"), fit@par_dims$beta, ncol(fit$beta))

  d <- characteristic_curve(item = 1:nitems, fit, range, draws, seed, FUN = icf)

  col <- c("#607D8B", "#B0BEC5") 

  ggplot(d, aes(x, value, group = variable)) +
    geom_path(colour = col[2], alpha = alpha) +
    geom_path(aes(colour = ind), size = 1) +
    scale_colour_manual(values = c(col[1], "transparent"), guide = "none") +
    scale_x_continuous(limits = c(-range, range), breaks = c(-range, 0, range), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, nitems), breaks = pretty_breaks(3), expand = c(0, 0)) + 
    xlab(expression(theta)) + 
    ylab("P(Y = 1)") + 
    theme_minimal()  
}

#' @title
#' Item information curve function
#' 
#' @description
#' plots item information curves
#' 
#' @param   item   item indicator (an integer)
#' @param   fit    posterior samples from an IRT model
#' @param   range  x axis limits
#' @param   draws  number of random parameter draws from the posterior    
#' @param   seed   sets the seed for random draws of posterior replications
#'
#' @details
#' High level wrapper for \code{characteristic_curve} used for plotting item information 
#' curves. 
#' 
iic <- function(item, fit, range = 3, draws = NULL, seed = NULL, alpha = 0.1) {
  if (length(item) > 1) { stop("Only single items are supported for use with icc()!") }

  d <- characteristic_curve(item, fit, range, draws, seed, FUN = iif)  

  col <- c("#039BE5", "#81D4FA") 

  ggplot(d, aes(x, value, group = variable)) +
    geom_path(colour = col[2], alpha = alpha) +
    geom_path(aes(colour = ind), size = 1) +
    scale_colour_manual(values = c(col[1], "transparent"), guide = "none") +
    scale_x_continuous(limits = c(-range, range), breaks = c(-range, 0, range), expand = c(0, 0)) +
    xlab(expression(theta)) + 
    ylab("information") + 
    theme_minimal()  
}

#' @title
#' Test information curve function
#' 
#' @description
#' plots Test information curves
#' 
#' @param   fit    posterior samples from an IRT model
#' @param   range  x axis limits
#' @param   draws  number of random parameter draws from the posterior    
#' @param   seed   sets the seed for random draws of posterior replications
#'
#' @details
#' High level wrapper for \code{characteristic_curve} used for plotting test information
#' curves. 
#' 
tic <- function(fit, range = 3, draws = NULL, seed = NULL, alpha = 0.1) {

  nitems <- ifelse(is(fit, "stanfit"), fit@par_dims$beta, ncol(fit$beta))

  d <- characteristic_curve(item = 1:nitems, fit, range, draws, seed, FUN = iif)

  col <- c("#607D8B", "#B0BEC5") 

  ggplot(d, aes(x, value, group = variable)) +
    geom_path(colour = col[2], alpha = alpha) +
    geom_path(aes(colour = ind), size = 1) +
    scale_colour_manual(values = c(col[1], "transparent"), guide = "none") +
    scale_x_continuous(limits = c(-range, range), breaks = c(-range, 0, range), expand = c(0, 0)) +
    xlab(expression(theta)) + 
    ylab("information") + 
    theme_minimal()  
}

#' @title
#' item characteristic function
#' 
#' @description 
#' calculates the value of the item characteristic function
#'
#' @param   alpha   item discrimination
#' @param   beta    item difficulty
#' @param   theta   person parameter
#'
icf <- function(alpha, beta, theta) {
  (1 + exp(-alpha*(theta - beta)))^-1
}

#' @title
#' item information function
#' 
#' @description 
#' calculates the value of the item information function
#'
#' @param   alpha   item discrimination
#' @param   beta    item difficulty
#' @param   theta   person parameter
#'
#' @export
#'
iif <- function(alpha, beta, theta) {
  alpha^2 * icf(alpha, beta, theta) * (1 - icf(alpha, beta, theta))
}
