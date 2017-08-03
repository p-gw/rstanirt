#' @title
#' item information curve
#' 
#' @description
#' plots the item information for a given item
#' 
#' @param   item   item indicator (an integer)
#' @param   fit    posterior samples from an IRT model
#' @param   range  x axis limits
#' @param   draws  number of random parameter draws from the posterior    
#'
#' @export
#'
iic <- function(item, fit, range = 5, draws = 0, seed = NULL) {
  if (!is.null(seed)) { set.seed(seed) }
 
  
  if (is(fit, "stanfit")) {
    cat("Argument 'fit' is of class 'stanfit'. \nExtracting posterior replications...\n")
    fit <- extract(fit, pars = c("theta"), include = FALSE)
  }

  d <- data.table("x" = seq(-range, range, length.out = 200))
  d[, "y" := iif(alpha = mean(fit$alpha[,item]), beta = mean(fit$beta[,item]), x)]

  if (draws > 0) { 
    s <- sample(1:nrow(fit$beta), draws, replace = FALSE)

    for (i in 1:draws) {
      d[, paste0("v", i) := iif(
        alpha = mean(fit$alpha[s[i], item]), 
        beta = mean(fit$beta[s[i], item]), 
        x)
      ]
    }
  }

  d <- melt(d, id.vars = "x")
  d[, "ind" := ifelse(variable == "y", FALSE, TRUE)]

  # init plot
  ggplot(d, aes(x, value, group = variable)) + 
    geom_path(colour = "#CE93D8", alpha = 0.4) +
    geom_path(aes(colour = ind), size = 1) + 
    scale_colour_manual(values = c("#9C27B0", "transparent"), guide = "none") + 
    scale_x_continuous(limits = c(-range, range), breaks = c(-range, 0, range), expand = c(0, 0)) + 
    xlab(expression(theta)) + 
    ylab("information")
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
