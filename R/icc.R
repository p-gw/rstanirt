#' @title
#' item characteristic curve
#' 
#' @description
#' plots the item characteristic curve for a given item
#' 
#' @param   item   item indicator (an integer)
#' @param   fit    posterior samples from an IRT model
#' @param   range  x axis limits
#' @param   draws  number of random parameter draws from the posterior    
#'
icc <- function(item, fit, range = 5, draws = 0, seed = NULL) {
  if (!is.null(seed)) { set.seed(seed) }

  if (is(fit, "stanfit")) {
    cat("Argument 'fit' is of class 'stanfit'. \nExtracting posterior replications...\n")
    fit <- extract(fit, pars = c("theta"), include = FALSE)
  }
 
  d <- data.table("x" = seq(-range, range, length.out = 200))
  d[, "y" := icf(alpha = mean(fit$alpha[,item]), beta = mean(fit$beta[,item]), x)]

  if (draws > 0) { 
    s <- sample(1:nrow(fit$beta), draws, replace = FALSE)

    for (i in 1:draws) {
      d[, paste0("v", i) := icf(
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
    geom_path(colour = "#80CBC4", alpha = 0.2) +
    geom_path(aes(colour = ind), size = 1) + 
    scale_colour_manual(values = c("#009688", "transparent"), guide = "none") + 
    scale_x_continuous(limits = c(-range, range), breaks = c(-range, 0, range), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), expand = c(0, 0.05)) + 
    xlab(expression(theta)) + 
    ylab("P(Y = 1)") + 
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
