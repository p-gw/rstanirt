#' @title
#' Posterior predictive score distribution plot
#
#' @description
#' Posterior predictive check of observed score distribution for IRT models
#
#' @param   y_rep     posterior replications of a stanfit object
#' @param   data      Stan data file (named list) 
#' @param   confint   confidence level
#'
#' @export
#'
ppc_observed_score <- function(y_rep, data, confint = 0.95) {
  extent <- 0:data$K 

  get_scores <- function(x, by) {
    table(factor(tapply(x, by, sum), levels = extent))
  }

  # calculate observed scores distributions 
  score_obs <- get_scores(data$y, data$j)

  # calculate replicated score distribution per iteration
  score_rep <- apply(y_rep, 1, function(x) { get_scores(x, data$j) })   

  # get quantiles
  bound <- (1 - confint)/2
  int <- c(0 + bound, 0.5, 1 - bound) 

  score_rep <- apply(score_rep, 1, function(x) { quantile(x, probs = int) }) 
  rownames(score_rep) <- c("lwr", "md", "upr")

  # data tidying for plotting
  plot_data <- data.frame(score_obs, t(score_rep))
  plot_data$Var1 <- as.numeric(as.character(plot_data$Var1))
  
  # init plot
  ggplot(data = plot_data, aes_string(x = "Var1")) + 
    geom_path(aes_string(y = "Freq"), colour = "orange") +
    geom_point(aes_string(y = "Freq"), colour = "orange") + 
    geom_path(aes_string(y = "md")) + 
    geom_point(aes_string(y = "md"), shape = 21, fill = "white") + 
    geom_path(aes_string(y = "lwr"), linetype = 3) + 
    geom_path(aes_string(y = "upr"), linetype = 3) +
    xlab("score") + 
    ylab("frequency")  
}
