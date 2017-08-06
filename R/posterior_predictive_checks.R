#' @title
#' Posterior predictive score distribution plot
#'
#' @description
#' Posterior predictive check of observed score distribution for IRT models
#'
#' @param   y_rep     posterior replications of a stanfit object
#' @param   data      Stan data file (named list) 
#' @param   draws     number of random draws from y_rep
#' @param   seed      set a seed for the random number generator
#' @param   alpha     opacity of lines for posterior predicted scores 
#'
ppc_observed_score <- function(y_rep, data, draws = NULL, seed = NULL, alpha = 0.1) {
  if (!is.null(seed)) { set.seed(seed) }

  extent <- 0:data$K 

  # calculate observed scores distributions 
  d <- data.table("x" = extent, "y" = get_scores(data$y, data$j, extent))
  
  # calculate replicated score distribution per iteration
  if (!is.null(draws)) {
    if (draws > nrow(y_rep)) {
      msg <- paste0("Argument 'draws' must be >= ", nrow(y_rep), "! Maximum number of draws used.")
      warning(msg, call. = FALSE, immediate. = TRUE)
      draws <- nrow(y_rep)
      samp  <- 1:draws
    } else {
      samp <- sample(nrow(y_rep), draws)
    }

    v <- paste0("v", samp)

    # calculate predicted scores
    d[, eval(v) := 0L] 
    
    for (i in 1:draws) {
      set(d, NULL, v[i], get_scores(y_rep[samp[i], ], data$j, extent))
    }
  }
  # data tidying for plotting
  d <- melt(d, id.vars = "x")
  d[, "ind" := ifelse(variable == "y", FALSE, TRUE)]

  col <- c("#FF5722",  "#FFCCBC")
  # init plot
  ggplot(d, aes(x, value, group = variable)) + 
    geom_path(colour = col[2], alpha = alpha) + 
    geom_path(aes(colour = ind), size = 1) + 
    scale_colour_manual(values = c(col[1], "transparent"), guide = "none") +  
    scale_x_continuous(limits = c(0, data$K), expand = c(0, 0)) + 
    xlab("score") + 
    ylab("frequency") + 
    theme_minimal() 
}

#' @title
#' score distribution chi-square type
#'
#' @description
#'
#'
ppc_observed_score_chisq <- function(y_rep, data, draws = NULL, seed = NULL, alpha = 1) {
  if (!is.null(seed)) { set.seed(seed) }

  extent <- 0:data$K 

  # calculate observed scores distributions 
  d <- data.table("y" = get_scores(data$y, data$j, extent))
  
  # calculate replicated score distribution per iteration
  if (!is.null(draws)) {
    if (draws > nrow(y_rep)) {
      msg <- paste0("Argument 'draws' must be >= ", nrow(y_rep), "! Maximum number of draws used.")
      warning(msg, call. = FALSE, immediate. = TRUE)
      draws <- nrow(y_rep)
      samp  <- 1:draws
    } else {
      samp <- sample(nrow(y_rep), draws)
    }

    v <- paste0("v", samp)

    # calculate predicted scores
    d[, eval(v) := 0L] 
    
    for (i in 1:draws) {
      set(d, NULL, v[i], get_scores(y_rep[samp[i], ], data$j, extent))
    }
  }

  # get expected score distribution
  d[, "expected" := rowMeans(.SD), .SDcols = v]

  d <- d[, lapply(.SD, function(x) c(chisq(x, y), chisq(x, expected))), .SDcols = v]
  d <- transpose(d)

  lim <- c(0, max(d))

  ggplot(d, aes(x = V1, y = V2)) + 
    geom_abline(intercept = 0, slope = 1, colour = "#9E9E9E") + 
    geom_point(colour = "#FF5722", alpha = alpha) + 
    expand_limits(x = lim, y = lim) +
    xlab("observed difference") + 
    ylab("predicted difference") +  
    theme_minimal()
}

pp_scores <- function(y_rep, data, draws, seed) {
  if (!is.null(seed)) { set.seed(seed) }

  extent <- 0:data$K 

  # calculate observed scores distributions 
  d <- data.table("y" = get_scores(data$y, data$j, extent))
  
  # calculate replicated score distribution per iteration
  if (!is.null(draws)) {
    if (draws > nrow(y_rep)) {
      msg <- paste0("Argument 'draws' must be >= ", nrow(y_rep), "! Maximum number of draws used.")
      warning(msg, call. = FALSE, immediate. = TRUE)
      draws <- nrow(y_rep)
      samp  <- 1:draws
    } else {
      samp <- sample(nrow(y_rep), draws)
    }

    v <- paste0("v", samp)

    # calculate predicted scores
    d[, eval(v) := 0L] 
    
    for (i in 1:draws) {
      set(d, NULL, v[i], get_scores(y_rep[samp[i], ], data$j, extent))
    }
  }
  return(d)
}
  
get_scores <- function(x, by, extent) {
  as.integer(table(factor(tapply(x, by, sum), levels = extent)))
}

chisq <- function(x, y) {
  sum((x - y)^2 / y)
}
