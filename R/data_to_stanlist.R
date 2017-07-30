#' @title
#' Stan-List from a long data.frame
#'
#' @description
#' Transfroms the data to an appropriate list for usage with Stan IRT Models
#' 
#' @param   data          A data.frame in long format 
#' @param   measure.var   Variable containing item responses
#' @param   item.id       Variable containing item identifiers
#' @param   person.id     Variable containing person identifiers
#' @param   group.id      Variable containing group identifiers (optional)
#'
long_to_stanlist <- function(data, measure.var, item.id, person.id, group.id = NULL) {
  if (!is.data.table(data)) { setDT(data) }

  if ( data[, .N] != data[complete.cases(data), .N]) {
    warning("Removed missing values from data.frame! Please check your data.")
    data <- data[complete.cases(data)]
  }

  # TODO: check for correct variable classes
  res <- list(
    "J" = data[, length(unique(get(person.id)))],
    "K" = data[, length(unique(get(item.id)))],
    "N" = data[, .N],
    "j" = data[, as.integer(get(person.id))],
    "k" = data[, as.integer(get(item.id))],
    "y" = data[, as.integer(get(measure.var))]
  )
  
  if (!is.null(group.id)) res$x <- data[, as.integer(get(group.id))]

  return(res)
}

#' @title
#' Stan-List from a wide data.frame
#'
#' @description
#' Transfroms the data to an appropriate list for usage with Stan IRT Models
#'
#' @param   data          A data.frame in wide format
#' @param   measure.vars  character string containing column names of item responses
#' @param   group.var     character string containing a grouping variable name (optional)
#'
wide_to_stanlist <- function(data, measure.vars, group.var = NULL) {
  if (!is.data.table(data)) { setDT(data) }

  data$id <- 1L:nrow(data)

  # wide to long
  data <- melt(data = data, id.vars = c("id", group.var), measure.vars = measure.vars)

  # long to stan list
  long_to_stanlist(data, "value", "variable", "id", group.var)
}
