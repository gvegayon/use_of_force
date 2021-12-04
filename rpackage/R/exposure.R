#' Dynamic Exposure
#'
#' Computes dynamic exposure in a longitudinal dataset without double counting
#' individuals.
#'
#' @param id_indiv Integer vector. Ids of individuals
#' @param id_events Integer vector. Ids of the events (must have a temporal
#' mapping).
#' @param actions Integer vector. Any value greater than one is considered
#' to be an action (true).
#' @param offset Integer. Offset in terms of lead
#' @export
#'
exposure_dyn <- function(
  id_indiv,
  id_events,
  actions,
  offset = 1
) {

  # Generating order with time
  ord <- order(id_events)

  ans <- do.call(cbind, exposure_dyn_(
    id_indiv = id_indiv[ord],
    id_events = id_events[ord],
    actions = actions[ord],
    offset = offset[ord]
  ))

  ans[order((1:nrow(ans))[ord]),]

}


set.seed(123)
x <- 1:10
y <- sample(10)

ord <- order(y)
y[ord]
y[ord][order(x[ord])] - y
