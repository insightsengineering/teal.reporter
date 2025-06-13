#' Concatenate teal_report objects
#'
#' @param ... teal_report objects to concatenate
#'
#' @return A teal_report object with concatenated code, .xData, and teal_card fields
#'
#' @export
#' @method c teal_report
c.teal_report <- function(...) {
  result <- NextMethod()
  teal.reporter::teal_card(result) <- do.call(c, lapply(list(...), teal.reporter::teal_card))
  result
} 