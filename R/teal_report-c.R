#' Concatenate `teal_report` objects
#'
#' @param ... (`teal_report`) objects to concatenate
#' @param verbose (`logical`) If `TRUE`, will print a warning when appending `teal_report` objects that
#' share elements of their `teal_card` objects.
#'
#' @return A [`teal_report`] object with combined [`teal_card`] elements.
#'
#' @export
#' @method c teal_report
c.teal_report <- function(..., verbose = TRUE) {
  result <- NextMethod()
  l <- Filter(function(x) inherits(x, "teal_report"), list(...))
  if (length(l) > 1) {
    teal_card(result) <- do.call(c, c(lapply(l, teal_card), verbose = verbose))
  }
  result
}
