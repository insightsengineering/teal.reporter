#' @export
`[.teal_report` <- function(x, names) {
  x <- NextMethod("`[`", x) # unverified doesn't need warning for code inconsistency
  x@teal_card <- x@teal_card # todo: https://github.com/insightsengineering/teal.reporter/issues/394
  x
}
