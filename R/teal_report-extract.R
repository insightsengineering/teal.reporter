#' @export
`[.teal_report` <- function(x, names) {
  x <- NextMethod("`[`", x) # unverified doesn't need warning for code inconsistency
  x@card <- x@card # todo: return code_chunks for given names
  x
}
