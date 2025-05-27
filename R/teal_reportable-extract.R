#' @export
`[.teal_data` <- function(x, names) {
  x <- NextMethod("`[`", x, check_code_names = x@verified) # unverified doesn't need warning for code inconsistency
  x@join_keys <- x@join_keys[names]
  x@report <- x@report # todo: return code_chunks for given names
  x
}
