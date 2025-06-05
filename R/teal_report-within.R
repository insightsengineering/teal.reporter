setMethod(
  "within",
  signature = c(data = "teal_report"),
  function(data, expr, keep_output = FALSE, ...) {
    expr <- substitute(expr)
    code <- deparse(expr)
    eval_code(data, code = code, keep_output = keep_output, ...)
  }
) 