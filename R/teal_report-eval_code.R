setMethod(
  "eval_code",
  signature = c("teal_report", "ANY"),
  function(object, code, cache = FALSE, code_block_opts = list(), ...) {
    logger::log_fatal("eval_code with 'teal_report'")
    new_object <- methods::callNextMethod(object = object, code = code, cache = cache, ...)
    if (inherits(new_object, "error")) {
      return(new_object)
    }
    temporary_q <- qenv()
    temporary_q@code <- setdiff(new_object@code, object@code)
    new_code <- get_code(temporary_q)
    if (length(new_code)) {
      report(new_object) <- c(
        report(object),
        do.call(code_chunk, args = c(list(code = new_code), code_block_opts)), # todo: cache is an attribute of a code chunk
        attr(new_object@code[[length(new_object@code)]], "cache")
      )
    }
    new_object
  }
)
