#' @export
setMethod(
  "eval_code",
  signature = c("teal_report", "character"),
  function(object, code, cache = FALSE, code_block_opts = list(), ...) {
    out <- methods::callNextMethod(object = object, code = code, cache = cache, ...)
    if (length(code)) {
      report(out) <- c(
        report(object),
        do.call(code_chunk, args = c(list(code = code), code_block_opts)), # todo: cache is an attribute of a code chunk
        attr(out@code[[length(out@code)]], "cache")
      )
    }

    out
  }
)


#' @export
setMethod(
  "eval_code",
  signature = c("teal_report", "language"),
  function(object, code, cache = FALSE, code_block_opts = list(), ...) {
    teal.code::eval_code(
      object = object,
      code = paste(vapply(lang2calls(code), deparse1, collapse = "\n", character(1L)), collapse = "\n"),
      cache = cache,
      code_block_opts = code_block_opts,
      ...
    )
  }
)

#' @export
setMethod(
  "eval_code",
  signature = c("teal_report", "expression"),
  function(object, code, cache = FALSE, code_block_opts = list(), ...) {
    srcref <- attr(code, "wholeSrcref")
    if (length(srcref)) {
      teal.code::eval_code(
        object = object,
        code = paste(attr(code, "wholeSrcref"), collapse = "\n"),
        cache = cache,
        code_block_opts = code_block_opts,
        ...
      )
    } else {
      Reduce(
        function(x, code_i) {
          teal.code::eval_code(object = x, code = code_i, cache = cache)
        },
        init = object,
        x = code
      )
    }


    methods::callNextMethod(
      object = object,
      code = paste(vapply(lang2calls(code), deparse1, collapse = "\n", character(1L)), collapse = "\n"),
      code_block_opts = code_block_opts,
      ...
    )
  }
)
