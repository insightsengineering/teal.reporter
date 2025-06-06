#' @importFrom teal.code eval_code
setMethod(
  "eval_code",
  signature = c(object = "teal_report"),
  function(object, code, keep_output = FALSE, code_block_opts = list(), ...) {
    new_object <- methods::callNextMethod(object = object, code = code, keep_output = keep_output, ...)
    if (inherits(new_object, "error")) {
      return(new_object)
    }
    temporary_q <- teal.code::qenv()
    temporary_q@code <- setdiff(new_object@code, object@code)
    new_code <- teal.code::get_code(temporary_q)
    if (length(new_code)) {
      teal_card(new_object) <- c(
        teal_card(object),
        do.call(code_chunk, args = c(list(code = new_code), code_block_opts)), # TODO: cache an attribute of code chunk
        attr(new_object@code[[length(new_object@code)]], "output")
      )
    }
    new_object
  }
)
