#' @inherit teal.code::eval_code
#' @param object (`teal_report`)
#' @param keep_output (`character` or `NULL`) Names of output objects in the environment
#' that are will be added in the card for the reporter.
#' These are shown in the card via the [tools::toHTML()] and [to_rmd()] implementations.
#' @param code_block_opts (`list`) Additional options for the R code chunk in R Markdown.
#' @return `teal_reporter` environment with the code evaluated and the outputs added
#' to the card or `qenv.error` if evaluation fails.
#' @importFrom teal.code eval_code
setMethod(
  "eval_code",
  signature = c(object = "teal_report"),
  function(object, code, keep_output = NULL, code_block_opts = list(), ...) {
    new_object <- methods::callNextMethod(object = object, code = code, ...)
    if (inherits(new_object, "error")) {
      return(new_object)
    }

    checkmate::assert(
      combine = "and",
      .var.name = "keep_output",
      checkmate::check_character(keep_output, null.ok = TRUE),
      checkmate::check_subset(keep_output, ls(new_object, all.names = TRUE), empty.ok = TRUE)
    )
    new_code <- .preprocess_code(code)
    if (length(new_code)) {
      teal_card(new_object) <- c(
        teal_card(new_object),
        do.call(code_chunk, args = c(list(code = new_code), code_block_opts))
      )
      teal_card(new_object) <- Reduce(
        function(result, this) c(result, new_object[[this]]),
        init = teal_card(new_object),
        x = keep_output
      )
    }
    new_object
  }
)

.preprocess_code <- getFromNamespace(".preprocess_code", "teal.code")
