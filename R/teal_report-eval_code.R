#' @name eval_code-teal_report
#' @rdname eval_code-teal_report
#' @aliases eval_code,teal_report-method
#'
#' @inherit teal.code::eval_code
#' @param object (`teal_report`)
#' @param code_block_opts (`list`) Additional options for the R code chunk in R Markdown.
#' @return `teal_reporter` environment with the code evaluated and the outputs added
#' to the card or `qenv.error` if evaluation fails.
#' @importFrom teal.code eval_code
#'
#' @examples
#' td <- teal.data::teal_data()
#' td <- teal.code::eval_code(td, "iris <- iris")
#' tr <- as.teal_report(td)
#' tr <- teal.code::eval_code(tr, "a <- 1")
#' tr <- teal.code::eval_code(tr, "b <- 2L # with comment")
#' tr <- teal.code::eval_code(tr, quote(library(checkmate)))
#' tr <- teal.code::eval_code(tr, expression(assert_number(a)))
#' teal_card(tr)
setMethod(
  "eval_code",
  signature = c(object = "teal_report"),
  function(object, code, code_block_opts = list(), ...) {
    new_object <- methods::callNextMethod(object = object, code = code, ...)
    if (inherits(new_object, "error")) {
      return(new_object)
    }
    new_blocks <- .code_to_card(x = setdiff(new_object@code, object@code), code_block_opts = code_block_opts)

    if (length(new_blocks)) {
      teal_card(new_object) <- c(teal_card(new_object), new_blocks)
    }
    new_object
  }
)
