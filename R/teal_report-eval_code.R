#' @importFrom teal.code eval_code
setMethod(
  "eval_code",
  signature = c(object = "teal_report"),
  function(object, code, keep_output = NULL, code_block_opts = list(), ...) {
    new_object <- methods::callNextMethod(object = object, code = code, ...)
    if (inherits(new_object, "error")) {
      return(new_object)
    }

    if (isTRUE(keep_output)) {
      keep_output <- setdiff(
        ls(new_object, all.names = TRUE, sorted = TRUE), ls(object, all.names = TRUE)
      )
    } else if (isFALSE(keep_output)) {
      keep_output <- NULL
    }

    checkmate::assert(
      combine = "and",
      .var.name = "keep_output",
      checkmate::check_character(keep_output, null.ok = TRUE),
      checkmate::check_subset(keep_output, ls(new_object, all.names = TRUE), empty.ok = TRUE)
    )
    temporary_q <- teal.code::qenv()
    temporary_q@code <- setdiff(new_object@code, object@code)
    new_code <- teal.code::get_code(temporary_q)
    if (length(new_code)) {
      teal_card(new_object) <- c(
        teal_card(new_object),
        do.call(code_chunk, args = c(list(code = new_code), code_block_opts))
      )
      teal_card(new_object) <- Reduce(
        function(result, this) {
          this_output <- new_object[[this]]
          c(
            result,
            structure(
              this_output,
              class = c("chunk_output", class(this_output))
            )
          )
        },
        init = teal_card(new_object),
        x = keep_output
      )
    }
    new_object
  }
)
