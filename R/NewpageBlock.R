#' @title `NewpageBlock`
#' @keywords internal
NewpageBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "NewpageBlock",
  inherit = ContentBlock,
  public = list(
    #' @description Returns a `NewpageBlock` object.
    #'
    #' @details Returns a `NewpageBlock` object with no content and the default style.
    #'
    #' @return `NewpageBlock`
    initialize = function() {
      super$set_content("\n\\newpage\n")
      invisible(self)
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
