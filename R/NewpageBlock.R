#' @title `NewpageBlock`
#' @docType class
#' @description
#' A `ContentBlock` subclass that represents a page break in a report output.
#'
#' @keywords internal
NewpageBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "NewpageBlock",
  inherit = ContentBlock,
  public = list(
    #' @description Initialize a `NewpageBlock` object.
    #'
    #' @details Returns a `NewpageBlock` object with no content and the default style.
    #'
    #' @return Object of class `NewpageBlock`, invisibly.
    #' @examples
    #' NewpageBlock <- getFromNamespace("NewpageBlock", "teal.reporter")
    #' block <- NewpageBlock$new()
    #'
    initialize = function() {
      super$set_content("\n\\newpage\n")
      invisible(self)
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
