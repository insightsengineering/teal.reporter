#' @title `NewpageBlock`
#' @keywords internal
NewpageBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "NewpageBlock",
  public = list(
    #' @description Returns a `NewpageBlock` object.
    #'
    #' @details Returns a `NewpageBlock` object with no content and the default style.
    #'
    #' @return `NewpageBlock`
    #' @examples
    #' block <- teal.reporter:::NewpageBlock$new()
    #'
    initialize = function() {
      invisible(self)
    },
    #' @description Returns the content of this `NewpageBlock`
    #'
    #' @return the content of this `NewpageBlock`
    #' @examples
    #' block <- teal.reporter:::NewpageBlock$new()
    #' block$get_content()
    #'
    get_content = function() {
      private$content
    }
  ),
  private = list(
    content = "\n\\newpage\n"
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
