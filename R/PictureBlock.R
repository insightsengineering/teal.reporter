#' @title `PictureBlock`
#' @keywords internal
PictureBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "PictureBlock",
  public = list(
    #' @description Returns a `PictureBlock` object.
    #'
    #' @details Returns a `PictureBlock` object with no content and the default style.
    #'
    #' @return `PictureBlock`
    #' @examples
    #' block <- teal.reporter:::PictureBlock$new()
    #'
    initialize = function() {
      invisible(self)
    },
    #' @description Sets the content of this `PictureBlock`.
    #'
    #' @details throws if argument is not `character(1)`.
    #'
    #' @param content (`character(1)`) a string assigned to this `PictureBlock`
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::PictureBlock$new()
    #' block$set_content("Base64 encoded picture")
    #'
    set_content = function(content) {
      checkmate::assert_string(content)
      private$content <- content
      invisible(self)
    },
    #' @description Returns the content of this `PictureBlock`
    #'
    #' @return the content of this `PictureBlock`
    #' @examples
    #' block <- teal.reporter:::PictureBlock$new()
    #' block$get_content()
    #'
    get_content = function() {
      private$content
    }
  ),
  private = list(
    content = character(0)
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
