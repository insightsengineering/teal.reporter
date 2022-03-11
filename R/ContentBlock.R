#' @title `ContentBlock`
#' @keywords internal
ContentBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "ContentBlock",
  public = list(
    #' @description Returns a `ContentBlock` object.
    #'
    #' @details Returns a `ContentBlock` object with no content and the default style.
    #'
    #' @return `ContentBlock`
    #' @examples
    #' block <- teal.reporter:::ContentBlock$new()
    #'
    initialize = function() {
      private$content <- character(0)
      invisible(self)
    },
    #' @description Sets content of this `ContentBlock`.
    #'
    #' @param content (`character(0)` or `character(1)`) a string literal or a file path assigned to this `ContentBlock`
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::ContentBlock$new()
    #' block$set_content("Base64 encoded picture")
    #'
    set_content = function(content) {
      checkmate::assert_character(content, min.len = 0, max.len = 1)
      private$content <- content
      invisible(self)
    },
    #' @description Returns the absolute path to content of this `ContentBlock`
    #'
    #' @return `character` content of this `ContentBlock`
    #' @examples
    #' block <- teal.reporter:::ContentBlock$new()
    #' block$get_content()
    #'
    get_content = function() {
      private$content
    },
    #' @description The copy constructor.
    #'
    #' @param name `character(1)` the name of the field
    #' @param value the value assigned to the field
    #'
    #' @return the value of the copied field
    #'
    deep_clone = function(name, value) {
      if (name == "content" && checkmate::assert_file_exists(value)) {
        copied_file <- tempfile()
        file.copy(value, copied_file, copy.date = TRUE)
        copied_file
      } else {
        value
      }
    }
  ),
  private = list(
    content = character(0)
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
