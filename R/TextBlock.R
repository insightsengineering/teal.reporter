#' @title `TextBlock`
#' @keywords internal
TextBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "TextBlock",
  public = list(
    #' @description Returns a `TextBlock` object.
    #'
    #' @details Returns a `TextBlock` object with no content and the default style.
    #'
    #' @return `TextBlock`
    #' @examples
    #' block <- teal.reporter:::TextBlock$new()
    #'
    initialize = function() {
      private$style <- private$styles[1]
      invisible(self)
    },
    #' @description Sets the style of this `TextBlock`.
    #'
    #' @details The style has bearing on the rendering of this block.
    #'
    #' @param style (`character(1)`) one of: `default`, `header2`, `header3`, `verbatim`.
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::TextBlock$new()
    #' block$set_style("header2")
    #'
    set_style = function(style) {
      private$style <- match.arg(style, private$styles)
      invisible(self)
    },
    #' @description Returns the style of this `TextBlock`.
    #'
    #' @return `character(1)` the style of this `TextBlock`
    #' @examples
    #' block <- teal.reporter:::TextBlock$new()
    #' block$get_style()
    #'
    get_style = function() {
      private$style
    },
    #' @description Sets the content of this `TextBlock`.
    #'
    #' @details throws if argument is not `character(1)`.
    #'
    #' @param content (`character(1)`) a string assigned to this `TextBlock`
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::TextBlock$new()
    #' block$set_content("Some text")
    #'
    set_content = function(content) {
      checkmate::assert_string(content)
      private$content <- content
      invisible(self)
    },
    #' @description Returns the content of this `TextBlock`
    #'
    #' @return the content of this `TextBlock`
    #' @examples
    #' block <- teal.reporter:::TextBlock$new()
    #' block$get_content()
    #'
    get_content = function() {
      private$content
    },
    #' @description Returns an array of styles available to this `TextBlock`.
    #'
    #' @return a `character` array of styles
    #' @examples
    #' block <- teal.reporter:::TextBlock$new()
    #' block$get_available_styles()
    #'
    get_available_styles = function() {
      private$styles
    }
  ),
  private = list(
    content = character(0),
    style = character(0),
    styles = c("default", "header2", "header3", "verbatim")
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
