#' @title `ListBlock`
#' @keywords internal
ListBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "ListBlock",
  inherit = ContentBlock,
  public = list(
    #' @description Returns a `ListBlock` object.
    #'
    #' @details Returns a `ListBlock` object with no content and the default style.
    #'
    #' @param content (`list()`) a list assigned to this `ListBlock`
    #' @param style (`character(1)`) one of: `default`, `fs`, `encodings`
    #'
    #' @return `ListBlock`
    #' @examples
    #' block <- teal.reporter:::ListBlock$new()
    #'
    initialize = function(content = list(), style = private$styles[1]) {
      self$set_content(content)
      self$set_style(style)
      invisible(self)
    },
    #' @description Sets the content of this `ListBlock`.
    #'
    #' @details throws if argument is not a `list`.
    #'
    #' @param content (`list`) a list in this `ListBlock`
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::ListBlock$new()
    #' block$set_content(list(A <- 1))

    set_content = function(content) {
      checkmate::assert_list(content, min.len = 0)
      super$set_content(deparse1(content))
    },
    #' @description Sets the style of this `listBlock`.
    #'
    #' @details The style has bearing on the rendering of this block.
    #'
    #' @param style (`character(1)`) one of: `default`, `fs`, `encodings`.
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::ListBlock$new()
    #' block$set_style("default")
    #'
    set_style = function(style) {
      private$style <- match.arg(style, private$styles)
      invisible(self)
    },
    #' @description Returns the style of this `listBlock`.
    #'
    #' @return `character(1)` the style of this `ListBlock`
    #' @examples
    #' block <- teal.reporter:::ListBlock$new()
    #' block$get_style()
    #'
    get_style = function() {
      private$style
    },
    #' @description Returns an array of styles available to this `ListBlock`.
    #'
    #' @return a `character` array of styles
    #' @examples
    #' block <- teal.reporter:::ListBlock$new()
    #' block$get_available_styles()
    #'
    get_available_styles = function() {
      private$styles
    }
  ),
  private = list(
    style = character(0),
    styles = c("default" ,"fs", "encodings")
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
