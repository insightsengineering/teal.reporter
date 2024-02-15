#' @title `TextBlock`
#' @keywords internal
TextBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "TextBlock",
  inherit = ContentBlock,
  public = list(
    #' @description Returns a `TextBlock` object.
    #'
    #' @details Returns a `TextBlock` object with no content and the default style.
    #'
    #' @param content (`character(1)` or `character(0)`) a string assigned to this `TextBlock`
    #' @param style (`character(1)`) one of: `"default"`, `"header2"`, `"header3"` `"verbatim"`
    #'
    #' @return `TextBlock`.
    #' @examples
    #' TextBlock <- getFromNamespace("TextBlock", "teal.reporter")
    #' block <- TextBlock$new()
    #'
    initialize = function(content = character(0), style = private$styles[1]) {
      super$set_content(content)
      self$set_style(style)
      invisible(self)
    },
    #' @description Sets the style of this `TextBlock`.
    #'
    #' @details The style has bearing on the rendering of this block.
    #'
    #' @param style (`character(1)`) one of: `"default"`, `"header2"`, `"header3"` `"verbatim"`
    #'
    #' @return `self`, invisibly.
    #' @examples
    #' TextBlock <- getFromNamespace("TextBlock", "teal.reporter")
    #' block <- TextBlock$new()
    #' block$set_style("header2")
    #'
    set_style = function(style) {
      private$style <- match.arg(style, private$styles)
      invisible(self)
    },
    #' @description Returns the style of this `TextBlock`.
    #'
    #' @return `character(1)` the style of this `TextBlock`.
    #' @examples
    #' TextBlock <- getFromNamespace("TextBlock", "teal.reporter")
    #' block <- TextBlock$new()
    #' block$get_style()
    #'
    get_style = function() {
      private$style
    },
    #' @description Returns an array of styles available to this `TextBlock`.
    #'
    #' @return A `character` array of styles.
    #' @examples
    #' TextBlock <- getFromNamespace("TextBlock", "teal.reporter")
    #' block <- TextBlock$new()
    #' block$get_available_styles()
    #'
    get_available_styles = function() {
      private$styles
    },
    #' @description Create the `TextBlock` from a list.
    #'
    #' @param x `named list` with two fields `c("text", "style")`.
    #' Use the `get_available_styles` method to get all possible styles.
    #'
    #' @return `self`, invisibly.
    #' @examples
    #' TextBlock <- getFromNamespace("TextBlock", "teal.reporter")
    #' block <- TextBlock$new()
    #' block$from_list(list(text = "sth", style = "default"))
    #'
    from_list = function(x) {
      checkmate::assert_list(x)
      checkmate::assert_names(names(x), must.include = c("text", "style"))
      self$set_content(x$text)
      self$set_style(x$style)
      invisible(self)
    },
    #' @description Convert the `TextBlock` to a list.
    #'
    #' @return `named list` with a text and style.
    #' @examples
    #' TextBlock <- getFromNamespace("TextBlock", "teal.reporter")
    #' block <- TextBlock$new()
    #' block$to_list()
    #'
    to_list = function() {
      list(text = self$get_content(), style = self$get_style())
    }
  ),
  private = list(
    style = character(0),
    styles = c("default", "header2", "header3", "verbatim")
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
