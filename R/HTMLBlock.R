#' @title `HTMLBlock`
#' @docType class
#' @description
#' Specialized `FileBlock` for managing HTML content in reports.
#' It's designed to handle various HTML content, and render the report as HTML,
#' however `htmlwidgets` objects can also be rendered to static document-ready format.
#'
#' @keywords internal
HTMLBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "HTMLBlock",
  inherit = ContentBlock,
  public = list(
    #' @description Initialize a `HTMLBlock` object.
    #'
    #' @param content An object that can be rendered as a HTML content assigned to
    #'   this `HTMLBlock`
    #'
    #' @return Object of class `HTMLBlock`, invisibly.
    initialize = function(content) {
      if (!missing(content)) {
        checkmate::assert_multi_class(content, private$supported_types)
        self$set_content(content)
      }
      invisible(self)
    },

    #' @description Create the `HTMLBlock` from a list.
    #'
    #' @param x (`named list`) with a single field `content` containing `shiny.tag`,
    #' `shiny.tag.list` or `htmlwidget`.
    #'
    #' @return `self`, invisibly.
    #' @examples
    #' HTMLBlock <- getFromNamespace("HTMLBlock", "teal.reporter")
    #' block <- HTMLBlock$new()
    #' block$from_list(list(content = shiny::tags$div("test")))
    #'
    from_list = function(x) {
      checkmate::assert_list(x, types = private$supported_types)
      checkmate::assert_names(names(x), must.include = "content")
      self$set_content(x$content)
      invisible(self)
    },

    #' @description Convert the `HTMLBlock` to a list.
    #'
    #' @return `named list` with a text and style.
    #' @examples
    #' HTMLBlock <- getFromNamespace("HTMLBlock", "teal.reporter")
    #' block <- HTMLBlock$new(shiny::tags$div("test"))
    #' block$to_list()
    #'
    to_list = function() {
      list(content = self$get_content())
    }
  ),
  private = list(
    supported_types = c("shiny.tag", "shiny.tag.list", "htmlwidget")
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
