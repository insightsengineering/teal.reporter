#' @title `HTMLBlock`
#' @docType class
#' @description
#' Specialized `FileBlock` for managing HTML content in reports.
#' It's designed to handle various HTML content, and render the report as HTML,
#' however htmlwidget objects can also be rendered to static document-ready format.
#'
#' @keywords internal
HTMLBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "HTMLBlock",
  inherit = FileBlock,
  public = list(
    #' @description Initialize a `HTMLBlock` object.
    #'
    #' @param content An object that can be rendered as a HTML content assigned to
    #'   this `HTMLBlock`
    #'
    #' @return Object of class `HTMLBlock`, invisibly.
    initialize = function(content) {
      if (!missing(content)) {
        self$set_content(content)
      }
      invisible(self)
    },
    #' @description Sets content of this `HTMLBlock`.
    #'
    #' @param content An object that can be rendered as a HTML content
    #' assigned to this `HTMLBlock`
    #'
    #' @return `self`, invisibly.
    #' @examples
    #' HTMLBlock <- getFromNamespace("HTMLBlock", "teal.reporter")
    #' block <- HTMLBlock$new()
    #' block$set_content(shiny::div("HTML Content"))
    #'
    set_content = function(content) {
      path <- tempfile(fileext = ".rds")
      saveRDS(content, file = path)
      super$set_content(path)
      invisible(self)
    }
  ),
  private = list(),
  lock_objects = TRUE,
  lock_class = TRUE
)
