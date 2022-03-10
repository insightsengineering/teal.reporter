#' @title `TableBlock`
#' @keywords internal
TableBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "TableBlock",
  public = list(
    #' @description Returns a `TableBlock` object.
    #'
    #' @details Returns a `TableBlock` object with no content and the default style.
    #'
    #' @return `TableBlock`
    #' @examples
    #' block <- teal.reporter:::TableBlock$new()
    #'
    initialize = function() {
      invisible(self)
    },
    #' @description Sets the content of this `TableBlock`.
    #'
    #' @details throws if argument is not `character(1)`.
    #'
    #' @param content (`character(1)`) a string path assigned to this `TableBlock`
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::TableBlock$new()
    #' table_path <- tempfile(fileext = ".csv")
    #' write.csv(mtcars, table_path)
    #' block$set_content(table_path)
    #'
    set_content = function(content) {
      checkmate::assert_string(content)
      checkmate::assert_file_exists(content)
      private$content <- content
      private$type <- tools::file_ext(content)
      invisible(self)
    },
    #' @description Returns the content of this `TableBlock`
    #'
    #' @return the content of this `TableBlock`
    #' @examples
    #' block <- teal.reporter:::TableBlock$new()
    #' block$get_content()
    #'
    get_content = function() {
      private$content
    },
    #' @description Returns the type of this `TableBlock`
    #'
    #' @return the type of this `TableBlock`
    #' @examples
    #' block <- teal.reporter:::TableBlock$new()
    #' block$get_type()
    #'
    get_type = function() {
      return(private$type)
    }
  ),
  private = list(
    content = character(0),
    type = character(0)
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
