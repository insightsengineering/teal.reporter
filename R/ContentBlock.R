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
    #' content_block <- getFromNamespace("ContentBlock", "teal.reporter")
    #' content_block$new()
    #'
    initialize = function() {
      private$content <- character(0)
      invisible(self)
    },
    #' @description Sets content of this `ContentBlock`.
    #'
    #' @param content (`character(0)` or `character(1)`) a string literal or a file path assigned to this `ContentBlock`
    #'
    #' @return invisibly self
    #' @examples
    #' content_block <- getFromNamespace("ContentBlock", "teal.reporter")
    #' block <- content_block$new()
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
    #' content_block <- getFromNamespace("ContentBlock", "teal.reporter")
    #' block <- content_block$new()
    #' block$get_content()
    #'
    get_content = function() {
      private$content
    },
    #' @description Create the `ContentBlock` from a list.
    #'
    #' @param x `named list` with two fields `c("text", "style")`.
    #' Use the `get_available_styles` method to get all possible styles.
    #'
    #' @return invisibly self
    from_list = function(x) {
      invisible(self)
    },
    #' @description Convert the `ContentBlock` to a list.
    #'
    #' @return `named list` with a text and style.
    to_list = function() {
      list()
    }
  ),
  private = list(
    content = character(0),
    # @description The copy constructor.
    #
    # @param name `character(1)` the name of the field
    # @param value the value assigned to the field
    #
    # @return the value of the copied field
    deep_clone = function(name, value) {
      if (name == "content" && checkmate::test_file_exists(value)) {
        extension <- ""
        split <- strsplit(basename(value), split = "\\.")
        # The below ensures no extension is found for files such as this: .gitignore but is found for files like
        # .gitignore.txt
        if (length(split[[1]]) > 1 && split[[1]][length(split[[1]]) - 1] != "") {
          extension <- split[[1]][length(split[[1]])]
          extension <- paste0(".", extension)
        }
        copied_file <- tempfile(fileext = extension)
        file.copy(value, copied_file, copy.date = TRUE, copy.mode = TRUE)
        copied_file
      } else {
        value
      }
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
