#' @title `ContentBlock`: A building block for report content
#' @docType class
#' @description This class represents a basic content unit in a report,
#' such as text, images, or other multimedia elements.
#' It serves as a foundation for constructing complex report structures.
#'
#' @keywords internal
ContentBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "ContentBlock",
  public = list(
    #' @description Sets content of this `ContentBlock`.
    #'
    #' @param content (`any`) R object
    #'
    #' @return `self`, invisibly.
    #' @examples
    #' ContentBlock <- getFromNamespace("ContentBlock", "teal.reporter")
    #' block <- ContentBlock$new()
    #' block$set_content("Base64 encoded picture")
    #'
    set_content = function(content) {
      private$content <- content
      invisible(self)
    },
    #' @description Retrieves the content assigned to this block.
    #'
    #' @return object stored in a `private$content` field
    #' @examples
    #' ContentBlock <- getFromNamespace("ContentBlock", "teal.reporter")
    #' block <- ContentBlock$new()
    #' block$get_content()
    #'
    get_content = function() {
      private$content
    },
    #' @description Create the `ContentBlock` from a list.
    #'
    #' @param x (`named list`) with two fields `text` and `style`.
    #' Use the `get_available_styles` method to get all possible styles.
    #'
    #' @return `self`, invisibly.
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
    content = NULL, # this can be any R object
    # @description The copy constructor.
    #
    # @param name (`character(1)`) the name of the field
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
