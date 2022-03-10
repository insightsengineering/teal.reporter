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
    #' @param content (`character(1)`) a string path assigned to this `PictureBlock`
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::PictureBlock$new()
    #' img_path <- system.file("img", "Rlogo.png", package = "png")
    #' block$set_content(img_path)
    #'
    set_content = function(content) {
      checkmate::assert_string(content)
      checkmate::assert_file_exists(content)
      private$content <- content
      private$type <- tools::file_ext(content)
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
    },
    #' @description Sets the title of this `PictureBlock`.
    #'
    #' @details throws if argument is not `character(1)`.
    #'
    #' @param title (`character(1)`) a string assigned to this `PictureBlock`
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::PictureBlock$new()
    #' block$set_title("Title")
    #'
    set_title = function(title) {
      checkmate::assert_string(title)
      private$title <- title
      invisible(self)
    },
    #' @description Returns the title of this `PictureBlock`
    #'
    #' @return the content of this `PictureBlock`
    #' @examples
    #' block <- teal.reporter:::PictureBlock$new()
    #' block$get_title()
    #'
    get_title = function() {
      private$title
    },
    #' @description Returns the type of this `PictureBlock`
    #'
    #' @return the type of this `PictureBlock`
    #' @examples
    #' block <- teal.reporter:::PictureBlock$new()
    #' block$get_type()
    #'
    get_type = function() {
      return(private$type)
    },
    #' @description set the dimensions of this `PictureBlock`
    #'
    #' @param dim `numeric` figure dimensions (width and height) in percent, length 2.
    #' @return `self`
    #' @examples
    #' block <- teal.reporter:::PictureBlock$new()
    #' block$set_dim(c(50, 50))
    #'
    set_dim = function(dim) {
      checkmate::assert_numeric(dim, len = 2)
      private$dim <- dim
      invisible(self)
    },
    #' @description get the dimensions of this `PictureBlock`
    #'
    #' @return `numeric` dim filed, dimensions.
    #' @examples
    #' block <- teal.reporter:::PictureBlock$new()
    #' block$get_dim()
    #'
    get_dim = function() {
      private$dim
    }
  ),
  private = list(
    content = character(0),
    type = character(0),
    title = "",
    dim = c(100, 100)
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
