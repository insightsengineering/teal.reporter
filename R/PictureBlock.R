#' @title `PictureBlock`
#' @keywords internal
#'
PictureBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "PictureBlock",
  inherit = ContentBlock,
  public = list(
    #' @description Returns a new `PictureBlock` object.
    #'
    #' @param plot (`ggplot`, `grid`) a picture in this `PictureBlock`
    #' @return a `PictureBlock` object
    #'
    initialize = function(plot) {
      if (!missing(plot)) {
        self$set_content(plot)
      }
      invisible(self)
    },
    #' @description Sets the content of this `PictureBlock`.
    #'
    #' @details throws if argument is not a `ggplot`, `grob` or `trellis` plot.
    #'
    #' @param content (`ggplot`, `grob`, `trellis`) a picture in this `PictureBlock`
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::PictureBlock$new()
    #' block$set_content(ggplot2::ggplot(iris))
    #'
    #' block <- teal.reporter:::PictureBlock$new()
    #' block$set_content(lattice::bwplot(1))
    #'
    #' block <- teal.reporter:::PictureBlock$new()
    #' block$set_content(ggplot2::ggplotGrob(ggplot2::ggplot(iris)))
    set_content = function(content) {
      checkmate::assert_multi_class(content, private$supported_plots)
      path <- tempfile(fileext = ".png")
      grDevices::png(filename = path, width = private$dim[1], height = private$dim[2])
      tryCatch(
        expr = {
          if (inherits(content, "grob")) {
            grid::grid.newpage()
            grid::grid.draw(content)
          } else if (inherits(content, "gg")) {
            print(content)
          } else if (inherits(content, "trellis")) {
            grid::grid.newpage()
            grid::grid.grabExpr(print(content), warn = 0, wrap.grobs = TRUE)
          }
          super$set_content(path)
        },
        finally = grDevices::dev.off()
      )
      invisible(self)
    },
    #' @description Finalizes this `PictureBlock`.
    #'
    #' @details Removes the temporary file created in the constructor.
    #'
    finalize = function() {
      try(unlink(super$get_content()))
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
    #' @description Sets the dimensions of this `PictureBlock`
    #'
    #' @param dim `numeric` figure dimensions (width and height) in pixels, length 2.
    #' @return `self`
    #' @examples
    #' block <- teal.reporter:::PictureBlock$new()
    #' block$set_dim(c(800, 600))
    #'
    set_dim = function(dim) {
      checkmate::assert_numeric(dim, len = 2)
      private$dim <- dim
      invisible(self)
    },
    #' @description Returns the dimensions of this `PictureBlock`
    #'
    #' @return `numeric` the array of 2 numeric values representing width and height in pixels.
    #' @examples
    #' block <- teal.reporter:::PictureBlock$new()
    #' block$get_dim()
    #'
    get_dim = function() {
      private$dim
    },
    #' @description Create the `TableBlock` from a list.
    #' The list should contain one named field, `"path"`.
    #' @param x `named list` with one field `"path"`, a path to the `RDS` file.
    #' @param base_path `character` with a path to the dir with the file.
    #' If the argument is `NULL` then only `x` is used. Default `NULL`.
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::PictureBlock$new()
    #' block$from_list(list(path = "file.png"))
    from_list = function(x, base_path = NULL) {
      checkmate::assert_list(x)
      checkmate::assert_names(names(x), must.include = "path")
      path <- if (!is.null(base_path)) {
        file.path(base_path, basename(x$path))
      } else {
        basename(x$path)
      }
      super$set_content(path)
      invisible(self)
    },
    #' @description Convert the `PictureBlock` to a list.
    #' @param base_path `character` with a path to the dir with the file.
    #' If the argument is `NULL` then only basename is returned. Default `.`.
    #' @return `named list` with a path to the file.
    #' @examples
    #' block <- teal.reporter:::PictureBlock$new()
    #' block$to_list()
    #' block$to_list("/path/sth")
    to_list = function(base_path = ".") {
      path <- if (!is.null(base_path)) {
        file.path(base_path, basename(super$get_content()))
      } else {
        basename(super$get_content())
      }
      list(path = path)
    }
  ),
  private = list(
    supported_plots = c("ggplot", "grob", "trellis"),
    type = character(0),
    title = "",
    dim = c(800, 600)
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
