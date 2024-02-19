#' @title `PictureBlock`
#' @docType class
#' @description
#' Specialized `FileBlock` for managing picture content in reports.
#' It's designed to handle plots from packages such as `ggplot2`, `grid`, or `lattice`.
#' It can save plots to files, set titles and specify dimensions.
#'
#' @keywords internal
PictureBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "PictureBlock",
  inherit = FileBlock,
  public = list(
    #' @description Initialize a `PictureBlock` object.
    #'
    #' @param plot (`ggplot` or `grid`) a picture in this `PictureBlock`
    #'
    #' @return Object of class `PictureBlock`, invisibly.
    initialize = function(plot) {
      if (!missing(plot)) {
        self$set_content(plot)
      }
      invisible(self)
    },
    #' @description Sets the content of this `PictureBlock`.
    #'
    #' @details Throws if argument is not a `ggplot`, `grob` or `trellis` plot.
    #'
    #' @param content (`ggplot` or `grob` or `trellis`) a picture in this `PictureBlock`
    #'
    #' @return `self`, invisibly.
    #' @examples
    #' library(ggplot2)
    #' library(lattice)
    #'
    #' PictureBlock <- getFromNamespace("PictureBlock", "teal.reporter")
    #' block <- PictureBlock$new()
    #' block$set_content(ggplot(iris))
    #'
    #' PictureBlock <- getFromNamespace("PictureBlock", "teal.reporter")
    #' block <- PictureBlock$new()
    #' block$set_content(bwplot(1))
    #'
    #' PictureBlock <- getFromNamespace("PictureBlock", "teal.reporter")
    #' block <- PictureBlock$new()
    #' block$set_content(ggplotGrob(ggplot(iris)))
    set_content = function(content) {
      checkmate::assert_multi_class(content, private$supported_plots)
      path <- tempfile(fileext = ".png")
      grDevices::png(filename = path, width = private$dim[1], height = private$dim[2])
      tryCatch(
        expr = {
          if (inherits(content, "grob")) {
            grid::grid.newpage()
            grid::grid.draw(content)
          } else if (inherits(content, c("gg", "Heatmap"))) { # "Heatmap" S4 from ComplexHeatmap
            print(content)
          } else if (inherits(content, "trellis")) {
            grid::grid.newpage()
            grid::grid.draw(grid::grid.grabExpr(print(content), warn = 0, wrap.grobs = TRUE))
          }
          super$set_content(path)
        },
        finally = grDevices::dev.off()
      )
      invisible(self)
    },
    #' @description Sets the title of this `PictureBlock`.
    #'
    #' @details Throws if argument is not `character(1)`.
    #'
    #' @param title (`character(1)`) a string assigned to this `PictureBlock`
    #'
    #' @return `self`, invisibly.
    #' @examples
    #' PictureBlock <- getFromNamespace("PictureBlock", "teal.reporter")
    #' block <- PictureBlock$new()
    #' block$set_title("Title")
    #'
    set_title = function(title) {
      checkmate::assert_string(title)
      private$title <- title
      invisible(self)
    },
    #' @description Get the title of this `PictureBlock`.
    #'
    #' @return The content of this `PictureBlock`.
    #' @examples
    #' PictureBlock <- getFromNamespace("PictureBlock", "teal.reporter")
    #' block <- PictureBlock$new()
    #' block$get_title()
    #'
    get_title = function() {
      private$title
    },
    #' @description Sets the dimensions of this `PictureBlock`.
    #'
    #' @param dim (`numeric(2)`) figure dimensions (width and height) in pixels, length 2.
    #'
    #' @return `self`, invisibly.
    #' @examples
    #' PictureBlock <- getFromNamespace("PictureBlock", "teal.reporter")
    #' block <- PictureBlock$new()
    #' block$set_dim(c(800, 600))
    #'
    set_dim = function(dim) {
      checkmate::assert_numeric(dim, len = 2)
      private$dim <- dim
      invisible(self)
    },
    #' @description Get `PictureBlock` dimensions as a numeric vector.
    #'
    #' @return `numeric` the array of 2 numeric values representing width and height in pixels.
    #' @examples
    #' PictureBlock <- getFromNamespace("PictureBlock", "teal.reporter")
    #' block <- PictureBlock$new()
    #' block$get_dim()
    get_dim = function() {
      private$dim
    }
  ),
  private = list(
    supported_plots = c("ggplot", "grob", "trellis", "Heatmap"),
    type = character(0),
    title = "",
    dim = c(800, 600)
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
