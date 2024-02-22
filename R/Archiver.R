#' @title `Archiver`: Base class for data archiving
#' @docType class
#' @description
#' A base `R6` class for implementing data archiving functionality.
#'
#' @keywords internal
Archiver <- R6::R6Class( # nolint: object_name_linter.
  classname = "Archiver",
  public = list(
    #' @description Initialize an `Archiver` object.
    #'
    #' @return Object of class `Archiver`, invisibly.
    #' @examples
    #' Archiver <- getFromNamespace("Archiver", "teal.reporter")
    #' Archiver$new()
    initialize = function() {
      invisible(self)
    },
    #' @description Finalizes an `Archiver` object.
    finalize = function() {
      # destructor
    },
    #' @description Reads data from the `Archiver`.
    #' Pure virtual method that should be implemented by inherited classes.
    read = function() {
      # returns Reporter instance
      stop("Pure virtual method.")
    },
    #' @description Writes data to the `Archiver`.
    #' Pure virtual method that should be implemented by inherited classes.
    write = function() {
      stop("Pure virtual method.")
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)

#' @title `FileArchiver`: A File-based `Archiver`
#' @docType class
#' @description
#' Inherits from `Archiver` to provide file-based archiving functionality.
#' Manages an output directory for storing archived data.
#'
#' @keywords internal
FileArchiver <- R6::R6Class( # nolint: object_name_linter.
  classname = "FileArchiver",
  inherit = Archiver,
  public = list(
    #' @description Initialize a `FileArchiver` object with a unique output directory.
    #'
    #' @return Object of class `FileArchiver`, invisibly.
    #' @examples
    #' FileArchiver <- getFromNamespace("FileArchiver", "teal.reporter")
    #' FileArchiver$new()
    initialize = function() {
      tmp_dir <- tempdir()
      output_dir <- file.path(tmp_dir, sprintf("archive_%s", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS4"))))
      dir.create(path = output_dir)
      private$output_dir <- output_dir
      invisible(self)
    },
    #' @description Finalizes a `FileArchiver` object.
    #' Cleans up by removing the output directory and its contents.
    finalize = function() {
      unlink(private$output_dir, recursive = TRUE)
    },
    #' @description Get `output_dir` field.
    #'
    #' @return `character` a `output_dir` field path.
    #' @examples
    #' FileArchiver <- getFromNamespace("FileArchiver", "teal.reporter")
    #' FileArchiver$new()$get_output_dir()
    get_output_dir = function() {
      private$output_dir
    }
  ),
  private = list(
    output_dir = character(0)
  )
)

#' @title `JSONArchiver`: A `JSON`-based `Archiver`
#' @docType class
#' @description
#' Inherits from `FileArchiver` to implement `JSON`-based archiving functionality.
#' Convert `Reporter` instances to and from `JSON` format.
#'
#' @keywords internal
JSONArchiver <- R6::R6Class( # nolint: object_name_linter.
  classname = "JSONArchiver",
  inherit = FileArchiver,
  public = list(
    #' @description Write a `Reporter` instance in `JSON` file.
    #' Serializes a given `Reporter` instance and saves it in the `Archiver`'s output directory,
    #' to this `JSONArchiver` object.
    #'
    #' @param reporter (`Reporter`) instance.
    #'
    #' @return `self`.
    #' @examples
    #' library(ggplot2)
    #'
    #' ReportCard <- getFromNamespace("ReportCard", "teal.reporter")
    #' card1 <- ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text", "header2")
    #' card1$append_plot(
    #'  ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )
    #'
    #' Reporter <- getFromNamespace("Reporter", "teal.reporter")
    #' reporter <- Reporter$new()
    #' reporter$append_cards(list(card1))
    #'
    #' JSONArchiver <- getFromNamespace("JSONArchiver", "teal.reporter")
    #' archiver <- JSONArchiver$new()
    #' archiver$write(reporter)
    #' archiver$get_output_dir()
    write = function(reporter) {
      checkmate::assert_class(reporter, "Reporter")
      unlink(list.files(private$output_dir, recursive = TRUE, full.names = TRUE))
      reporter$to_jsondir(private$output_dir)
      self
    },
    #' @description Read a `Reporter` instance from a `JSON` file.
    #' Converts a `Reporter` instance from the `JSON` file in the `JSONArchiver`'s output directory.
    #'
    #' @param path (`character(1)`) a path to the directory with all proper files.
    #'
    #' @return `Reporter` instance.
    #' @examples
    #' library(ggplot2)
    #'
    #' ReportCard <- getFromNamespace("ReportCard", "teal.reporter")
    #' card1 <- ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text", "header2")
    #' card1$append_plot(
    #'  ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )
    #'
    #' Reporter <- getFromNamespace("Reporter", "teal.reporter")
    #' reporter <- Reporter$new()
    #' reporter$append_cards(list(card1))
    #'
    #' JSONArchiver <- getFromNamespace("JSONArchiver", "teal.reporter")
    #' archiver <- JSONArchiver$new()
    #' archiver$write(reporter)
    #' archiver$get_output_dir()
    #'
    #' archiver$read()$get_cards()[[1]]$get_content()
    #' Reporter <- getFromNamespace("Reporter", "teal.reporter")
    #' blocks <- Reporter$new()
    #' blocks <- blocks$from_reporter(archiver$read())$get_blocks()
    #' Renderer <- getFromNamespace("Renderer", "teal.reporter")
    #' doc <- Renderer$new()$render(blocks)
    read = function(path = NULL) {
      checkmate::assert(
        checkmate::check_null(path),
        checkmate::check_directory_exists(path)
      )

      if (!is.null(path) && !identical(path, private$output_dir)) {
        unlink(list.files(private$output_dir, recursive = TRUE, full.names = TRUE))
        file.copy(list.files(path, full.names = TRUE), private$output_dir)
      }

      if (length(list.files(private$output_dir))) {
        Reporter$new()$from_jsondir(private$output_dir)
      } else {
        warning("The directory provided to the Archiver is empty.")
        Reporter$new()
      }
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
