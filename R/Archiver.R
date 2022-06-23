#' @title `Archiver`
#' @keywords internal
Archiver <- R6::R6Class( # nolint: object_name_linter.
  classname = "Archiver",
  public = list(
    #' @description Returns a `Archiver` object.
    #'
    #' @return a `Archiver` object
    #' @examples
    #' archiver <- teal.reporter:::Archiver$new()
    #'
    initialize = function() {
      invisible(self)
    },
    #' @description Finalizes an `Archiver` object.
    finalize = function() {
      # destructor
    },
    #' @description Pure virtual method for reading an `Archiver`.
    read = function() {
      # returns Reporter instance
      stop("Pure virtual method.")
    },
    #' @description Pure virtual method for writing an `Archiver`.
    write = function() {
      stop("Pure virtual method.")
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)

#' @title `RDSArchiver`
#' @keywords internal
FileArchiver <- R6::R6Class( # nolint: object_name_linter.
  classname = "FileArchiver",
  inherit = Archiver,
  public = list(
    #' @description Returns a `FileArchiver` object.
    #'
    #' @return a `FileArchiver` object
    #' @examples
    #' archiver <- teal.reporter:::FileArchiver$new()
    #'
    initialize = function() {
      tmp_dir <- tempdir()
      output_dir <- file.path(tmp_dir, sprintf("archive_%s", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS4"))))
      dir.create(path = output_dir)
      private$output_dir <- output_dir
      invisible(self)
    },
    #' @description Finalizes a `FileArchiver` object.
    finalize = function() {
      unlink(private$output_dir, recursive = TRUE)
    },
    #' @description get `output_dir` field
    #'
    #' @return `character` a `output_dir` field path.
    #' @examples
    #' archiver <- teal.reporter:::FileArchiver$new()
    #' archiver$get_output_dir()
    get_output_dir = function() {
      private$output_dir
    }
  ),
  private = list(
    output_dir = character(0)
  )
)

#' @title `JSONArchiver`
#' @keywords internal
JSONArchiver <- R6::R6Class( # nolint: object_name_linter.
  classname = "JSONArchiver",
  inherit = FileArchiver,
  public = list(
    #' @description write a `Reporter` instance in to this `JSONArchiver` object.
    #'
    #' @param reporter `Reporter` instance.
    #' @return invisibly self
    #' @examples
    #' card1 <- teal.reporter:::ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text", "header2")
    #' card1$append_plot(
    #'  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #'
    #' reporter <- teal.reporter:::Reporter$new()
    #' reporter$append_cards(list(card1))
    #'
    #' archiver <- teal.reporter:::JSONArchiver$new()
    #' archiver$write(reporter)
    #' archiver$get_output_dir()
    write = function(reporter) {
      checkmate::assert_class(reporter, "Reporter")
      unlink(list.files(private$output_dir, recursive = TRUE, full.names = TRUE))
      private$to_jsondir(reporter, private$output_dir)
      return(self)
    },
    #' @description read a `Reporter` instance from a directory with `JSONArchiver`.
    #'
    #' @param path `character(1)` a path to the directory with all proper files.
    #' @return `Reporter` instance.
    #' @examples
    #' card1 <- teal.reporter:::ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text", "header2")
    #' card1$append_plot(
    #'  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #'
    #' reporter <- teal.reporter:::Reporter$new()
    #' reporter$append_cards(list(card1))
    #'
    #' archiver <- teal.reporter:::JSONArchiver$new()
    #' archiver$write(reporter)
    #' archiver$get_output_dir()
    #'
    #' archiver$read()$get_cards()[[1]]$get_content()
    #' blocks <- teal.reporter:::Reporter$new()$from_reporter(archiver$read())$get_blocks()
    #' doc <- teal.reporter:::Renderer$new()$render(blocks)
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
        private$from_jsondir(private$output_dir)
      } else {
        warning("The directory provided to the Archiver is empty.")
        Reporter$new()
      }
    }
  ),
  private = list(
    to_jsondir = function(reporter, output_dir) {
      checkmate::assert_directory_exists(output_dir)

      json <- reporter$to_list(output_dir)

      cat(jsonlite::toJSON(json, auto_unbox = TRUE, force = TRUE),
        file = file.path(output_dir, "Report.json")
      )
      output_dir
    },
    from_jsondir = function(output_dir) {
      checkmate::assert_directory_exists(output_dir)
      checkmate::assert_true(length(list.files(output_dir)) > 0)
      dir_files <- list.files(output_dir)
      which_json <- grep("json$", dir_files)
      json <- jsonlite::read_json(file.path(output_dir, dir_files[which_json]))
      Reporter$new()$from_list(json, output_dir)
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
