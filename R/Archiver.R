#' @title `Archiver`
#' @keywords internal
Archiver <- R6::R6Class( # nolint: object_name_linter.
  classname = "Archiver",
  public = list(
    #' @description Returns an `Archiver` object.
    #'
    #' @return an `Archiver` object
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
    #'
    #' @return invisibly self
    write = function(reporter) {
      checkmate::assert_class(reporter, "Reporter")
      unlink(list.files(private$output_dir, recursive = TRUE, full.names = TRUE))
      reporter$to_jsondir(private$output_dir)
      self
    },
    #' @description read a `Reporter` instance from a directory with `JSONArchiver`.
    #'
    #' @param path `character(1)` a path to the directory with all proper files.
    #'
    #' @return `Reporter` instance.
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
