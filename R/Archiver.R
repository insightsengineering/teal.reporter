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
      private$content <- list()
      invisible(self)
    },
    #' @description Finalizes an `Archiver` object.
    finalize = function() {},
    read = function() {
      # returns reporter
    },
    write = function() {}
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)

#' @title `JSONArchiver`
#' @keywords internal
JSONArchiver <- R6::R6Class( # nolint: object_name_linter.
  classname = "JSONArchiver",
  inherit = Archiver,
  public = list(
    #' @description Returns a `JSONArchiver` object.
    #'
    #' @return a `JSONArchiver` object
    #' @examples
    #' archiver <- teal.reporter:::JSONArchiver$new()
    #'
    initialize = function(version) {
      tmp_dir <- tempdir()
      output_dir <- file.path(tmp_dir, sprintf("archive_%s", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS4"))))
      dir.create(path = output_dir)
      private$output_dir <- output_dir
      invisible(self)
    },
    #' @description Finalizes a `Renderer` object.
    finalize = function() {
      unlink(private$output_dir, recursive = TRUE)
    },
    write = function(reporter) {
      checkmate::assert_class(reporter, "Reporter")
      private$reporter2dir(reporter, private$output_dir)
      return(self)
    },
    read = function(path2zip) {
      checkmate::assert(
        checkmate::assert_file_exists(path2zip),
        checkmate::assert_true(missing(path2zip))
      )
      if (!missing(path2zip)) {
        unlink(private$output_dir, recursive = TRUE)
        zip::unzip(path2zip, exdir = private$output_dir)
        private$reporter <- private$dir2reporter(private$output_dir)
      }
      private$reporter
    }
  ),
  private = list(
    output_dir = character(0),
    reporter = Reporter$new(),
    reporter2dir = function(reporter) {
      json <- list()
      for (card in reporter$get_cards()) {
        for (block in card$get_content()) {
          block_class <- class(block)[1]

        }
      }
    },
    dir2reporter = function(dir) {
      dir_files <- list.files(dir)
      which_json <- grep("json$", dir_files)
      json <- jsonlite::read_json(dir_files[which_json])
      for (card in json$cards) {

      }
      reporter <- Reporter$new()
      reporter$append_cards(cards)
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
