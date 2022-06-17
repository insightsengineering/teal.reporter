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
    finalize = function() {},
    read = function() {
      # returns Reporter instance
    },
    write = function() {}
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)

#' @title `RDSArchiver`
#' @keywords internal
FileArchiver <- R6::R6Class( # nolint: object_name_linter.
  classname = "RDSArchiver",
  inherit = Archiver,
  public = list(
    #' @description Returns a `FileArchiver` object.
    #'
    #' @return a `FileArchiver` object
    #' @examples
    #' archiver <- teal.reporter:::FileArchiver$new()
    #'
    initialize = function(version) {
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
    #' @param reporter
    #' @param report_params
    #' @param datasets
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
    #' card2 <- teal.reporter:::ReportCard$new()
    #'
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text", "header2")
    #' lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- rtables::build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #' card2$append_table(iris)
    #'
    #' reporter <- teal.reporter:::Reporter$new()
    #' reporter$append_cards(list(card1, card2))
    #'
    #' archiver <- JSONArchiver$new()
    #' archiver$write(reporter)
    #'
    #' archiver$read()$get_cards()[[1]]$get_content()
    #' archiver$get_output_dir()
    #' zip::zipr("../test.zip", list.files(archiver$get_output_dir(), full.names = TRUE))
    #' archiver$read("../test.zip")$get_cards()[[1]]$get_content()[[3]]$get_content()
    write = function(reporter) {
      checkmate::assert_class(reporter, "Reporter")
      unlink(list.files(private$output_dir, recursive = TRUE, full.names = TRUE))
      reporter$to_jsondir(private$output_dir)
      return(self)
    },
    #' @examples
    #' card1 <- teal.reporter:::ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text", "header2")
    #' card1$append_plot(
    #'  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #'
    #' card2 <- teal.reporter:::ReportCard$new()
    #'
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text", "header2")
    #' lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- rtables::build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #' card2$append_table(iris)
    #'
    #' reporter <- teal.reporter:::Reporter$new()
    #' reporter$append_cards(list(card1, card2))
    #'
    #' archiver <- JSONArchiver$new()
    #' archiver$write(reporter)
    #'
    #' archiver$read()$get_cards()[[1]]$get_content()
    #' archiver$get_output_dir()
    #' zip::zipr("../test.zip", list.files(archiver$get_output_dir(), full.names = TRUE))
    #' archiver$read("../test.zip")$get_cards()[[1]]$get_content()[[3]]$get_content()
    read = function(path2zip = NULL) {
      checkmate::assert(
        checkmate::check_null(path2zip),
        checkmate::check_file_exists(path2zip, extension = "zip")
      )
      if (!is.null(path2zip)) {
        unlink(list.files(private$output_dir, recursive = TRUE, full.names = TRUE))
        zip::unzip(path2zip, exdir = private$output_dir)
      }
      Reporter$new()$from_jsondir(private$output_dir)
    }
  ),
  private = list(
    output_dir = character(0)
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
