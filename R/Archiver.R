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
    write = function(reporter, report_params = list(), datasets = NULL, version = "1") {
      checkmate::assert_class(reporter, "Reporter")
      unlink(list.files(private$output_dir, recursive = TRUE, full.names = TRUE))
      private$reporter2dir(reporter, private$output_dir, version)
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
      private$dir2reporter(private$output_dir)
    }
  ),
  private = list(
    output_dir = character(0),
    reporter2dir = function(reporter, output_dir, version) {
      if (version == "1") {
        json <- list(version = version, cards = list())
        json[["metadata"]] <- reporter$get_metadata()
        for (card in reporter$get_cards()) {
          card_class <- class(card)[1]
          new_blocks <- list()
          for (block in card$get_content()) {
            block_class <- class(block)[1]
            cblock <- switch(block_class,
                             TextBlock = block$to_list(),
                             PictureBlock = {
                               file.copy(block$get_content(), private$output_dir)
                               block$to_list()
                             },
                             TableBlock = {
                               file.copy(block$get_content(), private$output_dir)
                               block$to_list()
                             },
                             NewpageBlock = list(),
                             NULL
            )
            new_block <- list()
            new_block[[block_class]] <- cblock
            new_blocks <- c(new_blocks, new_block)
          }
          new_card <- list()
          new_card[["blocks"]] <- new_blocks
          new_card[["metadata"]] <- card$get_metadata()

          u_card <- list()
          u_card[[card_class]] <- new_card
          json$cards <- c(json$cards, u_card)
        }
      } else {
        stop("The provided version is not supported, Archiver.")
      }

      cat(jsonlite::toJSON(json, auto_unbox=TRUE, force = TRUE),
          file = file.path(private$output_dir, "Report.json"))
      private$output_dir
    },
    dir2reporter = function(dir) {
      dir_files <- list.files(dir)
      which_json <- grep("json$", dir_files)
      json <- jsonlite::read_json(file.path(private$output_dir, dir_files[which_json]))
      if (json$version == "1") {
        new_cards <- list()
        cards_names <- names(json$cards)
        cards_names <- gsub("[.][0-9]*$","", cards_names)
        for (iter_c in seq_along(json$cards)) {
          card_class <- cards_names[iter_c]
          new_card <- switch(card_class,
                             ReportCard = ReportCard$new(),
                             TealReportCard = TealReportCard$new()
          )
          blocks <- json$cards[[iter_c]]$blocks
          metadata <- json$cards[[iter_c]]$metadata
          blocks_names <- names(blocks)
          blocks_names <- gsub("[.][0-9]*$","", blocks_names)
          for (iter_b in seq_along(blocks)) {
            block_class <- blocks_names[iter_b]
            block <- blocks[[iter_b]]
            cblock <- switch(block_class,
                   TextBlock = TextBlock$new()$from_list(block),
                   PictureBlock = PictureBlock$new()$from_list(block, private$output_dir),
                   TableBlock = TableBlock$new()$from_list(block, private$output_dir),
                   NewpageBlock = NewPageBlock$new(),
                   NULL
            )
            new_card$append_content(cblock)
          }
          for (meta in names(metadata)) {
            new_card$append_metadata(meta, metadata[[meta]])
          }
          new_cards <- c(new_cards, new_card)
        }
      } else {
        stop("The provided version is not supported, Archiver.")
      }
      reporter <- Reporter$new()
      reporter$append_cards(new_cards)
      reporter$append_metadata(json$metadata)
      reporter
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
