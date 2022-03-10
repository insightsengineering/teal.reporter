#' @title `Renderer`
#' @keywords internal
Renderer <- R6::R6Class( # nolint: object_name_linter.
  classname = "Renderer",
  public = list(
    #' @description Returns a `Renderer` object.
    #'
    #' @details Returns a `Renderer` object.
    #'
    #' @return `Renderer` object.
    #' @examples
    #' renderer <- teal.reporter:::Renderer$new()
    initialize = function() {
      tmp_dir <- tempdir()
      output_dir <- file.path(tmp_dir, sprintf("report_%s", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS4"))))
      dir.create(path = output_dir)
      private$output_dir <- output_dir
      invisible(self)
    },
    #' @description finalize a `Renderer` object.
    finalize = function() {
      unlink(private$output_dir, recursive = TRUE)
    },
    #' @description getting the `Rmd` text which could be easily rendered later.
    #'
    #' @param blocks `list` of `c("TextBlock", "PictureBlock", "NewpageBlock")` objects.
    #' @param yaml_header `character` a `rmarkdown` `yaml` header.
    #' @return `character` a `Rmd` text (`yaml` header + body), ready to be rendered.
    #' @examples
    #' yaml_l <- list(
    #'   author = teal.reporter:::yaml_quoted("NEST"),
    #'   title = teal.reporter:::yaml_quoted("Report"),
    #'   date = teal.reporter:::yaml_quoted("07/04/2019"),
    #'   output = list(pdf_document = list(keep_tex = TRUE))
    #' )
    #' yaml_header <- teal.reporter:::md_header(yaml::as.yaml(yaml_l))
    #' # Card <- Card$new()
    #' # blocks <- Card$blocks
    #' text_block1 <- teal.reporter:::TextBlock$new()$set_content("text")$set_style("header2")
    #' text_block2 <- teal.reporter:::TextBlock$new()$set_content("text")
    #' text_block3 <- teal.reporter:::TextBlock$new()$set_content("tables")$set_style("header2")
    #' img_path <- system.file("img", "Rlogo.png", package = "png")
    #' picture_block <- teal.reporter:::PictureBlock$new()$set_content(img_path)
    #' table_block1 <- teal.reporter:::TableBlock$new()
    #' table_path1 <- tempfile(fileext = ".csv")
    #' write.csv(head(airquality), table_path1)
    #' table_block1$set_content(table_path1)
    #' table_block2 <- teal.reporter:::TableBlock$new()
    #' table_path2 <- tempfile(fileext = ".rds")
    #' lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- rtables::build_table(lyt, airquality)
    #' saveRDS(table_res2, table_path2)
    #' table_block2$set_content(table_path2)
    #' newpage_block <- teal.reporter:::NewpageBlock$new()
    #' blocks <- list(
    #'   text_block1, text_block2, picture_block, newpage_block,
    #'   text_block3, table_block1, table_block2, newpage_block
    #' )
    #' renderer <- teal.reporter:::Renderer$new()
    #' rmd_path <- renderer$renderRmd(blocks, yaml_header)
    renderRmd = function(blocks, yaml_header) {
      checkmate::assert_list(blocks, c("TextBlock", "PictureBlock", "NewpageBlock", "TableBlock"))
      if (missing(yaml_header)) {
        yaml_header <- md_header(yaml::as.yaml(list(title = "Report")))
      }

      parsed_yaml <- yaml_header
      parsed_blocks <- paste(unlist(lapply(blocks, function(b) private$block2md(b))), collapse = "\n\n")

      rmd_text <- paste0(parsed_yaml, "\n", parsed_blocks)
      tmp <- tempfile(fileext = ".Rmd")
      input_path <- file.path(
        private$output_dir,
        sprintf("input_%s.Rmd", gsub("[.]", "", format(Sys.time(), "%Y%m%d%H%M%OS3")))
      )
      cat(rmd_text, file = input_path)
      input_path
    },
    #' @description render blocks and `yaml` header to the output file
    #'
    #' @param blocks `list` of `c("TextBlock", "PictureBlock", "NewpageBlock")` objects.
    #' @param yaml_header `character` a `rmarkdown` `yaml` header.
    #' @param ... `rmarkdown::render` arguments, `input` and `output_dir` should not be updated.
    #' @return `character` path to the output
    #' @examples
    #' yaml_l <- list(
    #'   author = teal.reporter:::yaml_quoted("NEST"),
    #'   title = teal.reporter:::yaml_quoted("Report"),
    #'   date = teal.reporter:::yaml_quoted("07/04/2019"),
    #'   output = list(pdf_document = list(keep_tex = TRUE))
    #' )
    #' yaml_header <- teal.reporter:::md_header(yaml::as.yaml(yaml_l))
    #' # Card <- Card$new()
    #' # blocks <- Card$blocks
    #' text_block1 <- teal.reporter:::TextBlock$new()$set_content("text")$set_style("header2")
    #' text_block2 <- teal.reporter:::TextBlock$new()$set_content("text")
    #' text_block3 <- teal.reporter:::TextBlock$new()$set_content("tables")$set_style("header2")
    #' img_path <- system.file("img", "Rlogo.png", package = "png")
    #' picture_block <- teal.reporter:::PictureBlock$new()$set_content(img_path)
    #' table_block1 <- teal.reporter:::TableBlock$new()
    #' table_path1 <- tempfile(fileext = ".csv")
    #' write.csv(head(airquality), table_path1)
    #' table_block1$set_content(table_path1)
    #' table_block2 <- teal.reporter:::TableBlock$new()
    #' table_path2 <- tempfile(fileext = ".rds")
    #' lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- rtables::build_table(lyt, airquality)
    #' saveRDS(table_res2, table_path2)
    #' table_block2$set_content(table_path2)
    #' newpage_block <- teal.reporter:::NewpageBlock$new()
    #' blocks <- list(
    #'   text_block1, text_block2, picture_block, newpage_block,
    #'   text_block3, table_block1, table_block2, newpage_block
    #' )
    #' renderer <- teal.reporter:::Renderer$new()
    #' result_path <- renderer$render(blocks, yaml_header)
    render = function(blocks, yaml_header, ...) {
      args <- list(...)
      input_path <- self$renderRmd(blocks, yaml_header)
      args <- append(args, list(input = input_path, output_dir = private$output_dir, quiet = TRUE))
      args_nams <- unique(names(args))
      args <- lapply(args_nams, function(x) args[[x]])
      names(args) <- args_nams
      do.call(rmarkdown::render, args)
    },
    #' @description get `output_dir` field
    #'
    #' @return `character` a `output_dir` field path.
    #' @examples
    #' renderer <- teal.reporter:::Renderer$new()
    #' renderer$get_output_dir()
    get_output_dir = function() {
      private$output_dir
    }
  ),
  private = list(
    output_dir = character(0),
    textBlock2md = function(block) {
      text_style <- block$get_style()
      block_content <- block$get_content()
      switch(text_style,
        "default" = block_content,
        "verbatim" = paste0("\n```\n", block_content, "\n```\n"),
        "header2" = paste0("## ", block_content),
        "header3" = paste0("### ", block_content),
        block_content
      )
    },
    pictureBlock2md = function(block) {
      basename_pic <- basename(block$get_content())
      file.copy(block$get_content(), file.path(private$output_dir, basename_pic))
      title <- block$get_title()
      img_type <- tolower(block$get_type())
      if (img_type %in% c("png", "jpg", "jpeg")) {
        sprintf("![%s](%s){width=%s, height=%s}", title, basename_pic, block$get_dim()[1], block$get_dim()[2]) # nolint
      } else if (img_type %in% c("rds")) {
        glue::glue("```{r echo = FALSE}\nreadRDS('<<basename_pic>>')\n```", .open = "<<", .close = ">>")
      } else {
        NULL
      }
    },
    tableBlock2md = function(block) {
      basename_table <- basename(block$get_content())
      file.copy(block$get_content(), file.path(private$output_dir, basename_table))
      read_call <- switch(tolower(block$get_type()),
        "csv" = sprintf("read.csv('%s')", basename_table),
        "rds" = sprintf("readRDS('%s')", basename_table),
        sprintf("readLines('%s')", basename_table)
      )
      glue::glue("```{r echo = FALSE}\n<<read_call>>\n```", .open = "<<", .close = ">>")
    },
    block2md = function(block) {
      block_class <- class(block)[1]
      switch(block_class,
        TextBlock = private$textBlock2md(block),
        PictureBlock = private$pictureBlock2md(block),
        TableBlock = private$tableBlock2md(block),
        NewpageBlock = block$get_content(),
        ""
      )
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
