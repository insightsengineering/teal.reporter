#' @title `TealReportCard`
#' @description `r lifecycle::badge("experimental")`
#' A child of [`ReportCard`] that is used for teal specific applications.
#' In addition to the parent methods, it supports rendering teal specific elements such as
#' the source code, the encodings panel content and the filter panel content as part of the
#' meta data.
#' @export
#'
TealReportCard <- R6::R6Class( # nolint: object_name_linter.
  classname = "TealReportCard",
  inherit = ReportCard,
  public = list(
    #' @description Appends the source code to the `content` meta data of this `TealReportCard`.
    #'
    #' @param src (`character(1)`) code as text.
    #' @param ... any `rmarkdown` R chunk parameter and its value.
    #' But `eval` parameter is always set to `FALSE`.
    #' @return invisibly self
    #' @examples
    #' card <- TealReportCard$new()$append_src(
    #'   "ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()"
    #' )
    #' card$get_content()[[1]]$get_content()
    append_src = function(src, ...) {
      checkmate::assert_character(src, min.len = 0, max.len = 1)
      params <- list(...)
      params$eval <- FALSE
      rblock <- RcodeBlock$new(src)
      rblock$set_params(params)
      self$append_content(rblock)
      self$append_metadata("SRC", src)
      invisible(self)
    },
    #' @description Appends the filter state list to the `content` and `metadata` of this `TealReportCard`.
    #'  If the filter state list has an attribute named `formatted`, it appends it to the card otherwise it uses
    #'  the default `yaml::as.yaml` to format the list.
    #'  If the filter state list is empty, nothing is appended to the `content`.
    #'
    #' @param fs (`list`) of filter states.
    #' @return invisibly self
    #' @examples
    #' card <- TealReportCard$new()$append_fs(list(a = 1, b = 2))
    #' card$get_content()[[1]]$get_content()
    #'
    append_fs = function(fs) {
      # todo: whole class should be moved to teal
      checkmate::assert_class(fs, "teal_slices")
      if (length(fs) != 0) {
        states_list <- lapply(fs, function(x) {
          x_list <- shiny::isolate(shiny::reactiveValuesToList(x))
          if (
            inherits(x_list$choices, c("integer", "numeric", "Date", "POSIXct", "POSIXlt")) &&
              length(x_list$choices) == 2 &&
              length(x_list$selected) == 2
          ) {
            x_list$range <- paste(x_list$selected, collapse = " - ")
            x_list["selected"] <- NULL
          }
          if (!is.null(x$arg)) {
            x_list$arg <- if (x$arg == "subset") "Genes" else "Samples"
          }

          x_list <- x_list[
            c("dataname", "varname", "experiment", "arg", "expr", "selected", "range", "keep_na", "keep_inf")
          ]
          names(x_list) <- c(
            "Dataset name", "Variable name", "Experiment", "Filtering by", "Applied expression",
            "Selected Values", "Selected range", "Include NA values", "Include Inf values"
          )

          Filter(Negate(is.null), x_list)
        })

        self$append_text("Filter State", "header3")
        self$append_text(yaml::as.yaml(states_list), "verbatim")
      }
      self$append_metadata("FS", fs)
      invisible(self)
    },
    #' @description Appends the encodings list to the `content` and `metadata` of this `TealReportCard`.
    #'
    #' @param encodings (`list`) list of encodings selections of the teal app.
    #' @return invisibly self
    #' @examples
    #' card <- TealReportCard$new()$append_encodings(list(variable1 = "X"))
    #' card$get_content()[[1]]$get_content()
    #'
    append_encodings = function(encodings) {
      checkmate::assert_list(encodings)
      self$append_text("Selected Options", "header3")
      self$append_text(yaml::as.yaml(encodings, handlers = list(
        POSIXct = function(x) format(x, "%Y-%m-%d"),
        POSIXlt = function(x) format(x, "%Y-%m-%d"),
        Date = function(x) format(x, "%Y-%m-%d")
      )), "verbatim")
      self$append_metadata("Encodings", encodings)
      invisible(self)
    }
  ),
  private = list()
)
