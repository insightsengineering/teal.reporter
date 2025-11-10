# deprecated ------------------------------------------------------------------------------------------------------

#' Report previewer module
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Module offers functionalities to visualize, manipulate,
#' and interact with report cards that have been added to a report.
#' It includes a previewer interface to see the cards and options to modify the report before downloading.
#'
#' Cards are saved by the `shiny` bookmarking mechanism.
#'
#' For more details see the vignette: `vignette("previewerReporter", "teal.reporter")`.
#'
#' This function is deprecated and will be removed in the next release.
#' Please use `preview_report_button_ui()` and `preview_report_button_srv()`
#' to create a preview button that opens a modal with the report preview.
#'
#' @details `r global_knitr_details()`
#'
#' @name reporter_previewer_deprecated
#'
#' @param id (`character(1)`) `shiny` module instance id.
#' @param reporter (`Reporter`) instance.
#' @param global_knitr (`list`) of `knitr` parameters (passed to `knitr::opts_chunk$set`)
#'  for customizing the rendering process.
#' @param previewer_buttons (`character`) set of modules to include with `c("download", "load", "reset")` possible
#' values and `"download"` is required.
#' Default `c("download", "load", "reset")`
#' @inheritParams reporter_download_inputs
#'
#' @return `NULL`.
NULL

#' @rdname reporter_previewer_deprecated
#' @export
reporter_previewer_ui <- function(id) {
  ns <- shiny::NS(id)
  lifecycle::deprecate_soft(
    when = "0.5.0",
    what = "reporter_previewer_ui()",
    details = paste(
      "Calling `reporter_previewer_ui()` is deprecated and will be removed in the next release.\n",
      "Please use `report_load_ui()`, `download_report_button_ui()`, `reset_report_button_ui()`,",
      "and `preview_report_button_ui()` instead."
    )
  )
  bslib::page_fluid(
    shiny::tagList(
      shinyjs::useShinyjs(),
      shiny::singleton(
        shiny::tags$head(
          shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")),
          shiny::includeScript(system.file("js/extendShinyJs.js", package = "teal.reporter"))
        )
      ),

      # Extend shinyjs::js to include function defined in extendShinyJs.js
      shinyjs::extendShinyjs(text = "", functions = c("jumpToFocus", "enterToSubmit", "autoFocusModal")),
      shiny::tags$div(
        class = "well",
        style = "display: inline-flex; flex-direction: row; gap: 10px;",
        shiny::tags$span(id = ns("load_span"), report_load_ui(ns("load"), label = "Load Report")),
        shiny::tags$span(
          id = ns("download_span"), download_report_button_ui(ns("download"), label = "Download Report")
        ),
        shiny::tags$span(id = ns("reset_span"), reset_report_button_ui(ns("reset"), label = "Reset Report"))
      ),
      shiny::tags$div(reporter_previewer_content_ui(ns("previewer")))
    )
  )
}

#' @rdname reporter_previewer_deprecated
#' @export
reporter_previewer_srv <- function(id,
                                   reporter,
                                   global_knitr = getOption("teal.reporter.global_knitr"),
                                   rmd_output = getOption("teal.reporter.rmd_output"),
                                   rmd_yaml_args = getOption("teal.reporter.rmd_yaml_args"),
                                   previewer_buttons = c("download", "load", "reset")) {
  lifecycle::deprecate_soft(
    when = "0.5.0",
    what = "reporter_previewer_srv()",
    details = paste(
      "Calling `reporter_previewer_srv()` is deprecated and will be removed in the next release.\n",
      "Please use `report_load_srv()`, `download_report_button_srv()`, `reset_report_button_srv()`,",
      "and `preview_report_button_srv()` instead."
    )
  )
  checkmate::assert_subset(previewer_buttons, c("download", "load", "reset"), empty.ok = FALSE)
  checkmate::assert_true("download" %in% previewer_buttons)
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_subset(names(global_knitr), names(knitr::opts_chunk$get()))
  checkmate::assert_subset(
    rmd_output,
    c("html_document", "pdf_document", "powerpoint_presentation", "word_document"),
    empty.ok = FALSE
  )
  checkmate::assert_list(rmd_yaml_args, names = "named")
  checkmate::assert_names(
    names(rmd_yaml_args),
    subset.of = c("author", "title", "date", "output", "toc"),
    must.include = "output"
  )
  checkmate::assert_true(rmd_yaml_args[["output"]] %in% rmd_output)

  shiny::moduleServer(id, function(input, output, session) {
    if (!"load" %in% previewer_buttons) {
      shinyjs::hide(id = "load_span")
    }
    if (!"download" %in% previewer_buttons) {
      shinyjs::hide(id = "download_span")
    }
    if (!"reset" %in% previewer_buttons) {
      shinyjs::hide(id = "reset_span")
    }
    report_load_srv("load", reporter = reporter)
    download_report_button_srv(
      "download",
      reporter = reporter,
      global_knitr = global_knitr,
      rmd_output = rmd_output,
      rmd_yaml_args = rmd_yaml_args
    )
    reset_report_button_srv("reset", reporter = reporter)
    reporter_previewer_content_srv("previewer", reporter = reporter)
  })
}

#' @title `ContentBlock`: A building block for report content
#' @docType class
#' @description This class represents a basic content unit in a report,
#' such as text, images, or other multimedia elements.
#' It serves as a foundation for constructing complex report structures.
#'
#' @keywords internal
ContentBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "ContentBlock",
  public = list(
    #' @description Sets content of this `ContentBlock`.
    #'
    #' @param content (`any`) R object
    #'
    #' @return `self`, invisibly.
    #' @examples
    #' ContentBlock <- getFromNamespace("ContentBlock", "teal.reporter")
    #' block <- ContentBlock$new()
    #' block$set_content("Base64 encoded picture")
    #'
    set_content = function(content) {
      private$content <- content
      invisible(self)
    },
    #' @description Retrieves the content assigned to this block.
    #'
    #' @return object stored in a `private$content` field
    #' @examples
    #' ContentBlock <- getFromNamespace("ContentBlock", "teal.reporter")
    #' block <- ContentBlock$new()
    #' block$get_content()
    #'
    get_content = function() {
      private$content
    },
    #' @description Create the `ContentBlock` from a list.
    #'
    #' @param x (`named list`) with two fields `text` and `style`.
    #' Use the `get_available_styles` method to get all possible styles.
    #'
    #' @return `self`, invisibly.
    from_list = function(x) {
      invisible(self)
    },
    #' @description Convert the `ContentBlock` to a list.
    #'
    #' @return `named list` with a text and style.
    to_list = function() {
      list()
    }
  ),
  private = list(
    content = NULL, # this can be any R object
    # @description The copy constructor.
    #
    # @param name (`character(1)`) the name of the field
    # @param value the value assigned to the field
    #
    # @return the value of the copied field
    deep_clone = function(name, value) {
      if (name == "content" && checkmate::test_file_exists(value)) {
        extension <- ""
        split <- strsplit(basename(value), split = "\\.")
        # The below ensures no extension is found for files such as this: .gitignore but is found for files like
        # .gitignore.txt
        if (length(split[[1]]) > 1 && split[[1]][length(split[[1]]) - 1] != "") {
          extension <- split[[1]][length(split[[1]])]
          extension <- paste0(".", extension)
        }
        copied_file <- tempfile(fileext = extension)
        file.copy(value, copied_file, copy.date = TRUE, copy.mode = TRUE)
        copied_file
      } else {
        value
      }
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)


#' @title `RcodeBlock`
#' @docType class
#' @description
#' Specialized `ContentBlock` designed to embed `R` code in reports.
#'
#' @keywords internal
RcodeBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "RcodeBlock",
  inherit = ContentBlock,
  public = list(
    #' @description Initialize a `RcodeBlock` object.
    #'
    #' @details Returns a `RcodeBlock` object with no content and no parameters.
    #'
    #' @param content (`character(1)` or `character(0)`) a string assigned to this `RcodeBlock`
    #' @param ... any `rmarkdown` `R` chunk parameter and it value.
    #'
    #' @return Object of class `RcodeBlock`, invisibly.
    #' @examples
    #' RcodeBlock <- getFromNamespace("RcodeBlock", "teal.reporter")
    #' block <- RcodeBlock$new()
    #'
    initialize = function(content = character(0), ...) {
      checkmate::assert_class(content, "character")
      super$set_content(content)
      self$set_params(list(...))
      invisible(self)
    },
    #' @description Sets content of this `RcodeBlock`.
    #'
    #' @param content (`any`) R object
    #'
    #' @return `self`, invisibly.
    #' @examples
    #' RcodeBlock <- getFromNamespace("RcodeBlock", "teal.reporter")
    #' block <- RcodeBlock$new()
    #' block$set_content("a <- 1")
    #'
    set_content = function(content) {
      checkmate::assert_string(content)
      super$set_content(content)
    },
    #' @description Sets the parameters of this `RcodeBlock`.
    #'
    #' @details Configures `rmarkdown` chunk parameters for the `R` code block,
    #' influencing its rendering and execution behavior.
    #'
    #' @param params (`list`) any `rmarkdown` R chunk parameter and its value.
    #'
    #' @return `self`, invisibly.
    #' @examples
    #' RcodeBlock <- getFromNamespace("RcodeBlock", "teal.reporter")
    #' block <- RcodeBlock$new()
    #' block$set_params(list(echo = TRUE))
    #'
    set_params = function(params) {
      checkmate::assert_list(params, names = "named")
      checkmate::assert_subset(names(params), self$get_available_params())
      private$params <- params
      invisible(self)
    },
    #' @description Get the parameters of this `RcodeBlock`.
    #'
    #' @return `character` the parameters of this `RcodeBlock`.
    #' @examples
    #' RcodeBlock <- getFromNamespace("RcodeBlock", "teal.reporter")
    #' block <- RcodeBlock$new()
    #' block$get_params()
    #'
    get_params = function() {
      private$params
    },
    #' @description Get available array of parameters available to this `RcodeBlock`.
    #'
    #' @return A `character` array of parameters.
    #' @examples
    #' RcodeBlock <- getFromNamespace("RcodeBlock", "teal.reporter")
    #' block <- RcodeBlock$new()
    #' block$get_available_params()
    #'
    get_available_params = function() {
      names(knitr::opts_chunk$get())
    },
    #' @description Create the `RcodeBlock` from a list.
    #'
    #' @param x (`named list`) with two fields `text` and `params`.
    #' Use the `get_available_params` method to get all possible parameters.
    #'
    #' @return `self`, invisibly.
    #' @examples
    #' RcodeBlock <- getFromNamespace("RcodeBlock", "teal.reporter")
    #' block <- RcodeBlock$new()
    #' block$from_list(list(text = "sth", params = list()))
    #'
    from_list = function(x) {
      checkmate::assert_list(x)
      checkmate::assert_names(names(x), must.include = c("text", "params"))
      self$set_content(x$text)
      self$set_params(x$params)
      invisible(self)
    },
    #' @description Convert the `RcodeBlock` to a list.
    #'
    #' @return `named list` with a text and `params`.
    #' @examples
    #' RcodeBlock <- getFromNamespace("RcodeBlock", "teal.reporter")
    #' block <- RcodeBlock$new()
    #' block$to_list()
    #'
    to_list = function() {
      list(text = self$get_content(), params = self$get_params())
    }
  ),
  private = list(
    content = character(0),
    params = list()
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
