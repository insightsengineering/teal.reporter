#' Render `teal_card`
#' @inheritParams rmarkdown::render
#' @param input (`teal_report` or `teal_code`) object to render.
#' @param global_knitr (`list`) options to apply to every code chunk in a teal_card document.
#'  [Read more here](https://rmarkdown.rstudio.com/lesson-3.html#global-options).
#' @param rmd_yaml_args (`list`) going to be deprecated - applies only to the `Reporter` object as an
#'  equivalent of `metadata(<teal_card>)`.
#' @param keep_rmd (`logical(1)`) if `.Rmd` should be kept after rendering to desired `output_format`.
#' @param ... arguments passed to `rmarkdown::render`.
#' @examples
#' report <- teal_report()
#' teal_card(report) <- c(
#'   teal_card(report),
#'   "## Document section",
#'   "Lorem ipsum dolor sit amet"
#' )
#' report <- within(report, a <- 2)
#' report <- within(report, plot(a))
#' metadata(teal_card(report)) <- list(
#'   title = "My Document",
#'   author = "NEST"
#' )
#' if (interactive()) {
#'   render(report, output_format = rmarkdown::pdf_document(), global_knitr = list(fig.width = 10))
#' }
#' @export
render <- function(
    input,
    output_dir = getwd(),
    rmd_yaml_args = list(),
    global_knitr = getOption("teal.reporter.global_knitr"),
    keep_rmd = TRUE,
    ...) {
  checkmate::assert_multi_class(input, c("teal_report", "teal_card", "Reporter"))
  checkmate::assert_string(output_dir)
  checkmate::assert_list(rmd_yaml_args, names = "named")
  checkmate::assert_list(global_knitr, names = "named")
  checkmate::assert_subset(names(global_knitr), names(knitr::opts_chunk$get()))
  checkmate::assert_flag(keep_rmd)
  checkmate::assert_subset(names(list(...)), names(formals(rmarkdown::render)))

  # Set output dir to a new working directory. Absolute paths in rmarkdown files will break .Rmd portability
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  old_wd <- setwd(dir = output_dir)
  on.exit(setwd(old_wd))

  # This Rmd file is for render purpose as it contains evaluated code chunks and their outputs.
  rmd_filepath <- "report.Rmd"
  temp_rmd_content <- to_rmd(
    block = input,
    output_dir = ".",
    rmd_yaml_args = rmd_yaml_args,
    global_knitr = c(global_knitr, list(eval = FALSE)), # we don't want to rerun evaluated code chunks to render
    include_chunk_output = TRUE
  )
  cat(temp_rmd_content, file = rmd_filepath)
  args <- utils::modifyList(list(...), list(input = rmd_filepath))
  tryCatch(
    do.call(rmarkdown::render, args),
    finally = suppressWarnings(file.remove(rmd_filepath))
  )

  if (keep_rmd) {
    # This Rmd file doesn't contain chunk_outputs as theycan be reproduced when executing code-chunks
    out_rmd_content <- to_rmd(
      block = input,
      output_dir = ".",
      rmd_yaml_args = rmd_yaml_args,
      global_knitr = global_knitr,
      include_chunk_output = FALSE
    )
    cat(out_rmd_content, file = rmd_filepath)
  }
  output_dir
}
