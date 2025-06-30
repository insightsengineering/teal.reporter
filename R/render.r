#' Render `teal_card`
#' @inheritParams rmarkdown::render
#' @param input (`teal_report` or `teal_code`) object to render
#' @param global_knitr (`list`) # todo
#' @param rmd_yaml_args (`list`) going to be deprecated - applies to the `Reporter` object.
#' @param keep_rmd (`logical(1)`) if `.Rmd` should be kept after rendering to desired `output_format`
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
#' render(report, output_format = rmarkdown::pdf_document())
#' @export
render <- function(
    input,
    output_dir = getwd(),
    rmd_yaml_args = list(),
    global_knitr = getOption("teal.reporter.global_knitr"),
    keep_rmd = TRUE,
    ...) {
  checkmate::assert_subset(names(list(...)), names(formals(rmarkdown::render)))
  on.exit({
    # replace temporary file with reproducible rmd (with eval = TRUE)
    suppressWarnings(file.remove(rmd_filepath))
    if (keep_rmd) {
      out_rmd_content <- to_rmd(
        block = input,
        output_dir = output_dir,
        rmd_yaml_args = rmd_yaml_args,
        global_knitr = global_knitr,
        include_chunk_output = FALSE
      )
      cat(out_rmd_content, file = rmd_filepath)
    }
  })
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  rmd_filepath <- file.path(output_dir, "report.Rmd")
  temp_rmd_content <- to_rmd(
    block = input,
    output_dir = output_dir,
    rmd_yaml_args = rmd_yaml_args,
    global_knitr = c(global_knitr, list(eval = FALSE)), # we don't want to rerun evaluated code chunks to render
    include_chunk_output = TRUE
  )
  cat(temp_rmd_content, file = rmd_filepath)
  args <- utils::modifyList(
    list(...),
    list(
      input = rmd_filepath,
      output_dir = output_dir
    )
  )
  do.call(rmarkdown::render, args)
  output_dir
}
