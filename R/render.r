#' Render `teal_card`
#' @inheritParams rmarkdown::render
#' @param input (`teal_report` or `teal_code`) object to render
#' @param global_knitr (`list`) # todo
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
    output_dir = NULL,
    global_knitr = getOption("teal.reporter.global_knitr"),
    # todo: include collapse_subsequent_chunks?
    ...) {
  checkmate::assert_subset(names(list(...)), names(formals(rmarkdown::render)))
  on.exit(try(file.remove(file_path), silent = TRUE))
  if (is.null(output_dir)) output_dir <- getwd()
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  rmd_file_name <- "document.Rmd"
  rmd <- to_rmd(
    block = input,
    output_dir = output_dir,
    include_chunk_output = TRUE,
    # todo: include collapse_subsequent_chunks?
    global_knitr = global_knitr
  )
  cat(paste(rmd, collapse = "\n"), file = file.path(output_dir, rmd_file_name))
  args <- modifyList(
    list(...),
    list(
      input = file.path(output_dir, rmd_file_name),
      output_dir = output_dir
    )
  )
  do.call(rmarkdown::render, args)
}
