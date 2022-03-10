#' @title quoted string for `yaml`
#' @description add quoted attribute for `yaml` package
#' @param x `character`
#' @examples
#' yaml <- list(
#'   author = teal.reporter:::yaml_quoted("NEST"),
#'   title = teal.reporter:::yaml_quoted("Report"),
#'   date = teal.reporter:::yaml_quoted("07/04/2019"),
#'   output = list(pdf_document = list(keep_tex = TRUE))
#' )
#' yaml::as.yaml(yaml)
yaml_quoted <- function(x) {
  attr(x, "quoted") <- TRUE
  x
}

#' @title wrap a `yaml` string to the `markdown` header
#' @description wrap a `yaml` string to the `markdown` header.
#' @param x `character` `yaml` formatted string.
#' @examples
#' yaml <- list(
#'   author = teal.reporter:::yaml_quoted("NEST"),
#'   title = teal.reporter:::yaml_quoted("Report"),
#'   date = teal.reporter:::yaml_quoted("07/04/2019"),
#'   output = list(pdf_document = list(keep_tex = TRUE))
#' )
#' teal.reporter:::md_header(yaml::as.yaml(yaml))
md_header <- function(x) {
  paste0("---\n", x, "---\n")
}
