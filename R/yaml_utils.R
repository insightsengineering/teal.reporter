#' @title quoted string for `yaml`
#' @description add quoted attribute for `yaml` package
#' @param x `character`
#' @keywords internal
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
#' @keywords internal
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

#' @title Convert a character of a yaml boolean to a logical value
#' @description convert a character of a yaml boolean to a logical value.
#' @param input `character`
#' @param pos_logi `character` vector
#' @param neg_logi `character` vector
#' @param silent `logical`
#' @return `input` argument or the appropriate `logical` value.
#' @keywords internal
#' @examples
#' teal.reporter:::conv_str_logi("TRUE")
#' teal.reporter:::conv_str_logi("True")
#'
#' teal.reporter:::conv_str_logi("off")
#' teal.reporter:::conv_str_logi("n")
#'
#' teal.reporter:::conv_str_logi("sth")
#'
conv_str_logi <- function(input,
                          pos_logi = c("TRUE", "true", "True", "yes", "y", "Y", "on"),
                          neg_logi = c("FALSE", "false", "False", "no", "n", "N", "off"),
                          silent = FALSE) {
  checkmate::assert_character(input)
  all_logi <- c(pos_logi, neg_logi)
  is_logi <- input %in% all_logi
  is_pos_logi <- input %in% pos_logi
  if (is_logi) {
    if (is_pos_logi) {
      TRUE
    } else {
      FALSE
    }
  } else {
    input
  }
}

#' @title Get document output types from the rmarkdown package
#'
#' @description get document output types from the `rmarkdown` package.
#' @return `character` vector.
#' @export
#' @examples
#' rmd_outputs()
#'
rmd_outputs <- function() {
  rmarkdown_namespace <- asNamespace("rmarkdown")
  ls(rmarkdown_namespace)[grep("_document|_presentation", ls(rmarkdown_namespace))]
}

#' @title Get document output arguments from the rmarkdown package
#'
#' @description get document output arguments from the rmarkdown package
#' @param output_name `character``rmarkdown` output name.
#' @param default_values `logical` if to return a default values for each argument.
#' @export
#' @examples
#' rmd_output_arguments("pdf_document")
#' rmd_output_arguments("pdf_document", TRUE)
#'
rmd_output_arguments <- function(output_name, default_values = FALSE) {
  checkmate::assert_string(output_name)
  checkmate::assert_subset(output_name, rmd_outputs())

  rmarkdown_namespace <- asNamespace("rmarkdown")
  if (default_values) {
    formals(rmarkdown_namespace[[output_name]])
  } else {
    names(formals(rmarkdown_namespace[[output_name]]))
  }
}

#' @title arse a named list to the Rmd yaml header
#' @description
#' Parse a named list to the Rmd yaml header, so the developer gets automatically tabulated Rmd yaml header.
#' Only a non nested (flat) list will be processed,
#' where as a nested list is directly processed with the [`yaml::as.yaml`] function.
#' All Rmd yaml header fields from the vector are supported,
#' `c("author", "date", "title", "subtitle", "abstract", "keywords", "subject", "description", "category", "lang")`.
#' Moreover all `output`field types in the `rmarkdown` package and their arguments are supported.
#' @param input `named list` non nested with slots names and their values compatible with Rmd yaml header.
#' @param as_header `logical` optionally wrap with result with the `teal.reporter::md_header`, default `TRUE`.
#' @param convert_logi `logical`convert a character values to logical,
#'  if they are recognized as quoted yaml logical values , default `TRUE`.
#' @param multi_output `logical`multi `output` slots in the `input` argument, default `FALSE`.
#' @param silent `logical` suppress messages and warnings, default `FALSE`.
#' @return `character` result of [`yaml::as.yaml`], optionally wrapped with `teal.reporter::md_header`.
#' @export
#' @examples
#' # nested so using yaml::as.yaml directly
#' as_yaml_auto(
#'   list(author = "", output = list(pdf_document = list(toc = TRUE)))
#' )
#'
#' # auto parsing for a flat list, like shiny input
#' input <- list(author = "", output = "pdf_document", toc = TRUE, keep_tex = TRUE)
#' as_yaml_auto(input)
#'
#' as_yaml_auto(list(author = "", output = "pdf_document", toc = TRUE, keep_tex = "TRUE"))
#'
#' as_yaml_auto(list(author = "", output = "pdf_document", toc = TRUE, keep_tex = TRUE,
#'              wrong = 2))
#'
#' as_yaml_auto(list(author = "", output = "pdf_document", toc = TRUE, keep_tex = 2),
#'              silent = TRUE)
#'
#' input <- list(author = "", output = "pdf_document", toc = TRUE, keep_tex = "True")
#' as_yaml_auto(input)
#' as_yaml_auto(input, convert_logi = TRUE, silent = TRUE)
#' as_yaml_auto(input, silent = TRUE)
#' as_yaml_auto(input, convert_logi = FALSE, silent = TRUE)
#'
#' as_yaml_auto(
#'   list(author = "", output = "pdf_document",
#'        output = "html_document", toc = TRUE, keep_tex = TRUE),
#'   multi_output = TRUE
#' )
#' as_yaml_auto(
#'   list(author = "", output = "pdf_document",
#'        output = "html_document", toc = "True", keep_tex = TRUE),
#'   multi_output = TRUE
#' )
#'
as_yaml_auto <- function(input,
                         as_header = TRUE,
                         convert_logi = TRUE,
                         multi_output = FALSE,
                         silent = FALSE) {
  checkmate::assert_logical(as_header)
  checkmate::assert_logical(convert_logi)
  checkmate::assert_logical(silent)
  checkmate::assert_logical(multi_output)

  if (multi_output) {
    checkmate::assert_list(input, names = "named")
  } else {
    checkmate::assert_list(input, names = "unique")
  }

  is_nested <- function(x) any(unlist(lapply(x, is.list)))

  if (is_nested(input)) {
    result <- yaml::as.yaml(input)
    if (as_header) {
      result <- md_header(result)
    }
  } else {
    result <- list()

    input_nams <- names(input)

    # top fields
    top_fields <- c(
      "author", "date", "title", "subtitle", "abstract",
      "keywords", "subject", "description", "category", "lang"
    )

    for (t in top_fields) {
      if (t %in% input_nams) {
        result[[t]] <- input[[t]]
      }
    }

    # output field
    doc_types <- unlist(input[input_nams == "output"])

    if (length(doc_types)) {
      for (t in doc_types) {
        doc_type_args <- rmd_output_arguments(t, TRUE)
        doc_type_args_nams <- names(doc_type_args)
        any_doc_arg <- any(input_nams %in% doc_type_args_nams)

        not_found_args <- setdiff(input_nams, c(doc_type_args_nams, top_fields, "output"))
        if (isFALSE(silent) && length(not_found_args) > 0 && isFALSE(multi_output)) {
          warning(sprintf("Not recognized and skipped arguments: %s", paste(not_found_args, collapse = ", ")))
        }

        if (any_doc_arg) {
          doc_list <- list()
          doc_list[[t]] <- list()

          for (e in intersect(input_nams, doc_type_args_nams)) {
            if (!is.null(doc_type_args[[e]])) {
              expected_class <- class(doc_type_args[[e]])
              provided_class <- class(input[[e]])

              pos_logi <- c("TRUE", "true", "True", "yes", "y", "Y", "on")
              neg_logi <- c("FALSE", "false", "False", "no", "n", "N", "off")
              all_logi <- c(pos_logi, neg_logi)

              if ("character" %in% provided_class &&
                input[[e]] %in% all_logi &&
                "logical" %in% expected_class &&
                convert_logi) {
                input[[e]] <- conv_str_logi(input[[e]], pos_logi = pos_logi, neg_logi = neg_logi)
                if (isFALSE(silent)) {
                  message(
                    sprintf(
                      "The %s ('%s') yaml argument most probably should be a logical, so is automatically converted.",
                      e,
                      input[[e]]
                    )
                  )
                }
              } else if (isFALSE(silent) && length(intersect(expected_class, provided_class)) == 0) {
                warning(
                  sprintf(
                    "The %s yaml argument with a value %s (%s) looks to have a wrong type, probably should be a %s",
                    e,
                    input[[e]],
                    paste(provided_class, collapse = ", "),
                    paste(expected_class, collapse = ", ")
                  )
                )
              }
            }
            doc_list[[t]][[e]] <- input[[e]]
          }

          result[["output"]] <- append(result[["output"]], doc_list)
        } else {
          result[["output"]] <- append(result[["output"]], input[["output"]])
        }
      }
    }

    result <- yaml::as.yaml(result)
    if (as_header) {
      result <- md_header(result)
    }
  }

  structure(result, class = "yaml_header")
}

#' @title Print method for the yaml_header class
#'
#' @description Print method for the yaml_header class.
#' @param x `yaml_header` class object.
#' @param ... optional text.
#' @return NULL
#' @exportS3Method
#' @examples
#' input <- list(author = "", output = "pdf_document", toc = TRUE, keep_tex = TRUE)
#' out <- as_yaml_auto(input)
#' out
#' print(out)
#'
print.yaml_header <- function(x, ...) {
  cat(x, ...)
}
