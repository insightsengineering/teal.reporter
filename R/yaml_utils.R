#' Mark strings for quotation in `yaml` serialization
#'
#' This function is designed for use with the `yaml` package to explicitly,
#' It adds an attribute to character strings, indicating that they should be serialized with double quotes.
#'
#' @param x (`character`)
#' @keywords internal
#' @examples
#' library(yaml)
#' yaml_quoted <- getFromNamespace("yaml_quoted", "teal.reporter")
#' yaml <- list(
#'   author = yaml_quoted("NEST"),
#'   title = yaml_quoted("Report"),
#'   date = yaml_quoted("07/04/2019"),
#'   output = list(pdf_document = list(keep_tex = TRUE))
#' )
#' as.yaml(yaml)
yaml_quoted <- function(x) {
  attr(x, "quoted") <- TRUE
  x
}

#' Create `markdown` header from `yaml` string
#'
#' This function wraps a `yaml`-formatted string in Markdown header delimiters.
#'
#' @param x (`character`) `yaml` formatted string.
#' @keywords internal
#' @examples
#' library(yaml)
#' yaml_quoted <- getFromNamespace("yaml_quoted", "teal.reporter")
#' yaml <- list(
#'   author = yaml_quoted("NEST"),
#'   title = yaml_quoted("Report"),
#'   date = yaml_quoted("07/04/2019"),
#'   output = list(pdf_document = list(keep_tex = TRUE))
#' )
#' md_header <- getFromNamespace("md_header", "teal.reporter")
#' md_header(as.yaml(yaml))
md_header <- function(x) {
  paste0("---\n", x, "---\n")
}

#' Convert `yaml` representation of a boolean strings to logical Values
#'
#' Converts a single `character` string representing a `yaml` boolean value into a logical value in `R`.
#'
#' @param input (`character(1)`)
#' @param name (`charcter(1)`)
#' @param pos_logi (`character`) vector of `yaml` values which should be treated as `TRUE`.
#' @param neg_logi (`character`) vector of `yaml` values which should be treated as `FALSE`.
#' @param silent (`logical(1)`) if to suppress the messages and warnings.
#' @return `input` argument or the appropriate `logical` value.
#' @keywords internal
#' @examples
#' conv_str_logi <- getFromNamespace("conv_str_logi", "teal.reporter")
#' conv_str_logi("TRUE")
#' conv_str_logi("True")
#'
#' conv_str_logi("off")
#' conv_str_logi("n")
#'
#' conv_str_logi("sth")
conv_str_logi <- function(input,
                          name = "",
                          pos_logi = c("TRUE", "true", "True", "yes", "y", "Y", "on"),
                          neg_logi = c("FALSE", "false", "False", "no", "n", "N", "off"),
                          silent = TRUE) {
  checkmate::assert_string(input)
  checkmate::assert_string(name)
  checkmate::assert_character(pos_logi)
  checkmate::assert_character(neg_logi)
  checkmate::assert_flag(silent)

  all_logi <- c(pos_logi, neg_logi)
  if (input %in% all_logi) {
    if (isFALSE(silent)) {
      message(sprintf("The '%s' value should be a logical, so it is automatically converted.", input))
    }
    input %in% pos_logi
  } else {
    input
  }
}

#' Get document output types from the `rmarkdown` package
#'
#' @description
#'
#' Retrieves vector of available document output types from the `rmarkdown` package,
#' such as `pdf_document`, `html_document`, etc.
#'
#' @return `character` vector.
#' @export
#' @examples
#' rmd_outputs()
rmd_outputs <- function() {
  rmarkdown_namespace <- asNamespace("rmarkdown")
  ls(rmarkdown_namespace)[grep("_document|_presentation", ls(rmarkdown_namespace))]
}

#' Get document output arguments from the `rmarkdown` package
#'
#' @description
#'
#' Retrieves the arguments for a specified document output type from the `rmarkdown` package.
#'
#' @param output_name (`character`) `rmarkdown` output name.
#' @param default_values (`logical(1)`) if to return a default values for each argument.
#' @export
#' @examples
#' rmd_output_arguments("pdf_document")
#' rmd_output_arguments("pdf_document", TRUE)
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

#' Parse a named list to `yaml` header for an `Rmd` file
#'
#' @description
#'
#' Converts a named list into a `yaml` header for `Rmd`, handling output types and arguments
#' as defined in the `rmarkdown` package. This function simplifies the process of generating `yaml` headers.
#'
#' @details
#' This function processes a non-nested (flat) named list into a `yaml` header for an `Rmd` document.
#' It supports all standard `Rmd` `yaml` header fields, including `author`, `date`, `title`, `subtitle`,
#'  `abstract`, `keywords`, `subject`, `description`, `category`, and `lang`.
#' Additionally,  it handles `output` field types and arguments as defined in the `rmarkdown` package.
#'
#' @note Only non-nested lists are automatically parsed.
#' Nested lists require direct processing with `yaml::as.yaml`.
#'
#' @param input_list (`named list`) non nested with slots names and their values compatible with `Rmd` `yaml` header.
#' @param as_header (`logical(1)`) optionally wrap with result with the internal `md_header()`, default `TRUE`.
#' @param convert_logi (`logical(1)`) convert a character values to logical,
#'  if they are recognized as quoted `yaml` logical values , default `TRUE`.
#' @param multi_output (`logical(1)`) multi `output` slots in the `input` argument, default `FALSE`.
#' @param silent (`logical(1)`) suppress messages and warnings, default `FALSE`.
#' @return `character` with `rmd_yaml_header` class,
#' result of [`yaml::as.yaml`], optionally wrapped with internal `md_header()`.
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
#' as_yaml_auto(list(
#'   author = "", output = "pdf_document", toc = TRUE, keep_tex = TRUE,
#'   wrong = 2
#' ))
#'
#' as_yaml_auto(list(author = "", output = "pdf_document", toc = TRUE, keep_tex = 2),
#'   silent = TRUE
#' )
#'
#' input <- list(author = "", output = "pdf_document", toc = TRUE, keep_tex = "True")
#' as_yaml_auto(input)
#' as_yaml_auto(input, convert_logi = TRUE, silent = TRUE)
#' as_yaml_auto(input, silent = TRUE)
#' as_yaml_auto(input, convert_logi = FALSE, silent = TRUE)
#'
#' as_yaml_auto(
#'   list(
#'     author = "", output = "pdf_document",
#'     output = "html_document", toc = TRUE, keep_tex = TRUE
#'   ),
#'   multi_output = TRUE
#' )
#' as_yaml_auto(
#'   list(
#'     author = "", output = "pdf_document",
#'     output = "html_document", toc = "True", keep_tex = TRUE
#'   ),
#'   multi_output = TRUE
#' )
as_yaml_auto <- function(input_list,
                         as_header = TRUE,
                         convert_logi = TRUE,
                         multi_output = FALSE,
                         silent = FALSE) {
  checkmate::assert_logical(as_header)
  checkmate::assert_logical(convert_logi)
  checkmate::assert_logical(silent)
  checkmate::assert_logical(multi_output)

  if (multi_output) {
    checkmate::assert_list(input_list, names = "named")
  } else {
    checkmate::assert_list(input_list, names = "unique")
  }

  is_nested <- function(x) any(unlist(lapply(x, is.list)))
  if (is_nested(input_list)) {
    result <- input_list
  } else {
    result <- list()
    input_nams <- names(input_list)

    # top fields
    top_fields <- c(
      "author", "date", "title", "subtitle", "abstract",
      "keywords", "subject", "description", "category", "lang"
    )
    for (itop in top_fields) {
      if (itop %in% input_nams) {
        result[[itop]] <- switch(itop,
          date = as.character(input_list[[itop]]),
          input_list[[itop]]
        )
      }
    }

    # output field
    doc_types <- unlist(input_list[input_nams == "output"])
    if (length(doc_types)) {
      for (dtype in doc_types) {
        doc_type_args <- rmd_output_arguments(dtype, TRUE)
        doc_type_args_nams <- names(doc_type_args)
        any_output_arg <- any(input_nams %in% doc_type_args_nams)

        not_found_args <- setdiff(input_nams, c(doc_type_args_nams, top_fields, "output"))
        if (isFALSE(silent) && length(not_found_args) > 0 && isFALSE(multi_output)) {
          warning(sprintf("Not recognized and skipped arguments: %s", paste(not_found_args, collapse = ", ")))
        }

        if (any_output_arg) {
          doc_list <- list()
          doc_list[[dtype]] <- list()
          for (e in intersect(input_nams, doc_type_args_nams)) {
            if (is.logical(doc_type_args[[e]]) && is.character(input_list[[e]])) {
              pos_logi <- c("TRUE", "true", "True", "yes", "y", "Y", "on")
              neg_logi <- c("FALSE", "false", "False", "no", "n", "N", "off")
              all_logi <- c(pos_logi, neg_logi)
              if (input_list[[e]] %in% all_logi && convert_logi) {
                input_list[[e]] <- conv_str_logi(input_list[[e]], e,
                  pos_logi = pos_logi,
                  neg_logi = neg_logi, silent = silent
                )
              }
            }

            doc_list[[dtype]][[e]] <- input_list[[e]]
          }
          result[["output"]] <- append(result[["output"]], doc_list)
        } else {
          result[["output"]] <- append(result[["output"]], input_list[["output"]])
        }
      }
    }
  }

  result <- rapply(result, function(x) {
    if (inherits(x, "Date")) {
      as.character(x)
    } else {
      x
    }
  }, how = "replace")

  result <- yaml::as.yaml(result)
  if (as_header) {
    result <- md_header(result)
  }
  structure(result, class = "rmd_yaml_header")
}

#' Print method for the `yaml_header` class
#'
#' @param x (`rmd_yaml_header`) class object.
#' @param ... optional text.
#' @return `NULL`.
#' @exportS3Method
#' @examples
#' input <- list(author = "", output = "pdf_document", toc = TRUE, keep_tex = TRUE)
#' out <- as_yaml_auto(input)
#' out
#' print(out)
print.rmd_yaml_header <- function(x, ...) {
  cat(x, ...)
}

#' Extract field from `yaml` text
#'
#' Parses `yaml` text, extracting the specified field. Returns list names if it's a list;
#' otherwise, the field itself.
#'
#' @param yaml_text (`rmd_yaml_header` or `character`) vector containing the `yaml` text.
#' @param field_name (`character`) the name of the field to extract.
#'
#' @return If the field is a list, it returns the names of elements in the list; otherwise,
#' it returns the extracted field.
#'
#' @keywords internal
get_yaml_field <- function(yaml_text, field_name) {
  checkmate::assert_multi_class(yaml_text, c("rmd_yaml_header", "character"))
  checkmate::assert_string(field_name)

  yaml_obj <- yaml::yaml.load(yaml_text)

  result <- yaml_obj[[field_name]]
  if (is.list(result)) {
    result <- names(result)
  }
  result
}
