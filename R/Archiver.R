#' @title `Archiver`
#' @keywords internal
Archiver <- R6::R6Class( # nolint: object_name_linter.
  classname = "Archiver",
  public = list(
    #' @description Returns a `Archiver` object.
    #'
    #' @return a `Archiver` object
    #' @examples
    #' reporter <- teal.reporter:::Archiver$new()
    #'
    initialize = function() {
      private$content <- list()
      invisible(self)
    },
    archive = function(reporter) {
      checkmate::assert_class(reporter, "Reporter")
      reporter_json <- private$reporter2json(reporter)
      private$content <- reporter_json
    },
    # path to zip with json and files
    set_archive = function() {},
    # path to zip with json and files
    get_archive = function() {},
    get_cards = fucntion() {}
  ),
  private = list(
    content = list()
  )
)
