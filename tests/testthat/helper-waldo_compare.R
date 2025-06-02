# Register the `compare_proxy` method for the `Reporter` class only for use in
# testthat.
if (requireNamespace("waldo", quietly = TRUE)) {
  registerS3method(
    "compare_proxy",
    "Reporter",
    function(x, path = "x") {
      list(
        object = list(
          "get_cards()" = unname(x$get_cards()),
          "get_metadata()" = x$get_metadata(),
          "get_id()" = x$get_id(),
          "get_template()" = x$get_template()
        ),
        path = path
      )
    },
    env = asNamespace("waldo")
  )
}
