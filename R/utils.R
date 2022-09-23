#' Get bootstrap current version
#' @note will work properly mainly inside a tag `.renderHook`
#' @keywords internal
get_bs_version <- function() {
  theme <- bslib::bs_current_theme()
  if (bslib::is_bs_theme(theme)) {
    bslib::theme_version(theme)
  } else {
    "3"
  }
}
