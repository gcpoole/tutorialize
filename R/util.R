#' @export
tut_directory <- function() {
  file.path("~", "_311_Tutorial_")
}

tut_filename <- function(tutorial, user) {
  paste0(basename(tutorial), "_311_", user)
}

#' @export
submitToTA = function(x) {
  learnr::correct()
}
