#' @export
tut_directory <- function() {
  file.path("~", "_311_Tutorial_")
}

tut_filename <- function(tutorial, user) {
  paste0(basename(tutorial), "_311_", user)
}

#' @export
tut_to_tibble <- function(x) {
  data_list <- lapply(x, \(x) x[[6]])

  tib <-
    x %>%
    lapply(\(x) x[-length(x)]) %>%
    bind_rows() %>%
    mutate(label = sapply(data_list, \(x) x[["label"]] %>% ifelse(is.null(.), NA, .))) %>%
    mutate(data = data_list)

  tib
}

#' @export
submitToTA = function(x) {
  learnr::correct()
}
