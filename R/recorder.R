#' @export
recorder <- function(tutorial_id, tutorial_version, user_id, event, data) {
  tut_dir <- tut_directory()
  if(!file.exists(tut_dir)) dir.create(tut_dir)
  tut_file <- tut_filename(tutorial_id, user_id)
  if(!grepl("^section", event)) {
    recordFile <- file.path(tut_dir, tut_file)
    new_data <-
      structure(
        list(
          list(
            timestamp = Sys.time(),
            tutorial = tutorial_id,
            version = tutorial_version,
            user = user_id,
            event = event,
            data = data)),
        names = event)

    if(file.exists(recordFile)) {
      load(recordFile)
      tutorial_data <- c(tutorial_data, new_data)
    } else
      tutorial_data <- new_data
    save(tutorial_data, file = recordFile)
  }
}
