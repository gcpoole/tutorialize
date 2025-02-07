#' @export
mongo_recorder <- function(tutorial_id, tutorial_version, user_id, event, data) {

    #create data structure
  if(!grepl("^section", event)) {
    new_data <-
      dplyr::tibble(
        user = user_id,
        tutorial = tutorial_id,
        version = tutorial_version,
        event = event,
        timestamp = Sys.time(),
        data = list(recursive_unclass(data)))

    print(new_data$event)
    #write the data...
    #    events_collection$insert(new_data)
    getOption("tutorial.events_collection")$insert(new_data, auto_unbox = TRUE)
  }
}

#' @export
recorder <- function(tutorial_id, tutorial_version, user_id, event, data) {
  # get the directory for writing the output file
  tut_dir <- tut_directory()
  # create if it doesn't exist
  if(!file.exists(tut_dir)) dir.create(tut_dir)
  # make a file name
  tut_file <- tut_filename(tutorial_id, user_id)
  # if the event is not a "section event...
  if(!grepl("^section", event)) {
    # create the output file path.
    recordFile <- file.path(tut_dir, tut_file)
    #create data structure
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

    #write the data...
    if(file.exists(recordFile)) {
      load(recordFile)
      tutorial_data <- c(tutorial_data, new_data)
    } else
      tutorial_data <- new_data
    save(tutorial_data, file = recordFile)
  }
}

recursive_unclass <- function(l) {
  lapply(
    l,
    function(.l) {
      if(!is.null(attr(.l, "class"))) .l <- unclass(.l)
      if(is.list(.l)) .l <- recursive_unclass(.l)
      .l
    })
}
