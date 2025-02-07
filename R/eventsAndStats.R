#' Remove all events for a given user from the events table
#'
#' @param user The user id to remove -- must match the value in the `user` key
#'   in the collection
#' @param events_collection A mongo collection object pointing to the events
#'   collection.
#' @param commit If FALSE, the function will only summarize events to be
#'   removed; if TRUE, all events associated with `user` are removed.
#' @returns A tibble summary of events to be removed if commit is FALSE,
#'   otherwise TRUE if removal is successful.
#' @export
remove_user <- function(user, events_collection, commit = FALSE) {
  if(commit) {
    result <- events_collection$remove(query =  mongobits::j(user = user))
  } else {
    result <- events_collection$find(query = mongobits::j(user = user)) |>
      dplyr::as_tibble()
    if(nrow(result) > 0) {
      result <-
        result |>
        extract_event_data("label") |>
        dplyr::select(user, tutorial, event, label) |>
        dplyr::group_by(user, tutorial, event, label) |>
        dplyr::summarize(event_count = dplyr::n())
    }
  }
  result
}

#' Extract a field from 'data' column of a data.frame of events
#'
#' Attempts to extract a named member of the 'data' field as an atomic vector,
#' converting NULL to NA.  If any value in result has a length > 1, extracted
#' values are returned as a list.  Length of result will be equal to nrow(events).
#'
#' @param events A data.frame, typically returned from the find method of a
#'   mongo collection.
#' @param field_name A character vector containing of the name of the field to
#'   extract.
#' @export
extract_event_data <- function(events, field_name, column_name = field_name) {
  if(is.data.frame(events$data)) {
    result <- events$data[[field_name]]
  } else {
    result <- lapply(as.list(events$data), `[[`, field_name)
  }
  if(all(sapply(result, length) <= 1)) {
    result[sapply(result, length)==0] <- NA
    result <- unlist(result)
  }
  if(length(result) != nrow(events))
    stop("Could not extract '", field_name, "' as an atomic vector.")
  events[[column_name]] <- result
  events
}

#' Looks up the most recent name associated with each user
#'
#' @param events_collection Mongo collection objects pointing to the events
#'   collection.
#' @returns A tibble of users with their associated name and the date/time when
#'   the name was entered.
#' @export
identify_users <- function(events_collection) {

  users <-
    events_collection$find(query = mongobits::j(data.label = "00_users_name-1"))

  if(nrow(users) == 0)
    stop("No user id records were found.")

  users <-
    users |>
    extract_event_data("answer") |>
    dplyr::transmute(user, name = answer, timestamp) |>
    dplyr::group_by(user, name) |>
    dplyr::arrange(timestamp) |>
    dplyr::slice_tail(n=1)

  # result <- list()
  # for(i in 1:nrow(users)) {
  #   result[[i]] <-
  #     users_collection$update(
  #       mongobits::j(user = users$user[i]),
  #       mongobits::j(`$set` = list(name = users$name[i])),
  #       upsert = TRUE) |> unclass()
  # }
  #
  # dplyr::bind_cols(users, dplyr::bind_rows(result))

  users
}

#' Get events and behavioral statistics for each user and tutorial response.
#'
#' @param events_collection Mongo collection object for events collection
#' @param events A tibble derived from \code{fetch_events()}
#' @returns A tibble of statistics for each survey question.
#' @export
fetch_events <- function(events_collection) {
  events <-
    events_collection$find()

  # check for handled events
  un_event <- !events$event %in% c("exercise_submitted", "exercise_result", "question_submission", "exercise_hint", "session_start", "session_stop")
  if(any(un_event))
    stop("Unexpected event type(s): ", paste(unique(events$event[un_event]), collapse = ", "))

  events |>
    dplyr::as_tibble() |>
    # pull some useful fields from the data object
    extract_event_data("label", "question") |>
    extract_event_data("id", "event_id") |>
    extract_event_data("restore", "restored") |>
    extract_event_data("code") |>
    extract_event_data("output") |>
    extract_event_data("checked") |>
    extract_event_data("correct", "quest_correct") |>
    extract_event_data("error_message", "error") |>
    extract_event_data("feedback", "data") |>
    extract_event_data("correct", "code_correct") |>
    dplyr::mutate(
      code = trimws(code),
      output = trimws(output),
      correct = as.logical(pmax(quest_correct, code_correct, na.rm = TRUE)),
      checked = ifelse(event == "question_submission", TRUE, checked),
      restored = ifelse(is.na(restored), FALSE, restored)) |>
    dplyr::filter(!sapply(code, identical, "")) |>
    dplyr::arrange(user, tutorial, timestamp) |>
    # There are two events for each submission, the submitted event and the result
    # event.  All the necessary data from exercise_submitted are in result, so
    # drop submissions.  Also drop any event that occurred as a result of
    # restoring a session and drop session_stop because they are not reported when
    # user aborts a session.
    dplyr::filter(!restored & !event %in% c("session_stop", "exercise_submitted")) |>
    # drop some unneeded columns
    dplyr::select(-c(data, restored, version, event_id, quest_correct, code_correct)) |>
    # for each user and tutorial, assign a "session number," which changes
    # each time a user starts a session.
    dplyr::group_by(user, tutorial) |>
    dplyr::mutate(
      session = cumsum(event == "session_start")) |>
    dplyr::group_by(user, tutorial, session) |>
    dplyr::mutate(elapsed = timestamp - dplyr::lag(timestamp)) |>
    dplyr::filter(!event %in% c("session_start")) |>
    dplyr::mutate(is_response = event %in% c("exercise_result", "question_submission")) |>
    dplyr::group_by(user, tutorial, question)
}

#' @rdname fetch_events
#' @export
event_stats <- function(events) {
  questions_by_user <-
    dplyr::cross_join(
      events |> dplyr::group_by(user) |> dplyr::summarize(),
      events |> dplyr::group_by(tutorial, question) |> dplyr::summarize())

  dplyr::left_join(
    questions_by_user,
    events |>
      dplyr::group_by(user, tutorial, question) |>
      dplyr::arrange(user, tutorial, question, timestamp) |>
      dplyr::summarize(
        hint_row = min(Inf, which(event == "exercise_hint")[1], na.rm = TRUE),
        #succeeded = any(correct, na.rm = TRUE),
        time_allotted = sum(elapsed),
        responses = sum(is_response, na.rm = TRUE),
        runs = sum(is_response & !checked, na.rm = TRUE),
        submits = sum(is_response & checked, na.rm = TRUE),
        after_hint = sum(is_response & dplyr::row_number() > hint_row, na.rm = TRUE),
        correct = sum(correct, na.rm = TRUE)) |>
      # don't use "succeeded" flag on questions.  Just correct or incorrect...
      # dplyr::mutate(succeeded = ifelse(event == "exercise_result", succeeded, NA)) |>
      dplyr::select(-hint_row)) |>
    dplyr::left_join(
      x = events |>
        dplyr::filter(event != "exercise_hint") |>
        dplyr::group_by(user, tutorial, question, event) |>
        dplyr::summarize(),
      y = _
    ) |>
    dplyr::left_join(
      x = identify_users(events_collection),
      y = _) |>
    dplyr::ungroup()
}
