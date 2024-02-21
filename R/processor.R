#' Process a zip file of assignment submissions from D2L
#'
#' In D2L, all users submissions for a single assignment can be downloaded in a
#' .zip file.  These functions process results from such a zip file into a
#' tibble.
#'
#' \code{process_D2L_files} is a convenience wrapper that calls
#' \code{unzip_D2L_files()}, followed by \code{check_D2L_files()}, and
#' \code{import_D2L_files()}  on a .zip file downloaded from D2L.
#'
#' \code{unzip_D2L_files()} will unzip a D2L .zip file into a separate directory
#' within the parent directory of the D2L file specified by \code{zip}.
#'
#' \code{check_D2L_files()} will make sure all of the unzipped files can be
#' opened.  Any that can't be opened by R are placed into a "bad_files"
#' directory in the parent directory of the unzipped files with a warning.  In
#' this case the student may have to resubmit a file, or you might be able to
#' salvage the file manually and move it out of the "bad_files" folder.
#'
#' \code{import_D2L_files()} will read in and process all of the unzipped
#' student-submitted files in a folder.
#'
#' @return  \code{process_D2L_files} returns a tibble of all data for all
#'   students in the D2L .zip with, with a warning if any student's file was
#'   moved to the "bad_data" directory.
#'
#'   \code{unzip_D2L_files()} returns the name of the new folder containing the
#'   unzipped files.  The folder name is the assignment name in D2L, which is
#'   extracted from the .zip file name.
#'
#'   \code{check_D2L_files()} returns the \code{extracted_path} argument
#'   unaltered to facilitate piping the results of the command.
#'
#'   \code{import_D2L_files()} returns a tibble containing all data from all
#'   files in \code{extracted_directory}.
#'
#' @param zip Path to a D2L .zip file that contains tutorial submissions from
#'   multiple users
#' @param overwrite When TRUE, will overwrite any existing folder to which the
#'   .zip file has been extracted in the past.  When FALSE, the function fails
#'   with an error if there is an existing folder with the assignment name.
#' @param extracted_path The path to a folder containing files that were
#'   extracted by \code{unzip_D2L_files}.  The default value will invoke a file
#'   chooser.  Select a file *within* the folder to be processed.  Although you
#'   only select one file, all of the files in the same folder will imported. To
#'   view the contents of just one file submitted by a student, see
#'   \code{\link{tut_to_tibble}}
#'
#' @export
process_D2L_files <- function(zip = file.choose(), overwrite = F) {
  unzip_D2L_files(zip, overwrite) %>%
    check_D2L_files() %>%
    import_D2L_files()
}

#' @rdname process_D2L_files
#' @export
unzip_D2L_files <- function(zip = file.choose(), overwrite = F) {
  zipfile_path <- dirname(zip)
  zipfile_name <- basename(zip)
  extract_path <-
    file.path(
      zipfile_path,
      basename(
        regmatches(
          zipfile_name,
          regexpr("^.*(?=(\\ Download))", zipfile_name, perl = T))))
  if(file.exists(extract_path)) {
    if(overwrite) unlink(extract_path, recursive = TRUE)
    else stop("Extraction not complete. Destination folder exists.  To overwrite existing folder, set 'overwrite = TRUE'.")
  }
  unzip(zip, exdir = extract_path)
  return(extract_path)
}

#' @rdname process_D2L_files
#' @export
check_D2L_files <- function(extracted_path) {
  files_to_check <- list.files(extracted_path, recursive = TRUE, include.dirs = FALSE)
  files_to_check <- files_to_check[!grepl("/", files_to_check)]
  result <- lapply(
    files_to_check,
    function(x) {
      x_file <- file.path(extracted_path, x)
      tryCatch(
        suppressWarnings(load(x_file)),
        error = function(e) {
          error_dir <- file.path(extracted_path, "bad_files")
          error_file <- file.path(error_dir, x)
          if(!file.exists(error_dir)) {
            if(x_file != "index.html")
              dir.create(error_dir)
          }
          copy_success <- file.copy(x_file, error_file, overwrite = TRUE)
          if(!copy_success) stop("Could not copy ", x_file, " to ", error_dir)
          file.remove(x_file)
        }
      )
    }
  )
  bad <- files_to_check[sapply(result, `==`, TRUE)]
  bad <- bad[bad != "index.html"]
  if(length(bad) != 0)
    warning(
      "The following student submitted bad files.  See 'bad_files' directory:\n  ",
      paste0(bad, collapse = "\n  "))
  extracted_path
}

#' @rdname process_D2L_files
#' @export
import_D2L_files <- function(extracted_path = dirname(file.choose())) {
  files_to_check <- list.files(extracted_path, recursive = TRUE, include.dirs = FALSE)
  files_to_check <- files_to_check[!grepl("/", files_to_check)]
  lapply(
    files_to_check,
    function(x) {
      load(file.path(extracted_path, x))
      tut_to_tibble(tutorial_data)
      }
    ) %>%
    dplyr::bind_rows()
}


#' Convert a tutorial output list to a tibble
#'
#' Convert data lists from individual student tutorial submission files to a
#' tibble.
#'
#' Individual student submission files can be read using the \code{\link{load}}
#' function, which will result in a variable \code{tutorial_data} being created
#' in the global environment.  \code{tutorial_data} will contain a sorta yucky
#' list.  Pass \code{tutorial_data} to \code{tut_to_tibble} to have the data put
#' into a yummy tibble format.
#'
#' @return A tibble version of the data in \code{x}
#'
#' @param x The list that ends up in a \code{tutorial_data} variable in the
#'   global environment after using \code\{link{load}} to read in an individual
#'   student's tutorial submission file.
#'
#' @export
tut_to_tibble <- function(x) {
  data_list <- lapply(x, \(x) x[[6]])

  tib <-
    x %>%
    lapply(\(x) x[-length(x)]) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      label = extract_tutorial_data(data_list, "label"),
      code = extract_tutorial_data(data_list, "code")
    ) %>%
    dplyr::mutate(data = data_list)

  tib
}

extract_tutorial_data <- function(data_list, field_name) {
  sapply(data_list, \(x) x[[field_name]] %>% ifelse(is.null(.), NA, .))
}
