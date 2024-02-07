#' @export
tutorialize_setup <- function(tutorial_name, package_dir = getwd()) {

  # check to be sure package exists, tutorial exists, and answer key is not set
  # up.
  tutorial_dir <- get_tutorial_dirs(package_dir, tutorial_name)

  # see if tutorial is already set up for tutorialize
  key_dir <- file.path(tutorial_dir, "answer_key")
  if(file.exists(key_dir))
    stop("'", tutorial_dir, "' already contains an 'answer_key' directory.")

  # make sure the .Rmd file exists
  tutorial_file_name <- paste0(tutorial_name, ".Rmd")
  tutorial_file_path <- file.path(tutorial_dir, tutorial_file_name)
  if(!file.exists(tutorial_file_path))
    stop("'", tutorial_file_path, "' not found.")

  # create the answer key directory
  created <- dir.create(key_dir)
  if(!created) stop("Failed to create '", key_dir, "'")

  # copy the tutorial .Rmd file to the answer_key directory
  copied <- file.copy(
    tutorial_file_path,
    file.path(key_dir,  tutorial_file_name))

  # if copy is successful, delete the original
  if(copied)
    removed <- file.remove(tutorial_file_path)
  else
    stop("Failed to copy '", tutorial_file_path, "' to '", key_dir, "'.")

  # throw a warning if copy is successful but deletion is not.
  if(!removed)
    warning(
      "'", tutorial_file_name, "' successfully copied to '",
      key_dir, "' but could not be deleted from '", tutorial_dir)

  # If we got here, must be all good!  So return TRUE!
  TRUE
}

# Gets the tutorial directories from the specified package.  Returns one
# directory when tutorial_name is specified.  Returns all directories when
# tutorial_name is NULL.  `has_key` filters directories to those that have an
# answer_key dir within them (ignored when tutorial_name is specified).
get_tutorial_dirs <- function(package_dir, tutorial_name = NULL, has_key = TRUE) {
  # make sure all is well with package_dir
  check_dir(package_dir, FALSE)
  # create path to `\inst\tutorials`
  tutorials_dir <- file.path(package_dir, "inst", "tutorials")
  # make sure all is well with tutorial directory.
  check_dir(tutorials_dir)
  # if directory for a specific tutorial is requested...
  if(!is.null(tutorial_name)) {
    # ... if so check to be sure it's valid and return it.
    tutorial_dirs <- file.path(tutorials_dir, tutorial_name)
    check_dir(tutorial_dirs, FALSE)
  } else {
    # ... if not get all the files and dirs in \inst\tutorials
    all_file_info <-
      file.info(
        file.path(
          tutorials_dir,
          dir(tutorials_dir)))
    # eliminate files, leaving only dirs
    dir_info <- all_file_info[all_file_info$isdir,]
    tutorial_dirs <- rownames(dir_info)
    # if only directories with 'answer_key' subdirs are requested, filter that
    # as well.
    if(has_key) {
      tutorial_dirs <-
        tutorial_dirs[
          sapply(tutorial_dirs, \(x) file.exists(file.path(x, "answer_key")))
        ]
    }
  }
  tutorial_dirs
}

# check to see that a specified directory exists and return a custom message
# depending on value of `expect_use_tutorial`.  When TRUE, message points
# user toward usethis::use_tutorial.  Otherwise, generic "not found" message.
check_dir <- function(dir_name, expect_use_tutorial = TRUE) {
  if(!file.exists(dir_name))
    if(expect_use_tutorial)
      stop("`tutorialize()` requires tutorials created by 'usethis::use_tutorial()'.")
    else
      stop("Directory '", dir_name, "' not found.")
}

#' @export
tutorialize <- function(package_dir = getwd()) {

  file_warning <- "<!-- File created by tutorialize.  Do not edit by hand. -->"

  check_dir(package_dir)
  tutorialize_dirs <- get_tutorial_dirs(package_dir)
  if(length(tutorialize_dirs) == 0)
    stop("No tutorials are set up for tutorialize. See tutorialize_setup()")

  for (tut_dir in tutorialize_dirs) {
    file_name <- paste0(basename(tut_dir), ".Rmd")
    cat(paste0(file_name, "...  "))
    outfile_path <- file.path(tut_dir, file_name)
    if(file.exists(outfile_path))
      if(!any(grepl(paste0("^", file_warning, "$"), readLines(outfile_path))))
        stop("'", outfile_path, "' was not created by tutorialize.  Can't overwrite.")

    key_dir <- file.path(tut_dir, "answer_key")
    keyfile_path <- file.path(key_dir, file_name)
    if(!file.exists(keyfile_path)) {
      stop("Answer key file '", keyfile_path, "' not found.")
    }

    markdown <- readLines(keyfile_path, warn = F)

    # strip out the comments from the markdown
    markdown <- paste(markdown, collapse = "<new><line>")
    # regmatches(markdown, gregexec("<!--.*?-->", markdown, perl = T))
    markdown <- gsub("<!--.*?-->", "", markdown)
    if(!grepl("gradethis_setup", markdown))
      stop("A call to 'gradethis_setup()' in the setup chunk is required.")
    if(!grepl("tutorial\\.event_recorder", markdown))
      warning("No tutorial event recorder is defined in '", file_name, "'.")
    if(!grepl("tutorial.storage", markdown))
      warning("No tutorial storage location is defined in '", file_name, "'.")
    if(grepl("tutorialize::recorder", markdown)) {
      if(
        !grepl("tutorial:[[:space:]]*<new><line>", markdown) ||
        !grepl("<new><line>[[:space:]]+id:", markdown)
      ) stop("Tutorial name (`id:` under 'tutorial:' in YAML header) must be defined to use tutorialize::recorder")

    }

    markdown <- strsplit(markdown, "<new><line>")[[1]]

    yaml_delimiter <- which(grepl("^---[[:space:]]*$", markdown))
    if(length(yaml_delimiter) < 2) {
      stop("Can't identify end of YAML header in ", keyfild_path)
    }


    # add the tutorialize comment
    markdown <- c(
      markdown[1:yaml_delimiter[2]],
      file_warning,
      markdown[(yaml_delimiter[2]+1):length(markdown)])

    code_breaks <- which(grepl("^```", markdown, perl = T))
    if(length(code_breaks)%%2 == 1) stop("Un-closed code block in '", file_name, "'.")
    #  code_starts = which(grepl("^```\\{r", markdown, perl = T))
    if(length(code_breaks) > 0) {

      # if text is at end of file, need to append eof+1 as start of a code break.
      if(length(markdown) > tail(code_breaks, 1)) code_breaks <- c(code_breaks, length(markdown)+1)
      # adjust the ending code breaks to refer to first line of text.  That way
      # pointers are pointing as start of code and start of text.
      code_breaks <- c(1, suppressWarnings(code_breaks + c(0,1)))

      chunk_lines <- lapply(1:(length(code_breaks)-1), \(i) code_breaks[i]:(code_breaks[i+1]-1))
      chunks <- lapply(chunk_lines, \(i) structure(markdown[i], lines = i))
      names(chunks) <- rep("", length(chunks))

      # .Rmd files always start with text, so code is located in even indexes
      code_idx = seq(2, length(chunks), 2)
      token_list <- lapply(chunks[code_idx], get_tokens)
      chunk_names <- sapply(token_list, `[`, 1)
      names(chunks)[code_idx] <- sapply(token_list, `[`, 1)

      setups_to_remove <- character(0)

      for(i in 1:length(code_idx)) {
        tokens <- token_list[[i]]
        code_i <- code_idx[i]
        chunk <- chunks[[code_i]]

        if(any(grepl("=T$", tokens)))
          stop(
            "Use 'TRUE' rather than 'T' for learnR options, line ",
            min(attr(chunk, "lines")))

        if(any(grepl("^exercise=", tokens))) {
          if(tokens[1] == "") {
            stop(
              "Unnamed exercises detected, line ",
              min(attr(chunk, "lines")))
          }

          if(any(tokens == "exercise=tutorialize")) {
            # change to exercise = tutorialize to exercise = TRUE
            tokens[tokens == "exercise=tutorialize"] <- "exercise=TRUE"

            setup_token <- tokens[grepl("^exercise.setup=", tokens)]
            if(length(setup_token) == 0)
              setup_chunk <- character(0)
            else {
              setup_source <- strsplit(setup_token, "=")[[1]][2]
              source_idx <- which(names(chunks) == setup_source)
              if(length(source_idx) == 0)
                stop(
                  "The requested setup '",
                  setup_source,
                  "' is not a chunk name")
              if(any(!c("setup", "solution") %in% names(chunks[[setup_source]])))
                stop(
                  "The requested setup '",
                  setup_source,
                  "' must be an exercise and must appear before any reference to it.")

              # create a vector that will be added to.
              setup_code <- character(0)

              # there might be manually-defined setup_code for the chunk
              # referenced by the setup token.  If so, find it and pull the
              # assignment_ops
              referenced_manual_setup <- paste0(setup_source, "-setup")
              if(!is.null(chunks[[referenced_manual_setup]]) && !referenced_manual_setup %in% setups_to_remove)
                setup_code <- c(setup_code, get_assignment_ops(chunks[[referenced_manual_setup]]))
              # now parse the tutorialize setup chunk
              setup_code <- c(
                setup_code,
                get_assignment_ops(chunks[[setup_source]][["setup"]]))

              # now look for solution code in the referenced chunk
              solution_code <- get_assignment_ops(chunks[[setup_source]][["solution"]])
              setup_code <- c(setup_code, solution_code)

              # now, there could be a manual setup chunk for the current chunk.
              # If so, add that
              current_manual_setup <- paste0(tokens[1], "-setup")
              if(!is.null(chunks[[current_manual_setup]])) {
                current_manual_setup_chunk <- chunks[[current_manual_setup]]
                setup_code <- c(
                  setup_code,
                  current_manual_setup_chunk[!grepl("^```", current_manual_setup_chunk)])
                # tag the manual setup chunk for the current code for removal
                # because it's been added to the tutorialize setup chunk we are
                # building
                setups_to_remove <- c(setups_to_remove, current_manual_setup)
              }

              setup_chunk <- c(
                paste0("```{r ", tokens[1], "-setup}"),
                setup_code,
                "```",
                "")
              # get rid of setup token because each exercise chunk will have its
              # own setup
              tokens <- tokens[-which(tokens == setup_token)]
            }
            exercise_chunk <- c(
              paste0("```{r ", paste(tokens, collapse = ", "), "}"),
              get_starter_code(chunk),
              "```",
              "")

            solution_chunk <- get_solution_code(chunk, tokens[1])

            if(length(solution_chunk) > 0 )
              code_check_chunk <- c(
                paste0("```{r ", tokens[1], "-code-check}"),
                "grade_code()",
                "```")
            else
              code_check_chunk <- character(0)

            chunks[[code_i]] <- structure(
              list(
                setup = setup_chunk,
                exercise = exercise_chunk,
                solution = solution_chunk,
                code_check = code_check_chunk),
              lines = attr(chunk, "lines"))
          }
        }
      }
    }

    if(length(setups_to_remove) > 0)
      chunks <- chunks[-which(names(chunks) %in% setups_to_remove)]
    markdown <- unname(unlist(chunks))

    writeLines(unlist(chunks), outfile_path)
    cat("OK!\n")
  }
}

get_starter_code <- function(chunk) {
  chunk <- chunk[grepl("^#\\+", chunk)]
  if(length(chunk) == 0) return("")
  gsub("^#\\+ ?", "", chunk)
}

get_solution_code <- function(chunk, chunk_name) {
  chunk <- chunk[!grepl("^#\\+", chunk)]
  chunk <- chunk[!grepl("^```", chunk)]
  if(length(chunk[!grepl("^[[:space:]]*$", chunk)]) == 0)
    return(character(0))
  c(
    paste0("```{r ", chunk_name, "-solution}"),
    chunk,
    "```",
    ""
  )
}

get_assignment_ops <- function(chunk) {
  if(length(chunk) == 0) return(character(0))
  chunk <- chunk[!grepl("^```", chunk) & nchar(chunk) > 0]
  commands <- parse(text = chunk)
  command_strings <- lapply(commands, deparse)
  types <- sapply(commands, class)
  assign_ops <- types %in% c("<-", "=")
  assign_calls <- grepl("^assign\\(", sapply(command_strings, `[`, 1))
  unlist(command_strings[assign_ops | assign_calls])
}

get_tokens <- function(chunk) {
  chunk_def <- regmatches(chunk[1], gregexec("\\{([^\\}]*)\\}", chunk[1]))[[1]][2,1]
  # remove all spaces
  chunk_def <- gsub("[[:space:]]+", "", chunk_def)
  # remove "r"
  chunk_def <- gsub("^r,*", "", chunk_def)
  # tokenize
  tokens <- strsplit(chunk_def, ",")[[1]]
  if(length(tokens) == 0 || grepl("=", tokens[1])) tokens <- c("", tokens)
  tokens
}

get_token  <- function(parsed_chunk, name) {
  tokens <- attr(parsed_chunk, "tokens")
  ifelse(name %in% names(tokens), unname(tokens[name]), "")
}

parse_chunk <- function(chunk) {
  # find the chunk definition
  chunk_def <- regmatches(chunk[1], gregexec("\\{([^\\}]*)\\}", chunk[1]))[[1]][2,1]
  # remove all spaces
  chunk_def <- gsub("[[:space:]]+", "", chunk_def)
  # remove "r"
  chunk_def <- gsub("^r,*", "", chunk_def)
  # tokenize
  tokens <- strsplit(chunk_def, ",")[[1]]
  # if there are no tokens, return a blank name
  if(length(tokens) == 0) return(c(name = ""))

  tokens <- strsplit(tokens, "=")
  if(length(tokens[[1]]) == 1)
    tokens[[1]] <- c("name", tokens[[1]])
  else
    tokens <- c(list(c("name", "")), tokens)
  # find the non-name tokens
  structure(
    chunk[-c(1,length(chunk))],
    tokens = structure(sapply(tokens, `[`, 2), names = sapply(tokens, `[`, 1)),
    lines = attr(chunk, "lines")
  )
}
