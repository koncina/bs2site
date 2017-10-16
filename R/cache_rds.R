#' @importFrom readr read_rds write_rds
#' @importFrom purrr possibly walk is_null
#' @importFrom glue glue
#' @importFrom rlang global_env eval_tidy quo_is_null enquo quo_name
#' @importFrom knitr current_input
#' @importFrom tools file_path_sans_ext
#' @importFrom stringr str_match
#' @importFrom utils installed.packages

# Expanding from the first purrr::possibly only version to a more complete one:
# Check if rstudioapi is available: we do not want the function to fail on a headless installation
# Generate some error messages if the path cannot be retrieved...
get_active_document <- function() {
  if (isTRUE(getOption('knitr.in.progress'))) {
    path <- knitr::current_input()
  } else if ("rstudioapi" %in% rownames(installed.packages())) {
    current <- purrr::possibly(rstudioapi::getActiveDocumentContext, otherwise = list(path = ""))()[c("id", "path")]
    if (current[["id"]] == "#console" & current[["path"]] == "") stop("active document cannot be detected within the console", call. = FALSE)
    else path <- current[["path"]]
  }
  if (path == "") stop("could not detect path. Is the active document already saved?", call. = FALSE)
  path
}

get_cache_subfolder <- function() {
  file_name <- basename(get_active_document())
  n <- stringr::str_match(file_name, "^[_]?([[:alpha:]]+\\d+).*\\.Rmd")
  folder_name <- n[,2]
  if (is.na(folder_name)) folder_name <- gsub( "^_", "", tools::file_path_sans_ext(file_name))
  folder_name
}

rds_to_object <- function(path) {
  if (!file.exists(path)) stop(glue::glue("object is not in cache ({path})"), call. = FALSE)
  message(glue::glue("reading object from {path}"))
  assign(tools::file_path_sans_ext(basename(path)),
         readr::read_rds(path),
         envir = rlang::global_env())
}


#' Write objects to the website template cache folder
#'
#' Use this function to store objects as rds files in a standardised location and speed up knitting.
#'
#' @param object object to save.
#'
#' @param cache_folder defaults to `_cache` subfolder.
#'
#' @param ... Additional parameters passed to `readr::write_rds()` such as compression arguments.
#'
#' @return `write_cache()` returns the input object invisibly
#'
#' @export
write_cache <- function(object, cache_folder = "_cache", ...) {
  object <- enquo(object)
  cache_folder <- file.path(cache_folder, get_cache_subfolder())
  dir.create(cache_folder, showWarnings = FALSE, recursive = TRUE)
  output_file <- file.path(cache_folder, paste0(quo_name(object), ".rds"))
  readr::write_rds(rlang::eval_tidy(object), output_file, ...)
  message(glue::glue("saved object to {output_file}"))
  invisible(rlang::eval_tidy(object))
}

#' Read objects from the website template cache folder
#'
#' Use this function to load objects from rds files saved in a standardised location.
#'
#' @param object object to load. If missing, `read_cache()` will load all objects found in the cache folder.
#'
#' @param cache_folder defaults to `_cache` subfolder.
#'
#' @return `read_cache()` returns the relative path to the rds file invisibly.
#'
#' @export
read_cache <- function(object = NULL, cache_folder = "_cache") {
  object <- enquo(object)
  cache_folder <- file.path(cache_folder, get_cache_subfolder())
  # Replacing quo_is_missing by quo_is_null to set a default argument value.
  if (rlang::quo_is_null(object)) {
    cached_files <- list.files(cache_folder, pattern = ".rds", full.names = TRUE)
  } else {
    cached_files <- file.path(cache_folder, paste0(quo_name(object), ".rds"))
  }
  if (length(cached_files) > 0) {
    walk(cached_files, rds_to_object)
  } else {
    stop(glue::glue("could not find cached objects for this document in '{cache_folder}'"), call. = FALSE)
  }
}
