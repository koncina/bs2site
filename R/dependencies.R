#' @importFrom glue glue
#' @importFrom tibble enframe
#' @importFrom yaml yaml.load_file
#' @importFrom purrr set_names map
#' @importFrom readr read_file
#' @import dplyr tidyr

# Helper functions

# Walk only if a condition is met
walk_if <- function(.x, .condition, .f, ...) {
  if (!.condition) return(invisible(.x))
  walk(.x, .f, ...)
}

# return the subset not matching the pattern
str_subset_inv <- function(string, pattern) {
  string[!stringr::str_detect(string, pattern)]
}

# Return a tibble from utils::installed.packages()
get_installed_packages <- function() {
  installed.packages()[,c(1,3)] %>%
    as_tibble() %>%
    rename(package = Package,
           version = Version)
}

# Return a tibble from utils::available.packages()
get_available_packages <- function(repos = "https://cran.rstudio.com") {
  available.packages(repos = repos) %>%
    as_tibble() %>%
    select(package = Package,
           version = Version)
}

# List all Rmd files in path not starting with an underscore (hidden)
get_active_files <- function(path) {
  list.files(path, pattern = ".*\\.Rmd", full.names = TRUE, recursive = TRUE) %>%
    str_subset_inv(".*/_.*") %>%
    normalizePath()
}

#' Scan the site files for used packages
#'
#' Will extract all loaded packages using `library()`, `require()` or direct calls `::`.
#'
#' @param path folders containing the Rmd files.
#'
#' @return path to the Rmd files
#'
#' @export
scan_packages <- function(path = c("TD", "lectures", "site"), .root = ".") {
  path <- file.path(.root, path)
  if (!all(dir.exists(path))) stop("One or more provided folders do not exist")

  # Code found here: https://www.kaggle.com/drobinson/analysis-of-r-packages-on-stack-overflow-over-time
  # Link from Aurélien
  reg <- "(?:library|require)\\([\"\']?(.*?)[\"\']?\\)|([\\.a-zA-Z\\d]+)::[\\._a-zA-Z\\d]+[\\(|:]"

  # To avoid potential false positives, I changed the regex to expect a ( or :
  # i.e. explicit call like `package::my_function()`
  #  or a knitr format like `package::format:`

  get_active_files(path) %>%
    set_names() %>%
    set_names(basename) %>%
    enframe("name", "path") %>%
    mutate(content = map(path, read_file),
           content = map(content, stringr::str_replace_all, "#.*\n", "\n"),
           packages = stringr::str_match_all(content, reg),
           package = map(packages, ~ c(.[, 2:3]))) %>%
    unnest(package, .drop = TRUE) %>%
    select(-path) %>%
    filter(!is.na(package), package != "") %>%
    distinct(name, package)
}

#' @export
wanted_pkg <- function(pkg_list = "packages.yml") {
  if (!file.exists(pkg_list)) {
    warning(glue::glue("The dependencies list `{pkg_list}` was not found"), call. = FALSE)
    return()
  }

  init_variables <- function(source = NA, version = "0", ...) {
    tibble(source = source, version = version, args = list(list(...)))
  }
  
  yaml::yaml.load_file(pkg_list) %>%
    map(~invoke(init_variables, .)) %>%
    enframe("package", "details") %>%
    unnest() %>%
    mutate(is_installed = map2_lgl(package, version, devtools:::is_installed))
}


