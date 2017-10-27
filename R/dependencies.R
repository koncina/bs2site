#' @importFrom devtools install_cran install_github
#' @importFrom glue glue
#' @importFrom tibble enframe deframe
#' @importFrom yaml yaml.load_file
#' @importFrom purrr set_names map map_at map2 map2_lgl iwalk invoke invoke_map is_list
#' @importFrom readr read_file
#' @importFrom assertthat assert_that
#' @importFrom utils available.packages
#' @import dplyr tidyr

NULL
utils::globalVariables(c("content", "packages", "package", "name", "is_installed", "Package", "Version", "biocLite"))
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
  utils::available.packages(repos = repos) %>%
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

init_variables <- function(source = "cran", version = "0", ...) {
  tibble(source = source, version = as.character(version), args = list(list(...)))
}

#' Scan the site files for used packages
#'
#' Will extract all loaded packages using `library()`, `require()` or direct calls `::`.
#'
#' @param path folders containing the Rmd files (defaults to `c("TD", "lectures", "site")`).
#'
#' @return path to the Rmd files
#'
#' @export
scan_packages <- function(path = c("TD", "lectures", "site")) {
  if (!all(dir.exists(path))) stop("One or more provided folders do not exist")

  # Code found here: https://www.kaggle.com/drobinson/analysis-of-r-packages-on-stack-overflow-over-time
  # Link from Aurélien
  reg <- "(?:library|require)\\([\"\']?([\\.a-zA-Z\\d]+).*?[\"\']?\\)|([\\.a-zA-Z\\d]+)::[\\._a-zA-Z\\d]+[\\(|:]"

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

inst_cran <- function(package, ...) {
  message("Installing missing packages from CRAN")
  devtools::install_cran(package, ask = FALSE)
}


inst_bioconductor <- function(package, ...) {
  message("Installing missing packages from Bioconductor")
  source("https://bioconductor.org/biocLite.R")
  biocLite(package, ask = FALSE)
}

inst_github <- function(package, args, ...) {
  # Let's get the package name into the list 
  # paste it to username
  # and rename username to repo
  message("Installing missing packages from Github")
  args <- map2(args, package, ~map_at(., "username", paste0, "/", .y)) %>% 
    map(~set_names(., function(x) if_else(x == "username", "repo", x)))
  purrr::invoke_map(devtools::install_github, args)
}

#' Install missing packages
#'
#' Once the required packages (defined in `packages.yml`) are listed and checked, installs the missing ones.
#'
#' @param pkg_list path to the `packages.yml` file (defaults to `packages.yml`)
#' 
#' @param path path to the Rmd to be scanned for used packages. If `NULL`, no files are parsed.
#' 
#' @return a tibble with an updated `is_installed` column to confirm the success of all installations.
#'
#' @export
install_missing <- function(pkg_list = "packages.yml", path = c("TD", "lectures", "site")) {
  
  .df <- wanted_pkg(pkg_list)
  
  if (!is_null(path)) {
    .df <- bs2site::scan_packages(path) %>%
      distinct(package) %>%
      filter(!package %in% .df$package) %>%
      mutate(.init = list(init_variables())) %>%
      unnest(.init) %>%
      mutate(is_installed = map_lgl(package, devtools:::is_installed)) %>%
      bind_rows(.df)
  }

  .df %>% 
    filter(!is_installed, source != "ignore") %>%
    group_by(source) %>%
    nest() %>%
    deframe() %>%
    set_names(glue::glue("inst_{names(.)}")) %>%
    iwalk(~invoke(.y, .x))
    
  .df <- .df %>%
    mutate(is_installed = map2_lgl(package, version, devtools:::is_installed))
  
  if (any(!filter(.df, source != "ignore") %>% pull(is_installed))) {
    print(.df)
    stop("One or more required package(s) are still not installed")
  }
  
  .df
}

#' Lists the required packages
#'
#' The `packages.yml` file contains a list of packages that are required.
#'
#' @param pkg_list path to the `packages.yml` file (defaults to `packages.yml`)
#'
#' @return A tibble listing the package, the version, whether it is already installed or not.
#'
#' @export
wanted_pkg <- function(pkg_list = "packages.yml") {
  if (!file.exists(pkg_list)) {
    warning(glue::glue("The dependencies list `{pkg_list}` was not found"), call. = FALSE)
    return()
  }
  
  yaml::yaml.load_file(pkg_list) %>%
    walk(~assertthat::assert_that(is_list(.), msg = glue::glue("`{pkg_list}` should only contain lists"))) %>%
    map(~invoke(init_variables, .)) %>%
    enframe("package", "details") %>%
    unnest() %>%
    mutate(is_installed = map2_lgl(package, version, devtools:::is_installed))
}
