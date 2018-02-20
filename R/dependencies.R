#' @importFrom devtools install_cran install_github
#' @importFrom glue glue
#' @importFrom tibble enframe deframe
#' @importFrom yaml yaml.load_file
#' @importFrom purrr set_names map map_at map2 map2_lgl iwalk imap invoke invoke_map is_list is_empty is_character
#' @importFrom readr read_file
#' @importFrom assertthat assert_that
#' @importFrom utils available.packages
#' @import dplyr tidyr

NULL
utils::globalVariables(c("content", "packages", "package", "name", "filename", "is_pkg_installed", "Package", "Version", "biocLite"))
# Helper functions

# Adjusting devtools:::is_installed
# don't use NULL for lib.loc: installation of a loaded library might not be detected
is_installed <- function(pkg, version = 0) {
  installed_version <- tryCatch(utils::packageVersion(pkg, lib.loc = .libPaths()), error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

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
get_active_files <- function(path, recursive = TRUE) {
  list.files(path, pattern = ".*\\.Rmd", full.names = TRUE, recursive = recursive) %>%
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
#' @param path folders containing the Rmd files.
#' 
#' @param recursive logical should Rmd files be searched recursively
#'
#' @return path to the Rmd files
#'
pkg_scan <- function(path, recursive = TRUE) {
  if (!all(dir.exists(path))) stop("One or more provided folders do not exist")

  # Code found here: https://www.kaggle.com/drobinson/analysis-of-r-packages-on-stack-overflow-over-time
  # Link from Aurélien
  reg <- "(?:library|require)\\([\"\']?([\\.a-zA-Z\\d]+).*?[\"\']?\\)|([\\.a-zA-Z\\d]+)::[\\._a-zA-Z\\d]+[\\(|:]"

  # To avoid potential false positives, I changed the regex to expect a ( or :
  # i.e. explicit call like `package::my_function()`
  #  or a knitr format like `package::format:`

  get_active_files(path, recursive = recursive) %>%
    set_names() %>%
    set_names(basename) %>%
    enframe("filename", "path") %>%
    mutate(content = map(path, read_file),
           content = map(content, stringr::str_replace_all, "#.*\n", "\n"),
           packages = stringr::str_match_all(content, reg),
           package = map(packages, ~ c(.[, 2:3]))) %>%
    unnest(package, .drop = TRUE) %>%
    select(-path) %>%
    filter(!is.na(package), package != "") %>%
    distinct(filename, package) %>%
    group_by(package) %>%
    summarise(filename =  glue::collapse(filename, sep = ", "))
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

pkg_yaml <- function(pkg_list = "packages.yml") {
  if (!file.exists(pkg_list)) {
    warning(glue::glue("The dependencies list `{pkg_list}` was not found"), call. = FALSE)
    return(tibble(package = character(0), source = character(0), version = double(0)))
  }
  
  yaml::yaml.load_file(pkg_list) %>%
    walk(~assertthat::assert_that(is_list(.), msg = glue::glue("`{pkg_list}` should only contain lists"))) %>%
    map(~invoke(init_variables, .)) %>%
    enframe("package", "details") %>%
    unnest()
}

# Adapted from devtools:::pkg_source() 
get_pkg_source <- function(pkg) {
  if (length(pkg) != 1) stop("expecting the name of a single package...", call. = FALSE)
  desc <- suppressWarnings(utils::packageDescription(pkg))
  
  version <- "0"
  args <- list()
  
  if (!is.list(desc)) {
    source <- NA_character_
  } else if (identical(desc$Priority, "base")) {
    source <- "base"
    version <- desc$Version
  } else if (!is.null(desc$biocViews)) {
    source <- "bioconductor"
    version <- desc$Version
  } else if (!is.null(desc$GithubSHA1)) {
    source <- "github"
    version <- desc$Version
    args <- list(
      username = desc$GithubUsername,
      ref = desc$GithubRef # or desc$GithubSHA1 to be more conservative...
    )
  } else if (!is.null(desc$Repository) &&  desc$Repository == "CRAN") {
    source <- "cran"
    version <- desc$Version
  } else {
    if (!is.null(desc$Version)) warning("unsupported remote")
    source <- NA_character_
    args <- list()
  }
  tibble(source = source, version = version, args = list(args))
}

#' Lists the required packages
#'
#' The `packages.yml` file contains a list of packages that are required.
#'
#' @param path character vector pointing to the root of the bs2 repository containing the optional `packages.yml` file.
#' 
#' @param install boolean telling whether to install missing packages (defaults to `FALSE`)
#' 
#' @param scan boolean whether to scan source files for used packages (defaults to `TRUE`)
#' 
#' @param rmd_dir character vector containing the subfolders with the Rmd source files.
#' 
#' @param recursive logical should Rmd files be searched recursively
#' 
#' @return A tibble listing the package, the version, whether it is already installed or not.
#'
#' @export
pkg_list <- function(path = ".", install = FALSE, scan = TRUE, rmd_dir = c("TD", "lectures", "site"), recursive = TRUE) {
  .df <- pkg_yaml(file.path(path, "packages.yml"))
  if (isTRUE(scan)) .df <- full_join(.df, pkg_scan(file.path(path, rmd_dir), recursive = recursive),
                                     by = "package")
  .df
                   
}

#' @export
pkg_missing <- function(path = ".", install = FALSE, scan = TRUE, rmd_dir = c("TD", "lectures", "site"), recursive = TRUE) {
  
  # Checking if install is a character vector referring to an environmental variable
  if (is_character(install, 1)) {
    if (toupper(Sys.getenv(install)) %in% c("1", "T", "TRUE", "YES")) install <- TRUE
    else install <- FALSE
  }
  
  missing <- pkg_list(path = path, rmd_dir = rmd_dir, scan = scan, recursive = recursive) %>%
    mutate(version = replace(version, is.na(version), 0),
           source = replace(source, is.na(source), "cran"),
           is_pkg_installed = map2_lgl(package, version, is_installed)) %>% 
    filter(!is_pkg_installed,
           source %in% c("github", "bioconductor", "cran"))
  
  not_on_cran <- missing %>%
    filter(source == "cran") %>% 
    anti_join(get_available_packages(), by = "package")
  
  if (nrow(not_on_cran) > 0) {
    not_on_cran <- not_on_cran %>%
      select(package, filename) %>% 
      deframe() %>% 
      imap(~glue::glue("  {.y} (detected in {.x})")) %>%
      glue::collapse(sep = "\n")
    
    stop(glue::glue("One or more packages are not available on CRAN\n{not_on_cran}"), call. = FALSE)
  }
  
  if (isTRUE(install)) {
    missing %>%
      group_by(source) %>%
      nest() %>%
      deframe() %>%
      set_names(glue::glue("inst_{names(.)}")) %>%
      iwalk(~invoke(.y, .x))
    }
  
  # Final check: is everything available?
  missing <- missing %>%
    mutate(is_pkg_installed = map2_lgl(package, version, is_installed)) %>%
    filter(!is_pkg_installed) %>%
    pull(package) %>%
    glue::collapse(sep = ", ")
  
  if (!is_empty(missing)) {
    stop(glue::glue("The following packages are missing: {missing}\n Try to adjust install = TRUE"), call. = FALSE)
  }
}