#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom htmltools img a renderTags
#' @importFrom magrittr "%>%"

NULL

#' Create a navbar with the branding logos
#'
#' Reads the _site.yml to render the _navbar.html and replaces the navbar title by the branding logos.
#'
#' @param site_yml path to the `_site.yml` file.
#' 
#' @param logo_img vector containing the path to the logo file(s).
#' 
#' @param show_title boolean. If `TRUE`, the title will be shown next to the logo.
#'
#' @param relative_to relative path where main site files are located (`file.path(path, yaml_link)`): _e.g._ if navbar is used in a subfolder, adjust path to "..".
#'
#' @return NULL
#'
#' @export
create_navbar <- function(site_yml = file.path("site", "site.yml"), logo_img, show_title = FALSE, relative_to = NULL) {
  yaml <- rmarkdown:::yaml_load_file(site_yml)
  
  input_code <- glue::glue("<a class=\"navbar-brand\" href=\"index.html\">{yaml[[\"navbar\"]][\"title\"]}</a>")
  
  output_code <- map(logo_img, ~htmltools::img(src = ifelse(is.null(relative_to), ., file.path(relative_to, .)))) %>%
    htmltools::span() %>%
    htmltools::a(if_else(isTRUE(show_title), yaml[["navbar"]]["title"], NULL), class = "navbar-brand", href = "index.html") %>%
    htmltools::renderTags()
  
  output_code <- output_code[["html"]]
  navbar <- paste(readLines(rmarkdown::navbar_html(yaml[["navbar"]])), collapse = "\n")
  
  navbar <- sub(input_code, output_code, navbar)
  
  if (!is.null(relative_to)) {
    navbar <- stringr::str_replace_all(navbar, "href=\"", glue::glue("href=\"{relative_to}/")) # TODO: try to fix this in the yaml
  }
  
  writeLines(navbar, file.path(dirname(site_yml), "_navbar.html"))
}

