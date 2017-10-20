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
#' @param logo_img path to the logo files
#'
#' @return NULL
#'
#' @export
create_navbar <- function(site_yml = file.path("site", "site.yml"), logo_img) {
  yaml <- rmarkdown:::yaml_load_file_utf8(site_yml)
  
  input_code <- glue::glue("<a class=\"navbar-brand\" href=\"index.html\">{yaml[[\"navbar\"]][\"title\"]}</a>")
  
  output_code <- map(logo_img, ~htmltools::a(htmltools::img(src = .), class = "navbar-brand", href = "index.html")) %>%
    htmltools::renderTags()
  output_code <- output_code[["html"]]
  navbar <- paste(readLines(rmarkdown::navbar_html(yaml[["navbar"]])), collapse = "\n")
  
  writeLines(sub(input_code, output_code, navbar), file.path(dirname(site_yml), "_navbar.html"))
}

