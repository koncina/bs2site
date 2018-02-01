#' @importFrom purrr map transpose
#' @importFrom tibble tribble
#' @importFrom dplyr select

NULL

#' Use box colours defined for the rworkshop or biostat2 site
#'
#' Generates the `css/box.css` file and changes the css setting in `lectures/_output.yml` if required.
#'
#'@export
use_box_colours <- function() {
  if (!all(file.exists(file.path("lectures", "_output.yml")), dir.exists("css"))) stop("Run this function from the root of a bs2site formatted site", call. = FALSE)
  create_box_css(overwrite = TRUE)
  output_yaml <- yaml::read_yaml(file.path("lectures", "_output.yml"))
  output_yaml[["iosp::ioslides_plus"]][["css"]] <- unique(c(output_yaml[["iosp::ioslides_plus"]][["css"]], "../css/box.css"))
  yaml::write_yaml(output_yaml, file.path("lectures", "_output.yml"))
}

get_box_colours <- function() {
  tribble(~name, ~bg, ~header_bg, ~text, ~header_text, ~description,
          "intro", "#adebad", "#1f7a1f", NULL, NULL, "learning objectives/introduction",
          "warning", "#ffad99", "#991f00", NULL, NULL, "error / warning",
          "advice", "#ffec8b", "#eeb422", NULL, NULL, "advice",
          "code", "#2b557a", "#002240", NULL, NULL, "code examples",
          "practical", "#CAE1FF", "#65707F", NULL, NULL, "practicals / exercises") %>%
    mutate(.prefix = "")
}

create_box_css <- function(path = file.path("css", "box.css"), overwrite = FALSE) {
  
  css <- get_box_colours() %>%
    select(-description) %>%
    transpose() %>%
    purrr::invoke_map(.f = iosp::add_box_colour) %>%
    glue::collapse(sep = "\n") %>%
    paste(".box.outline {\n\tbackground-color: white;\n}\n")

  #css <- purrr::invoke_map(iosp::add_box_colour, colours) %>%
  #  glue::collapse(sep = "\n")
  
  if (is.null(path)) return(css)
  if (file.exists(path)) {
    if (isTRUE(overwrite)) {
      warning(glue::glue("overwriting {path}..."), call. = FALSE)
    } else {
      stop("file already exists: you might want to adjust `overwrite = TRUE`", call. = FALSE)
    }
  }
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  readr::write_file(css, path)
}

