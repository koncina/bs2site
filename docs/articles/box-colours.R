## ----setup, include = FALSE----------------------------------------------
library(dplyr)
library(purrr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- results = 'asis', echo = FALSE-------------------------------------
cat(bs2site:::create_box_css(path = NULL))

## ---- echo = FALSE-------------------------------------------------------
bs2site:::get_box_colours() %>%
  select(name, description) %>%
  knitr::kable()

## ---- echo = FALSE-------------------------------------------------------
draw_box <- function(colour, outline = FALSE) {
  htmltools::h3("Header") %>% htmltools::div(class = glue::glue("box {colour} {if_else(outline, 'outline', '')}"),
                                             "Some text using", htmltools::br(),
                                             htmltools::code(paste0(colour, if_else(outline, ' outline', '')))
                                             )
}

## ---- echo = FALSE, results = "asis"-------------------------------------
colours <- pull(bs2site:::get_box_colours(), name)

purrr::map(colours, draw_box) %>%
  htmltools::div(class = "box-container") %>%
  htmltools::doRenderTags()

## ---- echo = FALSE, results = "asis"-------------------------------------
purrr::map(colours, draw_box, outline = TRUE) %>%
  htmltools::div(class = "box-container") %>%
  htmltools::doRenderTags()

