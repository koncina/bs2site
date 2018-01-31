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
  knitr::kable()

## ---- echo = FALSE-------------------------------------------------------
draw_box <- function(colour) {
  htmltools::h3("Header") %>% htmltools::div(class = glue::glue("box {colour}"),
                                             "Some text using", htmltools::br(),
                                             htmltools::code(colour)
                                             )
}

## ---- echo = FALSE, results = "asis"-------------------------------------
colours <- pull(bs2site:::get_box_colours(), name)

glue::glue("bg-{colours}") %>%
  purrr::map(draw_box) %>%
  htmltools::div(class = "box-container") %>%
  htmltools::doRenderTags()

