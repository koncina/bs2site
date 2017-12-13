#' @import curl
#' @importFrom glue glue
#' @import jsonlite
#' @importFrom rlang set_names
#' @importFrom purrr map map_lgl is_null

NULL
utils::globalVariables(c("pipeline.id", "stage", "status", "artifacts_file.filename", "artifacts_file.size"))

# The addin should be optional: Using the same trick as in 
# the bookdown mathquill addin to load it as an external script
deploy_production <- function() {
  sys.source(system.file("scripts", "deploy_production.R", package = "bs2site", mustWork = TRUE),
             new.env())
}


gitlab_config <- function() {
  sys.source(system.file("scripts", "gitlab_config.R", package = "bs2site", mustWork = TRUE),
             new.env())  
}
