# The addin should be optional: Using the same trick as in 
# the bookdown mathquill addin to load it as an external script
deploy_production <- function() {
  sys.source(system.file("scripts", "deploy_production.R", package = "bs2site", mustWork = TRUE),
             new.env())
}

clean_build <- function() {
  sys.source(system.file("scripts", "clean_build.R", package = "bs2site", mustWork = TRUE),
             new.env())  
}

gitlab_config <- function() {
  sys.source(system.file("scripts", "gitlab_config.R", package = "bs2site", mustWork = TRUE),
             new.env())  
}
