#' @import curl
#' @importFrom glue glue
#' @import jsonlite
#' @importFrom rlang set_names
#' @importFrom purrr map map_lgl is_null

NULL
utils::globalVariables(c("pipeline.id", "stage", "status", "artifacts_file.filename", "artifacts_file.size"))

# Using the adding should be optional: using the trick like in 
# the  bookdown mathquill addin to load it as an external script
deploy_production <- function() {
  sys.source(system.file("scripts", "deploy_production.R", package = "bs2site", mustWork = TRUE),
             new.env())
}


gitlab_config <- function() {
  sys.source(system.file("scripts", "gitlab_config.R", package = "bs2site", mustWork = TRUE),
             new.env())  
}

# non interactive function to interact with the gitlab API
gitlab_manual_job <- function(gitlab_url, project_id, deploy_jobname, private_token) {
  my_handle <- curl::new_handle()
  handle_setheaders(my_handle, .list = list(`PRIVATE-TOKEN` = private_token))
  
  resp <- glue::glue("{gitlab_url}/api/v4/projects/{project_id}/jobs") %>%
    curl_fetch_memory(my_handle)
  
  if (resp[["status_code"]] != 200) stop(glue::glue("Server returned unexpected error code: {resp[['status_code']]}"), call. = FALSE)
  
  ci_pipeline <- rawToChar(resp[["content"]]) %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    filter(pipeline.id == max(pipeline.id))
  
  job_id <- filter(ci_pipeline, name == deploy_jobname) %>%
    pull(id)
  
  #build <- select(ci_pipeline, stage, status, artifacts_file.filename, artifacts_file.size) %>%
  build <- filter(ci_pipeline, stage == "build", status != "canceled")
  
  if (build[["status"]] != "success") stop(glue::glue("Build is missing with status {build[['status']]}"))
  
  if (!c("artifacts_file.filename", "artifacts_file.size") %in% names(build) ||
      any(is.na(build[["artifacts_file.filename"]]), is.na(build[["artifacts_file.size"]]))) {
    stop("Artifact is missing. Try to rebuild the website", call. = FALSE)
    }
  
  handle_setopt(my_handle, customrequest = "POST")
  
  resp <- glue::glue("{gitlab_url}/api/v4/projects/{project_id}/jobs/{job_id}/play") %>%
    curl_fetch_memory(my_handle)
}