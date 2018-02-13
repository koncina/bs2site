#' @import curl
#' @importFrom glue glue
#' @import jsonlite
#' @importFrom rlang set_names
#' @importFrom purrr map map_lgl is_null pluck

NULL

utils::globalVariables(c("pipeline.id", "stage", "status", "artifacts_file.filename", "artifacts_file.size", "return_value", "owner.id", "description"))

# Some helper functions to interact with the Gitlab API

# Status codes adapted from http://docs.gitlab.com/ce/api/#status-codes
gitlab_status <- function(status_code) {
  tibble::tribble(
    ~return_value, ~short_description, ~description,
    200, "OK", "The GET, PUT or DELETE request was successful, the resource(s) itself is returned as JSON.",
    204, "No Content", "The server has successfully fulfilled the request and that there is no additional content to send in the response payload body.",
    201, "Created", "The POST request was successful and the resource is returned as JSON.",
    304, "Not Modified", "Indicates that the resource has not been modified since the last request.",
    400, "Bad Request", "A required attribute of the API request is missing, e.g., the title of an issue is not given.",
    401, "Unauthorized", "The user is not authenticated, a valid user token is necessary.",
    403, "Forbidden", "The request is not allowed, e.g., the user is not allowed to delete a project.",
    404, "Not Found", "A resource could not be accessed, e.g., an ID for a resource could not be found.",
    405, "Method Not Allowed", "The request is not supported.",
    409, "Conflict", "A conflicting resource already exists, e.g., creating a project with a name that already exists.",
    412, "Denied", "Indicates the request was denied. May happen if the If-Unmodified-Since header is provided when trying to delete a resource, which was modified in between.",
    422, "Unprocessable", "The entity could not be processed.",
    500, "Server Error", "While handling the request something went wrong server-side."
  ) %>%
    filter(return_value == status_code)
}

# Generate a verbose error if the status code of the curl response is not the expected one
# returns the curl response (can be used in a pipeline)
expect_status <- function(response, status_code) {
  if (response[["status_code"]] != status_code) {
    stop(with(gitlab_status(response[["status_code"]]),
              glue::glue("Server returned unexpected error code: {return_value} ({short_description}).\n{description}"), call. = FALSE))
  }
  invisible(response)
}

# Set the curl handle to authentificate with the gitlab API
gitlab_handle <- function(private_token) {
  curl::new_handle() %>%
    handle_setheaders(.list = list(`PRIVATE-TOKEN` = private_token))
}

#

gitlab_curl <- function(url, private_token, status_code = 200, post = FALSE, forms = list()) {
  
  h <- gitlab_handle(private_token)
  if (isTRUE(post)) {
    if (!is_list(forms)) stop("forms should be provided as named list members")
    curl::handle_setopt(h, customrequest = "POST")
    invoke(curl::handle_setform, c(h, forms))
  }
  
  curl_fetch_memory(url, h) %>%
    expect_status(status_code) %>%
    with(rawToChar(content)) %>%
    jsonlite::fromJSON(flatten = TRUE)
}

# Get the gitlab project IDs
gitlab_projects <- function(gitlab_url, private_token) {
  glue::glue("{gitlab_url}/api/v4/projects?membership=true") %>%
    gitlab_curl(private_token) %>%
    mutate(name = glue::glue("{name} ({id})")) %>%
    select(name, id) %>%
    deframe()
}

# Get the jobs listed in the last used pipeline (highest ID)
gitlab_jobs <- function(gitlab_url, private_token, project_id) {
  glue::glue("{gitlab_url}/api/v4/projects/{project_id}/jobs") %>%
    gitlab_curl(private_token) %>%
    filter(ref == "master") %>%
    top_n(1, pipeline.id)
}

# Get the trigger token for the project (if more than one is available, let it fail)
gitlab_trigger <- function(gitlab_url, private_token, project_id, trigger_description = "bs2site") {
  
  # Get the current user id associated to the private token
  user_id <- glue::glue("{gitlab_url}/api/v4/user") %>%
    gitlab_curl(private_token) %>%
    pluck("id")
  
  response <-  gitlab_curl(glue::glue("{gitlab_url}/api/v4/projects/{project_id}/triggers"), private_token) 
  
  if (purrr::is_empty(response)) return(NA) # No token is defined
  
  token <- response %>%
    filter(owner.id == user_id, description == trigger_description) %>%
    pull(token)
  
  if (length(token) > 1) warning(glue::glue("Found multiple matching tokens ({length(token)}): using the first"), call. = FALSE)
  
  token[1]
}

gitlab_create_trigger <- function(gitlab_url, private_token, project_id, trigger_description = "bs2site") {
  glue::glue("{gitlab_url}/api/v4/projects/{project_id}/triggers") %>%
    gitlab_curl(private_token, status_code = 201, post = TRUE, forms = list(description = trigger_description))
}

# non interactive function to interact with the gitlab API
gitlab_manual_job <- function(gitlab_url, private_token, project_id, deploy_jobname) {
  
  ci_pipeline <- gitlab_jobs(gitlab_url, private_token, project_id)
  
  job_id <- filter(ci_pipeline, name == deploy_jobname) %>%
    top_n(1, id) %>%
    pull(id)
  
  build <- top_n(filter(ci_pipeline, stage == "preview", ref == "master", status != "canceled"), 1, id)
  
  if (build[["status"]] != "success") stop(glue::glue("Build is missing with status {build[['status']]}"))
  
  if (!c("artifacts_file.filename", "artifacts_file.size") %in% names(build) ||
      any(is.na(build[["artifacts_file.filename"]]), is.na(build[["artifacts_file.size"]]))) {
    stop("Artifact is missing. Try to rebuild the website", call. = FALSE)
  }
  
  glue::glue("{gitlab_url}/api/v4/projects/{project_id}/jobs/{job_id}/play") %>%
    gitlab_curl(private_token, post = TRUE)
}
