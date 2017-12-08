#' @importFrom rstudioapi showDialog showQuestion getPersistentValue setPersistentValue
#' @importFrom shiny textInput runGadget dialogViewer observeEvent stopApp
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @import curl
#' @importFrom glue glue
#' @import jsonlite
#' @importFrom rlang set_names
#' @importFrom purrr map map_lgl is_null

NULL

#' @export
deploy_production <- function() {
  config <- map(set_names(c("gitlab_url", "gitlab_project_id", "gitlab_private_token", "gitlab_deploy_job")),
                   rstudioapi::getPersistentValue)
  
  # We keep the project_id as a character and are not checking if it's a number: the request will fail
  if (any(map_lgl(config, is_null)) || any(nchar(config) == 0)) {
    rstudioapi::showDialog("Error", "Adjust your gitlab settings", "")
    stop("Invalid gitlab configuration", call. = FALSE)
    }
  
  if (!rstudioapi::showQuestion(title = "Confirm", ok = "OK", cancel = "Cancel", "Are you sure you want to deploy to production?")) return()
  
  run_manual(config$gitlab_url, config$gitlab_project_id, config$gitlab_deploy_job, config$gitlab_private_token)
}

deploy_config <- function() {
  my_textInput <- function(inputId, label) {
    textInput(inputId, label, value = rstudioapi::getPersistentValue(inputId))
  }
  ui <- miniPage(
    gadgetTitleBar("Set gitlab settings"),
    
    miniContentPanel(
      my_textInput("gitlab_url", "Gitlab url"),
      my_textInput("gitlab_project_id", "Project ID (number)"),
      my_textInput("gitlab_private_token", "Private token"),
      my_textInput("gitlab_deploy_job", "Deploy job name")
    )
  )
  
  server <- function(input, output) {
    observeEvent(input$done, {
      map(set_names(c("gitlab_url", "gitlab_project_id", "gitlab_private_token", "gitlab_deploy_job")), 
          ~rstudioapi::setPersistentValue(.x, input[[.x]]))
      stopApp()
    })
    observeEvent(input$cancel, {
      stopApp()
    })
  }
  
  runGadget(ui, server, viewer = dialogViewer("bs2site gitlab settings", height = 200))
}


run_manual <- function(gitlab_url, project_id, deploy_jobname, private_token) {
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
  
  build <- select(ci_pipeline, stage, status, artifacts_file.filename, artifacts_file.size) %>%
    filter(stage == "build", status != "canceled")
  
  if (build[["status"]] != "success") stop(glue::glue("Build is missing with status {build[['status']]}"))
  
  if (any(is.na(build[["artifacts_file.filename"]]), is.na(build[["artifacts_file.size"]]))) stop("Artifact is missing. Try to rebuild the website")
  
  handle_setopt(my_handle, customrequest = "POST")
  
  resp <- glue::glue("{gitlab_url}/api/v4/projects/{project_id}/jobs/{job_id}/play") %>%
    curl_fetch_memory(my_handle)
}