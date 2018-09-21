local({
  config <- get_gitlab_settings()
  
  pipeline_token <- gitlab_trigger(config$gitlab_url, config$gitlab_private_token, config$gitlab_project_id)
  
  if (is.na(pipeline_token)) {
    if (!rstudioapi::showQuestion(title = "Create trigger?", ok = "Yes", cancel = "No", "Do you want to create a trigger token?"))
      stop("Did not trigger the build. No matching token.", call. = FALSE)
    
    pipeline_token <- purrr::pluck(gitlab_create_trigger(config$gitlab_url, config$gitlab_private_token, config$gitlab_project_id), "token")
  }
  
  # Try to detect the current branch
  current_branch <- purrr::possibly(system, otherwise = NULL)("git rev-parse --abbrev-ref HEAD", inter = TRUE)
  
  if (is.null(current_branch)) {
    current_branch <- "master"
    warning("Could not detect current branch name: triggering a new build for `master`")
  }
  
  gitlab_curl(glue::glue("{config$gitlab_url}/api/v4/projects/{config$gitlab_project_id}/trigger/pipeline"),
              config$gitlab_private_token,
              status_code = 201,
              post = TRUE,
              forms = list(`variables[CLEAN]` = "true",
                           ref = current_branch,
                           token = pipeline_token))

})
