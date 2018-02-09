local({
  if (RStudio.Version()[["version"]] < "1.1.57") stop("RStudio version >= 1.1.57 is required", call. = FALSE)
  
  sys.source(system.file("scripts", "private_key.R", package = "bs2site", mustWork = TRUE))
  
  config <- purrr::map(rlang::set_names(c("gitlab_url", "gitlab_project_id", "gitlab_keyring", "gitlab_deploy_job", "gitlab_disable_confirm")),
                       rstudioapi::getPersistentValue)
  
  config <- c(config, gitlab_private_token = get_private_token(config$gitlab_url, config$gitlab_keyring))
  
  # We keep the project_id as a character and are not checking if it's a number: the request will fail
  if (any(map_lgl(config, is_null)) || any(nchar(config) == 0)) {
    rstudioapi::showDialog("Error", "Adjust your gitlab settings", "")
    stop("Invalid gitlab configuration", call. = FALSE)
  }
  
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
