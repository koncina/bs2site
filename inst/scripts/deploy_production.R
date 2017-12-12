local({
  config <- purrr::map(rlang::set_names(c("gitlab_url", "gitlab_project_id", "gitlab_private_token", "gitlab_deploy_job", "gitlab_disable_confirm")),
                rstudioapi::getPersistentValue)
  
  # We keep the project_id as a character and are not checking if it's a number: the request will fail
  if (any(map_lgl(config, is_null)) || any(nchar(config) == 0)) {
    rstudioapi::showDialog("Error", "Adjust your gitlab settings", "")
    stop("Invalid gitlab configuration", call. = FALSE)
  }
  
  if (!isTRUE(as.logical(config$gitlab_disable_confirm)) && !rstudioapi::showQuestion(title = "Confirm", ok = "OK", cancel = "Cancel", "Are you sure you want to deploy to production?")) return()
  
  gitlab_manual_job(config$gitlab_url, config$gitlab_private_token, config$gitlab_project_id, config$gitlab_deploy_job)
})