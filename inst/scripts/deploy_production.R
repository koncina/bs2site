local({
  config <- get_gitlab_settings()
  
  if (!isTRUE(as.logical(config$gitlab_disable_confirm)) && !rstudioapi::showQuestion(title = "Confirm", ok = "OK", cancel = "Cancel", "Are you sure you want to deploy to production?")) return()
  
  gitlab_manual_job(config$gitlab_url, config$gitlab_private_token, config$gitlab_project_id, config$gitlab_deploy_job)
})