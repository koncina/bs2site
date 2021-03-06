get_gitlab_settings <- function() {
  if (RStudio.Version()[["version"]] < "1.1.57") stop("RStudio version >= 1.1.57 is required", call. = FALSE)
  
  sys.source(system.file("scripts", "private_key.R", package = "bs2site", mustWork = TRUE))
  
  config <- purrr::map(rlang::set_names(c("gitlab_url", "gitlab_project_id", "gitlab_keyring", "gitlab_deploy_job", "gitlab_disable_confirm")),
                       rstudioapi::getPersistentValue)
  
  config <- purrr::map_at(config, c("gitlab_keyring", "gitlab_disable_confirm"), as.logical)
  
  config <- c(config, gitlab_private_token = get_private_token(config$gitlab_url, config$gitlab_keyring))
  
  # We keep the project_id as a character and are not checking if it's a number: the request will fail
  if (any(map_lgl(config, is_null)) || any(nchar(config) == 0)) {
    rstudioapi::showDialog("Error", "Adjust your gitlab settings", "")
    stop("Invalid gitlab configuration", call. = FALSE)
  }
  
  config
}
