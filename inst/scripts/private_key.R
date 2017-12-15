get_keyring_service <- function(url) {
  # Extracts the servername and prepends bs2site login
  gsub("(?:.*:\\/\\/)?([^/]+)\\/.*", "bs2site@\\1", url)
}

get_private_token <- function(url, use_keyring = TRUE) {
  if (isTRUE(use_keyring) && !"keyring" %in% utils::installed.packages()) stop("Could not find the required keyring package", call. = FALSE)
  if (use_keyring) {
    purrr::possibly(keyring::key_get, otherwise = NULL)(get_keyring_service(url))
  } else {
    rstudioapi::getPersistentValue("gitlab_private_token")
  }
}

set_private_token <- function(url, private_token, use_keyring = TRUE) {
  #if (!nchar(url) > 0) stop("url is missing", call. = FALSE)
  if (isTRUE(use_keyring) && !"keyring" %in% utils::installed.packages()) stop("Could not find the required keyring package", call. = FALSE)
  if (use_keyring) {
    keyring::key_set_with_value(get_keyring_service(url), password = private_token)
    # Checking if token was stored as plain text before and removing it
    if (isTRUE(nchar(rstudioapi::getPersistentValue("gitlab_private_token")) > 0)) {
      rstudioapi::setPersistentValue("gitlab_private_token", NULL)
      warning("Removed plain text private token from the project folder.", call. = FALSE)
    }
  } else {
    warning("Consider using the keyring package: your private token is stored as plain text in your project folder", call. = FALSE)
    rstudioapi::setPersistentValue("gitlab_private_token", private_token)
  }
}