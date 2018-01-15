local({
  if (RStudio.Version()[["version"]] < "1.1.57") stop("RStudio version >= 1.1.57 is required", call. = FALSE)
  
  sys.source(system.file("scripts", "private_key.R", package = "bs2site", mustWork = TRUE))
  
  # Testing if FALSE is set explicitly. Otherwise, falling back to TRUE 
  use_keyring <- !rlang::is_false(as.logical(rstudioapi::getPersistentValue("gitlab_keyring")))
  
  private_key <- purrr::possibly(get_private_token, otherwise = NULL)(rstudioapi::getPersistentValue("gitlab_url"), use_keyring)
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Set gitlab settings"),
    miniUI::miniContentPanel(
      shiny::fluidRow(
        shiny::column(6,
                      shiny::wellPanel(
                        shiny::textInput("gitlab_url", "Gitlab url", value = rstudioapi::getPersistentValue("gitlab_url")),
                        shiny::passwordInput("gitlab_private_token", "Private token", value = private_key),
                        shiny::checkboxInput("gitlab_keyring", "Use system credential store (recommended, requires the keyring package)", use_keyring),
                        miniUI::miniButtonBlock(
                          shiny::actionButton("connect", "Connect to gitlab", primary = TRUE)
                        )
                      )
        ),
        shiny::column(6,
                      shiny::wellPanel(
                        shiny::selectInput("gitlab_project_id", label = "Select project", 
                                           choices = rstudioapi::getPersistentValue("gitlab_project_id")),
                        shiny::selectInput("gitlab_deploy_job", label = "Select deploy job", 
                                           choices = rstudioapi::getPersistentValue("gitlab_deploy_job")),
                        shiny::checkboxInput("gitlab_disable_confirm", "Disable confirmation dialog", isTRUE(as.logical(rstudioapi::getPersistentValue("gitlab_disable_confirm"))))
                      )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    
    shiny::observeEvent(input$connect, {
      projects <- purrr::safely(gitlab_projects)(input$gitlab_url, input$gitlab_private_token)
      
      if (!is_null(projects$error)) {
        rstudioapi::showDialog("Error", purrr::pluck(projects, "error", "message"), "")
        return()
      }
      shiny::updateSelectInput(session, "gitlab_project_id",
                               choices = projects$result)
      
    })
    
    shiny::observeEvent(input$gitlab_project_id, {
      jobs <- possibly(~pull(gitlab_jobs(..1, ..2, ..3), "name"), otherwise = "")(input$gitlab_url, input$gitlab_private_token, input$gitlab_project_id)
      
      shiny::updateSelectInput(session, "gitlab_deploy_job",
                               choices = jobs)
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$gitlab_keyring, {
      if (isTRUE(input$gitlab_keyring) && !"keyring" %in% utils::installed.packages()) {
        rstudioapi::showDialog("Warning", "The keyring package is not installed: the private token will be stored as plain text!", "")
        shiny::updateCheckboxInput(session, "gitlab_keyring", value = FALSE)
      }
    })
    
    shiny::observeEvent(input$done, {
      
      err <- purrr::pluck(purrr::safely(set_private_token)(input$gitlab_url, input$gitlab_private_token, use_keyring = input$gitlab_keyring), "error", "message")
      
      if (!is.null(err)) {
        rstudioapi::showDialog("Error", err, "")
        return()
      }
      
      map(purrr::set_names(c("gitlab_url", "gitlab_project_id", "gitlab_keyring", "gitlab_deploy_job", "gitlab_disable_confirm")), 
          ~rstudioapi::setPersistentValue(.x, input[[.x]]))
      
      
      shiny::stopApp()
    })
    
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
    
  }
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("bs2site gitlab settings", width = 800, height = 400))
})
