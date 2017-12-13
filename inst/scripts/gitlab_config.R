local({
  my_textInput <- function(inputId, label) {
    shiny::textInput(inputId, label, value = rstudioapi::getPersistentValue(inputId))
  }
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Set gitlab settings"),
    miniUI::miniContentPanel(
      shiny::fluidRow(
        shiny::column(6,
               shiny::wellPanel(
                 my_textInput("gitlab_url", "Gitlab url"),
                 my_textInput("gitlab_private_token", "Private token"),
                 miniUI::miniButtonBlock(
                   shiny::actionButton("connect", "Connect to gitlab", primary = TRUE)
                 )
               )
        ),
        shiny::column(6,
               shiny::wellPanel(
                 shiny::selectInput("gitlab_project_id", label = "Select the project", 
                             choices = rstudioapi::getPersistentValue("gitlab_project_id")),
                 shiny::selectInput("gitlab_deploy_job", label = "Select the job", 
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
        rstudioapi::showDialog("Error", projects[["error"]][["message"]], "")
        return()
      }
      shiny::updateSelectInput(session, "gitlab_project_id",
                        choices = projects$result)
      
    })
    
    shiny::observeEvent(input$gitlab_project_id, {
      jobs <- gitlab_jobs(input$gitlab_url, input$gitlab_private_token, input$gitlab_project_id)[["name"]]
      
      shiny::updateSelectInput(session, "gitlab_deploy_job",
                        choices = jobs)
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$done, {
      map(purrr::set_names(c("gitlab_url", "gitlab_project_id", "gitlab_private_token", "gitlab_deploy_job", "gitlab_disable_confirm")), 
          ~rstudioapi::setPersistentValue(.x, input[[.x]]))
      shiny::stopApp()
    })
    
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
    
  }
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("bs2site gitlab settings", width = 800, height = 400))
})
