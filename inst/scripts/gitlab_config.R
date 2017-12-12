local({
  my_textInput <- function(inputId, label) {
    shiny::textInput(inputId, label, value = rstudioapi::getPersistentValue(inputId))
  }
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Set gitlab settings"),
    
    miniUI::miniContentPanel(
      my_textInput("gitlab_url", "Gitlab url"),
      my_textInput("gitlab_project_id", "Project ID (number)"),
      my_textInput("gitlab_private_token", "Private token"),
      my_textInput("gitlab_deploy_job", "Deploy job name")
    )
  )
  
  server <- function(input, output) {
    shiny::observeEvent(input$done, {
      map(set_names(c("gitlab_url", "gitlab_project_id", "gitlab_private_token", "gitlab_deploy_job")), 
          ~rstudioapi::setPersistentValue(.x, input[[.x]]))
      shiny::stopApp()
    })
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
  }
  
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("bs2site gitlab settings", height = 400))
})