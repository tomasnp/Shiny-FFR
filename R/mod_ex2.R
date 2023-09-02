#' ex2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ex2_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      selectInput(ns("my_selection"), "Choisissez une option:", choices = c("Option 1", "Option 2", "Option 3"), selected = "Option 1"),
      actionButton(ns("my_button"), "Cliquez ici"),
      textOutput(ns("selected_output"))
    )
  )
}

mod_ex2_server <- function(id,vals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$my_button, {
      selected_option <- input$my_selection
      if (!is.null(selected_option)) {
        output$selected_output <- renderText({
          paste("Vous avez sélectionné :", selected_option)
        })
      } else {
        output$selected_output <- renderText({
          "Aucune option sélectionnée"
        })
      }
    })
  })
}


## To be copied in the UI
# mod_ex2_ui("ex2_1")

## To be copied in the server
# mod_ex2_server("ex2_1")
