#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Gestion des inputs du sidebarpanel pour en faire des variables globales (recuperables dans les differents onglets)
  vals <- reactiveValues()
  observe({
    vals$compet = input$compet
  })
  observe({
    vals$equipe = input$equipe
  })

  observe({
    vals$equipeAdv = input$equipeAdv
  })

  observe({
    vals$annee = input$annee
  })

  observe({
    vals$valid = input$submit
  })

  observe({
    vals$X = input$X
  })

  observe({
    vals$Y = input$Y
  })


  # Gestion des onglets
  observeEvent(input$tabs, {
    if (input$tabs==1){ mod_ex1_server("ex1_1",vals) }
  })


  #Propose uniquement les équipes de la compet séléctionnné
  observeEvent(input$compet, {
    selected_compet <- unique(input$compet)

    # Enregistrez les choix actuellement sélectionnés
    current_selected_team <- input$equipe

    filtered_event <- subset(event, Competition %in% selected_compet)

    available_teams <- unique(filtered_event$name)

    # Utilisez les choix actuellement sélectionnés comme valeurs sélectionnées dans updatePickerInput
    updatePickerInput(session, "equipe", choices = c(sort(available_teams)), selected = current_selected_team)
    vals$valid <- 0
  })

  observeEvent(input$equipe,{
    selected_team <- unique(input$equipe)
    selected_compet <- unique(input$compet)

    # Enregistrez les choix actuellement sélectionnés pour l'équipe adverse
    current_selected_adv_team <- input$equipeAdv

    filtered_event <- subset(event, name %in% selected_team & Competition %in% selected_compet)

    available_teams <- unique(filtered_event$name2)

    # Utilisez les choix actuellement sélectionnés comme valeurs sélectionnées dans updatePickerInput
    updatePickerInput(session, "equipeAdv", choices = c(sort(available_teams)), selected = current_selected_adv_team)
    vals$valid <- 0
  })

  observeEvent(input$equipeAdv, {
    selected_adv_team <- unique(input$equipeAdv)
    selected_team <- unique(input$equipe)
    selected_compet <- unique(input$compet)

    # Enregistrez les choix actuellement sélectionnés pour l'année
    current_selected_year <- input$annee

    filtered_event <- subset(event, name %in% selected_team & name2 %in% selected_adv_team & Competition %in% selected_compet)

    available_years <- unique(filtered_event$season)

    # Utilisez les choix actuellement sélectionnés comme valeurs sélectionnées dans updatePickerInput
    updateSelectizeInput(session, "annee", choices = c(sort(available_years)), selected = current_selected_year)
    vals$valid <- 0
  })

  observeEvent(input$annee, {
    vals$valid <- 0
  })

  observeEvent(input$X, {
    vals$valid <- 0
  })

  observeEvent(input$Y, {
    vals$valid <- 0
  })

  observeEvent(input$reset, {
    updatePickerInput(session,  "compet","Compétition", selected = "")
    updatePickerInput(session, "equipe", "Equipe", selected = "")
    updatePickerInput(session,  "annee","Année", selected = "")
    updatePickerInput(session, "equipeAdv", "Equipe Adverse", selected = "")
  })

  observeEvent(input$switch, {
    equipe_value <- input$equipe
    equipeAdv_value <- input$equipeAdv

    updatePickerInput(session, "equipeAdv", selected = equipe_value)
    updatePickerInput(session, "equipe", selected = equipeAdv_value)

  })

}



