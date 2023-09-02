#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    shinyjs::useShinyjs(),
    shinybrowser::detect(),
    tags$head(tags$style(HTML("#sidebar {background-color: #002A61;} .navbar-default { background-color: #002A61 !important;} .tab-panel{ color: #164194} .navbar-header { width:100%; } .navbar-brand { width: 100%; text-align: center }")),
              tags$style(HTML(".navbar { height: 50px; min-height:50px !important; } .navbar-nav > li > a, .navbar-brand { padding-top:15px !important; height: 50px; }")),
              tags$style(HTML("body {overflow: hidden !important;}"))

    ),

    fluidPage(
      navbarPage(
        theme = shinythemes::shinytheme("flatly"),
        title = div(a(tags$img(src = 'www/background.png', height = "50px"), style = "position:fixed; left:0px; top:0px;"), "Shiny FFR", a(tags$img(src = 'www/background_reverse.png', height = "50px"), style = "position:fixed; right:0px; top:0px;"))
      ),
      sidebarLayout(
        sidebarPanel(id="sidebar", style="height: 88vh;",

                     div(style = "color: white;",
                         pickerInput(
                           "compet","Compétition",
                           choices=c(sort(unique(event$Competition))), options = list(`actions-box` = TRUE),multiple = T)
                           ),
                     div(style = "color: white;",
                         pickerInput(
                           "annee","Année",
                           choices=c(sort(unique(event$season))), options = list(`actions-box` = TRUE),multiple = T)
                     ),
                     div(style = "color: white;",
                         pickerInput(
                           "equipe","Equipe",
                           choices=c(sort(unique(event$name))), options = list(`actions-box` = TRUE),multiple = T)
                     ),
                     div(style = "color: white;",
                         pickerInput(
                           "equipeAdv","Equipe Adverse",
                           choices=c(sort(unique(event$name2))), options = list(`actions-box` = TRUE),multiple = T)
                     ),
                     div(style = "color: white;",sliderInput("X", "Hauteur", -10, 110, value = c(-10, 110), sep = "")),
                     div(style = "color: white;",sliderInput("Y", "Largeur", 0, 70, value = c(0, 70), sep = "")),
                     actionButton("reset", "Effacer le filtre", style="color:white; background-color:#164194; border-color:#164194;"),
                     br(),br(),
                     actionButton("switch", "Switch teams", style="color:black; background-color:white; border-color:white;"),
                     br(),br(),
                     useShinyjs(),
                     actionButton("submit", "Valider Sélection", style="color:white; background-color:red; border-color:red;"),
                     br(), br(),
                    tags$img(src = "www/france-rugby-ffr-logo.png", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                    width = 2


        ),
        mainPanel(id="mainpanel",
                  tabsetPanel(id = "tabs",
                              tabPanel(id = "tab_ex1", "xPts", mod_ex1_ui("ex1_1"), align="center", value = 1, style="height: 82vh; overflow-y: auto;", br()),
                              #tabPanel(id = "tab_ex2", "Onglet 2", mod_ex2_ui("ex2_1"), align="center", value = 2, style="height: 82vh; overflow-y: auto;", br())
                              ),
                  width = 10
                  )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Shiny FFR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
