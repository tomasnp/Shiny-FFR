#' ex1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_ex1_ui <- function(id) {
  ns <- NS(id)

  dashboardPage(
    skin = "red",
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      fluidPage(
        sidebarPanel(
          id = ns("sidebaronglet1"), style = "height: 140vh; overflow-y: auto; max-height: 140vh;",

          selectInput(ns("selected_event"), "Action", choices = unique(event$StatName), selected = "Jeu au pied"),
          uiOutput(ns("select_choice")),
          div(
            pickerInput(
              ns("Type"),"Type",
              choices = c(sort(unique(event$Type))),
              options = list(`actions-box` = TRUE),
              multiple = TRUE
            )
          ),
          radioButtons(ns("role"), "Rôle de l'équipe", choices=list("Attaque" = 1, "Défense" = 2),selected = 1),

          selectInput(ns("echeance"), "Echeance", choices = c("Durée","Temps de jeu"), selected = "Temps de jeu"),
          uiOutput(ns("slider")),
          checkboxInput(ns("wRuck"), "Origine avec Ruck", value = FALSE),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("wRuck")),
            pickerInput(ns("OrigineRuck"),"Origine", choices = c(sort(unique(event$OrigineRuck))), selected = c(sort(unique(event$OrigineRuck))), options = list(`actions-box` = TRUE), multiple = TRUE),

          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == false", ns("wRuck")),
            pickerInput(ns("Origine"),"Origine", choices = c(sort(unique(event$OrigineJAP))), selected = c(sort(unique(event$OrigineJAP))), options = list(`actions-box` = TRUE), multiple = TRUE),

          ),
          br(),
          # sliderInput(ns("X"), "Hauteur", -10, 110, value = c(-10, 110), sep = ""),
          # sliderInput(ns("Y"), "Largeur", 0, 70, value = c(0, 70), sep = ""),

          div(style = "color: black;", sliderInput(ns("tpsReg"), "Temps Réglementaire", 0, 90, value = c(0, 90), sep = "")),

          checkboxInput(ns("Ponder"), "Pondération par équipe", value = FALSE),
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("Ponder")),
            selectInput(ns("ponder_team"), "Equipe sur laquelle pondérer", choices = c(sort(unique(event$name)))),
            numericInput(ns("ponderation"), "Coefficient de pondération", value = 1, min = 0, step = 0.5),
            selectInput(ns("ponder_team_adv"), "Equipe Adverse sur laquelle pondérer", choices = c(sort(unique(event$name2)))),
            numericInput(ns("ponderation_adv"), "Coefficient de pondération", value = 1, min = 0, step = 0.5),
          ),

          width = 2
        ),
        ###### Main Panel ######
        mainPanel(
          id = ns("mainpanel"),
          tabBox(width = "900px",
                 selected = "Détail",
                 tabPanel("Détail",
                          fluidRow(
                            column(7,
                                   box(
                                     title = "",
                                     width = NULL,
                                     solidBorder = TRUE,
                                     plotOutput(ns("my_plot"), height = "600px", width = "500px", click = ns("plot_click"), brush = ns("plot_brush")),
                                     br(),
                                     column(10, textOutput(ns("nb_data"))),
                                     column(2, actionButton(ns("help"), "?", style = "color:#164194; background-color:white; border-color:#164194; height:30px; width:30px; padding: 0px 0px 0px 0px;"))
                                   )
                            ),
                            column(5,
                                   box(title = "Pointer un événement ou séléctionner une zone", width = NULL,solidBorder = TRUE,
                                          DTOutput(ns('tablePoint')),br(),
                                          verbatimTextOutput(ns("click_coordinates"))
                                          ))
                          )
                 ),
                 tabPanel("Zones",
                          fluidRow(
                            column(7, box(title = "diffXPD :  (xPtsDiff après échéance – xPtsDiff de l'action) " , width = NULL,
                                          plotOutput(ns("zone_plot"), height = "600px", width = "500px"),br(),
                                           textOutput(ns("nb_data_zones")), style = "text-align:center", solidBorder = TRUE,)

                            ),
                            column(5,
                                   tabBox(height = "700px", width = "400px", selected = "Nombre Zones",
                                           tabPanel("Nombre Zones",
                                                    numericInput(ns("XZones"), "Nombre de zones en Hauteur:", value = 2, min = 1),
                                                    numericInput(ns("YZones"), "Nombre de zones en Largeur:", value = 2, min = 1),
                                                    checkboxInput(ns("detail"), "Séléction détaillé des Zones", value = FALSE),

                                                    conditionalPanel(
                                                      condition = sprintf("input['%s'] == true", ns("detail")),
                                                      uiOutput(ns("x_interval_inputs")),
                                                      uiOutput(ns("y_interval_inputs"))
                                                    )

                                                    ),
                                            tabPanel("Lissage Gaussien",
                                                     checkboxInput(ns("lissage"), "Lissage Gaussien", value = FALSE),
                                                     conditionalPanel(
                                                       condition = sprintf("input['%s'] == true", ns("lissage")),
                                                       numericInput(ns("window_size"), "Rayon de lissage", value = 1, min = 0, step = 0.5),
                                                       numericInput(ns("coeff_liss"), "Coeff de lissage:", value = 4, min = 0, step = 0.5)
                                                       )

                                                     ),
                                        )
                            )
                          )
                 ),

                 tabPanel("Tableau",
                            DTOutput(ns('table')),
                 ),
                 tabPanel("Equipes",
                          DTOutput(ns('tableEquipe')),
                 ),
                 tabPanel("Type",
                          DTOutput(ns('tableType')),
                 ),
                 tabPanel("Choix",
                          DTOutput(ns('tableChoix')),
                 )
          ),

          br(),
          ###### xPts ######
          fluidRow(
            column(
              4, box(title = "xPtsFor Moyen", width = NULL, uiOutput(ns("avgXPF")), style = "font-size: 25px;", solidBorder = TRUE,)
            ),
            column(
              4, box(title = "xPtsAgainst Moyen", width = NULL, uiOutput(ns("avgXPA")), style = "font-size: 25px;", solidBorder = TRUE,)
            ),
            column(
              4, box(title = "xPtsDiff Moyen", width = NULL, uiOutput(ns("avgXPD")), style = "font-size: 25px;", solidBorder = TRUE,)
            ),
            actionButton(ns("XPOINTS"), "?",style = "color:#164194; background-color:white; border-color:#164194; height:30px; width:30px; padding: 0px 0px 0px 0px;")
          ),


          br(),
          ###### Max / XPD ######
          tabBox(width = "500px", side = "right",
                 tabPanel("XPD",
                          tabBox(height = "200px", width = "500px",
                                 tabPanel(uiOutput(ns("dynamic_tab_title")),
                                          fluidRow(
                                            column(
                                              6, uiOutput(ns("box_avgXPDseq")), style = "font-size: 25px;", solidBorder = TRUE
                                            ),
                                            column(
                                              6, box(title = "Diff XPD Moyen après échéance", width = NULL, uiOutput(ns("avg_diffXPDseq")), style = "font-size: 25px;", solidBorder = TRUE,)
                                            )
                                          )
                                 ),
                                 tabPanel("xPts fin possession",
                                          div(style = "overflow-y: auto; max-height: 210px;",
                                              fluidRow(
                                                column(
                                                  4, box(title = "xPtsDiff Moyen fin de possession", width = NULL, uiOutput(ns("avgXPDfinposs")), style = "font-size: 25px;", solidBorder = TRUE,)
                                                ),
                                                column(
                                                  4, box(title = "Diff XPD Moyen fin de possession", width = NULL, uiOutput(ns("avg_diffXPDposs")), style = "font-size: 25px;", solidBorder = TRUE,)
                                                ),

                                              )
                                          )
                                 ),
                                 tabPanel("xPts fin séquence",
                                          fluidRow(
                                            column(
                                              4, box(title = "xPtsDiff Moyen fin séquence", width = NULL, uiOutput(ns("avgXPDfinseq")), style = "font-size: 25px;", solidBorder = TRUE,)
                                            ),
                                            column(
                                              4, box(title = "Diff XPD Moyen fin séquence", width = NULL, uiOutput(ns("avg_diffXPDfinseq")), style = "font-size: 25px;", solidBorder = TRUE,)
                                            ),
                                          )
                                 )
                          )
                 ),
                 tabPanel("Max",
                          tabBox(height = "380px", width = "500px",
                                 tabPanel("Max xPts écheance",
                                          fluidRow(
                                            h6("Meilleur"),
                                            column(
                                              4, uiOutput(ns("box_maxXPD_seq")), style = "font-size: 25px;", solidBorder = TRUE
                                            ),
                                            column(4,
                                                   uiOutput(ns("box_maxXPF_seq")), style = "font-size: 25px;", solidBorder = TRUE
                                            ),
                                            column(
                                              4,  uiOutput(ns("box_minXPA_seq")), style = "font-size: 25px;", solidBorder = TRUE
                                            ),
                                            h6("Pire"),
                                            column(
                                              4, uiOutput(ns("box_minXPD_seq")), style = "font-size: 25px;", solidBorder = TRUE
                                            ),
                                            column(4,
                                                   uiOutput(ns("box_minXPF_seq")), style = "font-size: 25px;", solidBorder = TRUE
                                            ),
                                            column(
                                              4,  uiOutput(ns("box_maxXPA_seq")), style = "font-size: 25px;", solidBorder = TRUE
                                            ),
                                          )
                                 ),
                                 tabPanel("Max xPts fin possession",
                                          fluidRow(
                                            h6("Meilleur"),
                                            column(
                                              4, box(title = "Max xPtsDiff dans possession", width = NULL, uiOutput(ns("maxXPD_finposs")), style = "font-size: 25px;", solidBorder = TRUE,)
                                            ),
                                            column(
                                              4, box(title = "Max xPtsFor dans possession", width = NULL, uiOutput(ns("maxXPF_finposs")), style = "font-size: 25px;", solidBorder = TRUE,)
                                            ),
                                            column(
                                              4, box(title = "Min xPtsAgainst dans possession", width = NULL, uiOutput(ns("minXPA_finposs")), style = "font-size: 25px;", solidBorder = TRUE,)
                                            ),
                                            h6("Pire"),
                                            column(
                                              4, box(title = "Min xPtsDiff dans possession", width = NULL, uiOutput(ns("minXPD_finposs")), style = "font-size: 25px;", solidBorder = TRUE,)
                                            ),
                                            column(
                                              4, box(title = "Min xPtsFor dans possession", width = NULL, uiOutput(ns("minXPF_finposs")), style = "font-size: 25px;", solidBorder = TRUE,)
                                            ),
                                            column(
                                              4, box(title = "Max xPtsAgainst dans possession", width = NULL, uiOutput(ns("maxXPA_finposs")), style = "font-size: 25px;", solidBorder = TRUE,)
                                            )
                                          )
                                 ),
                                 tabPanel("Max xPts fin séquence",
                                          fluidRow(
                                            h6("Meilleur"),
                                            column(
                                              4, box(title = "Max xPtsDiff dans séquence", width = NULL, uiOutput(ns("maxXPD_finseq")), style = "font-size: 25px;", solidBorder = TRUE,)
                                            ),
                                            column(
                                              4, box(title = "Max xPtsFor dans séquence", width = NULL, uiOutput(ns("maxXPF_finseq")), style = "font-size: 25px;", solidBorder = TRUE,)
                                            ),
                                            column(
                                              4, box(title = "Min xPtsAgainst dans séquence", width = NULL, uiOutput(ns("minXPA_finseq")), style = "font-size: 25px;", solidBorder = TRUE,)
                                            ),
                                            h6("Pire"),
                                            column(
                                              4, box(title = "Min xPtsDiff dans séquence", width = NULL, uiOutput(ns("minXPD_finseq")), style = "font-size: 25px;", solidBorder = TRUE,)
                                            ),
                                            column(
                                              4, box(title = "Min xPtsFor dans séquence", width = NULL, uiOutput(ns("minXPF_finseq")), style = "font-size: 25px;", solidBorder = TRUE,)
                                            ),
                                            column(
                                              4, box(title = "Max xPtsAgainst dans séquence", width = NULL, uiOutput(ns("maxXPA_finseq")), style = "font-size: 25px;", solidBorder = TRUE,)
                                            )

                                          )
                                 )
                          ),

                 )
          ),
          br(),
          br(),
          ###### Issues ######
          tabBox(width = "500px",
                 tabPanel("Echéance",
                          plotlyOutput(ns("issuesEch"))
                 ),
                 tabPanel("Fin Possesion",
                          plotlyOutput(ns("issuesPoss"))
                 ),
                 tabPanel("Fin Séquence",
                          plotlyOutput(ns("issuesSeq"))
                 )
          ),


          width = 10
        )
      )
    )
  )
}

mod_ex1_server <- function(id, vals) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    color_value <- function(x) {
      if (is.na(x)) {
        return("black")
      } else if (x > 2) {
        return("#276221")
      } else if (x > 1 & x <= 2) {
        return("#276221")
      } else if (x > 0 & x <= 1) {
        return("green")
      } else if (x < 0 & x > -1) {
        return("#ff2c2c")
      } else if (x <= -1 & x > -2) {
        return("#de0a26")
      } else if (x == 0) {
        return("white")
      } else {  # x <= -2
        return("#c30010")
      }

    }

    value_color <- function(x) {
      if (is.na(x)) {
        return("black")
      } else if (x > 2) {
        return("#c30010")
      } else if (x > 1 & x <= 2) {
        return("#de0a26")
      } else if (x > 0 & x <= 1) {
        return("#ff2c2c")
      } else {
        return("white")
      }
    }

    map_colors <- function(x) {
      if (is.na(x)) {
        return("black")
      } else if (x == 0) {
        return("white")
      } else if (x > 3) {
        return("#276221")  # dark green
      } else if (x > 2 & x <= 3) {
        return("#388E3C")  # green
      } else if (x > 1 & x <= 2) {
        return("#4CAF50")  # light green
      } else if (x > 0.5 & x <= 1) {
        return("#81C784")  # lighter green
      } else if (x > 0 & x <= 0.5) {
        return("#C8E6C9")  # lightest green
      } else if (x < 0 & x >= -0.5) {
        return("#FFCDD2")  # lightest red
      } else if (x < -0.5 & x >= -1) {
        return("#E57373")  # lighter red
      } else if (x < -1 & x >= -2) {
        return("#F44336")  # light red
      } else if (x < -2 & x >= -3) {
        return("#D32F2F")  # red
      } else {  # x < -3
        return("#B71C1C")  # dark red
      }
    }

    ###### FIELDS #####

    field <- function() {
      par(mar = c(0, 0, 0, 0))  # Supprime les marges
      plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 70), ylim = c(-10, 110), xaxt = 'n', yaxt = 'n')

      # Draw rectangles
      rect(0, -10, 70, 0, col = "#ffe3e3",lwd = 1.5)
      rect(0, 0, 70, 22, col = "#ffe3e3",lwd = 1.5)
      rect(0, 22, 70, 40, col = "#dfe0f0",border = NA)
      rect(0, 40, 70, 78, col = "#f1f0fc",border = NA)
      rect(0, 78, 70, 100, col = "#dfede4",lwd = 1.5)
      rect(0, 100, 70, 110, col = "#dfede4",lwd = 1.5)
      lines(c(0,0), c(0,100), col = "black", lwd = 1.5)
      lines(c(70,70), c(0,100), col = "black", lwd = 1.5)
      lines(c(0,70), c(50,50), col = "black", lwd = 1.5)

      # Draw lines
      lines(c(0,70), c(22,22), col = "black", lwd = 0.8)
      lines(c(0,70), c(78,78), col = "black", lwd = 0.8)

      lines(c(0,70), c(40,40), col = "black", lty = 3, lwd = 0.75)
      lines(c(0,70), c(60,60), col = "black", lty = 3, lwd = 0.75)
      lines(c(0,70), c(95,95), col = "black", lty = 3, lwd = 0.75)
      lines(c(0,70), c(5,5), col = "black", lty = 3, lwd = 0.75)

      lines(c(5,5), c(0,100), col = "black", lty = 3, lwd = 0.75)
      lines(c(65,65), c(0,100), col = "black", lty = 3, lwd = 0.75)
      # tiers
      lines(c(23.3,23.3), c(0,100), col = "red", lwd = 0.4)
      lines(c(46.6,46.6), c(0,100), col = "red", lwd = 0.4)
      # 15 m
      lines(c(15,15), c(92.5,97.5), col = "black", lwd = 0.8)
      lines(c(15,15), c(75.5,80.5), col = "black", lwd = 0.8)
      lines(c(15,15), c(47.5,52.5), col = "black", lwd = 0.8)
      lines(c(15,15), c(19.5,24.5), col = "black", lwd = 0.8)
      lines(c(15,15), c(2.5,7.5), col = "black", lwd = 0.8)
      lines(c(55,55), c(92.5,97.5), col = "black", lwd = 0.8)
      lines(c(55,55), c(75.5,80.5), col = "black", lwd = 0.8)
      lines(c(55,55), c(47.5,52.5), col = "black", lwd = 0.8)
      lines(c(55,55), c(19.5,24.5), col = "black", lwd = 0.8)
      lines(c(55,55), c(2.5,7.5), col = "black", lwd = 0.8)
      # Additional rectangles (with border)
      rect(10, 10, 60, 90, border = "#3fa9eb")
      rect(0, 40, 70, 60, border = "#7c3896")

      # Additional segments (for thirds and quarters)
      segments(c(23.3,46.6), rep(22, 2), c(23.3,46.6), rep(78, 2), col = "red", lwd = 0.4)
      segments(c(29.1,35,40.8), rep(22, 3), c(29.1,35,40.8), rep(78, 3), col = "#d9a925", lwd = 0.3)


      # zones JAP
      lines(c(0,10), c(73,73), col = "#7289d4", lwd = 0.4)
      lines(c(60,70), c(73,73), col = "#7289d4", lwd = 0.4)
      lines(c(60,60), c(73,83), col = "#7289d4", lwd = 0.4)
      lines(c(10,10), c(73,83), col = "#7289d4", lwd = 0.4)
      lines(c(10,60), c(83,83), col = "#7289d4", lwd = 0.4)

      # Draw points
      points(c(15,55), c(15,15), pch = 1, cex = 6)
      # Poteaux
      lines(c(33,32.5), c(100,107.5), col = "black", lwd = 2)
      lines(c(37,37.5), c(100,107.5), col = "black", lwd = 2)
      lines(c(33,37), c(102.5,102.5), col = "black", lwd = 2)
      lines(c(33,32.5), c(0,-7.5), col = "black", lwd = 2)
      lines(c(37,37.5), c(0,-7.5), col = "black", lwd = 2)
      lines(c(33,37), c(-2.5,-2.5), col = "black", lwd = 2)
    }

    field_zone1 <- function(){
      lines(c(0,70), c(40,40), col = "black", lty = 3, lwd = 0.75)
      lines(c(0,70), c(60,60), col = "black", lty = 3, lwd = 0.75)
      lines(c(0,70), c(95,95), col = "black", lty = 3, lwd = 0.75)
      lines(c(0,70), c(5,5), col = "black", lty = 3, lwd = 0.75)

      lines(c(5,5), c(0,100), col = "black", lty = 3, lwd = 0.75)
      lines(c(65,65), c(0,100), col = "black", lty = 3, lwd = 0.75)
      # tiers
      lines(c(23.3,23.3), c(0,100), col = "red", lwd = 0.4)
      lines(c(46.6,46.6), c(0,100), col = "red", lwd = 0.4)
      # 15 m
      lines(c(15,15), c(92.5,97.5), col = "black", lwd = 0.8)
      lines(c(15,15), c(75.5,80.5), col = "black", lwd = 0.8)
      lines(c(15,15), c(47.5,52.5), col = "black", lwd = 0.8)
      lines(c(15,15), c(19.5,24.5), col = "black", lwd = 0.8)
      lines(c(15,15), c(2.5,7.5), col = "black", lwd = 0.8)
      lines(c(55,55), c(92.5,97.5), col = "black", lwd = 0.8)
      lines(c(55,55), c(75.5,80.5), col = "black", lwd = 0.8)
      lines(c(55,55), c(47.5,52.5), col = "black", lwd = 0.8)
      lines(c(55,55), c(19.5,24.5), col = "black", lwd = 0.8)
      lines(c(55,55), c(2.5,7.5), col = "black", lwd = 0.8)
      # Additional rectangles (with border)
      rect(10, 10, 60, 90, border = "#3fa9eb")
      rect(0, 40, 70, 60, border = "#7c3896")

      # Additional segments (for thirds and quarters)
      segments(c(23.3,46.6), rep(22, 2), c(23.3,46.6), rep(78, 2), col = "red", lwd = 0.4)
      segments(c(29.1,35,40.8), rep(22, 3), c(29.1,35,40.8), rep(78, 3), col = "#d9a925", lwd = 0.3)

      # zones JAP
      lines(c(0,10), c(73,73), col = "#7289d4", lwd = 0.4)
      lines(c(60,70), c(73,73), col = "#7289d4", lwd = 0.4)
      lines(c(60,60), c(73,83), col = "#7289d4", lwd = 0.4)
      lines(c(10,10), c(73,83), col = "#7289d4", lwd = 0.4)
      lines(c(10,60), c(83,83), col = "#7289d4", lwd = 0.4)


    }

    field_zone2 <- function(){
      lines(c(0,0), c(-10,110), col = "black", lwd = 2)
      lines(c(70,70), c(-10,110), col = "black", lwd = 2)
      lines(c(0,70), c(50,50), col = "black", lwd = 2)
      lines(c(0,70), c(100,100), col = "black", lwd = 2)
      lines(c(0,70), c(0,0), col = "black", lwd = 2)
      lines(c(0,70), c(110,110), col = "black", lwd = 2)
      lines(c(0,70), c(-10,-10), col = "black", lwd = 2)
      # Draw 22
      lines(c(0,70), c(22,22), col = "black", lwd = 2)
      lines(c(0,70), c(78,78), col = "black", lwd = 2)

      # Poteaux
      lines(c(33,32.5), c(100,107.5), col = "black", lwd = 2)
      lines(c(37,37.5), c(100,107.5), col = "black", lwd = 2)
      lines(c(33,37), c(102.5,102.5), col = "black", lwd = 2)
      lines(c(33,32.5), c(0,-7.5), col = "black", lwd = 2)
      lines(c(37,37.5), c(0,-7.5), col = "black", lwd = 2)
      lines(c(33,37), c(-2.5,-2.5), col = "black", lwd = 2)

    }


    # Preparation event (reactive value)
    event_subset <- reactive({
      prepare_event_subset(input, event, vals)
    })


    filter_event <- function(input) {
      event_current <- event_subset()
      deb <- input$tpsReg[1]
      fin <- input$tpsReg[2]

      #View(event_current[, c("StatName","Type","TimeVideo","TeamId","name", "X.sens","Y.sens","XPtsDiff","StatNameseq","nameseq","Xseq","Yseq","XPDseq")])
      if (input$role == 1) {
        event_filt <- event_current %>% filter(StatName %in% input$selected_event, name %in% vals$equipe, name2 %in% vals$equipeAdv, X.sens/10 >= vals$X[1], X.sens/10 <= vals$X[2],Y.sens/10 >= vals$Y[1],Y.sens/10 <= vals$Y[2], GameSeconds/60 <= fin, GameSeconds/60 >= deb )
      } else if(input$role == 2){
        event_filt <- event_current %>% filter(StatName %in% input$selected_event, name2 %in% vals$equipe, name %in% vals$equipeAdv, 100-X.sens/10 >= vals$X[1], 100-X.sens/10 <= vals$X[2],70-Y.sens/10 >= vals$Y[1], 70-Y.sens/10 <= vals$Y[2], GameSeconds/60 <= fin, GameSeconds/60 >= deb)
      }
      if(!(input$selected_event %in% c("Coup d'envoi","Passe","Franchissement","Offload","Conversion","Maul","Faute off" ))){
        if (input$wRuck) {
          event_filt <- event_filt %>% filter(OrigineRuck %in% input$OrigineRuck)
        } else {
          event_filt <- event_filt %>% filter(OrigineJAP %in% input$Origine)
        }
      }

      return(event_filt)
    }

    ###### PREPARE #####
    prepare_event_subset <- function(input, event, vals){
      # Crée un sous-ensemble de event basé sur les inputs en fonction du role de l'équipe

      event_subset <- event %>% filter(name %in% c(vals$equipeAdv, vals$equipe), name2 %in% c(vals$equipeAdv, vals$equipe),season %in% vals$annee,Competition %in% vals$compet)

      #View(event_subset[, c("StatName","TimeVideo","name", "X.sens","Y.sens","XPtsDiff","StatNameseq","nameseq","Xseq","Yseq","XPDseq")])

      # variables créées dans la fonction
      event_subset$XPDseq <- NA
      event_subset$XPFseq <- NA
      event_subset$XPAseq <- NA
      event_subset$StatNameseq <- NA
      event_subset$Xseq <- NA
      event_subset$Yseq <- NA
      event_subset$nameseq <- NA
      event_subset$maxXPDseq <- NA
      event_subset$minXPDseq <- NA
      event_subset$maxXPFseq <- NA
      event_subset$minXPFseq <- NA
      event_subset$minXPAseq <- NA
      event_subset$maxXPAseq <- NA
      event_subset$diff_XPDseq <- NA
      event_subset$diff_XPDfinseq <- NA
      event_subset$diff_XPDposs <- NA
      event_subset$diff_XPAseq <- NA
      event_subset$diff_XPFseq <- NA
      event_subset$XPFmaxfinposs <- NA
      event_subset$XPAminfinposs <- NA
      event_subset$XPDmaxfinposs <- NA
      event_subset$XPFminfinposs <- NA
      event_subset$XPAmaxfinposs <- NA
      event_subset$XPDminfinposs <- NA
      event_subset$StatNamefinposs <- NA
      event_subset$namefinposs <- NA
      event_subset$XPFfinposs <- NA
      event_subset$XPAfinposs <- NA
      event_subset$XPDfinposs <- NA
      event_subset$XPFfinseq <- NA
      event_subset$XPAfinseq <- NA
      event_subset$XPDfinseq <- NA
      event_subset$StatNamefinseq <- NA
      event_subset$namefinseq <- NA
      event_subset$maxXPFfinseq <- NA
      event_subset$minXPAfinseq <- NA
      event_subset$maxXPDfinseq <- NA
      event_subset$minXPFfinseq <- NA
      event_subset$maxXPAfinseq <- NA
      event_subset$minXPDfinseq <- NA
      #input non vides
      if(vals$valid){

        if(nrow(event_subset) > 0 ){

          if (input$role == 1) {
            X1 <- event_subset$X.sens/10 >= vals$X[1]
            X2 <- event_subset$X.sens/10 <= vals$X[2]
            Y1 <- event_subset$Y.sens/10 >= vals$Y[1]
            Y2 <- event_subset$Y.sens/10 <= vals$Y[2]
          } else if(input$role == 2){
            X1 <- 100-event_subset$X.sens/10 >= vals$X[1]
            X2 <- 100-event_subset$X.sens/10 <= vals$X[2]
            Y1 <- 70-event_subset$Y.sens/10 >= vals$Y[1]
            Y2 <- 70-event_subset$Y.sens/10 <= vals$Y[2]
          }

          req(input$sliderInput)

          if (!is.null(input$sliderInput) && length(input$sliderInput) > 0) {
            seq_time <- input$sliderInput
          } else {
            seq_time <- 0 # ou une autre valeur par défaut
          }

          if(length(seq_time) > 0 && seq_time == 0){
            event_subset$XPDseq <- event_subset$XPtsDiff
            event_subset$XPFseq <- event_subset$xPtsFor
            event_subset$XPAseq <- event_subset$xPtsAgainst
            event_subset$StatNameseq <- event_subset$StatName
            event_subset$Xseq <- event_subset$X.sens
            event_subset$Yseq <- event_subset$Y.sens
            event_subset$nameseq <- event_subset$name
            event_subset$maxXPDseq <- event_subset$XPtsDiff
            event_subset$maxXPFseq <- event_subset$xPtsFor
            event_subset$minXPAseq <- event_subset$xPtsAgainst
          }else{

            ##### XP fin de possession #####
            suppressWarnings({
              for (i in which(!(event_subset$StatName %in% c("Essai", "Conversion", "Penalite", "Faute off", "Faute def", "Turnover concedee")) & (event_subset$StatName %in% input$selected_event) & X1 & X2 & Y1 & Y2 )) {
                arretjeu <- which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$TimeVideo < event_subset$TimeVideo[i] + 900 & event_subset$StatName %in% c("Essai", "Touche","Drop","Melee", "Faute off", "Coup d'envoi", "Penalite"))[1]
                finposs <- which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$TimeVideo < event_subset$TimeVideo[i] + 900 & event_subset$TeamId[i] != event_subset$TeamId & !(event_subset$StatName %in% c("Turnover concedee", "Faute def")))[1]
                fautedef <- which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$TimeVideo < event_subset$TimeVideo[i] + 900 & event_subset$StatName == "Faute def")[1]
                postfinposs <- which(event_subset$GameId == event_subset$GameId[finposs] & event_subset$TimeVideo > event_subset$TimeVideo[finposs] & event_subset$TimeVideo < event_subset$TimeVideo[i] + 900 & !(event_subset$StatName %in% c("Maul", "Passe", "Jeu au pied", "Franchissement", "Offload")))[1]


                avarretjeu <- which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$TimeVideo <= event_subset$TimeVideo[arretjeu] & !(event_subset$StatName %in% c("Maul", "Passe", "Jeu au pied", "Franchissement", "Offload")))
                avfinposs <- which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$TimeVideo <= event_subset$TimeVideo[finposs] & !(event_subset$StatName %in% c("Maul", "Passe", "Jeu au pied", "Franchissement", "Offload")))
                avfautedef <- which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$TimeVideo < event_subset$TimeVideo[fautedef] & !(event_subset$StatName %in% c("Maul", "Passe", "Jeu au pied", "Franchissement", "Offload")))
                mmteam <- which(event_subset$TeamId == event_subset$TeamId[i])
                diffteam <- which(event_subset$TeamId != event_subset$TeamId[i])
                #Gestion des avantages
                if (event_subset$ExclAvantage[i] != 0) {
                  event_subset$XPFfinposs[i] <- NA
                  event_subset$XPAfinposs[i] <- NA
                  event_subset$XPFmaxfinposs[i] <- NA
                  event_subset$XPAminfinposs[i] <- NA
                  event_subset$XPDmaxfinposs[i] <- NA
                  event_subset$XPFminfinposs[i] <- NA
                  event_subset$XPAmaxfinposs[i] <- NA
                  event_subset$XPDminfinposs[i] <- NA}
                else {

                  event_subset$namefinposs[i] <-
                    #Gestion cas où un essai arrive après un avantage sur faute def
                    ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & event_subset$StatName[arretjeu] == "Essai" & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                           event_subset$name[arretjeu],
                           #Gestion faute def
                           ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[finposs] & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                                  ifelse(event_subset$TeamId[fautedef] == event_subset$TeamId[i],
                                         event_subset$name[fautedef],
                                         event_subset$name[fautedef]),
                                  #Cas où arrêt de jeu avant la fin de possession
                                  ifelse((event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & !is.na(arretjeu)) | is.na(finposs),
                                         event_subset$name[arretjeu],
                                         #Cas où la fin de possession est un événement sans Xpts
                                         ifelse(event_subset$StatName[finposs] %in% c("Maul", "Passe", "Jeu au pied", "Franchissement", "Offload"),
                                                ifelse(event_subset$TeamId[postfinposs] == event_subset$TeamId[i],
                                                       event_subset$name[postfinposs],
                                                       event_subset$name[postfinposs]),
                                                #Cas où le premier événement est une fin de possession classique
                                                event_subset$name[finposs]))))

                  event_subset$StatNamefinposs[i] <-
                    #Gestion cas où un essai arrive après un avantage sur faute def
                    ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & event_subset$StatName[arretjeu] == "Essai" & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                           event_subset$StatName[arretjeu],
                           #Gestion faute def
                           ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[finposs] & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                                  ifelse(event_subset$TeamId[fautedef] == event_subset$TeamId[i],
                                         event_subset$StatName[fautedef],
                                         event_subset$StatName[fautedef]),
                                  #Cas où arrêt de jeu avant la fin de possession
                                  ifelse((event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & !is.na(arretjeu)) | is.na(finposs),
                                         event_subset$StatName[arretjeu],
                                         #Cas où la fin de possession est un événement sans Xpts
                                         ifelse(event_subset$StatName[finposs] %in% c("Maul", "Passe", "Jeu au pied", "Franchissement", "Offload"),
                                                ifelse(event_subset$TeamId[postfinposs] == event_subset$TeamId[i],
                                                       event_subset$StatName[postfinposs],
                                                       event_subset$StatName[postfinposs]),
                                                #Cas où le premier événement est une fin de possession classique
                                                event_subset$StatName[finposs]))))

                  event_subset$XPFfinposs[i] <-
                    #Gestion cas où un essai arrive après un avantage sur faute def
                    ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & event_subset$StatName[arretjeu] == "Essai" & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                           event_subset$xPtsFor[arretjeu],
                           #Gestion faute def
                           ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[finposs] & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                                  ifelse(event_subset$TeamId[fautedef] == event_subset$TeamId[i],
                                         event_subset$xPtsAgainst[fautedef],
                                         event_subset$xPtsFor[fautedef]),
                                  #Cas où arrêt de jeu avant la fin de possession
                                  ifelse((event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & !is.na(arretjeu)) | is.na(finposs),
                                         event_subset$xPtsFor[arretjeu],
                                         #Cas où la fin de possession est un événement sans Xpts
                                         ifelse(event_subset$StatName[finposs] %in% c("Maul", "Passe", "Jeu au pied", "Franchissement", "Offload"),
                                                ifelse(event_subset$TeamId[postfinposs] == event_subset$TeamId[i],
                                                       event_subset$xPtsFor[postfinposs],
                                                       event_subset$xPtsAgainst[postfinposs]),
                                                #Cas où le premier événement est une fin de possession classique
                                                event_subset$xPtsAgainst[finposs]))))
                  event_subset$XPAfinposs[i] <-
                    ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & event_subset$StatName[arretjeu] == "Essai" & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                           event_subset$xPtsAgainst[arretjeu],
                           ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[finposs] & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                                  ifelse(event_subset$TeamId[fautedef] == event_subset$TeamId[i],
                                         event_subset$xPtsFor[fautedef],
                                         event_subset$xPtsAgainst[fautedef]),
                                  ifelse((event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & !is.na(arretjeu)) | is.na(finposs),
                                         event_subset$xPtsAgainst[arretjeu],
                                         ifelse(event_subset$StatName[finposs] %in% c("Maul", "Passe", "Jeu au pied", "Franchissement", "Offload"),
                                                ifelse(event_subset$TeamId[postfinposs] == event_subset$TeamId[i],
                                                       event_subset$xPtsAgainst[postfinposs],
                                                       event_subset$xPtsFor[postfinposs]),
                                                event_subset$xPtsFor[finposs]))))
                  event_subset$XPFmaxfinposs[i] <-
                    #Gestion cas où un essai arrive après un avantage sur faute def
                    ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & event_subset$StatName[arretjeu] == "Essai" & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                           max(event_subset$xPtsFor[intersect(avarretjeu,mmteam)], event_subset$xPtsAgainst[intersect(avarretjeu,diffteam)]),
                           #Gestion faute def
                           ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[finposs] & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                                  ifelse(event_subset$TeamId[fautedef] == event_subset$TeamId[i],
                                         max(event_subset$xPtsFor[intersect(avfautedef,mmteam)], event_subset$xPtsAgainst[intersect(avfautedef,diffteam)],event_subset$xPtsAgainst[fautedef]),
                                         max(event_subset$xPtsFor[intersect(avfautedef,mmteam)], event_subset$xPtsAgainst[intersect(avfautedef,diffteam)],event_subset$xPtsFor[fautedef])),
                                  #Cas où arrêt de jeu avant la fin de possession
                                  ifelse((event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & !is.na(arretjeu)) | is.na(finposs),
                                         max(event_subset$xPtsFor[intersect(avarretjeu,mmteam)], event_subset$xPtsAgainst[intersect(avarretjeu,diffteam)]),
                                         #Cas où la fin de possession est un événement sans Xpts
                                         ifelse(event_subset$StatName[finposs] %in% c("Maul", "Passe", "Jeu au pied", "Franchissement", "Offload"),
                                                ifelse(event_subset$TeamId[postfinposs] == event_subset$TeamId[i],
                                                       max(event_subset$xPtsFor[intersect(avfinposs,mmteam)], event_subset$xPtsAgainst[intersect(intersect(avfinposs,diffteam),which(event_subset$TimeVideo < event_subset$TimeVideo[finposs]))],event_subset$xPtsFor[postfinposs]), #2 intersects pour que ça ne considère pas le changement de possession à 0pts dans le calcul (important surtout pour les min XPA)
                                                       max(event_subset$xPtsFor[intersect(avfinposs,mmteam)], event_subset$xPtsAgainst[intersect(intersect(avfinposs,diffteam),which(event_subset$TimeVideo < event_subset$TimeVideo[finposs]))],event_subset$xPtsAgainst[postfinposs])),
                                                #Cas où le premier événement est une fin de possession classique
                                                max(event_subset$xPtsFor[intersect(avfinposs,mmteam)], event_subset$xPtsAgainst[intersect(avfinposs,diffteam)])))))
                  event_subset$XPFminfinposs[i] <-
                    #Gestion cas où un essai arrive après un avantage sur faute def
                    ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & event_subset$StatName[arretjeu] == "Essai" & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                           min(event_subset$xPtsFor[intersect(avarretjeu,mmteam)], event_subset$xPtsAgainst[intersect(avarretjeu,diffteam)]),
                           #Gestion faute def
                           ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[finposs] & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                                  ifelse(event_subset$TeamId[fautedef] == event_subset$TeamId[i],
                                         min(event_subset$xPtsFor[intersect(avfautedef,mmteam)], event_subset$xPtsAgainst[intersect(avfautedef,diffteam)],event_subset$xPtsAgainst[fautedef]),
                                         min(event_subset$xPtsFor[intersect(avfautedef,mmteam)], event_subset$xPtsAgainst[intersect(avfautedef,diffteam)],event_subset$xPtsFor[fautedef])),
                                  #Cas où arrêt de jeu avant la fin de possession
                                  ifelse((event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & !is.na(arretjeu)) | is.na(finposs),
                                         min(event_subset$xPtsFor[intersect(avarretjeu,mmteam)], event_subset$xPtsAgainst[intersect(avarretjeu,diffteam)]),
                                         #Cas où la fin de possession est un événement sans Xpts
                                         ifelse(event_subset$StatName[finposs] %in% c("Maul", "Passe", "Jeu au pied", "Franchissement", "Offload"),
                                                ifelse(event_subset$TeamId[postfinposs] == event_subset$TeamId[i],
                                                       min(event_subset$xPtsFor[intersect(avfinposs,mmteam)], event_subset$xPtsAgainst[intersect(intersect(avfinposs,diffteam),which(event_subset$TimeVideo < event_subset$TimeVideo[finposs]))],event_subset$xPtsFor[postfinposs]), #2 intersects pour que ça ne considère pas le changement de possession à 0pts dans le calcul (important surtout pour les min XPA)
                                                       min(event_subset$xPtsFor[intersect(avfinposs,mmteam)], event_subset$xPtsAgainst[intersect(intersect(avfinposs,diffteam),which(event_subset$TimeVideo < event_subset$TimeVideo[finposs]))],event_subset$xPtsAgainst[postfinposs])),
                                                #Cas où le premier événement est une fin de possession classique
                                                min(event_subset$xPtsFor[intersect(avfinposs,mmteam)], event_subset$xPtsAgainst[intersect(avfinposs,diffteam)])))))
                  event_subset$XPAminfinposs[i] <-
                    ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & event_subset$StatName[arretjeu] == "Essai" & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                           min(event_subset$xPtsAgainst[intersect(avarretjeu,mmteam)], event_subset$xPtsFor[intersect(avarretjeu,diffteam)]),
                           #Gestion faute def
                           ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[finposs] & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                                  ifelse(event_subset$TeamId[fautedef] == event_subset$TeamId[i],
                                         min(event_subset$xPtsAgainst[intersect(avfautedef,mmteam)], event_subset$xPtsFor[intersect(avfautedef,diffteam)],event_subset$xPtsFor[fautedef]),
                                         min(event_subset$xPtsAgainst[intersect(avfautedef,mmteam)], event_subset$xPtsFor[intersect(avfautedef,diffteam)],event_subset$xPtsAgainst[fautedef])),
                                  #Cas où arrêt de jeu avant la fin de possession
                                  ifelse((event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & !is.na(arretjeu)) | is.na(finposs),
                                         min(event_subset$xPtsAgainst[intersect(avarretjeu,mmteam)], event_subset$xPtsFor[intersect(avarretjeu,diffteam)]),
                                         #Cas où la fin de possession est un événement sans Xpts
                                         ifelse(event_subset$StatName[finposs] %in% c("Maul", "Passe", "Jeu au pied", "Franchissement", "Offload"),
                                                ifelse(event_subset$TeamId[postfinposs] == event_subset$TeamId[i],
                                                       min(event_subset$xPtsAgainst[intersect(avfinposs,mmteam)], event_subset$xPtsFor[intersect(intersect(avfinposs,diffteam),which(event_subset$TimeVideo < event_subset$TimeVideo[finposs]))],event_subset$xPtsAgainst[postfinposs]),
                                                       min(event_subset$xPtsAgainst[intersect(avfinposs,mmteam)], event_subset$xPtsFor[intersect(intersect(avfinposs,diffteam),which(event_subset$TimeVideo < event_subset$TimeVideo[finposs]))],event_subset$xPtsFor[postfinposs])), #2 intersects pour que ça ne considère pas le changement de possession à 0pts dans le calcul (important surtout pour les min XPA)
                                                #Cas où le premier événement est une fin de possession classique
                                                min(event_subset$xPtsAgainst[intersect(avfinposs,mmteam)], event_subset$xPtsFor[intersect(avfinposs,diffteam)])))))
                  event_subset$XPAmaxfinposs[i] <-
                    ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & event_subset$StatName[arretjeu] == "Essai" & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                           max(event_subset$xPtsAgainst[intersect(avarretjeu,mmteam)], event_subset$xPtsFor[intersect(avarretjeu,diffteam)]),
                           #Gestion faute def
                           ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[finposs] & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                                  ifelse(event_subset$TeamId[fautedef] == event_subset$TeamId[i],
                                         max(event_subset$xPtsAgainst[intersect(avfautedef,mmteam)], event_subset$xPtsFor[intersect(avfautedef,diffteam)],event_subset$xPtsFor[fautedef]),
                                         max(event_subset$xPtsAgainst[intersect(avfautedef,mmteam)], event_subset$xPtsFor[intersect(avfautedef,diffteam)],event_subset$xPtsAgainst[fautedef])),
                                  #Cas où arrêt de jeu avant la fin de possession
                                  ifelse((event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & !is.na(arretjeu)) | is.na(finposs),
                                         max(event_subset$xPtsAgainst[intersect(avarretjeu,mmteam)], event_subset$xPtsFor[intersect(avarretjeu,diffteam)]),
                                         #Cas où la fin de possession est un événement sans Xpts
                                         ifelse(event_subset$StatName[finposs] %in% c("Maul", "Passe", "Jeu au pied", "Franchissement", "Offload"),
                                                ifelse(event_subset$TeamId[postfinposs] == event_subset$TeamId[i],
                                                       max(event_subset$xPtsAgainst[intersect(avfinposs,mmteam)], event_subset$xPtsFor[intersect(intersect(avfinposs,diffteam),which(event_subset$TimeVideo < event_subset$TimeVideo[finposs]))],event_subset$xPtsAgainst[postfinposs]),
                                                       max(event_subset$xPtsAgainst[intersect(avfinposs,mmteam)], event_subset$xPtsFor[intersect(intersect(avfinposs,diffteam),which(event_subset$TimeVideo < event_subset$TimeVideo[finposs]))],event_subset$xPtsFor[postfinposs])), #2 intersects pour que ça ne considère pas le changement de possession à 0pts dans le calcul (important surtout pour les max XPA)
                                                #Cas où le premier événement est une fin de possession classique
                                                max(event_subset$xPtsAgainst[intersect(avfinposs,mmteam)], event_subset$xPtsFor[intersect(avfinposs,diffteam)])))))
                  event_subset$XPDmaxfinposs[i] <-
                    #Gestion cas où un essai arrive après un avantage sur faute def
                    ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & event_subset$StatName[arretjeu] == "Essai" & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                           max(event_subset$XPtsDiff[intersect(avarretjeu,mmteam)], -event_subset$XPtsDiff[intersect(avarretjeu,diffteam)]),
                           #Gestion faute def
                           ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[finposs] & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                                  ifelse(event_subset$TeamId[fautedef] == event_subset$TeamId[i],
                                         max(event_subset$XPtsDiff[intersect(avfautedef,mmteam)], -event_subset$XPtsDiff[intersect(avfautedef,diffteam)],-event_subset$XPtsDiff[fautedef]),
                                         max(event_subset$XPtsDiff[intersect(avfautedef,mmteam)], -event_subset$XPtsDiff[intersect(avfautedef,diffteam)],event_subset$XPtsDiff[fautedef])),
                                  #Cas où arrêt de jeu avant la fin de possession
                                  ifelse((event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & !is.na(arretjeu)) | is.na(finposs),
                                         max(event_subset$XPtsDiff[intersect(avarretjeu,mmteam)], -event_subset$XPtsDiff[intersect(avarretjeu,diffteam)]),
                                         #Cas où la fin de possession est un événement sans Xpts
                                         ifelse(event_subset$StatName[finposs] %in% c("Maul", "Passe", "Jeu au pied", "Franchissement", "Offload"),
                                                ifelse(event_subset$TeamId[postfinposs] == event_subset$TeamId[i],
                                                       max(event_subset$XPtsDiff[intersect(avfinposs,mmteam)], -event_subset$XPtsDiff[intersect(intersect(avfinposs,diffteam),which(event_subset$TimeVideo < event_subset$TimeVideo[finposs]))],event_subset$XPtsDiff[postfinposs]),
                                                       max(event_subset$XPtsDiff[intersect(avfinposs,mmteam)], -event_subset$XPtsDiff[intersect(intersect(avfinposs,diffteam),which(event_subset$TimeVideo < event_subset$TimeVideo[finposs]))],-event_subset$XPtsDiff[postfinposs])), #Cas où le max/min est le 0 dde la fin de poss ?
                                                #Cas où le premier événement est une fin de possession classique
                                                max(event_subset$XPtsDiff[intersect(avfinposs,mmteam)], -event_subset$XPtsDiff[intersect(avfinposs,diffteam)])))))
                  event_subset$XPDminfinposs[i] <-
                    #Gestion cas où un essai arrive après un avantage sur faute def
                    ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & event_subset$StatName[arretjeu] == "Essai" & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                           min(event_subset$XPtsDiff[intersect(avarretjeu,mmteam)], -event_subset$XPtsDiff[intersect(avarretjeu,diffteam)]),
                           #Gestion faute def
                           ifelse(event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[arretjeu] & event_subset$TimeVideo[fautedef] < event_subset$TimeVideo[finposs] & !is.na(fautedef) & !is.na(arretjeu) & !is.na(finposs),
                                  ifelse(event_subset$TeamId[fautedef] == event_subset$TeamId[i],
                                         min(event_subset$XPtsDiff[intersect(avfautedef,mmteam)], -event_subset$XPtsDiff[intersect(avfautedef,diffteam)],-event_subset$XPtsDiff[fautedef]),
                                         min(event_subset$XPtsDiff[intersect(avfautedef,mmteam)], -event_subset$XPtsDiff[intersect(avfautedef,diffteam)],event_subset$XPtsDiff[fautedef])),
                                  #Cas où arrêt de jeu avant la fin de possession
                                  ifelse((event_subset$TimeVideo[arretjeu] < event_subset$TimeVideo[finposs] & !is.na(arretjeu)) | is.na(finposs),
                                         min(event_subset$XPtsDiff[intersect(avarretjeu,mmteam)], -event_subset$XPtsDiff[intersect(avarretjeu,diffteam)]),
                                         #Cas où la fin de possession est un événement sans Xpts
                                         ifelse(event_subset$StatName[finposs] %in% c("Maul", "Passe", "Jeu au pied", "Franchissement", "Offload"),
                                                ifelse(event_subset$TeamId[postfinposs] == event_subset$TeamId[i],
                                                       min(event_subset$XPtsDiff[intersect(avfinposs,mmteam)], -event_subset$XPtsDiff[intersect(intersect(avfinposs,diffteam),which(event_subset$TimeVideo < event_subset$TimeVideo[finposs]))],event_subset$XPtsDiff[postfinposs]),
                                                       min(event_subset$XPtsDiff[intersect(avfinposs,mmteam)], -event_subset$XPtsDiff[intersect(intersect(avfinposs,diffteam),which(event_subset$TimeVideo < event_subset$TimeVideo[finposs]))],-event_subset$XPtsDiff[postfinposs])), #Cas où le min/min est le 0 dde la fin de poss ?
                                                #Cas où le premier événement est une fin de possession classique
                                                min(event_subset$XPtsDiff[intersect(avfinposs,mmteam)], -event_subset$XPtsDiff[intersect(avfinposs,diffteam)])))))

                }
              }
              event_subset$XPDfinposs <- event_subset$XPFfinposs - event_subset$XPAfinposs
              event_subset$XPFmaxfinposs[event_subset$XPFmaxfinposs == -Inf] <- NA
              event_subset$XPAminfinposs[event_subset$XPAminfinposs == Inf] <- NA
              event_subset$XPDmaxfinposs[event_subset$XPDmaxfinposs == -Inf] <- NA
              event_subset$XPFminfinposs[event_subset$XPFminfinposs == -Inf] <- NA
              event_subset$XPAmaxfinposs[event_subset$XPAmaxfinposs == Inf] <- NA
              event_subset$XPDminfinposs[event_subset$XPDminfinposs == -Inf] <- NA
            })
            cpt <- 0

            #### XP fin de séquence #####
            suppressWarnings({
              for (i in which(!(event_subset$StatName %in% c("Essai", "Conversion", "Penalite", "Faute off", "Faute def")) & (event_subset$StatName %in% input$selected_event) & X1 & X2 & Y1 & Y2 )) {

                #Gestion des avantages
                if (event_subset$ExclAvantage[i] != 0) {
                  event_subset$maxXPFfinseq[i] <- NA
                  event_subset$minXPAfinseq[i] <- NA
                  event_subset$maxXPDfinseq[i] <- NA
                  event_subset$minXPFfinseq[i] <- NA
                  event_subset$maxXPAfinseq[i] <- NA
                  event_subset$minXPDfinseq[i] <- NA}
                else {
                  tsarrjeu <- which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$TimeVideo < event_subset$TimeVideo[i] + 900 & event_subset$StatName %in% c("Essai", "Touche","Drop", "Melee", "Faute off", "Faute def", "Coup d'envoi"))[1]
                  mmteam <- which(event_subset$TeamId == event_subset$TeamId[i])
                  diffteam <- which(event_subset$TeamId != event_subset$TeamId[i])
                  fautedef <- which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Faute def"))[1]
                  avarrjeu <- which(event_subset$GameId == event_subset$GameId[i] & !(event_subset$StatName %in% c("Conversion", "Penalite", "Jeu au pied", "Maul", "Turnover concedee", "Franchissement", "Passe")) & event_subset$TimeVideo > event_subset$TimeVideo[i] & event_subset$TimeVideo < event_subset$TimeVideo[tsarrjeu])

                  if (is.na(tsarrjeu)) {
                    tsarrjeu <- which.max(event_subset$TimeVideo[which(event_subset$GameId[i] == event_subset$GameId)])
                    avarrjeu <- which(event_subset$GameId == event_subset$GameId[i] & !(event_subset$StatName %in% c("Conversion", "Penalite", "Jeu au pied", "Maul", "Turnover concedee", "Franchissement", "Passe")) & event_subset$TimeVideo > event_subset$TimeVideo[i] & event_subset$TimeVideo < event_subset$TimeVideo[tsarrjeu])
                    event_subset$maxXPFfinseq[i] <- max(event_subset$xPtsFor[intersect(mmteam,avarrjeu)],event_subset$xPtsAgainst[intersect(diffteam,avarrjeu)])
                    event_subset$minXPAfinseq[i] <- min(event_subset$xPtsAgainst[intersect(mmteam,avarrjeu)],event_subset$xPtsFor[intersect(diffteam,avarrjeu)])
                    event_subset$maxXPDfinseq[i] <- max(event_subset$XPtsDiff[intersect(mmteam,avarrjeu)],-event_subset$XPtsDiff[intersect(diffteam,avarrjeu)])
                  }
                  else{
                    event_subset$namefinseq[i] <- event_subset$name[tsarrjeu]
                    event_subset$StatNamefinseq[i] <- event_subset$StatName[tsarrjeu]
                    event_subset$XPFfinseq[i] <- #Gestion des fautes def
                      ifelse((event_subset$TeamId[tsarrjeu] == event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] !=  "Faute def") | (event_subset$TeamId[tsarrjeu] != event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] ==  "Faute def"),
                             event_subset$xPtsFor[tsarrjeu],
                             event_subset$xPtsAgainst[tsarrjeu])
                    event_subset$XPAfinseq[i] <-
                      ifelse((event_subset$TeamId[tsarrjeu] == event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] !=  "Faute def") | (event_subset$TeamId[tsarrjeu] != event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] ==  "Faute def"),
                             event_subset$xPtsAgainst[tsarrjeu],
                             event_subset$xPtsFor[tsarrjeu])
                    event_subset$maxXPFfinseq[i] <-
                      #Gestion des faute def en arrêt de jeu
                      ifelse((event_subset$TeamId[tsarrjeu] == event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] !=  "Faute def") | (event_subset$TeamId[tsarrjeu] != event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] ==  "Faute def"),
                             max(event_subset$xPtsFor[intersect(mmteam,avarrjeu)],event_subset$xPtsAgainst[intersect(diffteam,avarrjeu)],event_subset$xPtsFor[tsarrjeu], na.rm = TRUE),
                             max(event_subset$xPtsFor[intersect(mmteam,avarrjeu)], event_subset$xPtsAgainst[intersect(diffteam,avarrjeu)],event_subset$xPtsAgainst[tsarrjeu], na.rm = TRUE))

                    event_subset$minXPFfinseq[i] <-
                      #Gestion des faute def en arrêt de jeu
                      ifelse((event_subset$TeamId[tsarrjeu] == event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] !=  "Faute def") | (event_subset$TeamId[tsarrjeu] != event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] ==  "Faute def"),
                             min(event_subset$xPtsFor[intersect(mmteam,avarrjeu)],event_subset$xPtsAgainst[intersect(diffteam,avarrjeu)],event_subset$xPtsFor[tsarrjeu], na.rm = TRUE),
                             min(event_subset$xPtsFor[intersect(mmteam,avarrjeu)], event_subset$xPtsAgainst[intersect(diffteam,avarrjeu)],event_subset$xPtsAgainst[tsarrjeu], na.rm = TRUE))

                    event_subset$minXPAfinseq[i] <-
                      ifelse((event_subset$TeamId[tsarrjeu] == event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] !=  "Faute def") | (event_subset$TeamId[tsarrjeu] != event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] ==  "Faute def"),
                             min(event_subset$xPtsAgainst[intersect(mmteam,avarrjeu)],event_subset$xPtsFor[intersect(diffteam,avarrjeu)],event_subset$xPtsAgainst[tsarrjeu], na.rm = TRUE),
                             min(event_subset$xPtsAgainst[intersect(mmteam,avarrjeu)], event_subset$xPtsFor[intersect(diffteam,avarrjeu)],event_subset$xPtsFor[tsarrjeu], na.rm = TRUE))

                    event_subset$maxXPAfinseq[i] <-
                      ifelse((event_subset$TeamId[tsarrjeu] == event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] !=  "Faute def") | (event_subset$TeamId[tsarrjeu] != event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] ==  "Faute def"),
                             max(event_subset$xPtsAgainst[intersect(mmteam,avarrjeu)],event_subset$xPtsFor[intersect(diffteam,avarrjeu)],event_subset$xPtsAgainst[tsarrjeu], na.rm = TRUE),
                             max(event_subset$xPtsAgainst[intersect(mmteam,avarrjeu)], event_subset$xPtsFor[intersect(diffteam,avarrjeu)],event_subset$xPtsFor[tsarrjeu], na.rm = TRUE))

                    event_subset$maxXPDfinseq[i] <-
                      ifelse((event_subset$TeamId[tsarrjeu] == event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] !=  "Faute def") | (event_subset$TeamId[tsarrjeu] != event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] ==  "Faute def"),
                             max(event_subset$XPtsDiff[intersect(mmteam,avarrjeu)],-event_subset$XPtsDiff[intersect(diffteam,avarrjeu)],event_subset$XPtsDiff[tsarrjeu], na.rm = TRUE),
                             max(event_subset$XPtsDiff[intersect(mmteam,avarrjeu)], -event_subset$XPtsDiff[intersect(diffteam,avarrjeu)],-event_subset$XPtsDiff[tsarrjeu], na.rm = TRUE))
                    event_subset$minXPDfinseq[i] <-
                      ifelse((event_subset$TeamId[tsarrjeu] == event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] !=  "Faute def") | (event_subset$TeamId[tsarrjeu] != event_subset$TeamId[i] & event_subset$StatName[tsarrjeu] ==  "Faute def"),
                             min(event_subset$XPtsDiff[intersect(mmteam,avarrjeu)],-event_subset$XPtsDiff[intersect(diffteam,avarrjeu)],event_subset$XPtsDiff[tsarrjeu], na.rm = TRUE),
                             min(event_subset$XPtsDiff[intersect(mmteam,avarrjeu)], -event_subset$XPtsDiff[intersect(diffteam,avarrjeu)],-event_subset$XPtsDiff[tsarrjeu], na.rm = TRUE))

                  }
                }
              }
              event_subset$XPDfinseq <- event_subset$XPFfinseq - event_subset$XPAfinseq
              event_subset$maxXPFfinseq[event_subset$maxXPFfinseq == -Inf] <- NA
              event_subset$minXPAfinseq[event_subset$minXPAfinseq == Inf] <- NA
              event_subset$maxXPDfinseq[event_subset$maxXPDfinseq == -Inf] <- NA
              event_subset$minXPFfinseq[event_subset$minXPFfinseq == -Inf] <- NA
              event_subset$maxXPAfinseq[event_subset$maxXPAfinseq == Inf] <- NA
              event_subset$minXPDfinseq[event_subset$minXPDfinseq == -Inf] <- NA
            })


            #### XP échéance ####
            for (i in which( !(event_subset$StatName %in% c("Essai", "Conversion", "Penalite")) & (event_subset$StatName %in% input$selected_event) & X1 & X2 & Y1 & Y2 )){
              cpt <- cpt+1
              # print(cpt)

              if(input$echeance == "Durée"){
                arrjeu <- which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche","Drop", "Melee", "Faute off", "Faute def", "Coup d'envoi"))[1]
                meltouche <- which(event_subset$GameId == event_subset$GameId[i] & event_subset$TimeVideo > event_subset$TimeVideo[i] + seq_time)[1]
                rucks <- which(event_subset$GameId == event_subset$GameId[i] & event_subset$StatName == "Ruck" & event_subset$TimeVideo > event_subset$TimeVideo[i] & event_subset$TimeVideo <= event_subset$TimeVideo[i] + seq_time)
                rech <- which(event_subset$GameId == event_subset$GameId[i] & !(event_subset$StatName %in% c("Conversion", "Penalite", "Jeu au pied", "Maul", "Turnover concedee", "Franchissement")) & event_subset$TimeVideo > event_subset$TimeVideo[i])
                arrjeumax <- which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off","Coup d'envoi"))[1]
                infarrjeu <- which(event_subset$TimeVideo <= event_subset$TimeVideo[arrjeumax])
                mmteam <- which(event_subset$TeamId == event_subset$TeamId[i])
                diffteam <- which(event_subset$TeamId != event_subset$TeamId[i])
                fautedef <- which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Faute def"))[1]
                inffautedef <- which(event_subset$TimeVideo < event_subset$TimeVideo[fautedef])
                meltouchemax <- which(event_subset$GameId == event_subset$GameId[i] & event_subset$TimeVideo > event_subset$TimeVideo[i] + seq_time)[1]
                infmeltouche <- which(event_subset$TimeVideo <= event_subset$TimeVideo[meltouchemax])
                inftps <- which(event_subset$TimeVideo <= event_subset$TimeVideo[i] + seq_time)
                retourpen <- which(event_subset$GameId == event_subset$GameId[i] & event_subset$TimeVideo > event_subset$TimeVideo[i] & event_subset$TimeVideo < event_subset$TimeVideo[i] + 900 & event_subset$StatName %in% c("Touche", "Melee", "Penalite"))[1]

                if (!is.na(retourpen) & event_subset$ExclAvantage[i] != 0 & event_subset$TimeVideo[retourpen] <= event_subset$TimeVideo[i] + seq_time) {
                  event_subset$XPFseq[i] <- NA
                  event_subset$XPAseq[i] <- NA
                  event_subset$XPDseq[i] <- NA
                  event_subset$maxXPFseq[i] <- NA
                  event_subset$minXPAseq[i] <- NA
                  event_subset$maxXPDseq[i] <- NA
                  event_subset$minXPFseq[i] <- NA
                  event_subset$maxXPAseq[i] <- NA
                  event_subset$minXPDseq[i] <- NA
                }else {

                  event_subset$XPDseq[i] <- #Gestion du cas où on a un arrêt de jeu dans les seq_time secondes
                    ifelse(event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] < seq_time & !is.na(arrjeu),
                           #Echange des XP pour les arrêts de jeu
                           ifelse((event_subset$TeamId[arrjeu] == event_subset$TeamId[i] & event_subset$StatName[arrjeu] !=  "Faute def") | (event_subset$TeamId[arrjeu] != event_subset$TeamId[i] & event_subset$StatName[arrjeu] ==  "Faute def"),
                                  event_subset$XPtsDiff[arrjeu],
                                  -event_subset$XPtsDiff[arrjeu]),

                           #Cas où on a touche ou mêlée après la fin des seq_timesec mais que ça a été sifflé avant
                           ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee") & !is.na(meltouche),
                                  #Echange des XP pour ce cas
                                  ifelse(event_subset$TeamId[meltouche] == event_subset$TeamId[i],
                                         event_subset$XPtsDiff[meltouche],
                                         -event_subset$XPtsDiff[meltouche]),
                                  #Cas où on a un ruck pour terminer les seq_time sec
                                  ifelse(length(rucks) == 0,
                                         event_subset$XPtsDiff[i],
                                         ifelse(event_subset$TeamId[rucks][which.max(event_subset$TimeVideo[rucks])] == event_subset$TeamId,
                                                event_subset$XPtsDiff[rucks][which.max(event_subset$TimeVideo[rucks])],
                                                -event_subset$XPtsDiff[rucks][which.max(event_subset$TimeVideo[rucks])]
                                         ))))

                  event_subset$XPFseq[i] <- #Gestion du cas où on a un arrêt de jeu dans les seq_time secondes
                    ifelse(event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] <= seq_time & !is.na(arrjeu),
                           #Echange des XP pour les arrêts de jeu
                           ifelse((event_subset$TeamId[arrjeu] == event_subset$TeamId[i] & event_subset$StatName[arrjeu] !=  "Faute def") | (event_subset$TeamId[arrjeu] != event_subset$TeamId[i] & event_subset$StatName[arrjeu] ==  "Faute def"),
                                  event_subset$xPtsFor[arrjeu],
                                  event_subset$xPtsAgainst[arrjeu]),

                           #Cas où on a touche mêlée après la fin des seq_timesec mais que ça a été sifflé avant
                           ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee") & !is.na(meltouche),
                                  #Echange des XP pour ce cas
                                  ifelse(event_subset$TeamId[meltouche] == event_subset$TeamId[i],
                                         event_subset$xPtsFor[meltouche],
                                         event_subset$xPtsAgainst[meltouche]),
                                  #Cas où on a un ruck pour terminer les seq_time sec
                                  ifelse(length(rucks) == 0,
                                         event_subset$xPtsFor[i],
                                         ifelse(event_subset$TeamId[rucks][which.max(event_subset$TimeVideo[rucks])] == event_subset$TeamId,
                                                event_subset$xPtsFor[rucks][which.max(event_subset$TimeVideo[rucks])],
                                                event_subset$xPtsAgainst[rucks][which.max(event_subset$TimeVideo[rucks])]
                                         )))
                    )
                  event_subset$XPAseq[i] <- #Gestion du cas où on a un arrêt de jeu dans les seq_time secondes
                    ifelse(event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] < seq_time & !is.na(arrjeu),
                           #Echange des XP pour les arrêts de jeu
                           ifelse((event_subset$TeamId[arrjeu] == event_subset$TeamId[i] & event_subset$StatName[arrjeu] !=  "Faute def") | (event_subset$TeamId[arrjeu] != event_subset$TeamId[i] & event_subset$StatName[arrjeu] ==  "Faute def"),
                                  event_subset$xPtsAgainst[arrjeu],
                                  event_subset$xPtsFor[arrjeu]),

                           #Cas où on a touche mêlée après la fin des seq_timesec mais que ça a été sifflé avant
                           ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee") & !is.na(meltouche),
                                  #Echange des XP pour ce cas
                                  ifelse(event_subset$TeamId[meltouche] == event_subset$TeamId[i],
                                         event_subset$xPtsAgainst[meltouche],
                                         event_subset$xPtsFor[meltouche]),
                                  #Cas où on a un ruck pour terminer les seq_time sec
                                  ifelse(length(rucks) == 0,
                                         event_subset$xPtsAgainst[i],
                                         ifelse(event_subset$TeamId[rucks][which.max(event_subset$TimeVideo[rucks])] == event_subset$TeamId,
                                                event_subset$xPtsAgainst[rucks][which.max(event_subset$TimeVideo[rucks])],
                                                event_subset$xPtsFor[rucks][which.max(event_subset$TimeVideo[rucks])]
                                         )))
                    )

                  event_subset$StatNameseq[i] <- #Gestion du cas où on a un arrêt de jeu dans les seq_time secondes
                    ifelse(event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] < seq_time & !is.na(arrjeu),
                           event_subset$StatName[which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def"))][1],

                           #Cas où on a touche ou mêlée après la fin des seq_timesec mais que ça a été sifflé avant
                           ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee") & !is.na(meltouche),
                                  #Echange des XP pour ce cas
                                  event_subset$StatName[meltouche],
                                  #Cas où on a un ruck pour terminer les seq_time sec
                                  ifelse(length(rucks) == 0,
                                         event_subset$StatName[i],
                                         event_subset$StatName[rucks][which.max(event_subset$TimeVideo[rucks])]
                                  )))

                  event_subset$nameseq[i] <- #Gestion du cas où on a un arrêt de jeu dans les seq_time secondes
                    ifelse(event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] < seq_time & !is.na(arrjeu),
                           event_subset$name[which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def"))][1],

                           #Cas où on a touche ou mêlée après la fin des seq_timesec mais que ça a été sifflé avant
                           ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee") & !is.na(meltouche),
                                  #Echange des XP pour ce cas
                                  event_subset$name[meltouche],
                                  #Cas où on a un ruck pour terminer les seq_time sec
                                  ifelse(length(rucks) == 0,
                                         event_subset$name[i],
                                         event_subset$name[rucks][which.max(event_subset$TimeVideo[rucks])]
                                  )))

                  event_subset$Xseq[i] <- #Gestion du cas où on a un arrêt de jeu dans les seq_time secondes
                    ifelse(event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] < seq_time & !is.na(arrjeu),
                           event_subset$X.sens[which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def"))][1],

                           ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee") & !is.na(meltouche),
                                  #Echange des XP pour ce cas
                                  event_subset$X.sens[meltouche],
                                  ifelse(length(rucks) == 0,
                                         event_subset$X.sens[i],
                                         event_subset$X.sens[rucks][which.max(event_subset$TimeVideo[rucks])]
                                  )))

                  event_subset$Yseq[i] <- #Gestion du cas où on a un arrêt de jeu dans les seq_time secondes
                    ifelse(event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] < seq_time & !is.na(arrjeu),
                           event_subset$Y.sens[which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def"))][1],

                           ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee") & !is.na(meltouche),
                                  #Echange des XP pour ce cas
                                  event_subset$Y.sens[meltouche],
                                  ifelse(length(rucks) == 0,
                                         event_subset$Y.sens[i],
                                         event_subset$Y.sens[rucks][which.max(event_subset$TimeVideo[rucks])]
                                  )))
                  event_subset$maxXPDseq[i] <-
                    ifelse( #Je prends le max de XPF entre l'action de départ et celle d'arrivée déterminée avec le ifelse
                      #Cas arrêt de jeu dans les seq_time sec
                      event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] <= seq_time & !is.na(arrjeu),
                      max(event_subset$XPtsDiff[intersect(intersect(rech,mmteam),infarrjeu)],-event_subset$XPtsDiff[intersect(intersect(rech,diffteam),infarrjeu)]),
                      #Cas faute def dans les seq_timesec => on applique le max entre le max des XPF des actions et le XPF/FPA de la faute def selon le TeamId
                      #Si faute def a le même TeamId, on compare le max des actions avant avec l'inversion
                      ifelse(event_subset$TimeVideo[fautedef] - event_subset$TimeVideo[i] <= seq_time & event_subset$TeamId[fautedef] == event_subset$TeamId[i] & !is.na(fautedef),
                             max(event_subset$XPtsDiff[intersect(intersect(rech,mmteam),inffautedef)],-event_subset$XPtsDiff[fautedef], -event_subset$xPtsAgainst[intersect(intersect(rech,diffteam),inffautedef)]), #Gestion arrêt de jeu par faute def
                             #Si faute def a un TeamId différent, on trouve le max des XPF des actions avant et de la faute def
                             ifelse(event_subset$TimeVideo[fautedef] - event_subset$TimeVideo[i] <= seq_time & event_subset$TeamId[fautedef] != event_subset$TeamId[i] & !is.na(fautedef),
                                    max(event_subset$XPtsDiff[intersect(intersect(rech,mmteam),inffautedef)],event_subset$XPtsDiff[fautedef], -event_subset$XPtsDiff[intersect(intersect(rech,diffteam),inffautedef)]), #Simplifiable en mettant <= car on prendrait directement en compte la faute def (vu qu'ici on prend aussi le XPF)
                                    #Cas où on a touche ou mêlée en première action en dehors des seq_timesec
                                    ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee") & !is.na(meltouche),
                                           max(event_subset$XPtsDiff[intersect(intersect(rech,mmteam),infmeltouche)], -event_subset$XPtsDiff[intersect(intersect(rech,diffteam),infmeltouche)]), #Cas touche ou mêlée immédiatement après les seq_time sec
                                           #Cas où on as pas de fin d'action avant les seq_timesec
                                           if(all(is.na(event_subset$XPtsDiff[intersect(intersect(rech,mmteam),inftps)])) & all(is.na(-event_subset$XPtsDiff[intersect(intersect(rech,diffteam),inftps)]))) NA
                                           else max(c(event_subset$XPtsDiff[intersect(intersect(rech,mmteam),inftps)], -event_subset$XPtsDiff[intersect(intersect(rech,diffteam),inftps)]), na.rm = TRUE)

                                    ))))
                  event_subset$minXPDseq[i] <-
                    ifelse( #Je prends le min de XPF entre l'action de départ et celle d'arrivée déterminée avec le ifelse
                      #Cas arrêt de jeu dans les seq_time sec
                      event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] <= seq_time & !is.na(arrjeu),
                      min(event_subset$XPtsDiff[intersect(intersect(rech,mmteam),infarrjeu)],-event_subset$XPtsDiff[intersect(intersect(rech,diffteam),infarrjeu)]),
                      #Cas faute def dans les seq_timesec => on applique le min entre le min des XPF des actions et le XPF/FPA de la faute def selon le TeamId
                      #Si faute def a le même TeamId, on compare le min des actions avant avec l'inversion
                      ifelse(event_subset$TimeVideo[fautedef] - event_subset$TimeVideo[i] <= seq_time & event_subset$TeamId[fautedef] == event_subset$TeamId[i] & !is.na(fautedef),
                             min(event_subset$XPtsDiff[intersect(intersect(rech,mmteam),inffautedef)],-event_subset$XPtsDiff[fautedef], -event_subset$xPtsAgainst[intersect(intersect(rech,diffteam),inffautedef)]), #Gestion arrêt de jeu par faute def
                             #Si faute def a un TeamId différent, on trouve le min des XPF des actions avant et de la faute def
                             ifelse(event_subset$TimeVideo[fautedef] - event_subset$TimeVideo[i] <= seq_time & event_subset$TeamId[fautedef] != event_subset$TeamId[i] & !is.na(fautedef),
                                    min(event_subset$XPtsDiff[intersect(intersect(rech,mmteam),inffautedef)],event_subset$XPtsDiff[fautedef], -event_subset$XPtsDiff[intersect(intersect(rech,diffteam),inffautedef)]), #Simplifiable en mettant <= car on prendrait directement en compte la faute def (vu qu'ici on prend aussi le XPF)
                                    #Cas où on a touche ou mêlée en première action en dehors des seq_timesec
                                    ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee") & !is.na(meltouche),
                                           min(event_subset$XPtsDiff[intersect(intersect(rech,mmteam),infmeltouche)], -event_subset$XPtsDiff[intersect(intersect(rech,diffteam),infmeltouche)]), #Cas touche ou mêlée immédiatement après les seq_time sec
                                           #Cas où on as pas de fin d'action avant les seq_timesec
                                           if(all(is.na(event_subset$XPtsDiff[intersect(intersect(rech,mmteam),inftps)])) & all(is.na(-event_subset$XPtsDiff[intersect(intersect(rech,diffteam),inftps)]))) NA
                                           else min(c(event_subset$XPtsDiff[intersect(intersect(rech,mmteam),inftps)], -event_subset$XPtsDiff[intersect(intersect(rech,diffteam),inftps)]), na.rm = TRUE)

                                    ))))
                  event_subset$minXPAseq[i] <-
                    ifelse( #Je prends le max de XPF entre l'action de départ et celle d'arrivée déterminée avec le ifelse
                      #Cas arrêt de jeu dans les seq_time sec
                      event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] <= seq_time & !is.na(arrjeu),
                      min(event_subset$xPtsAgainst[intersect(intersect(rech,mmteam),infarrjeu)], event_subset$xPtsFor[intersect(intersect(rech,diffteam),infarrjeu)]),
                      #Cas faute def dans les seq_timesec => on applique le max entre le max des XPF des actions et le XPF/FPA de la faute def selon le TeamId
                      #Si faute def a le même TeamId, on compare le max des actions avant avec l'inversion
                      ifelse(event_subset$TimeVideo[fautedef] - event_subset$TimeVideo[i] <= seq_time & event_subset$TeamId[fautedef] == event_subset$TeamId[i] & !is.na(fautedef),
                             min(event_subset$xPtsAgainst[intersect(intersect(rech,mmteam),inffautedef)],event_subset$xPtsFor[fautedef], event_subset$xPtsFor[intersect(intersect(rech,diffteam),inffautedef)]), #Gestion arrêt de jeu par faute def
                             #Si faute def a un TeamId différent, on trouve le max des XPF des actions avant et de la faute def
                             ifelse(event_subset$TimeVideo[fautedef] - event_subset$TimeVideo[i] <= seq_time & event_subset$TeamId[fautedef] != event_subset$TeamId[i] & !is.na(fautedef),
                                    min(event_subset$xPtsAgainst[intersect(intersect(rech,mmteam),inffautedef)],event_subset$xPtsAgainst[fautedef], event_subset$xPtsFor[fautedef], event_subset$xPtsFor[intersect(intersect(rech,diffteam),inffautedef)]), #Simplifiable en mettant <= car on prendrait directement en compte la faute def (vu qu'ici on prend aussi le XPF)
                                    #Cas où on a touche ou mêlée en première action en dehors des seq_timesec
                                    ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee") & !is.na(meltouche),
                                           min(event_subset$xPtsAgainst[intersect(intersect(rech,mmteam),infmeltouche)], event_subset$xPtsFor[intersect(intersect(rech,diffteam),infmeltouche)]), #Cas touche ou mêlée immédiatement après les seq_time sec
                                           #Cas où on as pas de fin d'action avant les seq_timesec
                                           if(all(is.na(event_subset$xPtsAgainst[intersect(intersect(rech,mmteam),inftps)])) & all(is.na(event_subset$xPtsFor[intersect(intersect(rech,diffteam),inftps)]))) NA
                                           else min(c(event_subset$xPtsAgainst[intersect(intersect(rech,mmteam),inftps)], event_subset$xPtsFor[intersect(intersect(rech,diffteam),inftps)]), na.rm = TRUE)
                                    ))))
                  event_subset$maxXPAseq[i] <-
                    ifelse( #Je prends le max de XPF entre l'action de départ et celle d'arrivée détermaxée avec le ifelse
                      #Cas arrêt de jeu dans les seq_time sec
                      event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] <= seq_time & !is.na(arrjeu),
                      max(event_subset$xPtsAgainst[intersect(intersect(rech,mmteam),infarrjeu)], event_subset$xPtsFor[intersect(intersect(rech,diffteam),infarrjeu)]),
                      #Cas faute def dans les seq_timesec => on applique le max entre le max des XPF des actions et le XPF/FPA de la faute def selon le TeamId
                      #Si faute def a le même TeamId, on compare le max des actions avant avec l'inversion
                      ifelse(event_subset$TimeVideo[fautedef] - event_subset$TimeVideo[i] <= seq_time & event_subset$TeamId[fautedef] == event_subset$TeamId[i] & !is.na(fautedef),
                             max(event_subset$xPtsAgainst[intersect(intersect(rech,mmteam),inffautedef)],event_subset$xPtsFor[fautedef], event_subset$xPtsFor[intersect(intersect(rech,diffteam),inffautedef)]), #Gestion arrêt de jeu par faute def
                             #Si faute def a un TeamId différent, on trouve le max des XPF des actions avant et de la faute def
                             ifelse(event_subset$TimeVideo[fautedef] - event_subset$TimeVideo[i] <= seq_time & event_subset$TeamId[fautedef] != event_subset$TeamId[i] & !is.na(fautedef),
                                    max(event_subset$xPtsAgainst[intersect(intersect(rech,mmteam),inffautedef)],event_subset$xPtsAgainst[fautedef], event_subset$xPtsFor[fautedef], event_subset$xPtsFor[intersect(intersect(rech,diffteam),inffautedef)]), #Simplifiable en mettant <= car on prendrait directement en compte la faute def (vu qu'ici on prend aussi le XPF)
                                    #Cas où on a touche ou mêlée en première action en dehors des seq_timesec
                                    ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee") & !is.na(meltouche),
                                           max(event_subset$xPtsAgainst[intersect(intersect(rech,mmteam),infmeltouche)], event_subset$xPtsFor[intersect(intersect(rech,diffteam),infmeltouche)]), #Cas touche ou mêlée immédiatement après les seq_time sec
                                           #Cas où on as pas de fin d'action avant les seq_timesec
                                           if(all(is.na(event_subset$xPtsAgainst[intersect(intersect(rech,mmteam),inftps)])) & all(is.na(event_subset$xPtsFor[intersect(intersect(rech,diffteam),inftps)]))) NA
                                           else max(c(event_subset$xPtsAgainst[intersect(intersect(rech,mmteam),inftps)], event_subset$xPtsFor[intersect(intersect(rech,diffteam),inftps)]), na.rm = TRUE)
                                    ))))
                  event_subset$maxXPFseq[i] <-
                    ifelse( #Je prends le max de XPF entre l'action de départ et celle d'arrivée déterminée avec le ifelse
                      #Cas arrêt de jeu dans les 30 sec
                      event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] <= seq_time & !is.na(arrjeu),
                      max(event_subset$xPtsFor[intersect(intersect(rech,mmteam),infarrjeu)], event_subset$xPtsAgainst[intersect(intersect(rech,diffteam),infarrjeu)]),
                      #Cas faute def dans les 30sec => on applique le max entre le max des XPF des actions et le XPF/FPA de la faute def selon le TeamId
                      #Si faute def a le même TeamId, on compare le max des actions avant avec l'inversion
                      ifelse(event_subset$TimeVideo[fautedef] - event_subset$TimeVideo[i] <= seq_time & event_subset$TeamId[fautedef] == event_subset$TeamId[i] & !is.na(fautedef),
                             max(event_subset$xPtsFor[intersect(intersect(rech,mmteam),inffautedef)],event_subset$xPtsAgainst[fautedef],event_subset$xPtsAgainst[intersect(intersect(rech,diffteam),inffautedef)]), #Gestion arrêt de jeu par faute def
                             #Si faute def a un TeamId différent, on trouve le max des XPF des actions avant et de la faute def
                             ifelse(event_subset$TimeVideo[fautedef] - event_subset$TimeVideo[i] <= seq_time & event_subset$TeamId[fautedef] != event_subset$TeamId[i] & !is.na(fautedef),
                                    max(event_subset$xPtsFor[intersect(intersect(rech,mmteam),inffautedef)],event_subset$xPtsFor[fautedef],event_subset$xPtsAgainst[intersect(intersect(rech,diffteam),inffautedef)]), #Simplifiable en mettant <= car on prendrait directement en compte la faute def (vu qu'ici on prend aussi le XPF)
                                    #Cas où on a touche ou mêlée en première action en dehors des 30sec
                                    ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee") & !is.na(meltouche),
                                           max(event_subset$xPtsFor[intersect(intersect(rech,mmteam),infmeltouche)], event_subset$xPtsAgainst[intersect(intersect(rech,diffteam),infmeltouche)]), #Cas touche ou mêlée immédiatement après les 30 sec
                                           #Cas où on as pas de fin d'action avant les 30sec
                                           if(all(is.na(event_subset$xPtsFor[intersect(intersect(rech,mmteam),inftps)])) & all(is.na(event_subset$xPtsAgainst[intersect(intersect(rech,diffteam),inftps)]))) NA
                                           else max(c(event_subset$xPtsFor[intersect(intersect(rech,mmteam),inftps)], event_subset$xPtsAgainst[intersect(intersect(rech,diffteam),inftps)]), na.rm = TRUE)
                                    ))))
                  event_subset$minXPFseq[i] <-
                    ifelse( #Je prends le min de XPF entre l'action de départ et celle d'arrivée déterminée avec le ifelse
                      #Cas arrêt de jeu dans les 30 sec
                      event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] <= seq_time & !is.na(arrjeu),
                      min(event_subset$xPtsFor[intersect(intersect(rech,mmteam),infarrjeu)], event_subset$xPtsAgainst[intersect(intersect(rech,diffteam),infarrjeu)]),
                      #Cas faute def dans les 30sec => on applique le min entre le min des XPF des actions et le XPF/FPA de la faute def selon le TeamId
                      #Si faute def a le même TeamId, on compare le min des actions avant avec l'inversion
                      ifelse(event_subset$TimeVideo[fautedef] - event_subset$TimeVideo[i] <= seq_time & event_subset$TeamId[fautedef] == event_subset$TeamId[i] & !is.na(fautedef),
                             min(event_subset$xPtsFor[intersect(intersect(rech,mmteam),inffautedef)],event_subset$xPtsAgainst[fautedef],event_subset$xPtsAgainst[intersect(intersect(rech,diffteam),inffautedef)]), #Gestion arrêt de jeu par faute def
                             #Si faute def a un TeamId différent, on trouve le min des XPF des actions avant et de la faute def
                             ifelse(event_subset$TimeVideo[fautedef] - event_subset$TimeVideo[i] <= seq_time & event_subset$TeamId[fautedef] != event_subset$TeamId[i] & !is.na(fautedef),
                                    min(event_subset$xPtsFor[intersect(intersect(rech,mmteam),inffautedef)],event_subset$xPtsFor[fautedef],event_subset$xPtsAgainst[intersect(intersect(rech,diffteam),inffautedef)]), #Simplifiable en mettant <= car on prendrait directement en compte la faute def (vu qu'ici on prend aussi le XPF)
                                    #Cas où on a touche ou mêlée en première action en dehors des 30sec
                                    ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee") & !is.na(meltouche),
                                           min(event_subset$xPtsFor[intersect(intersect(rech,mmteam),infmeltouche)], event_subset$xPtsAgainst[intersect(intersect(rech,diffteam),infmeltouche)]), #Cas touche ou mêlée immédiatement après les 30 sec
                                           #Cas où on as pas de fin d'action avant les 30sec
                                           if(all(is.na(event_subset$xPtsFor[intersect(intersect(rech,mmteam),inftps)])) & all(is.na(event_subset$xPtsAgainst[intersect(intersect(rech,diffteam),inftps)]))) NA
                                           else min(c(event_subset$xPtsFor[intersect(intersect(rech,mmteam),inftps)], event_subset$xPtsAgainst[intersect(intersect(rech,diffteam),inftps)]), na.rm = TRUE)
                                    ))))

                  event_subset$maxXPDseq[event_subset$maxXPDseq == -Inf] <- NA
                  event_subset$minXPAseq[event_subset$minXPAseq == Inf] <- NA
                  event_subset$maxXPFseq[event_subset$maxXPDseq == -Inf] <- NA
                  event_subset$minXPDseq[event_subset$minXPDseq == -Inf] <- NA
                  event_subset$maxXPAseq[event_subset$maxXPAseq == Inf] <- NA
                  event_subset$minXPFseq[event_subset$minXPDseq == -Inf] <- NA

                  # si l'équipe garde la balle on garde les coordonnées, si elle l'a perd on inverse
                  if(!(event_subset$nameseq[i] %in% event_subset$name[i]) || !(event_subset$name[i] %in% event_subset$nameseq[i])){
                    event_subset$Xseq[i] <- 1000 - event_subset$Xseq[i]
                    event_subset$Yseq[i] <- 700 - event_subset$Yseq[i]
                  }else if(event_subset$nameseq[i] %in% event_subset$name[i] || (event_subset$name[i] %in% event_subset$nameseq[i])){
                    event_subset$Xseq[i] <- event_subset$Xseq[i]
                    event_subset$Yseq[i] <- event_subset$Yseq[i]
                  }
                }

              }else if(input$echeance == "Temps de jeu"){
                arrjeu <- which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$TimeVideo < event_subset$TimeVideo[i] + 900 & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def", "Coup d'envoi"))[1]
                nruck <- which(event_subset$GameId == event_subset$GameId[i] & event_subset$TimeVideo > event_subset$TimeVideo[i] & event_subset$TimeVideo < event_subset$TimeVideo[i] + 900 & event_subset$StatName == "Ruck")[seq_time]
                evt_mmteam <- which(event_subset$GameId == event_subset$GameId[i] & !(event_subset$StatName %in% c("Conversion", "Penalite", "Jeu au pied", "Maul", "Turnover concedee", "Franchissement")) & event_subset$TeamId == event_subset$TeamId[i] & event_subset$TimeVideo > event_subset$TimeVideo[i])
                evt_diffteam <- which(event_subset$GameId == event_subset$GameId[i] & event_subset$TeamId != event_subset$TeamId[i] & !(event_subset$StatName %in% c("Conversion", "Penalite", "Jeu au pied", "Maul", "Turnover concedee", "Franchissement")) & event_subset$TimeVideo > event_subset$TimeVideo[i])
                infarrjeu <- which(event_subset$TimeVideo < event_subset$TimeVideo[arrjeu])
                infegarrjeu <- which(event_subset$TimeVideo <= event_subset$TimeVideo[arrjeu])
                infegnruck <- which(event_subset$TimeVideo <= event_subset$TimeVideo[nruck])
                retourpen <- which(event_subset$GameId == event_subset$GameId[i] & event_subset$TimeVideo > event_subset$TimeVideo[i] & event_subset$TimeVideo < event_subset$TimeVideo[i] + 900 & event_subset$StatName %in% c("Touche", "Melee", "Penalite"))[1]

                if (!is.na(retourpen) & event_subset$ExclAvantage[i] != 0 & !is.na(nruck) & event_subset$TimeVideo[retourpen] < event_subset$TimeVideo[nruck]) {
                  event_subset$XPFseq[i] <- NA
                  event_subset$XPAseq[i] <- NA
                  event_subset$XPDseq[i] <- NA
                  event_subset$maxXPFseq[i] <- NA
                  event_subset$minXPAseq[i] <- NA
                  event_subset$maxXPDseq[i] <- NA
                  event_subset$minXPFseq[i] <- NA
                  event_subset$maxXPAseq[i] <- NA
                  event_subset$minXPDseq[i] <- NA
                }else{
                  event_subset$XPDseq[i] <-  ifelse((event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck] & !is.na(event_subset$TimeVideo[arrjeu])) | is.na(event_subset$TimeVideo[nruck]),
                                                    #Inversion des XP dans le cas d'arrêts de jeu
                                                    ifelse((event_subset$TeamId[arrjeu] == event_subset$TeamId[i] & event_subset$StatName[arrjeu] != "Faute def") | (event_subset$TeamId[arrjeu] != event_subset$TeamId[i] & event_subset$StatName[arrjeu] == "Faute def"),
                                                           event_subset$XPtsDiff[arrjeu],
                                                           -event_subset$XPtsDiff[arrjeu]),
                                                    #Inversion dans le cas du 5ème ruck
                                                    ifelse((event_subset$TeamId[nruck] == event_subset$TeamId[i] & event_subset$StatName[nruck] != "Faute def") | (event_subset$TeamId[nruck] != event_subset$TeamId[i] & event_subset$StatName[nruck] == "Faute def"),
                                                           event_subset$XPtsDiff[nruck],
                                                           -event_subset$XPtsDiff[nruck])

                  )
                  event_subset$XPFseq[i] <- #Gestion avantage puis attribution des XP
                    ifelse((event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck] & !is.na(event_subset$TimeVideo[arrjeu]) | is.na(event_subset$TimeVideo[nruck])),
                           #Inversion des XP dans le cas d'arrêts de jeu
                           ifelse((event_subset$TeamId[arrjeu] == event_subset$TeamId[i] & event_subset$StatName[arrjeu] != "Faute def") | (event_subset$TeamId[arrjeu] != event_subset$TeamId[i] & event_subset$StatName[arrjeu] == "Faute def"),
                                  event_subset$xPtsFor[arrjeu],
                                  event_subset$xPtsAgainst[arrjeu]),
                           #Inversion
                           ifelse(((event_subset$TeamId[nruck] == event_subset$TeamId[i] & event_subset$StatName[nruck] != "Faute def") | (event_subset$TeamId[nruck] != event_subset$TeamId[i] & event_subset$StatName[nruck] == "Faute def")),
                                  event_subset$xPtsFor[nruck],
                                  event_subset$xPtsAgainst[nruck])

                    )
                  event_subset$XPAseq[i] <- ifelse((event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck] & !is.na(event_subset$TimeVideo[arrjeu])) | is.na(event_subset$TimeVideo[nruck]),
                                                   #Inversion des XP dans le cas d'arrêts de jeu
                                                   ifelse((event_subset$TeamId[arrjeu] == event_subset$TeamId[i] & event_subset$StatName[arrjeu] != "Faute def") | (event_subset$TeamId[arrjeu] != event_subset$TeamId[i] & event_subset$StatName[arrjeu] == "Faute def"),
                                                          event_subset$xPtsAgainst[arrjeu],
                                                          event_subset$xPtsFor[arrjeu]),
                                                   #Inversion dans le cas du 5ème ruck
                                                   ifelse((event_subset$TeamId[nruck] == event_subset$TeamId[i] & event_subset$StatName[nruck] != "Faute def") | (event_subset$TeamId[nruck] != event_subset$TeamId[i] & event_subset$StatName[nruck] == "Faute def"),
                                                          event_subset$xPtsAgainst[nruck],
                                                          event_subset$xPtsFor[nruck])
                  )

                  event_subset$StatNameseq[i] <- ifelse((event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck] & !is.na(event_subset$TimeVideo[arrjeu])) | is.na(event_subset$TimeVideo[nruck]),
                                                        event_subset$StatName[arrjeu],
                                                        event_subset$StatName[nruck]

                  )
                  event_subset$nameseq[i] <- ifelse((event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck] & !is.na(event_subset$TimeVideo[arrjeu])) | is.na(event_subset$TimeVideo[nruck]),
                                                    event_subset$name[arrjeu],
                                                    event_subset$name[nruck]

                  )

                  event_subset$Xseq[i] <- ifelse((event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck] & !is.na(event_subset$TimeVideo[arrjeu])) | is.na(event_subset$TimeVideo[nruck]),
                                                 event_subset$X.sens[arrjeu],
                                                 event_subset$X.sens[nruck]

                  )
                  event_subset$Yseq[i] <- ifelse((event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck] & !is.na(event_subset$TimeVideo[arrjeu])) | is.na(event_subset$TimeVideo[nruck]),
                                                 event_subset$Y.sens[arrjeu],
                                                 event_subset$Y.sens[nruck]

                  )

                  event_subset$maxXPDseq[i] <- ifelse((event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck] & !is.na(event_subset$TimeVideo[arrjeu])) | is.na(event_subset$TimeVideo[nruck]),
                                                      #Inversion des XP dans le cas d'arrêts de jeu
                                                      ifelse(event_subset$StatName[arrjeu] != "Faute def",
                                                             max(event_subset$XPtsDiff[intersect(evt_mmteam,infegarrjeu)], -event_subset$XPtsDiff[intersect(evt_diffteam,infegarrjeu)]),
                                                             ifelse(event_subset$TeamId[arrjeu] == event_subset$TeamId[i] & !is.na(event_subset$TimeVideo[arrjeu]),
                                                                    max(event_subset$XPtsDiff[intersect(evt_mmteam,infarrjeu)], -event_subset$XPtsDiff[arrjeu],-event_subset$XPtsDiff[intersect(evt_diffteam,infarrjeu)]),
                                                                    max(event_subset$XPtsDiff[intersect(evt_mmteam,infarrjeu)], event_subset$XPtsDiff[arrjeu],-event_subset$XPtsDiff[intersect(evt_diffteam,infarrjeu)]))),
                                                      max(event_subset$XPtsDiff[intersect(evt_mmteam,infegnruck)], -event_subset$XPtsDiff[intersect(evt_diffteam,infegnruck)])
                  )

                  event_subset$minXPDseq[i] <- ifelse((event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck] & !is.na(event_subset$TimeVideo[arrjeu])) | is.na(event_subset$TimeVideo[nruck]),
                                                      #Inversion des XP dans le cas d'arrêts de jeu
                                                      ifelse(event_subset$StatName[arrjeu] != "Faute def",
                                                             min(event_subset$XPtsDiff[intersect(evt_mmteam,infegarrjeu)], -event_subset$XPtsDiff[intersect(evt_diffteam,infegarrjeu)]),
                                                             ifelse(event_subset$TeamId[arrjeu] == event_subset$TeamId[i] & !is.na(event_subset$TimeVideo[arrjeu]),
                                                                    min(event_subset$XPtsDiff[intersect(evt_mmteam,infarrjeu)], -event_subset$XPtsDiff[arrjeu],-event_subset$XPtsDiff[intersect(evt_diffteam,infarrjeu)]),
                                                                    min(event_subset$XPtsDiff[intersect(evt_mmteam,infarrjeu)], event_subset$XPtsDiff[arrjeu],-event_subset$XPtsDiff[intersect(evt_diffteam,infarrjeu)]))),
                                                      min(event_subset$XPtsDiff[intersect(evt_mmteam,infegnruck)], -event_subset$XPtsDiff[intersect(evt_diffteam,infegnruck)])
                  )
                  event_subset$maxXPFseq[i] <- ifelse(event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck] & !is.na(event_subset$TimeVideo[arrjeu]) | is.na(event_subset$TimeVideo[nruck]),
                                                      #Inversion des XP dans le cas d'arrêts de jeu
                                                      ifelse(event_subset$StatName[arrjeu] != "Faute def",
                                                             max(event_subset$xPtsFor[intersect(evt_mmteam, infegarrjeu)], event_subset$xPtsAgainst[intersect(evt_diffteam, infegarrjeu)]),
                                                             ifelse(event_subset$TeamId[arrjeu] == event_subset$TeamId[i] & !is.na(event_subset$TimeVideo[arrjeu]),
                                                                    max(event_subset$xPtsFor[intersect(evt_mmteam,infarrjeu)], event_subset$xPtsAgainst[arrjeu],event_subset$xPtsAgainst[intersect(evt_diffteam,infarrjeu)]),
                                                                    max(event_subset$xPtsFor[intersect(evt_mmteam,infarrjeu)], event_subset$xPtsFor[arrjeu],event_subset$xPtsAgainst[intersect(evt_diffteam,infarrjeu)]))),
                                                      max(event_subset$xPtsFor[intersect(evt_mmteam,infegnruck)], event_subset$xPtsAgainst[intersect(evt_diffteam,infegnruck)])
                  )

                  event_subset$minXPFseq[i] <- ifelse(event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck] & !is.na(event_subset$TimeVideo[arrjeu]) | is.na(event_subset$TimeVideo[nruck]),
                                                      #Inversion des XP dans le cas d'arrêts de jeu
                                                      ifelse(event_subset$StatName[arrjeu] != "Faute def",
                                                             min(event_subset$xPtsFor[intersect(evt_mmteam, infegarrjeu)], event_subset$xPtsAgainst[intersect(evt_diffteam, infegarrjeu)]),
                                                             ifelse(event_subset$TeamId[arrjeu] == event_subset$TeamId[i] & !is.na(event_subset$TimeVideo[arrjeu]),
                                                                    min(event_subset$xPtsFor[intersect(evt_mmteam,infarrjeu)], event_subset$xPtsAgainst[arrjeu],event_subset$xPtsAgainst[intersect(evt_diffteam,infarrjeu)]),
                                                                    min(event_subset$xPtsFor[intersect(evt_mmteam,infarrjeu)], event_subset$xPtsFor[arrjeu],event_subset$xPtsAgainst[intersect(evt_diffteam,infarrjeu)]))),
                                                      min(event_subset$xPtsFor[intersect(evt_mmteam,infegnruck)], event_subset$xPtsAgainst[intersect(evt_diffteam,infegnruck)])
                  )

                  event_subset$maxXPAseq[i] <- ifelse((event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck] & !is.na(event_subset$TimeVideo[arrjeu])) | is.na(event_subset$TimeVideo[nruck]),
                                                      #Inversion des XP dans le cas d'arrêts de jeu
                                                      ifelse(event_subset$StatName[arrjeu] != "Faute def",
                                                             max(event_subset$xPtsFor[intersect(evt_diffteam,infegarrjeu)], event_subset$xPtsAgainst[intersect(evt_mmteam,infegarrjeu)]),
                                                             ifelse(event_subset$TeamId[arrjeu] == event_subset$TeamId[i] & !is.na(event_subset$TimeVideo[arrjeu]),
                                                                    max(event_subset$xPtsFor[intersect(evt_diffteam,infarrjeu)], event_subset$xPtsFor[arrjeu],event_subset$xPtsAgainst[intersect(evt_mmteam,infarrjeu)]),
                                                                    max(event_subset$xPtsFor[intersect(evt_diffteam,infarrjeu)], event_subset$xPtsAgainst[arrjeu],event_subset$xPtsAgainst[intersect(evt_mmteam,infarrjeu)]))),
                                                      max(event_subset$xPtsFor[intersect(evt_diffteam,infegnruck)], event_subset$xPtsAgainst[intersect(evt_mmteam,infegnruck)])
                  )
                  event_subset$minXPAseq[i] <- ifelse((event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck] & !is.na(event_subset$TimeVideo[arrjeu])) | is.na(event_subset$TimeVideo[nruck]),
                                                      #Inversion des XP dans le cas d'arrêts de jeu
                                                      ifelse(event_subset$StatName[arrjeu] != "Faute def",
                                                             min(event_subset$xPtsFor[intersect(evt_diffteam,infegarrjeu)], event_subset$xPtsAgainst[intersect(evt_mmteam,infegarrjeu)]),
                                                             ifelse(event_subset$TeamId[arrjeu] == event_subset$TeamId[i] & !is.na(event_subset$TimeVideo[arrjeu]),
                                                                    min(event_subset$xPtsFor[intersect(evt_diffteam,infarrjeu)], event_subset$xPtsFor[arrjeu],event_subset$xPtsAgainst[intersect(evt_mmteam,infarrjeu)]),
                                                                    min(event_subset$xPtsFor[intersect(evt_diffteam,infarrjeu)], event_subset$xPtsAgainst[arrjeu],event_subset$xPtsAgainst[intersect(evt_mmteam,infarrjeu)]))),
                                                      min(event_subset$xPtsFor[intersect(evt_diffteam,infegnruck)], event_subset$xPtsAgainst[intersect(evt_mmteam,infegnruck)])
                  )



                  # si l'équipe garde la balle on garde les coordonnées, si elle l'a perd on inverse
                  if(!(event_subset$nameseq[i] %in% event_subset$name[i]) || !(event_subset$name[i] %in% event_subset$nameseq[i])){
                    event_subset$Xseq[i] <- 1000 - event_subset$Xseq[i]
                    event_subset$Yseq[i] <- 700 - event_subset$Yseq[i]
                  }else if(event_subset$nameseq[i] %in% event_subset$name[i] || (event_subset$name[i] %in% event_subset$nameseq[i])){
                    event_subset$Xseq[i] <- event_subset$Xseq[i]
                    event_subset$Yseq[i] <- event_subset$Yseq[i]
                  }
                  event_subset$maxXPDseq[event_subset$maxXPDseq == -Inf] <- NA
                  event_subset$minXPAseq[event_subset$minXPAseq == Inf] <- NA
                  event_subset$maxXPFseq[event_subset$maxXPFseq == -Inf] <- NA
                  event_subset$minXPDseq[event_subset$minXPDseq == Inf] <- NA
                  event_subset$maxXPAseq[event_subset$maxXPAseq == -Inf] <- NA
                  event_subset$minXPFseq[event_subset$minXPFseq == Inf] <- NA
                }
              }
            }
            event_subset$diff_XPDseq <- event_subset$XPDseq - event_subset$XPtsDiff
            event_subset$diff_XPDfinseq <- event_subset$XPDfinseq - event_subset$XPtsDiff
            event_subset$diff_XPDposs <- event_subset$XPDfinposs - event_subset$XPtsDiff
            event_subset$diff_XPAseq <- event_subset$XPAseq - event_subset$xPtsAgainst
            event_subset$diff_XPFseq <- event_subset$XPFseq - event_subset$xPtsFor

          }
        }else{
          event_subset$XPDseq <- NA
          event_subset$XPFseq <- NA
          event_subset$XPAseq <- NA
          event_subset$StatNameseq <- NA
          event_subset$Xseq <- NA
          event_subset$Yseq <- NA
          event_subset$nameseq <- NA
          event_subset$maxXPDseq <- NA
          event_subset$minXPAseq <- NA
          event_subset$maxXPFseq <- NA
          event_subset$minXPDseq <- NA
          event_subset$maxXPAseq <- NA
          event_subset$minXPFseq <- NA
          event_subset <- head(event_subset, 0)
          event_subset$diff_XPDseq <- event_subset$XPDseq - event_subset$XPtsDiff
          event_subset$diff_XPDfinseq <- event_subset$XPDfinseq - event_subset$XPtsDiff
          event_subset$diff_XPDposs <- event_subset$XPDfinposs - event_subset$XPtsDiff
          event_subset$diff_XPAseq <- event_subset$XPAseq - event_subset$xPtsAgainst
          event_subset$diff_XPFseq <- event_subset$XPFseq - event_subset$xPtsFor
          return(event_subset)
        }
      }
      #View(event_subset[, c("StatName","TimeVideo","name", "X.sens","Y.sens","XPtsDiff","StatNameseq","nameseq","Xseq","Yseq","XPDseq")])
      return(event_subset)
    }


    ###### BOX #####
    output$avgXPDseq <- renderUI({
      event_sub <- filter_event(input)

      # Gère le cas où l'input Type est vide
      if (is.null(input$Type)) {
        event_sub <- event_sub
      } else {
        event_sub <- event_sub[event_sub$Type %in% input$Type,]

      }

      if (input$role == 1) {
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        mean_XPD <- round(weighted.mean(c(event_sub$XPDseq,event_ponder$XPDseq,event_ponder_adv$XPDseq, event_ponder_double$XPDseq),c(rep(1, times = length(event_sub$XPDseq)), rep(input$ponderation, times = length(event_ponder$XPDseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDseq))), na.rm = TRUE), 3)
      } else if (input$role == 2) {
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        mean_XPD <- -round(weighted.mean(c(event_sub$XPDseq,event_ponder$XPDseq,event_ponder_adv$XPDseq, event_ponder_double$XPDseq),c(rep(1, times = length(event_sub$XPDseq)), rep(input$ponderation, times = length(event_ponder$XPDseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDseq))), na.rm = TRUE), 3)
      }

      # Si tous les événements sont NA
      if (is.na(mean_XPD)) {
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }

      div(
        style = paste0("color: white; background-color:", color_value(mean_XPD), ";"),
        as.character(mean_XPD)
      )
    })

    output$avg_diffXPDfinseq <- renderUI({
      event_sub <- filter_event(input)

      # Gère le cas où l'input Type est vide
      if (is.null(input$Type)) {
        event_sub <- event_sub
      } else {
        event_sub <- event_sub[event_sub$Type %in% input$Type,]

      }

      if (input$role == 1) {
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        mean_XPD <- round(weighted.mean(c(event_sub$diff_XPDfinseq,event_ponder$diff_XPDfinseq,event_ponder_adv$diff_XPDfinseq, event_ponder_double$diff_XPDfinseq),c(rep(1, times = length(event_sub$diff_XPDfinseq)), rep(input$ponderation, times = length(event_ponder$diff_XPDfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$diff_XPDfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$diff_XPDfinseq))), na.rm = TRUE), 3)
      } else if (input$role == 2) {
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        mean_XPD <- -round(weighted.mean(c(event_sub$diff_XPDfinseq,event_ponder$diff_XPDfinseq,event_ponder_adv$diff_XPDfinseq, event_ponder_double$diff_XPDfinseq),c(rep(1, times = length(event_sub$diff_XPDfinseq)), rep(input$ponderation, times = length(event_ponder$diff_XPDfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$diff_XPDfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$diff_XPDfinseq))), na.rm = TRUE), 3)
      }

      # Si tous les événements sont NA
      if (is.na(mean_XPD)) {
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }

      div(
        style = paste0("color: white; background-color:", color_value(mean_XPD), ";"),
        as.character(mean_XPD)
      )
    })

    output$avg_diffXPDseq <- renderUI({
      event_sub <- filter_event(input)

      # Gère le cas où l'input Type est vide
      if (is.null(input$Type)) {
        event_sub <- event_sub
      } else {
        event_sub <- event_sub[event_sub$Type %in% input$Type,]

      }

      if (input$role == 1) {
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        mean_XPD <- round(weighted.mean(c(event_sub$diff_XPDseq,event_ponder$diff_XPDseq,event_ponder_adv$diff_XPDseq, event_ponder_double$diff_XPDseq),c(rep(1, times = length(event_sub$diff_XPDseq)), rep(input$ponderation, times = length(event_ponder$diff_XPDseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$diff_XPDseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$diff_XPDseq))), na.rm = TRUE), 3)
      } else if (input$role == 2) {
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        mean_XPD <- -round(weighted.mean(c(event_sub$diff_XPDseq,event_ponder$diff_XPDseq,event_ponder_adv$diff_XPDseq, event_ponder_double$diff_XPDseq),c(rep(1, times = length(event_sub$diff_XPDseq)), rep(input$ponderation, times = length(event_ponder$diff_XPDseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$diff_XPDseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$diff_XPDseq))), na.rm = TRUE), 3)
      }

      # Si tous les événements sont NA
      if (is.na(mean_XPD)) {
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }

      div(
        style = paste0("color: white; background-color:", color_value(mean_XPD), ";"),
        as.character(mean_XPD)
      )
    })

    output$avg_diffXPDposs <- renderUI({
      event_sub <- filter_event(input)

      # Gère le cas où l'input Type est vide
      if (is.null(input$Type)) {
        event_sub <- event_sub
      } else {
        event_sub <- event_sub[event_sub$Type %in% input$Type,]

      }

      if (input$role == 1) {
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        mean_XPD <- round(weighted.mean(c(event_sub$diff_XPDposs,event_ponder$diff_XPDposs,event_ponder_adv$diff_XPDposs, event_ponder_double$diff_XPDposs),c(rep(1, times = length(event_sub$diff_XPDposs)), rep(input$ponderation, times = length(event_ponder$diff_XPDposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$diff_XPDposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$diff_XPDposs))), na.rm = TRUE), 3)
      } else if (input$role == 2) {
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        mean_XPD <- -round(weighted.mean(c(event_sub$diff_XPDposs,event_ponder$diff_XPDposs,event_ponder_adv$diff_XPDposs, event_ponder_double$diff_XPDposs),c(rep(1, times = length(event_sub$diff_XPDposs)), rep(input$ponderation, times = length(event_ponder$diff_XPDposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$diff_XPDposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$diff_XPDposs))), na.rm = TRUE), 3)
      }

      # Si tous les événements sont NA
      if (is.na(mean_XPD)) {
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }

      div(
        style = paste0("color: white; background-color:", color_value(mean_XPD), ";"),
        as.character(mean_XPD)
      )
    })

    output$maxXPD_seq <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      if (is.null(input$Type)) {
        event_sub <- event_sub
      }
      else {
        event_sub <- event_sub %>%  filter(Type %in% input$Type)
      }


      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$maxXPDseq,event_ponder$maxXPDseq,event_ponder_adv$maxXPDseq, event_ponder_double$maxXPDseq),c(rep(1, times = length(event_sub$maxXPDseq)), rep(input$ponderation, times = length(event_ponder$maxXPDseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPDseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPDseq))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- -round(weighted.mean(c(event_sub$minXPDseq,event_ponder$minXPDseq,event_ponder_adv$minXPDseq, event_ponder_double$minXPDseq),c(rep(1, times = length(event_sub$minXPDseq)), rep(input$ponderation, times = length(event_ponder$minXPDseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPDseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPDseq))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    output$minXPD_seq <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      if (is.null(input$Type)) {
        event_sub <- event_sub
      }
      else {
        event_sub <- event_sub %>%  filter(Type %in% input$Type)
      }


      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$minXPDseq,event_ponder$minXPDseq,event_ponder_adv$minXPDseq, event_ponder_double$minXPDseq),c(rep(1, times = length(event_sub$minXPDseq)), rep(input$ponderation, times = length(event_ponder$minXPDseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPDseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPDseq))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- -round(weighted.mean(c(event_sub$maxXPDseq,event_ponder$maxXPDseq,event_ponder_adv$maxXPDseq, event_ponder_double$maxXPDseq),c(rep(1, times = length(event_sub$maxXPDseq)), rep(input$ponderation, times = length(event_ponder$maxXPDseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPDseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPDseq))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    output$minXPF_seq <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$minXPFseq,event_ponder$minXPFseq,event_ponder_adv$minXPFseq, event_ponder_double$minXPFseq),c(rep(1, times = length(event_sub$minXPFseq)), rep(input$ponderation, times = length(event_ponder$minXPFseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPFseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPFseq))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$minXPAseq,event_ponder$minXPAseq,event_ponder_adv$minXPAseq, event_ponder_double$minXPAseq),c(rep(1, times = length(event_sub$minXPAseq)), rep(input$ponderation, times = length(event_ponder$minXPAseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPAseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPAseq))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    output$maxXPF_seq <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$maxXPFseq,event_ponder$maxXPFseq,event_ponder_adv$maxXPFseq, event_ponder_double$maxXPFseq),c(rep(1, times = length(event_sub$maxXPFseq)), rep(input$ponderation, times = length(event_ponder$maxXPFseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPFseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPFseq))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$maxXPAseq,event_ponder$maxXPAseq,event_ponder_adv$maxXPAseq, event_ponder_double$maxXPAseq),c(rep(1, times = length(event_sub$maxXPAseq)), rep(input$ponderation, times = length(event_ponder$maxXPAseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPAseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPAseq))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    output$minXPA_seq <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$minXPAseq,event_ponder$minXPAseq,event_ponder_adv$minXPAseq, event_ponder_double$minXPAseq),c(rep(1, times = length(event_sub$minXPAseq)), rep(input$ponderation, times = length(event_ponder$minXPAseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPAseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPAseq))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$minXPFseq,event_ponder$minXPFseq,event_ponder_adv$minXPFseq, event_ponder_double$minXPFseq),c(rep(1, times = length(event_sub$minXPFseq)), rep(input$ponderation, times = length(event_ponder$minXPFseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPFseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPFseq))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", value_color(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    output$maxXPA_seq <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$maxXPAseq,event_ponder$maxXPAseq,event_ponder_adv$maxXPAseq, event_ponder_double$maxXPAseq),c(rep(1, times = length(event_sub$maxXPAseq)), rep(input$ponderation, times = length(event_ponder$maxXPAseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPAseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPAseq))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$maxXPFseq,event_ponder$maxXPFseq,event_ponder_adv$maxXPFseq, event_ponder_double$maxXPFseq),c(rep(1, times = length(event_sub$maxXPFseq)), rep(input$ponderation, times = length(event_ponder$maxXPFseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPFseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPFseq))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", value_color(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    # BOX XPts
    output$avgXPD <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        mean_XPD <- round(weighted.mean(c(event_sub$XPtsDiff,event_ponder$XPtsDiff,event_ponder_adv$XPtsDiff, event_ponder_double$XPtsDiff),c(rep(1, times = length(event_sub$XPtsDiff)), rep(input$ponderation, times = length(event_ponder$XPtsDiff)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPtsDiff)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPtsDiff))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        mean_XPD <- -round(weighted.mean(c(event_sub$XPtsDiff,event_ponder$XPtsDiff,event_ponder_adv$XPtsDiff, event_ponder_double$XPtsDiff),c(rep(1, times = length(event_sub$XPtsDiff)), rep(input$ponderation, times = length(event_ponder$XPtsDiff)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPtsDiff)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPtsDiff))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(mean_XPD) & is.null(input$Origine) & is.null(input$OrigineRuck) & (!is.null(vals$equipeAdv) & !is.null(vals$equipe)) & !(is.null(vals$compet) || is.null(vals$annee))  )  {
        # code à exécuter si event_subset est vide, par exemple :
        showModal(modalDialog(
          title = "Données xPts indisponibles",
          "Veuillez séléctionner d'autres filtres",
          easyClose = TRUE,
          footer = NULL
        ))
      }
      div(
        style = paste0("color: white; background-color:", color_value(mean_XPD), ";"),
        as.character(mean_XPD)
      )

    })

    output$avgXPF <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        mean_XPF <- round(weighted.mean(c(event_sub$xPtsFor,event_ponder$xPtsFor,event_ponder_adv$xPtsFor, event_ponder_double$xPtsFor),c(rep(1, times = length(event_sub$xPtsFor)), rep(input$ponderation, times = length(event_ponder$xPtsFor)), rep(input$ponderation_adv, times = length(event_ponder_adv$xPtsFor)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$xPtsFor))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        mean_XPF <- round(weighted.mean(c(event_sub$xPtsAgainst,event_ponder$xPtsAgainst,event_ponder_adv$xPtsAgainst, event_ponder_double$xPtsAgainst),c(rep(1, times = length(event_sub$xPtsAgainst)), rep(input$ponderation, times = length(event_ponder$xPtsAgainst)), rep(input$ponderation_adv, times = length(event_ponder_adv$xPtsAgainst)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$xPtsAgainst))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(mean_XPF) & is.null(input$Origine) & is.null(input$OrigineRuck) & (!is.null(vals$equipeAdv) & !is.null(vals$equipe)) & !(is.null(vals$compet) || is.null(vals$annee)) )  {
        # code à exécuter si event_subset est vide, par exemple :
        showModal(modalDialog(
          title = "Données xPts indisponibles",
          "Veuillez séléctionner des filtres",
          easyClose = TRUE,
          footer = NULL
        ))
      }
      div(
        style = paste0("color: white; background-color:", color_value(mean_XPF), ";"),
        as.character(mean_XPF)
      )

    })

    output$avgXPA <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        mean_XPA <- round(weighted.mean(c(event_sub$xPtsAgainst,event_ponder$xPtsAgainst,event_ponder_adv$xPtsAgainst, event_ponder_double$xPtsAgainst),c(rep(1, times = length(event_sub$xPtsAgainst)), rep(input$ponderation, times = length(event_ponder$xPtsAgainst)), rep(input$ponderation_adv, times = length(event_ponder_adv$xPtsAgainst)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$xPtsAgainst))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        mean_XPA <- round(weighted.mean(c(event_sub$xPtsFor,event_ponder$xPtsFor,event_ponder_adv$xPtsFor, event_ponder_double$xPtsFor),c(rep(1, times = length(event_sub$xPtsFor)), rep(input$ponderation, times = length(event_ponder$xPtsFor)), rep(input$ponderation_adv, times = length(event_ponder_adv$xPtsFor)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$xPtsFor))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(mean_XPA) & is.null(input$Origine) & is.null(input$OrigineRuck) & (!is.null(vals$equipeAdv) & !is.null(vals$equipe)) & !(is.null(vals$compet) || is.null(vals$annee))  )  {
        # code à exécuter si event_subset est vide, par exemple :
        showModal(modalDialog(
          title = "Données xPts indisponibles",
          "Veuillez séléctionner d'autres filtres",
          easyClose = TRUE,
          footer = NULL
        ))
      }
      div(
        style = paste0("color: white; background-color:", value_color(mean_XPA), ";"),
        as.character(mean_XPA)
      )

    })

    # fin seq
    output$avgXPDfinseq <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        mean_XPD <- round(weighted.mean(c(event_sub$XPDfinseq,event_ponder$XPDfinseq,event_ponder_adv$XPDfinseq, event_ponder_double$XPDfinseq),c(rep(1, times = length(event_sub$XPDfinseq)), rep(input$ponderation, times = length(event_ponder$XPDfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDfinseq))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        mean_XPD <- -round(weighted.mean(c(event_sub$XPDfinseq,event_ponder$XPDfinseq,event_ponder_adv$XPDfinseq, event_ponder_double$XPDfinseq),c(rep(1, times = length(event_sub$XPDfinseq)), rep(input$ponderation, times = length(event_ponder$XPDfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDfinseq))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(mean_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(mean_XPD), ";"),
        as.character(mean_XPD)
      )

    })

    output$avgXPFfinseq <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        mean_XPD <- round(weighted.mean(c(event_sub$XPFfinseq,event_ponder$XPFfinseq,event_ponder_adv$XPFfinseq, event_ponder_double$XPFfinseq),c(rep(1, times = length(event_sub$XPFfinseq)), rep(input$ponderation, times = length(event_ponder$XPFfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPFfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPFfinseq))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        mean_XPD <- round(weighted.mean(c(event_sub$XPAfinseq,event_ponder$XPAfinseq,event_ponder_adv$XPAfinseq, event_ponder_double$XPAfinseq),c(rep(1, times = length(event_sub$XPAfinseq)), rep(input$ponderation, times = length(event_ponder$XPAfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPAfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPAfinseq))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(mean_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(mean_XPD), ";"),
        as.character(mean_XPD)
      )

    })

    output$avgXPAfinseq <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        mean_XPD <- round(weighted.mean(c(event_sub$XPAfinseq,event_ponder$XPAfinseq,event_ponder_adv$XPAfinseq, event_ponder_double$XPAfinseq),c(rep(1, times = length(event_sub$XPAfinseq)), rep(input$ponderation, times = length(event_ponder$XPAfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPAfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPAfinseq))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        mean_XPD <- round(weighted.mean(c(event_sub$XPFfinseq,event_ponder$XPFfinseq,event_ponder_adv$XPFfinseq, event_ponder_double$XPFfinseq),c(rep(1, times = length(event_sub$XPFfinseq)), rep(input$ponderation, times = length(event_ponder$XPFfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPFfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPFfinseq))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(mean_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", value_color(mean_XPD), ";"),
        as.character(mean_XPD)
      )

    })

    output$maxXPF_finseq <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$maxXPAfinseq,event_ponder$maxXPAfinseq,event_ponder_adv$maxXPAfinseq, event_ponder_double$maxXPAfinseq),c(rep(1, times = length(event_sub$maxXPAfinseq)), rep(input$ponderation, times = length(event_ponder$maxXPAfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPAfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPAfinseq))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$minXPAfinseq,event_ponder$minXPAfinseq,event_ponder_adv$minXPAfinseq, event_ponder_double$minXPAfinseq),c(rep(1, times = length(event_sub$minXPAfinseq)), rep(input$ponderation, times = length(event_ponder$minXPAfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPAfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPAfinseq))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    output$minXPF_finseq <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        min_XPD <- round(weighted.mean(c(event_sub$minXPAfinseq,event_ponder$minXPAfinseq,event_ponder_adv$minXPAfinseq, event_ponder_double$minXPAfinseq),c(rep(1, times = length(event_sub$minXPAfinseq)), rep(input$ponderation, times = length(event_ponder$minXPAfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPAfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPAfinseq))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        min_XPD <- round(weighted.mean(c(event_sub$minXPAfinseq,event_ponder$minXPAfinseq,event_ponder_adv$minXPAfinseq, event_ponder_double$minXPAfinseq),c(rep(1, times = length(event_sub$minXPAfinseq)), rep(input$ponderation, times = length(event_ponder$minXPAfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPAfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPAfinseq))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(min_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(min_XPD), ";"),
        as.character(min_XPD)
      )

    })

    output$minXPA_finseq <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$minXPAfinseq,event_ponder$minXPAfinseq,event_ponder_adv$minXPAfinseq, event_ponder_double$minXPAfinseq),c(rep(1, times = length(event_sub$minXPAfinseq)), rep(input$ponderation, times = length(event_ponder$minXPAfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPAfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPAfinseq))), na.rm = TRUE), 3)
      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$minXPFfinseq,event_ponder$minXPFfinseq,event_ponder_adv$minXPFfinseq, event_ponder_double$minXPFfinseq),c(rep(1, times = length(event_sub$minXPFfinseq)), rep(input$ponderation, times = length(event_ponder$minXPFfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPFfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPFfinseq))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", value_color(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    output$maxXPA_finseq <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$maxXPAfinseq,event_ponder$maxXPAfinseq,event_ponder_adv$maxXPAfinseq, event_ponder_double$maxXPAfinseq),c(rep(1, times = length(event_sub$maxXPAfinseq)), rep(input$ponderation, times = length(event_ponder$maxXPAfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPAfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPAfinseq))), na.rm = TRUE), 3)
      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$maxXPFfinseq,event_ponder$maxXPFfinseq,event_ponder_adv$maxXPFfinseq, event_ponder_double$maxXPFfinseq),c(rep(1, times = length(event_sub$maxXPFfinseq)), rep(input$ponderation, times = length(event_ponder$maxXPFfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPFfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPFfinseq))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", value_color(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    output$maxXPD_finseq <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      if (is.null(input$Type)) {
        event_sub <- event_sub
      }
      else {
        event_sub <- event_sub %>%  filter(Type %in% input$Type)
      }


      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$maxXPDfinseq,event_ponder$maxXPDfinseq,event_ponder_adv$maxXPDfinseq, event_ponder_double$maxXPDfinseq),c(rep(1, times = length(event_sub$maxXPDfinseq)), rep(input$ponderation, times = length(event_ponder$maxXPDfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPDfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPDfinseq))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- -round(weighted.mean(c(event_sub$minXPDfinseq,event_ponder$minXPDfinseq,event_ponder_adv$minXPDfinseq, event_ponder_double$minXPDfinseq),c(rep(1, times = length(event_sub$minXPDfinseq)), rep(input$ponderation, times = length(event_ponder$minXPDfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPDfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPDfinseq))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    output$minXPD_finseq <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      if (is.null(input$Type)) {
        event_sub <- event_sub
      }
      else {
        event_sub <- event_sub %>%  filter(Type %in% input$Type)
      }


      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$minXPDfinseq,event_ponder$minXPDfinseq,event_ponder_adv$minXPDfinseq, event_ponder_double$minXPDfinseq),c(rep(1, times = length(event_sub$minXPDfinseq)), rep(input$ponderation, times = length(event_ponder$minXPDfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPDfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPDfinseq))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- -round(weighted.mean(c(event_sub$maxXPDfinseq,event_ponder$maxXPDfinseq,event_ponder_adv$maxXPDfinseq, event_ponder_double$maxXPDfinseq),c(rep(1, times = length(event_sub$maxXPDfinseq)), rep(input$ponderation, times = length(event_ponder$maxXPDfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPDfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPDfinseq))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    # fin poss
    output$avgXPDfinposs <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        mean_XPD <- round(weighted.mean(c(event_sub$XPDfinposs,event_ponder$XPDfinposs,event_ponder_adv$XPDfinposs, event_ponder_double$XPDfinposs),c(rep(1, times = length(event_sub$XPDfinposs)), rep(input$ponderation, times = length(event_ponder$XPDfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDfinposs))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        mean_XPD <- -round(weighted.mean(c(event_sub$XPDfinposs,event_ponder$XPDfinposs,event_ponder_adv$XPDfinposs, event_ponder_double$XPDfinposs),c(rep(1, times = length(event_sub$XPDfinposs)), rep(input$ponderation, times = length(event_ponder$XPDfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDfinposs))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(mean_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(mean_XPD), ";"),
        as.character(mean_XPD)
      )

    })

    output$avgXPFfinposs <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        mean_XPD <- round(weighted.mean(c(event_sub$XPFfinposs,event_ponder$XPFfinposs,event_ponder_adv$XPFfinposs, event_ponder_double$XPFfinposs),c(rep(1, times = length(event_sub$XPFfinposs)), rep(input$ponderation, times = length(event_ponder$XPFfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPFfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPFfinposs))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        mean_XPD <- round(weighted.mean(c(event_sub$XPAfinposs,event_ponder$XPAfinposs,event_ponder_adv$XPAfinposs, event_ponder_double$XPAfinposs),c(rep(1, times = length(event_sub$XPAfinposs)), rep(input$ponderation, times = length(event_ponder$XPAfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPAfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPAfinposs))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(mean_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(mean_XPD), ";"),
        as.character(mean_XPD)
      )

    })

    output$avgXPAfinposs <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        mean_XPD <- round(weighted.mean(c(event_sub$XPAfinposs,event_ponder$XPAfinposs,event_ponder_adv$XPAfinposs, event_ponder_double$XPAfinposs),c(rep(1, times = length(event_sub$XPAfinposs)), rep(input$ponderation, times = length(event_ponder$XPAfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPAfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPAfinposs))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        mean_XPD <- round(weighted.mean(c(event_sub$XPFfinposs,event_ponder$XPFfinposs,event_ponder_adv$XPFfinposs, event_ponder_double$XPFfinposs),c(rep(1, times = length(event_sub$XPFfinposs)), rep(input$ponderation, times = length(event_ponder$XPFfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPFfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPFfinposs))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(mean_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", value_color(mean_XPD), ";"),
        as.character(mean_XPD)
      )

    })

    output$maxXPD_finposs <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      if (is.null(input$Type)) {
        event_sub <- event_sub
      }
      else {
        event_sub <- event_sub %>%  filter(Type %in% input$Type)
      }


      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$XPDmaxfinposs,event_ponder$XPDmaxfinposs,event_ponder_adv$XPDmaxfinposs, event_ponder_double$XPDmaxfinposs),c(rep(1, times = length(event_sub$XPDmaxfinposs)), rep(input$ponderation, times = length(event_ponder$XPDmaxfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDmaxfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDmaxfinposs))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- -round(weighted.mean(c(event_sub$XPDminfinposs,event_ponder$XPDminfinposs,event_ponder_adv$XPDminfinposs, event_ponder_double$XPDminfinposs),c(rep(1, times = length(event_sub$XPDminfinposs)), rep(input$ponderation, times = length(event_ponder$XPDminfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDminfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDminfinposs))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    output$minXPD_finposs <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      if (is.null(input$Type)) {
        event_sub <- event_sub
      }
      else {
        event_sub <- event_sub %>%  filter(Type %in% input$Type)
      }


      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$XPDminfinposs,event_ponder$XPDminfinposs,event_ponder_adv$XPDminfinposs, event_ponder_double$XPDminfinposs),c(rep(1, times = length(event_sub$XPDminfinposs)), rep(input$ponderation, times = length(event_ponder$XPDminfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDminfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDminfinposs))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- -round(weighted.mean(c(event_sub$XPDmaxfinposs,event_ponder$XPDmaxfinposs,event_ponder_adv$XPDmaxfinposs, event_ponder_double$XPDmaxfinposs),c(rep(1, times = length(event_sub$XPDmaxfinposs)), rep(input$ponderation, times = length(event_ponder$XPDmaxfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDmaxfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDmaxfinposs))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    output$maxXPF_finposs <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$XPFmaxfinposs,event_ponder$XPFmaxfinposs,event_ponder_adv$XPFmaxfinposs, event_ponder_double$XPFmaxfinposs),c(rep(1, times = length(event_sub$XPFmaxfinposs)), rep(input$ponderation, times = length(event_ponder$XPFmaxfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPFmaxfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPFmaxfinposs))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$XPAminfinposs,event_ponder$XPAminfinposs,event_ponder_adv$XPAminfinposs, event_ponder_double$XPAminfinposs),c(rep(1, times = length(event_sub$XPAminfinposs)), rep(input$ponderation, times = length(event_ponder$XPAminfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPAminfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPAminfinposs))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    output$minXPF_finposs <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$XPFminfinposs,event_ponder$XPFminfinposs,event_ponder_adv$XPFminfinposs, event_ponder_double$XPFminfinposs),c(rep(1, times = length(event_sub$XPFminfinposs)), rep(input$ponderation, times = length(event_ponder$XPFminfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPFminfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPFminfinposs))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$XPAminfinposs,event_ponder$XPAminfinposs,event_ponder_adv$XPAminfinposs, event_ponder_double$XPAminfinposs),c(rep(1, times = length(event_sub$XPAminfinposs)), rep(input$ponderation, times = length(event_ponder$XPAminfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPAminfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPAminfinposs))), na.rm = TRUE), 3)
      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    output$maxXPA_finposs <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$XPAmaxfinposs,event_ponder$XPAmaxfinposs,event_ponder_adv$XPAmaxfinposs, event_ponder_double$XPAmaxfinposs),c(rep(1, times = length(event_sub$XPAmaxfinposs)), rep(input$ponderation, times = length(event_ponder$XPAmaxfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPAmaxfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPAmaxfinposs))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$XPFmaxfinposs,event_ponder$XPFmaxfinposs,event_ponder_adv$XPFmaxfinposs, event_ponder_double$XPFmaxfinposs),c(rep(1, times = length(event_sub$XPFmaxfinposs)), rep(input$ponderation, times = length(event_ponder$XPFmaxfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPFmaxfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPFmaxfinposs))), na.rm = TRUE), 3)

      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", value_color(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    output$minXPA_finposs <- renderUI({
      event_sub <- filter_event(input)
      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub[event_sub$Type %in% input$Type,]

      )

      if(input$role == 1){
        event_ponder <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
        event_sub <- event_sub %>%  filter(X.sens/10 >= vals$X[1] & X.sens/10 <= vals$X[2] & Y.sens/10 >= vals$Y[1] & Y.sens/10 <= vals$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$XPAminfinposs,event_ponder$XPAminfinposs,event_ponder_adv$XPAminfinposs, event_ponder_double$XPAminfinposs),c(rep(1, times = length(event_sub$XPAminfinposs)), rep(input$ponderation, times = length(event_ponder$XPAminfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPAminfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPAminfinposs))), na.rm = TRUE), 3)

      }else if(input$role == 2){
        event_ponder <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
        event_ponder_adv <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
        event_ponder_double <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
        event_sub <- event_sub %>%  filter(100-X.sens/10 >= vals$X[1] & 100-X.sens/10 <= vals$X[2] & 70-Y.sens/10 >= vals$Y[1] & 70-Y.sens/10 <= vals$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
        max_XPD <- round(weighted.mean(c(event_sub$XPFminfinposs,event_ponder$XPFminfinposs,event_ponder_adv$XPFminfinposs, event_ponder_double$XPFminfinposs),c(rep(1, times = length(event_sub$XPFminfinposs)), rep(input$ponderation, times = length(event_ponder$XPFminfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPFminfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPFminfinposs))), na.rm = TRUE), 3)

      }
      # si tous les event sont NA
      if (is.na(max_XPD) )  {
        # code à exécuter si event_subset est vide, par exemple :
        return(div(style = "color: blue;font-size: 20px;", "Pas de data: Séléctionnez d'autres filtres ou Validez la séléction"))
      }
      div(
        style = paste0("color: white; background-color:", value_color(max_XPD), ";"),
        as.character(max_XPD)
      )

    })

    output$dynamic_tab_title <- renderUI({
      w <- ifelse(input$echeance == "Durée", "secondes", "temps de jeu")
      paste("xPts après", input$sliderInput, w)
    })

    output$box_avgXPDseq <- renderUI({
      w <- ifelse(input$echeance == "Durée", "secondes", "temps de jeu")
      box(
        title = paste("xPtsDiff après", input$sliderInput, w),
        width = NULL,
        uiOutput(ns("avgXPDseq"))
      )
    })

    output$box_maxXPF_seq <- renderUI({
      w <- ifelse(input$echeance == "Durée", "secondes", "temps de jeu")
      box(
        title = paste("Max xPtsFor dans les", input$sliderInput, w),
        width = NULL,
        uiOutput(ns("maxXPF_seq"))
      )
    })

    output$box_minXPF_seq <- renderUI({
      w <- ifelse(input$echeance == "Durée", "secondes", "temps de jeu")
      box(
        title = paste("Min xPtsFor dans les", input$sliderInput, w),
        width = NULL,
        uiOutput(ns("minXPF_seq"))
      )
    })

    output$box_maxXPD_seq <- renderUI({
      w <- ifelse(input$echeance == "Durée", "secondes", "temps de jeu")
      box(
        title = paste("Max xPtsDiff dans les", input$sliderInput, w),
        width = NULL,
        uiOutput(ns("maxXPD_seq"))
      )
    })

    output$box_minXPD_seq <- renderUI({
      w <- ifelse(input$echeance == "Durée", "secondes", "temps de jeu")
      box(
        title = paste("Min xPtsDiff dans les", input$sliderInput, w),
        width = NULL,
        uiOutput(ns("minXPD_seq"))
      )
    })

    output$box_maxXPA_seq <- renderUI({
      w <- ifelse(input$echeance == "Durée", "secondes", "temps de jeu")
      box(
        title = paste("Max xPtsAgainst dans les", input$sliderInput, w),
        width = NULL,
        uiOutput(ns("maxXPA_seq"))
      )
    })

    output$box_minXPA_seq <- renderUI({
      w <- ifelse(input$echeance == "Durée", "secondes", "temps de jeu")
      box(
        title = paste("Min xPtsAgainst dans les", input$sliderInput, w),
        width = NULL,
        uiOutput(ns("minXPA_seq"))
      )
    })


    ###### TERRAIN #####
    output$my_plot <- renderPlot({
      minX <- vals$X[1]
      maxX <- vals$X[2]
      minY <- vals$Y[1]
      maxY <- vals$Y[2]

      # on charge le dataframe réduit filtré par les input
      event_sub <- filter_event(input)

      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub %>% filter(Type %in% input$Type)
      )

      ifelse(is.null(input$Choix),
             event_sub <- event_sub,
             event_sub <- event_sub %>% filter(Choix %in% input$Choix)
      )

      ifelse(is.null(input$Origine),
             event_sub <- event_sub,
             event_sub <- event_sub %>% filter(OrigineJAP %in% input$Origine)
      )
      ifelse(is.null(input$OrigineRuck),
             event_sub <- event_sub,
             event_sub <- event_sub %>% filter(OrigineRuck %in% input$OrigineRuck)
      )

       #View(event_sub[, c("GameId","StatName","Type","TimeVideo","TeamId","name", "X.sens","Y.sens","XPtsDiff","StatNameseq","nameseq","Xseq","Yseq","XPDseq","OrigineJAP")])


      if (input$role == 1) {
        x <- event_sub$Y.sens/10
        y <- event_sub$X.sens/10
        x1 <- event_sub$Yseq/10
        y1 <- event_sub$Xseq/10-0.5
        # point après tps de jeu vert si l'équipe a gardé la possess, rouge sinon
        col1 <- ifelse(event_sub$name == event_sub$nameseq, "green", "red")
      } else if(input$role == 2){   # on inverse l'affichage des events si on regarde en défense (events subis)
        x <- 70-event_sub$Y.sens/10
        y <- 100 -event_sub$X.sens/10
        x1 <- 70 -event_sub$Yseq/10
        y1 <- 100-event_sub$Xseq/10-0.5
        col1 <- ifelse(event_sub$name != event_sub$nameseq, "green", "red")
      }
      ggplot() +
        geom_point(aes(x=x, y=y))


      field()


      #sile terrain est complet on afficher pas les pointillés rouges
      if( !(minX == -10 &  maxY == 70 & minY == 0 & maxX == 110) ){
        xx <- c(minX, minX, maxX, maxX, minX)
        yy <- c(minY, maxY, maxY, minY, minY)

        # Dessiner le rectangle
        lines(yy, xx, col = "red", lwd = 3, lty = 3)
      }

      # Events
      points(x, y, pch = 20, cex = 2.5, col = "#002A61")
      points(x, y, pch = 20, cex = 2, col = "white")

      # Fin Events
      points(x1, y1, pch = 20, cex = 2.5, col = "black")
      points(x1, y1, pch = 20, cex = 2, col = col1)
        text(x = 60, y = 107, labels = paste("En-but Equipe Adverse"), col = rgb(0, 0, 0, 0.3))
        text(x = 63, y = -7, labels = paste("En-but Equipe"), col = rgb(0, 0, 0, 0.3))

      arrows(x, y, x1, y1, length=0.1, angle=30, col="black", lwd=2)
      usethis::use_data(event_sub, overwrite = TRUE)

    })

    # fonctions pour le zone_plot
    generate_x_zones <- function(Zone, XMAX) {
      x_breaks <- c(vals$Y[1]*10)

      # Si xZone est 1, la seule zone est de -100 à XMAX
      if(Zone == 1) {
        x_breaks <- c(x_breaks, XMAX)
      } else {
        # Sinon, diviser l'intervalle 0 à XMAX en (xZone) sous-intervalles
        interval_length <- XMAX / (Zone)
        x_breaks <- c(x_breaks, round(seq(from = interval_length, to = XMAX, by = interval_length)))
      }

      # Si la dernière limite est inférieure à XMAX
      if(tail(x_breaks, n = 1) < XMAX) {
        x_breaks[length(x_breaks)] <- XMAX
      }

      # Générer les labels pour les zones
      x_labels <- paste(head(x_breaks, -1), tail(x_breaks, -1), sep = "-")

      # Renommer le premier label en "0-"
      x_labels[1] <- paste("0", round(tail(x_breaks, -1)[1]), sep = "-")

      return(list("breaks" = x_breaks, "labels" = x_labels))
    }

    zones_df <- function(input_df, origine, XMAX) {
      xZone <- input$XZones
      yZone <- input$YZones
      minX <- vals$X[1]*10
      maxX <- vals$X[2]*10
      minY <- vals$Y[1]*10
      maxY <- vals$Y[2]*10

      ifelse(input$wRuck, df <- input_df[which(input_df$OrigineRuck %in% origine), ], df <- input_df[which(input_df$OrigineJAP %in% origine), ])

      if (nrow(df) != 0) {

        if (input$detail) {

          x_intervals <- unlist(lapply(1:(xZone - 1), function(i) {
            input[[paste0("x_interval", i)]]*10
          }))

          # Récupére les valeurs des intervalles Y depuis les uiOutput
          y_intervals <- unlist(lapply(1:(yZone - 1), function(i) {
            input[[paste0("y_interval", i)]]*10
          }))

          ifelse(xZone ==1,  x_breaks <- c(minX, maxX),x_breaks <- c(minX, x_intervals, maxX))
          ifelse(yZone == 1, y_breaks <- c(minY, maxY), y_breaks <- c(minY, y_intervals, maxY))

          # labels : "0-10", "10-22" etc..
          x_labels <- paste(head(x_breaks, -1), tail(x_breaks, -1), sep = "-")
          y_labels <- paste(head(y_breaks, -1), tail(y_breaks, -1), sep = "-")

          # on stocke les labels/zones dans des variables
          df$X_ZoneF <- cut(df$X.sens, breaks = x_breaks, labels = x_labels, include.lowest = TRUE)
          df$Y_ZoneF <- cut(df$Y.sens, breaks = y_breaks, labels = y_labels, include.lowest = TRUE)
        } else {
          x_breaks <- seq(from = minX, to = maxX, length.out = xZone + 1)
          x_labels <- paste(head(x_breaks, -1), tail(x_breaks, -1), sep = "-")
          y_breaks <- seq(from = minY, to = maxY, length.out = yZone + 1)
          y_labels <- paste(head(y_breaks, -1), tail(y_breaks, -1), sep = "-")
          df$X_ZoneF <- cut(df$X.sens, breaks = x_breaks, labels = x_labels, include.lowest = TRUE)
          df$Y_ZoneF <- cut(df$Y.sens, breaks = y_breaks, labels = y_labels, include.lowest = TRUE)
        }
        validate(
          need(any(!is.na(df$diff_XPDseq) & !is.na(df$X_ZoneF) & !is.na(df$Y_ZoneF)),
               paste("Sélection Non Valid"))
        )


        validate(
          need(any(input$sliderInput != 0),
               paste("Pas de xPtsdiff après échéance car 0 est séléctionné dans","'", input$echeance, "'"))
        )

        avg_XPD <- aggregate(df$diff_XPDseq, by=list(X_ZoneF=df$X_ZoneF, Y_ZoneF=df$Y_ZoneF), FUN=mean, na.rm = TRUE)

        count_XPD <- aggregate(df$diff_XPDseq, by=list(X_ZoneF=df$X_ZoneF, Y_ZoneF=df$Y_ZoneF), FUN=length)

        full_grid <- expand.grid(X_ZoneF=levels(df$X_ZoneF), Y_ZoneF=levels(df$Y_ZoneF))

        # Fusionner le tableau complet avec les données moyennes
        avg_XPD_full <- merge(full_grid, avg_XPD, by=c("X_ZoneF", "Y_ZoneF"), all.x=TRUE)

        # Fusionner le tableau complet avec les comptes
        count_XPD_full <- merge(full_grid, count_XPD, by=c("X_ZoneF", "Y_ZoneF"), all.x=TRUE)

        # Remplacer les NAs par 0 dans le tableau de comptes
        count_XPD_full$x[is.na(count_XPD_full$x)] <- 0


        data <- avg_XPD_full
        matrix_result <- matrix(nrow = yZone, ncol = xZone)


        # Définir les niveaux pour X_ZoneF et Y_ZoneF
        levels_X_ZoneF <- x_labels
        levels_Y_ZoneF <- y_labels

        # Convertir X_ZoneF et Y_ZoneF en facteurs avec les niveaux spécifiés
        data$X_ZoneF <- factor(data$X_ZoneF, levels = levels_X_ZoneF)
        data$Y_ZoneF <- factor(data$Y_ZoneF, levels = levels_Y_ZoneF)

        # Créer une fonction pour déterminer l'indice dans la matrice pour une paire donnée de X_ZoneF et Y_ZoneF
        get_matrix_index <- function(xx, yy) {
          x_index <- which(levels_X_ZoneF == xx)
          y_index <- which(levels_Y_ZoneF == yy)
          return(c(x_index, y_index))
        }
        # Remplir la matrice avec les valeurs Avg_XPD
        for(i in 1:nrow(data)) {
          indices <- get_matrix_index(data$X_ZoneF[i], data$Y_ZoneF[i])
          matrix_result[indices[2], indices[1]] <- data$x[i]
        }

        #lissage gaussien
        matrix_smooth <- function(mat, window_size, smoothing_factor) {
          nrows <- nrow(mat)
          ncols <- ncol(mat)

          smoothed <- matrix(0, nrow = nrows, ncol = ncols)

          weights <- matrix(0, nrow = nrows, ncol = ncols)

          for (i in 1:nrows) {
            for (j in 1:ncols) {
              min_row <- max(i - window_size, 1)
              max_row <- min(i + window_size, nrows)
              min_col <- max(j - window_size, 1)
              max_col <- min(j + window_size, ncols)

              window <- mat[min_row:max_row, min_col:max_col]
              pond<- matrix(0, nrow = nrows, ncol = ncols)
              for(k in min_row:max_row){
                for(l in min_col:max_col){
                  pond[k,l]<-ifelse(!is.na(mat[k,l]),1 / (1 + max(abs(k-i),abs(l-j)) * smoothing_factor),0)
                }
              }
              smoothed[i, j] <- ifelse(is.na(mat[i,j]),NA,sum(window * pond[min_row:max_row, min_col:max_col],na.rm = T) / sum(pond[min_row:max_row, min_col:max_col]))
            }
          }

          return(smoothed)
        }

        # terrain lissé
        smoothed <- matrix_smooth(matrix_result, window_size = input$window_size, smoothing_factor = input$coeff_liss)

        if(input$lissage){
          matrix_result <- smoothed
        }else if(input$lissage == FALSE){
          matrix_result <- matrix_result
        }

        matrix_result <- t(matrix_result)

        # Si matrix_result a une seule colonne, la convertir en une matrice à une colonne
        if (ncol(matrix_result) == 1) {
          matrix_result <- matrix(matrix_result, ncol = 1)
        }
        if (nrow(matrix_result) == 1) {
          matrix_result <- matrix(matrix_result, nrow = 1)
        }

        matrix_result <- matrix_result[, seq_len(ncol(matrix_result)), drop = FALSE]

        #View(matrix_result)
        #View(df[,c("StatName",'TimeVideo',"X.sens","X_ZoneF","Y.sens","Y_ZoneF","diff_XPD","XPDseq","XPtsDiff","OrigineJAP","OrigineRuck")])

        xmin <- -10
        xmax <- 110
        ymin <- 0
        ymax <- 70

        plot(1,1, type = "n", xlab = "", ylab = "", xlim = c(ymin, ymax), ylim = c(xmin-10, xmax+10))

        field_zone1()
        y_seq <- sapply(y_breaks, function(x) x / 10)

        x_seq <- sapply(x_breaks, function(x) x / 10)

        # plot de chaque zones en fonctions de la matrice
        for (i in 1:nrow(matrix_result)) {
          for (j in 1:ncol(matrix_result)) {
            xleft <- y_seq[j]
            ybottom <- x_seq[i]
            xright <- y_seq[j + 1]
            ytop <- x_seq[i + 1]
            value <- matrix_result[i, j]
            if(input$role==2) {
              xleft <- ymax - xleft
              ybottom <- xmax - ybottom
              xright <- ymax - xright
              ytop <- xmax - ytop
              value <- -value
            }

            if (is.na(value)) {
              col <- rgb(1, 1, 1, alpha = 0) # Fully transparent
              label <- ""  # Display "NA" instead of the average value
            } else {
              col <- map_colors(value)
              label <- round(value, 2)
            }
            current_par <- par()
            par(font = 2)

            rect(xleft, ybottom, xright, ytop, col = col, border = "white", lty = 2)
            if(xZone ==1 ){
              if(!is.na(value)){rect(mean(c(xleft, xright))-2.5, mean(c(ybottom, ytop))-0.5, mean(c(xleft, xright))+2.5, mean(c(ybottom, ytop))+4.5, col = 'white', border = "black")}
              text(x = mean(c(xleft, xright)), y = mean(c(ybottom, ytop)+2), labels = label)}
            else{
              if(!is.na(value)){rect(mean(c(xleft, xright))-2.5, mean(c(ybottom, ytop))-2.5, mean(c(xleft, xright))+2.5, mean(c(ybottom, ytop))+2.5, col = 'white', border = "black")}
              text(x = mean(c(xleft, xright)), y = mean(c(ybottom, ytop)), labels = label)}
          }
        }

        field_zone2()
        text(x = ymax-11, y = xmax+7, labels = paste("En-but Equipe Adverse"), col = rgb(0, 0, 0, 0.3))
        text(x = ymax-7, y = xmin - 7, labels = paste("En-but Equipe"), col = rgb(0, 0, 0, 0.3))

      }else{
        field()
      }
    }


    # terrain diffXPD par zone
    output$zone_plot <- renderPlot({
      minX <- vals$X[1]
      maxX <- vals$X[2]
      minY <- vals$Y[1]
      maxY <- vals$Y[2]
      # on charge le dataframe réduit filtré par les input
      event_sub <- filter_event(input)

      if (input$wRuck) {
        event_sub <- event_sub %>% filter(OrigineRuck %in% input$OrigineRuck)
        inog <- input$OrigineRuck
      } else {
        event_sub <- event_sub %>% filter(OrigineJAP %in% input$Origine)
        inog <- input$Origine
      }

      # Gère le cas où l'input Type est vide
      ifelse(is.null(input$Type),
             event_sub <- event_sub,
             event_sub <- event_sub %>% filter(Type %in% input$Type)
      )

      #terrain
      par(mar = c(0, 0, 0, 0))  # Supprime les marges
      zones_df(event_sub, inog, maxX*10)
      usethis::use_data(event_sub, overwrite = TRUE)

    })


    ###### TABLE #####
    output$table <- DT::renderDT({
      event_sub <- filter_event(input)

      # Gère le cas où l'input Type est vide
      if (is.null(input$Type)) {
        event_sub <- event_sub
      } else {
        event_sub <- event_sub[event_sub$Type %in% input$Type,]
      }


      column_names <- c("name","name2","Joueur","GameId", "GameSeconds", "X.sens", "Y.sens", "X.end", "Y.end", "Type", "Result", "TimeVideo",
                        "xPtsFor", "xPtsAgainst", "XPtsDiff",
                        "StatNameseq", "Xseq", "Yseq", "nameseq",
                        "XPFseq", "XPAseq","XPDseq",
                        "maxXPFseq", "maxXPAseq","maxXPDseq",
                        "minXPFseq","minXPAseq","minXPDseq",
                        "diff_XPFseq", "diff_XPAseq","diff_XPDseq",
                        "StatNamefinseq", "namefinseq",
                        "XPFfinseq", "XPAfinseq","XPDfinseq",
                        "maxXPFfinseq","maxXPAfinseq","maxXPDfinseq",
                        "minXPFfinseq","minXPAfinseq","minXPDfinseq",
                        "diff_XPDfinseq",
                        "StatNamefinposs", "namefinposs",
                        "XPFfinposs", "XPAfinposs", "XPDfinposs",
                        "XPFmaxfinposs","XPAmaxfinposs", "XPDmaxfinposs",
                        "XPFminfinposs","XPAminfinposs", "XPDminfinposs",
                        "diff_XPDposs",
                        "ScorePour", "ScoreContre",
                        "ScoreHome", "ScoreAway", "Circuit",
                        "Cellule", "Choix", "OrigineJAP", "OrigineRuck",
                        "PerfxPD", "PerfxPF", "PerfxPA")

      event_sub <- event_sub[, column_names]
      numeric_columns <- sapply(event_sub, is.numeric)
      event_sub[, numeric_columns] <- round(event_sub[, numeric_columns], 3)

      # Génération de la colonne de valeurs pour la couleur
      event_sub$colorvalues <- as.integer(event_sub$name == event_sub$nameseq)

      dt <- datatable(
        data = event_sub,
        rownames = FALSE,
        extensions = c("Scroller", "FixedColumns", "Buttons"),

        options = list(
          dom = "Bfrtip",
          scrollY = 400, scrollX = 400, scroller = TRUE,
          stateSave = TRUE,
          # fixer les colonnes :
          fixedColumns = list(leftColumns = 2),


          # selection :
          buttons = list(
            list(extend ='csv', exportOptions = list(columns = ':visible')),
            list(extend ='excel', exportOptions = list(columns = ':visible')),
            list(extend = 'colvis', columns = ':not(.noVis)'),
            list(extend = 'colvisGroup', text = 'Show all', show = ':hidden'),
            list(extend = 'colvisGroup', text = 'Hide all', hide = ':visible'),
            'selectAll', 'selectNone', 'selectRows'
          ),
          filter = "top"
          ,
          columnDefs = list(list(visible = FALSE, targets =  c("X.sens", "Y.sens", "X.end", "Y.end", "TimeVideo","colorvalues",
                                                               "maxXPFseq", "maxXPAseq","maxXPDseq",
                                                               "minXPFseq","minXPAseq","minXPDseq","maxXPFfinseq","maxXPAfinseq","maxXPDfinseq",
                                                               "minXPFfinseq","minXPAfinseq","minXPDfinseq","XPFmaxfinposs","XPAmaxfinposs", "XPDmaxfinposs",
                                                               "XPFminfinposs","XPAminfinposs", "XPDminfinposs"
                                                               )))
        )
      )

      # Mise à jour de la datatable avec la nouvelle colonne colorvalues
      dt <- formatStyle(dt, "StatNameseq", valueColumns = "colorvalues",
                        color = "white",
                        backgroundColor = styleInterval(cuts = 0.5, values = c("red", "green")))

      return(dt)
      usethis::use_data(event_sub, overwrite = TRUE)
    }, server = TRUE)

    output$tableEquipe <- DT::renderDT({
      event_sub <- filter_event(input)

      # Gère le cas où l'input Type est vide
      if (is.null(input$Type)) {
        event_sub <- event_sub
      } else {
        event_sub <- event_sub[event_sub$Type %in% input$Type,]
      }

      column_names <- c("name", "xPtsFor", "xPtsAgainst", "XPtsDiff",
                        "XPFseq", "XPAseq","XPDseq",
                        "maxXPFseq", "maxXPAseq","maxXPDseq",
                        "minXPFseq","minXPAseq","minXPDseq",
                        "diff_XPFseq", "diff_XPAseq","diff_XPDseq",
                        "XPFfinseq", "XPAfinseq","XPDfinseq",
                        "maxXPFfinseq","maxXPAfinseq","maxXPDfinseq",
                        "minXPFfinseq","minXPAfinseq","minXPDfinseq",
                        "diff_XPDfinseq",
                        "XPFfinposs", "XPAfinposs", "XPDfinposs",
                        "XPFmaxfinposs","XPAmaxfinposs", "XPDmaxfinposs",
                        "XPFminfinposs","XPAminfinposs", "XPDminfinposs",
                        "diff_XPDposs",
                        "ScorePour", "ScoreContre",
                        "ScoreHome", "ScoreAway",
                        "PerfxPD", "PerfxPF", "PerfxPA","Xseq", "Yseq")

      event_sub <- event_sub[,column_names]

      actions_per_team <- event_sub %>%
        group_by(name) %>%
        summarise(NbActions = n(), .groups="keep")

      event_E <- event_sub %>%
        group_by(name) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE)

      event_E <- left_join(event_E, actions_per_team, by = "name")
      event_E <- event_E %>% relocate(NbActions, .after = "name")


      numeric_columns <- sapply(event_E, is.numeric)
      event_E[, numeric_columns] <- round(event_E[, numeric_columns], 3)

      if(vals$valid){
        colon <- c( "maxXPFseq", "maxXPAseq","maxXPDseq",
                    "minXPFseq","minXPAseq","minXPDseq","maxXPFfinseq","maxXPAfinseq","maxXPDfinseq",
                    "minXPFfinseq","minXPAfinseq","minXPDfinseq","XPFmaxfinposs","XPAmaxfinposs", "XPDmaxfinposs",
                    "XPFminfinposs","XPAminfinposs", "XPDminfinposs")
      }else{colon <- c()}

      dt <- datatable(
        data = event_E,
        rownames = FALSE,
        extensions = c("Scroller", "FixedColumns", "Buttons"),

        options = list(
          dom = "Bfrtip",
          scrollY = 400, scrollX = 400, scroller = TRUE,
          stateSave = TRUE,
          # fixer les colonnes :
          fixedColumns = list(leftColumns = 2),


          # selection :
          buttons = list(
            list(extend ='csv', exportOptions = list(columns = ':visible')),
            list(extend ='excel', exportOptions = list(columns = ':visible')),
            list(extend = 'colvis', columns = ':not(.noVis)'),
            list(extend = 'colvisGroup', text = 'Show all', show = ':hidden'),
            list(extend = 'colvisGroup', text = 'Hide all', hide = ':visible'),
            'selectAll', 'selectNone', 'selectRows'
          ),
          filter = "top",
          columnDefs = list(list(visible = FALSE, targets = colon))
        )
      )

      return(dt)
    }, server = TRUE)

    output$tableType <- DT::renderDT({
      event_sub <- filter_event(input)

      # Gère le cas où l'input Type est vide
      if (is.null(input$Type)) {
        event_sub <- event_sub
      } else {
        event_sub <- event_sub[event_sub$Type %in% input$Type,]
      }

      column_names <- c("Type", "xPtsFor", "xPtsAgainst", "XPtsDiff",
                        "XPFseq", "XPAseq","XPDseq",
                        "maxXPFseq", "maxXPAseq","maxXPDseq",
                        "minXPFseq","minXPAseq","minXPDseq",
                        "diff_XPFseq", "diff_XPAseq","diff_XPDseq",
                        "XPFfinseq", "XPAfinseq","XPDfinseq",
                        "maxXPFfinseq","maxXPAfinseq","maxXPDfinseq",
                        "minXPFfinseq","minXPAfinseq","minXPDfinseq",
                        "diff_XPDfinseq",
                        "XPFfinposs", "XPAfinposs", "XPDfinposs",
                        "XPFmaxfinposs","XPAmaxfinposs", "XPDmaxfinposs",
                        "XPFminfinposs","XPAminfinposs", "XPDminfinposs",
                        "diff_XPDposs",
                        "ScorePour", "ScoreContre",
                        "ScoreHome", "ScoreAway",
                        "PerfxPD", "PerfxPF", "PerfxPA","Xseq", "Yseq")

      event_sub <- event_sub[,column_names]

      actions_per_type <- event_sub %>%
        group_by(Type) %>%
        summarise(NbActions = n(), .groups="keep")

      event_E <- event_sub %>%
        group_by(Type) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE)

      event_E <- left_join(event_E, actions_per_type, by = "Type")
      event_E <- event_E %>% relocate(NbActions, .after = "Type")

      numeric_columns <- sapply(event_E, is.numeric)
      event_E[, numeric_columns] <- round(event_E[, numeric_columns], 3)

      if(vals$valid){
        colon <- c( "maxXPFseq", "maxXPAseq","maxXPDseq",
                    "minXPFseq","minXPAseq","minXPDseq","maxXPFfinseq","maxXPAfinseq","maxXPDfinseq",
                    "minXPFfinseq","minXPAfinseq","minXPDfinseq","XPFmaxfinposs","XPAmaxfinposs", "XPDmaxfinposs",
                    "XPFminfinposs","XPAminfinposs", "XPDminfinposs")
      }else{colon <- c()}

      dt <- datatable(
        data = event_E,
        rownames = FALSE,
        extensions = c("Scroller", "FixedColumns", "Buttons"),

        options = list(
          dom = "Bfrtip",
          scrollY = 400, scrollX = 400, scroller = TRUE,
          stateSave = TRUE,
          # fixer les colonnes :
          fixedColumns = list(leftColumns = 2),


          # selection :
          buttons = list(
            list(extend ='csv', exportOptions = list(columns = ':visible')),
            list(extend ='excel', exportOptions = list(columns = ':visible')),
            list(extend = 'colvis', columns = ':not(.noVis)'),
            list(extend = 'colvisGroup', text = 'Show all', show = ':hidden'),
            list(extend = 'colvisGroup', text = 'Hide all', hide = ':visible'),
            'selectAll', 'selectNone', 'selectRows'
          ),
          filter = "top",
          columnDefs = list(list(visible = FALSE, targets =  colon))
        )
      )

      return(dt)
    }, server = TRUE)

    output$tableChoix <- DT::renderDT({
      event_sub <- filter_event(input)

      # Gère le cas où l'input Type est vide
      if (is.null(input$Type)) {
        event_sub <- event_sub
      } else {
        event_sub <- event_sub[event_sub$Type %in% input$Type,]
      }


      column_names <- c("Choix", "xPtsFor", "xPtsAgainst", "XPtsDiff",
                        "XPFseq", "XPAseq","XPDseq",
                        "maxXPFseq", "maxXPAseq","maxXPDseq",
                        "minXPFseq","minXPAseq","minXPDseq",
                        "diff_XPFseq", "diff_XPAseq","diff_XPDseq",
                        "XPFfinseq", "XPAfinseq","XPDfinseq",
                        "maxXPFfinseq","maxXPAfinseq","maxXPDfinseq",
                        "minXPFfinseq","minXPAfinseq","minXPDfinseq",
                        "diff_XPDfinseq",
                        "XPFfinposs", "XPAfinposs", "XPDfinposs",
                        "XPFmaxfinposs","XPAmaxfinposs", "XPDmaxfinposs",
                        "XPFminfinposs","XPAminfinposs", "XPDminfinposs",
                        "diff_XPDposs",
                        "ScorePour", "ScoreContre",
                        "ScoreHome", "ScoreAway",
                        "PerfxPD", "PerfxPF", "PerfxPA","Xseq", "Yseq")

      event_sub <- event_sub[,column_names]

      actions_per_type <- event_sub %>%
        group_by(Choix) %>%
        summarise(NbActions = n(), .groups="keep")

      event_E <- event_sub %>%
        group_by(Choix) %>%
        summarise_if(is.numeric, mean, na.rm = TRUE)

      event_E <- left_join(event_E, actions_per_type, by = "Choix")
      event_E <- event_E %>% relocate(NbActions, .after = "Choix")

      numeric_columns <- sapply(event_E, is.numeric)
      event_E[, numeric_columns] <- round(event_E[, numeric_columns], 3)

      if(vals$valid){
        colon <- c( "maxXPFseq", "maxXPAseq","maxXPDseq",
                    "minXPFseq","minXPAseq","minXPDseq","maxXPFfinseq","maxXPAfinseq","maxXPDfinseq",
                    "minXPFfinseq","minXPAfinseq","minXPDfinseq","XPFmaxfinposs","XPAmaxfinposs", "XPDmaxfinposs",
                    "XPFminfinposs","XPAminfinposs", "XPDminfinposs")
      }else{colon <- c()}

      dt <- datatable(
        data = event_E,
        rownames = FALSE,
        extensions = c("Scroller", "FixedColumns", "Buttons"),

        options = list(
          dom = "Bfrtip",
          scrollY = 400, scrollX = 400, scroller = TRUE,
          stateSave = TRUE,
          # fixer les colonnes :
          fixedColumns = list(leftColumns = 2),


          # selection :
          buttons = list(
            list(extend ='csv', exportOptions = list(columns = ':visible')),
            list(extend ='excel', exportOptions = list(columns = ':visible')),
            list(extend = 'colvis', columns = ':not(.noVis)'),
            list(extend = 'colvisGroup', text = 'Show all', show = ':hidden'),
            list(extend = 'colvisGroup', text = 'Hide all', hide = ':visible'),
            'selectAll', 'selectNone', 'selectRows'
          ),
          filter = "top",
          columnDefs = list(list(visible = FALSE, targets = colon))
        )
      )

      return(dt)
    }, server = TRUE)



    output$issuesEch <- renderPlotly({
      event_sub <- filter_event(input)

      if (is.null(input$Type)) {
        event_sub <- event_sub
      } else {
        event_sub <- event_sub[event_sub$Type %in% input$Type,]
      }

      event_sub <- event_sub %>%
        drop_na(nameseq) %>%
        mutate(wBallseq = ifelse(nameseq == name, "gagne", "concede"))

      total_events <- nrow(event_sub)

      stats_table <- event_sub %>%

        group_by(StatNameseq, wBallseq) %>%
        summarise(n = n()) %>%
        mutate(Pourcentage = round(n / total_events * 100, 1)) %>%
        arrange(desc(n))

      stats_table$col <- ifelse(stats_table$wBallseq == "gagne", "green",
                                ifelse(stats_table$wBallseq == "concede", "red", NA))


      stats_table$BarText <- paste("<b>",stats_table$wBallseq, "<br>", stats_table$Pourcentage, "%</b>")
      suppressWarnings({
      p <- plotly::plot_ly(stats_table, x = ~n, y = ~StatNameseq, type = 'bar',
                           marker = list(color = ~I(factor(col)), line = list(color = 'white', width = 6)),
                           text = ~BarText,
                           insidetextanchor = 'middle',
                           insidetextfont = list(color = '#FFFFFF'),
                           hoverinfo = 'x')

      p <- plotly::config(p, displayModeBar = FALSE)
      t <- ifelse(input$echeance == "Durée", "secondes", "tps de jeu")
      p <- p %>% layout(showlegend = FALSE,  # Supprimez la légende
                        yaxis = list(title = "Issues", autorange = "reversed"),
                        xaxis = list(title = "Nombre de données"),
                        title = paste("Répartition des événements dans les", input$sliderInput, t, "suivant(e)s"),
                        height = 400)
      })

      return(p)
    })
    output$issuesPoss <- renderPlotly({
      event_sub <- filter_event(input)

      if (is.null(input$Type)) {
        event_sub <- event_sub
      } else {
        event_sub <- event_sub[event_sub$Type %in% input$Type,]
      }

      event_sub <- event_sub %>%
        drop_na(namefinposs) %>%
        mutate(wBallseq = ifelse(namefinposs == name, "gagne", "concede"))

      total_events <- nrow(event_sub)

      stats_table <- event_sub %>%

        group_by(StatNamefinposs, wBallseq) %>%
        summarise(n = n()) %>%
        mutate(Pourcentage = round(n / total_events * 100, 1)) %>%
        arrange(desc(n))

      stats_table$col <- ifelse(stats_table$wBallseq == "gagne", "green",
                                ifelse(stats_table$wBallseq == "concede", "red", NA))


      stats_table$BarText <- paste("<b>",stats_table$wBallseq, "<br>", stats_table$Pourcentage, "%</b>")
      suppressWarnings({
        p <- plotly::plot_ly(stats_table, x = ~n, y = ~StatNamefinposs, type = 'bar',
                             marker = list(color = ~I(factor(col)), line = list(color = 'white', width = 6)),
                             text = ~BarText,
                             insidetextanchor = 'middle',
                             insidetextfont = list(color = '#FFFFFF'),
                             hoverinfo = 'x')

        p <- plotly::config(p, displayModeBar = FALSE)
        t <- ifelse(input$echeance == "Durée", "secondes", "tps de jeu")
        p <- p %>% layout(showlegend = FALSE,  # Supprimez la légende
                          yaxis = list(title = "Issues", autorange = "reversed"),
                          xaxis = list(title = "Nombre de données"),
                          title = paste("Répartition des événements à la fin de la possession"),
                          height = 400)
      })

      return(p)
    })

    output$issuesSeq <- renderPlotly({
      event_sub <- filter_event(input)

      if (is.null(input$Type)) {
        event_sub <- event_sub
      } else {
        event_sub <- event_sub[event_sub$Type %in% input$Type,]
      }

      event_sub <- event_sub %>%
        drop_na(namefinseq) %>%
        mutate(wBallseq = ifelse(namefinseq == name, "gagne", "concede"))

      total_events <- nrow(event_sub)

      stats_table <- event_sub %>%

        group_by(StatNamefinseq, wBallseq) %>%
        summarise(n = n()) %>%
        mutate(Pourcentage = round(n / total_events * 100, 1)) %>%
        arrange(desc(n))

      stats_table$col <- ifelse(stats_table$wBallseq == "gagne", "green",
                                ifelse(stats_table$wBallseq == "concede", "red", NA))


      stats_table$BarText <- paste("<b>",stats_table$wBallseq, "<br>", stats_table$Pourcentage, "%</b>")
      suppressWarnings({
        p <- plotly::plot_ly(stats_table, x = ~n, y = ~StatNamefinseq, type = 'bar',
                             marker = list(color = ~I(factor(col)), line = list(color = 'white', width = 6)),
                             text = ~BarText,
                             insidetextanchor = 'middle',
                             insidetextfont = list(color = '#FFFFFF'),
                             hoverinfo = 'x')

        p <- plotly::config(p, displayModeBar = FALSE)
        p <- p %>% layout(showlegend = FALSE,  # Supprimez la légende
                          yaxis = list(title = "Issues", autorange = "reversed"),
                          xaxis = list(title = "Nombre de données"),
                          title = paste("Répartition des événements à la fin de la séquence"),
                          height = 400)
      })

      return(p)
    })

    output$click_coordinates <- renderText({
      if (!is.null(input$plot_click) && is.numeric(input$plot_click$x) && is.numeric(input$plot_click$y)) {
        click_x <- input$plot_click$x
        click_y <- input$plot_click$y
        paste("x =", round(click_y,2), "et y =", round(click_x,2))
      } else {
        "Position x et y du clic."
      }
    })

    output$tablePoint <- DT::renderDT({
      event_sub <- filter_event(input)
      event_sub$Y.sens.div10 <- event_sub$Y.sens / 10
      event_sub$X.sens.div10 <- event_sub$X.sens / 10
      selected_rows <- NULL
      numeric_columns <- sapply(event_sub, is.numeric)
      event_sub[, numeric_columns] <- round(event_sub[, numeric_columns], 3)

      if (!is.null(input$plot_click$x) && !is.null(input$plot_click$y)) {

        if(input$role==1){
          click_x <- input$plot_click$x
          click_y <- input$plot_click$y
        }else if(input$role==2){
          click_x <- 70-input$plot_click$x
          click_y <-  100-input$plot_click$y
        }
        rayon <- 1
        # Filtrer les lignes de event_sub où Y.sens.div10 et X.sens.div10 sont dans le rayon autour du click
        selected_rows <- subset(event_sub, abs(Y.sens.div10 - click_x) <= rayon & abs(X.sens.div10 - click_y) <= rayon)
      }

      if (!is.null(input$plot_brush$xmin)) {

        brush_xmin <- input$plot_brush$xmin
        brush_xmax <- input$plot_brush$xmax
        brush_ymin <- input$plot_brush$ymin
        brush_ymax <- input$plot_brush$ymax
        # Filtrer les lignes de event_sub où Y.sens.div10 et X.sens.div10 sont dans le cadre du brush
        ifelse(input$role == 2,
               selected_rows <- subset(event_sub,70-Y.sens.div10 >= brush_xmin & 70-Y.sens.div10 <= brush_xmax & 100-X.sens.div10 >= brush_ymin & 100-X.sens.div10 <= brush_ymax),
               selected_rows <- subset(event_sub,Y.sens.div10 >= brush_xmin & Y.sens.div10 <= brush_xmax & X.sens.div10 >= brush_ymin & X.sens.div10 <= brush_ymax)
        )

      }

      if (!is.null(selected_rows)) {
        selected_rows <- selected_rows[, c("Joueur","name","xPtsFor", "xPtsAgainst","XPtsDiff","StatNameseq","nameseq","XPDseq","XPFseq","XPAseq", "maxXPDseq","TimeVideo")]

        dt <- datatable(
          data = selected_rows,
          rownames = FALSE,
          extensions = c("Scroller", "FixedColumns", "Buttons"),
          options = list(autoWidth = TRUE, scrollX = TRUE, scrollY = "400px", searching = FALSE, paging = TRUE, info = FALSE, fixedColumns = list(leftColumns = 1), dom = 'lrtip', scroller = TRUE)
        )


      }
    })

    output$nb_data <- renderText({
      deb <- input$tpsReg[1]
      fin <- input$tpsReg[2]
      if (input$role == 1) {
        if (is.null(input$Type)) {
          select_event <- event %>%
            filter(StatName %in% input$selected_event,name %in% vals$equipe,name2 %in% vals$equipeAdv,Competition %in% vals$compet,season %in% vals$annee,X.sens/10 >= vals$X[1],X.sens/10 <= vals$X[2],Y.sens/10 >= vals$Y[1],Y.sens/10 <= vals$Y[2],GameSeconds/60 <= fin,GameSeconds/60 >= deb
            )
        } else {
          select_event <- event %>%
            filter(StatName %in% input$selected_event,Type %in% input$Type,name %in% vals$equipe,name2 %in% vals$equipeAdv,Competition %in% vals$compet,season %in% vals$annee,X.sens/10 >= vals$X[1],X.sens/10 <= vals$X[2],Y.sens/10 >= vals$Y[1],Y.sens/10 <= vals$Y[2],GameSeconds/60 <= fin,GameSeconds/60 >= deb
            )
        }
      } else if (input$role == 2) {
        if (is.null(input$Type)) {
          select_event <- event %>%
            filter(StatName %in% input$selected_event,name2 %in% vals$equipe,name %in% vals$equipeAdv,Competition %in% vals$compet,season %in% vals$annee, 100 - X.sens/10 >= vals$X[1],100 - X.sens/10 <= vals$X[2],70 - Y.sens/10 >= vals$Y[1],70 - Y.sens/10 <= vals$Y[2],GameSeconds/60 <= fin,GameSeconds/60 >= deb
            )
        } else {
          select_event <- event %>%
            filter(StatName %in% input$selected_event, Type %in% input$Type,name2 %in% vals$equipe,name %in% vals$equipeAdv,Competition %in% vals$compet,season %in% vals$annee,100 - X.sens/10 >= vals$X[1],100 - X.sens/10 <= vals$X[2],70 - Y.sens/10 >= vals$Y[1],70 - Y.sens/10 <= vals$Y[2],GameSeconds/60 <= fin,GameSeconds/60 >= deb
            )
        }
      }

      paste("Nombre de données :", nrow(select_event))
    })

    output$nb_data_zones <- renderText({
      event_sub <- filter_event(input)
      if (input$wRuck) {
        og <- event_sub$OrigineRuck
        inog <- input$OrigineRuck
      } else {
        og <- event_sub$OrigineJAP
        inog <- input$Origine
      }
      if(input$role == 1){
        ifelse(is.null(input$Type),
               select_event <- event_sub[event_sub$StatName %in% input$selected_event & og %in% inog & event_sub$name %in% vals$equipe & event_sub$name2 %in% vals$equipeAdv & event_sub$Competition %in% vals$compet & event_sub$season %in% vals$annee & event_sub$X.sens/10 >= vals$X[1] & event_sub$X.sens/10 <= vals$X[2] & event_sub$Y.sens /10>= vals$Y[1] & event_sub$Y.sens/10 <= vals$Y[2], ],
               select_event <- event_sub[event_sub$StatName %in% input$selected_event & event_sub$Type %in% input$Type & og %in% inog & event_sub$name %in% vals$equipe & event_sub$name2 %in% vals$equipeAdv & event_sub$Competition %in% vals$compet & event_sub$season %in% vals$annee & event_sub$X.sens/10 >= vals$X[1] & event_sub$X.sens/10 <= vals$X[2] & event_sub$Y.sens /10>= vals$Y[1] & event_sub$Y.sens/10 <= vals$Y[2], ]
        )
      }else if(input$role == 2){
        ifelse(is.null(input$Type),
               select_event <- event_sub[event_sub$StatName %in% input$selected_event & og %in% inog & event_sub$name2 %in% vals$equipe & event_sub$name %in% vals$equipeAdv & event_sub$Competition %in% vals$compet & event_sub$season %in% vals$annee & 100-event_sub$X.sens/10 >= vals$X[1] & 100-event_sub$X.sens/10 <= vals$X[2] & 70-event_sub$Y.sens/10 >= vals$Y[1] & 70-event_sub$Y.sens/10 <= vals$Y[2], ],
               select_event <- event_sub[event_sub$StatName %in% input$selected_event & event_sub$Type %in% input$Type & og %in% inog & event_sub$name2 %in% vals$equipe & event_sub$name %in% vals$equipeAdv & event_sub$Competition %in% vals$compet & event_sub$season %in% vals$annee & 100-event_sub$X.sens/10 >= vals$X[1] & 100-event_sub$X.sens/10 <= vals$X[2] & 70-event_sub$Y.sens/10 >= vals$Y[1] & 70-event_sub$Y.sens/10 <= vals$Y[2], ]
        )
      }
      paste("Nombre de données zones :", nrow(select_event))
    })

    output$slider <- renderUI({
      req(input$echeance)
      if (input$echeance == "Temps de jeu") {
        sliderInput(ns("sliderInput"), "Temps de jeu:", min = 0, max = 10, value = 3)
      } else if (input$echeance == "Durée") {
        sliderInput(ns("sliderInput"), "Durée:", min = 0, max = 120, value = 30)
      }
    })


    output$y_interval_inputs <- renderUI({
      yZones <- input$YZones
      yMax <- vals$Y[2]
      intervalValues <- seq(vals$Y[1], yMax, length.out = yZones+1)
      if(yZones !=1 ){
        numericInputs <- lapply(1:(yZones-1), function(i) {
          ma <- ifelse(i == yZones-1, yMax, round(intervalValues[i+2]))
          mi <- ifelse(i == 1, 0, round(intervalValues[i]))
          column(3, numericInput(ns(paste0("y_interval", i)), paste0("Y", i, " :"), value = round(intervalValues[i+1]), step = 2,max = ma-1, min = mi+1))
        })
        fluidRow(numericInputs)
      }
    })

    output$x_interval_inputs <- renderUI({
      xZones <- input$XZones
      xMax <- vals$X[2]
      intervalValues <- seq(vals$X[1], xMax, length.out = xZones +1)
      if(xZones !=1 ){
        numericInputs <- lapply(1:(xZones-1), function(i) {
          ma <- ifelse(i == xZones-1, xMax  ,round(intervalValues[i+2]))
          mi <- ifelse(i == 1,vals$X[1] ,round(intervalValues[i]))
          column(3, numericInput(ns(paste0("x_interval", i)), paste0("X", i, " :"), value = round(intervalValues[i+1]), step = 2,max = ma-1, min = mi+1))
        })
        fluidRow(numericInputs)
      }
    })


    ###### OBSERVE #####

    observeEvent(input$help, {
      showModal(modalDialog(
        title = "Informations",
        HTML(paste(
          "<b/>","EN ATTAQUE : Actions effectués par Equipe","</b>",  "<i/>","<br/>",
          "<b/>","- Point blanc: position de départ de l'action","</b>",
          "<br/>","<b/>","- Point coloré: position de l'action après échéance séléctionné:","</b>",
          "<br/>", "&nbsp;","&nbsp;","&nbsp;","<b/>", "+ Vert","</b>", "si la possession est conservé par l'Equipe",
          "<br/>","&nbsp;","&nbsp;","&nbsp;","<b/>","+ Rouge","</b>", "si la possession est perdu par l'Equipe et donc pour l'Equipe adverse","<br/>","<br/>",
          "</i>","<b/>","EN DEFENSE : Actions concédées par Equipe donc effectués par Equipe Adverse","</b>",  "<i/>","<br/>",
          "<b/>","- Même signification des points","</b>","<br/>",
          "<b/>","- Le sens du terrain reste le même donc l'Equipe Adverse attaque vers le bas","</b>","<br/>"
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })


    observeEvent(vals$XPOINTS, {
      showModal(modalDialog(
        title = "Informations sur les xPoints",
        HTML(paste(
          "<b/>","⬆︎ CI-DESSUS :","<br/>"," Vous disposez des expected points for, against et diff des actions (xPtsFor, xPtsAgainst, xPtsDiff)","</b>",  "<i/>","<br/>","<br/>",
          "</i>","<b/>","⬇︎ CI-DESSOUS :","<br/>","Vous disposez des 3 types d'informations toujours pour l'Equipe : ","</b>",  "<i/>","<br/>",
          "<b/>","- xPts de l'échéance (tps de jeu/durée): est gardé le dernier évenement parmi (Touche,Essai,Faute, Mêlée) ou le 3e ruck si pas de dernier événement ","</b>","<br/>",
          "<b/>","- xPts de la fin de la possession: événement où la possession est perdu (Turnover, Faute)","</b>","<br/>",
          "<b/>","- xPts de la fin de la séquence: événement où le jeu s'arrête (Touche, Essai, Faute, Mêlée)","</b>","<br/>"
        )),
        easyClose = TRUE,
        footer = NULL
      ))
    })

    observeEvent(input$selected_event, {
      selected_ev <- unique(input$selected_event)
      filtered_ev <- subset(event, StatName %in% selected_ev)
      available_choices <- unique(filtered_ev$Choix)
      if (input$selected_event %in% c("Ruck", "Maul")) {
        # Afficher le selectInput si la valeur sélectionnée est "Ruck"
        output$select_choice <- renderUI({
          pickerInput(ns("Choix"), "Choix", choices = c(sort(available_choices)),options = list(`actions-box` = TRUE), multiple = TRUE)
        })
      } else {
        # Masquer le selectInput pour toutes les autres valeurs sélectionnées
        output$select_choice <- renderUI({})
      }
      available_types <- unique(filtered_ev$Type)
      updatePickerInput(session, "Type", choices = c(sort(available_types)) )
    })

    observeEvent(input$Choix, {
      event_sub <- filter_event(input)
      selected_ev <- unique(input$Choix)
      filtered_ev <- subset(event_sub, Choix %in% selected_ev)
      available_types <- unique(filtered_ev$Type)
      updatePickerInput(session, "Type", choices = c(sort(available_types)), selected = c(sort(available_types)) )
    })

    observeEvent(input$Type, {
      selected_ev <- unique(input$Type)
      current_origine <- input$Origine
      filtered_ev <- subset(event, Type %in% selected_ev)
      if(input$wRuck){
        available_types <- unique(filtered_ev$OrigineRuck)
      }else{
        available_types <- unique(filtered_ev$OrigineJAP)
      }
      updatePickerInput(session, "Origine", choices = c(sort(available_types)), selected = current_origine )
    })


    observeEvent(vals$equipe, {
      available_teams <- vals$equipe
      updateSelectInput(session, "ponder_team", choices = c(sort(available_teams)))
    })

    observeEvent(vals$equipeAdv, {
      available_teams <- vals$equipeAdv
      updateSelectInput(session, "ponder_team_adv", choices = c(sort(available_teams)))
    })




  })
}

