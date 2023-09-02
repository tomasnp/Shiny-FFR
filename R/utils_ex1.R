#' utils_ex1
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @importFrom data.table ":="
#'
#' @noRd




outputFunctions <- function(input, output, session, myReactiveValues) {

  filter_event <- function(input) {
    event_current <- myReactiveValues$event_subset()
    View(event_current)
    if (input$role == 1) {
      event_filt <- event_current %>% filter(StatName %in% input$selected_event, name %in% myReactiveValues$equipe, name2 %in% myReactiveValues$equipeAdv, X.sens/10 >= input$X[1], X.sens/10 <= input$X[2],Y.sens/10 >= input$Y[1],Y.sens/10 <= input$Y[2])
    } else if(input$role == 2){
      event_filt <- event_current %>% filter(StatName %in% input$selected_event, name2 %in% myReactiveValues$equipe, name %in% myReactiveValues$equipeAdv, 100-X.sens/10 >= input$X[1], 100-X.sens/10 <= input$X[2],70-Y.sens/10 >= input$Y[1], 70-Y.sens/10 <= input$Y[2])
    }

    return(event_filt)
  }



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
      click_x <- input$plot_click$x
      click_y <- input$plot_click$y
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
      selected_rows <- subset(event_sub,
                              Y.sens.div10 >= brush_xmin & Y.sens.div10 <= brush_xmax &
                                X.sens.div10 >= brush_ymin & X.sens.div10 <= brush_ymax)
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


  output$avgXPD_after_time <- renderUI({
    event_sub <- filter_event(input)

    # Gère le cas où l'input Type est vide
    if (is.null(input$Type)) {
      event_sub <- event_sub
    } else {
      event_sub <- event_sub[event_sub$Type %in% input$Type,]

    }

    if (input$role == 1) {
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      mean_XPD <- round(weighted.mean(c(event_sub$XPDseq,event_ponder$XPDseq,event_ponder_adv$XPDseq, event_ponder_double$XPDseq),c(rep(1, times = length(event_sub$XPDseq)), rep(input$ponderation, times = length(event_ponder$XPDseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDseq))), na.rm = TRUE), 3)
    } else if (input$role == 2) {
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      mean_XPD <- -round(weighted.mean(c(event_sub$XPDseq,event_ponder$XPDseq,event_ponder_adv$XPDseq, event_ponder_double$XPDseq),c(rep(1, times = length(event_sub$XPDseq)), rep(input$ponderation, times = length(event_ponder$XPDseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDseq))), na.rm = TRUE), 3)
    }

    # Si tous les événements sont NA
    if (is.na(mean_XPD)) {
      return(div(style = "color: red;", "No data for the action"))
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      max_XPD <- round(weighted.mean(c(event_sub$maxXPDseq,event_ponder$maxXPDseq,event_ponder_adv$maxXPDseq, event_ponder_double$maxXPDseq),c(rep(1, times = length(event_sub$maxXPDseq)), rep(input$ponderation, times = length(event_ponder$maxXPDseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPDseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPDseq))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      max_XPD <- -round(weighted.mean(c(event_sub$maxXPDseq,event_ponder$maxXPDseq,event_ponder_adv$maxXPDseq, event_ponder_double$maxXPDseq),c(rep(1, times = length(event_sub$maxXPDseq)), rep(input$ponderation, times = length(event_ponder$maxXPDseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPDseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPDseq))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(max_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      max_XPD <- round(weighted.mean(c(event_sub$maxXPFseq,event_ponder$maxXPFseq,event_ponder_adv$maxXPFseq, event_ponder_double$maxXPFseq),c(rep(1, times = length(event_sub$maxXPFseq)), rep(input$ponderation, times = length(event_ponder$maxXPFseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPFseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPFseq))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      max_XPD <- round(weighted.mean(c(event_sub$minXPAseq,event_ponder$minXPAseq,event_ponder_adv$minXPAseq, event_ponder_double$minXPAseq),c(rep(1, times = length(event_sub$minXPAseq)), rep(input$ponderation, times = length(event_ponder$minXPAseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPAseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPAseq))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(max_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      max_XPD <- round(weighted.mean(c(event_sub$minXPAseq,event_ponder$minXPAseq,event_ponder_adv$minXPAseq, event_ponder_double$minXPAseq),c(rep(1, times = length(event_sub$minXPAseq)), rep(input$ponderation, times = length(event_ponder$minXPAseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPAseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPAseq))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      max_XPD <- round(weighted.mean(c(event_sub$maxXPFseq,event_ponder$maxXPFseq,event_ponder_adv$maxXPFseq, event_ponder_double$maxXPFseq),c(rep(1, times = length(event_sub$maxXPFseq)), rep(input$ponderation, times = length(event_ponder$maxXPFseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPFseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPFseq))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(max_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
    }
    div(
      style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
      as.character(max_XPD)
    )

  })

  # BOX XPts
  output$avgXPD <- renderUI({
    event_sub <- filter_event(input)
    View(event_sub)
    # Gère le cas où l'input Type est vide
    ifelse(is.null(input$Type),
           event_sub <- event_sub,
           event_sub <- event_sub[event_sub$Type %in% input$Type,]

    )

    if(input$role == 1){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      mean_XPD <- round(weighted.mean(c(event_sub$XPtsDiff,event_ponder$XPtsDiff,event_ponder_adv$XPtsDiff, event_ponder_double$XPtsDiff),c(rep(1, times = length(event_sub$XPtsDiff)), rep(input$ponderation, times = length(event_ponder$XPtsDiff)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPtsDiff)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPtsDiff))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      mean_XPD <- -round(weighted.mean(c(event_sub$XPtsDiff,event_ponder$XPtsDiff,event_ponder_adv$XPtsDiff, event_ponder_double$XPtsDiff),c(rep(1, times = length(event_sub$XPtsDiff)), rep(input$ponderation, times = length(event_ponder$XPtsDiff)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPtsDiff)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPtsDiff))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(mean_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      mean_XPF <- round(weighted.mean(c(event_sub$xPtsFor,event_ponder$xPtsFor,event_ponder_adv$xPtsFor, event_ponder_double$xPtsFor),c(rep(1, times = length(event_sub$xPtsFor)), rep(input$ponderation, times = length(event_ponder$xPtsFor)), rep(input$ponderation_adv, times = length(event_ponder_adv$xPtsFor)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$xPtsFor))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      mean_XPF <- round(weighted.mean(c(event_sub$xPtsAgainst,event_ponder$xPtsAgainst,event_ponder_adv$xPtsAgainst, event_ponder_double$xPtsAgainst),c(rep(1, times = length(event_sub$xPtsAgainst)), rep(input$ponderation, times = length(event_ponder$xPtsAgainst)), rep(input$ponderation_adv, times = length(event_ponder_adv$xPtsAgainst)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$xPtsAgainst))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(mean_XPF) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      mean_XPA <- round(weighted.mean(c(event_sub$xPtsAgainst,event_ponder$xPtsAgainst,event_ponder_adv$xPtsAgainst, event_ponder_double$xPtsAgainst),c(rep(1, times = length(event_sub$xPtsAgainst)), rep(input$ponderation, times = length(event_ponder$xPtsAgainst)), rep(input$ponderation_adv, times = length(event_ponder_adv$xPtsAgainst)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$xPtsAgainst))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      mean_XPA <- round(weighted.mean(c(event_sub$xPtsFor,event_ponder$xPtsFor,event_ponder_adv$xPtsFor, event_ponder_double$xPtsFor),c(rep(1, times = length(event_sub$xPtsFor)), rep(input$ponderation, times = length(event_ponder$xPtsFor)), rep(input$ponderation_adv, times = length(event_ponder_adv$xPtsFor)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$xPtsFor))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(mean_XPA) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
    }
    div(
      style = paste0("color: white; background-color:", color_value(mean_XPA), ";"),
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      mean_XPD <- round(weighted.mean(c(event_sub$XPDfinseq,event_ponder$XPDfinseq,event_ponder_adv$XPDfinseq, event_ponder_double$XPDfinseq),c(rep(1, times = length(event_sub$XPDfinseq)), rep(input$ponderation, times = length(event_ponder$XPDfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDfinseq))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      mean_XPD <- -round(weighted.mean(c(event_sub$XPDfinseq,event_ponder$XPDfinseq,event_ponder_adv$XPDfinseq, event_ponder_double$XPDfinseq),c(rep(1, times = length(event_sub$XPDfinseq)), rep(input$ponderation, times = length(event_ponder$XPDfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDfinseq))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(mean_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      mean_XPD <- round(weighted.mean(c(event_sub$XPFfinseq,event_ponder$XPFfinseq,event_ponder_adv$XPFfinseq, event_ponder_double$XPFfinseq),c(rep(1, times = length(event_sub$XPFfinseq)), rep(input$ponderation, times = length(event_ponder$XPFfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPFfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPFfinseq))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      mean_XPD <- round(weighted.mean(c(event_sub$XPAfinseq,event_ponder$XPAfinseq,event_ponder_adv$XPAfinseq, event_ponder_double$XPAfinseq),c(rep(1, times = length(event_sub$XPAfinseq)), rep(input$ponderation, times = length(event_ponder$XPAfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPAfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPAfinseq))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(mean_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      mean_XPD <- round(weighted.mean(c(event_sub$XPAfinseq,event_ponder$XPAfinseq,event_ponder_adv$XPAfinseq, event_ponder_double$XPAfinseq),c(rep(1, times = length(event_sub$XPAfinseq)), rep(input$ponderation, times = length(event_ponder$XPAfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPAfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPAfinseq))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      mean_XPD <- round(weighted.mean(c(event_sub$XPFfinseq,event_ponder$XPFfinseq,event_ponder_adv$XPFfinseq, event_ponder_double$XPFfinseq),c(rep(1, times = length(event_sub$XPFfinseq)), rep(input$ponderation, times = length(event_ponder$XPFfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPFfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPFfinseq))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(mean_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
    }
    div(
      style = paste0("color: white; background-color:", color_value(mean_XPD), ";"),
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      max_XPD <- round(weighted.mean(c(event_sub$maxXPFfinseq,event_ponder$maxXPFfinseq,event_ponder_adv$maxXPFfinseq, event_ponder_double$maxXPFfinseq),c(rep(1, times = length(event_sub$maxXPFfinseq)), rep(input$ponderation, times = length(event_ponder$maxXPFfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPFfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPFfinseq))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      max_XPD <- round(weighted.mean(c(event_sub$minXPAfinseq,event_ponder$minXPAfinseq,event_ponder_adv$minXPAfinseq, event_ponder_double$minXPAfinseq),c(rep(1, times = length(event_sub$minXPAfinseq)), rep(input$ponderation, times = length(event_ponder$minXPAfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPAfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPAfinseq))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(max_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
    }
    div(
      style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
      as.character(max_XPD)
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      max_XPD <- round(weighted.mean(c(event_sub$minXPAfinseq,event_ponder$minXPAfinseq,event_ponder_adv$minXPAfinseq, event_ponder_double$minXPAfinseq),c(rep(1, times = length(event_sub$minXPAfinseq)), rep(input$ponderation, times = length(event_ponder$minXPAfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$minXPAfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$minXPAfinseq))), na.rm = TRUE), 3)
    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      max_XPD <- round(weighted.mean(c(event_sub$maxXPFfinseq,event_ponder$maxXPFfinseq,event_ponder_adv$maxXPFfinseq, event_ponder_double$maxXPFfinseq),c(rep(1, times = length(event_sub$maxXPFfinseq)), rep(input$ponderation, times = length(event_ponder$maxXPFfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPFfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPFfinseq))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(max_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
    }
    div(
      style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      max_XPD <- round(weighted.mean(c(event_sub$maxXPDfinseq,event_ponder$maxXPDfinseq,event_ponder_adv$maxXPDfinseq, event_ponder_double$maxXPDfinseq),c(rep(1, times = length(event_sub$maxXPDfinseq)), rep(input$ponderation, times = length(event_ponder$maxXPDfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPDfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPDfinseq))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      max_XPD <- -round(weighted.mean(c(event_sub$maxXPDfinseq,event_ponder$maxXPDfinseq,event_ponder_adv$maxXPDfinseq, event_ponder_double$maxXPDfinseq),c(rep(1, times = length(event_sub$maxXPDfinseq)), rep(input$ponderation, times = length(event_ponder$maxXPDfinseq)), rep(input$ponderation_adv, times = length(event_ponder_adv$maxXPDfinseq)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$maxXPDfinseq))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(max_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      mean_XPD <- round(weighted.mean(c(event_sub$XPDfinposs,event_ponder$XPDfinposs,event_ponder_adv$XPDfinposs, event_ponder_double$XPDfinposs),c(rep(1, times = length(event_sub$XPDfinposs)), rep(input$ponderation, times = length(event_ponder$XPDfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDfinposs))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      mean_XPD <- -round(weighted.mean(c(event_sub$XPDfinposs,event_ponder$XPDfinposs,event_ponder_adv$XPDfinposs, event_ponder_double$XPDfinposs),c(rep(1, times = length(event_sub$XPDfinposs)), rep(input$ponderation, times = length(event_ponder$XPDfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDfinposs))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(mean_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      mean_XPD <- round(weighted.mean(c(event_sub$XPFfinposs,event_ponder$XPFfinposs,event_ponder_adv$XPFfinposs, event_ponder_double$XPFfinposs),c(rep(1, times = length(event_sub$XPFfinposs)), rep(input$ponderation, times = length(event_ponder$XPFfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPFfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPFfinposs))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      mean_XPD <- round(weighted.mean(c(event_sub$XPAfinposs,event_ponder$XPAfinposs,event_ponder_adv$XPAfinposs, event_ponder_double$XPAfinposs),c(rep(1, times = length(event_sub$XPAfinposs)), rep(input$ponderation, times = length(event_ponder$XPAfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPAfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPAfinposs))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(mean_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      mean_XPD <- round(weighted.mean(c(event_sub$XPAfinposs,event_ponder$XPAfinposs,event_ponder_adv$XPAfinposs, event_ponder_double$XPAfinposs),c(rep(1, times = length(event_sub$XPAfinposs)), rep(input$ponderation, times = length(event_ponder$XPAfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPAfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPAfinposs))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      mean_XPD <- round(weighted.mean(c(event_sub$XPFfinposs,event_ponder$XPFfinposs,event_ponder_adv$XPFfinposs, event_ponder_double$XPFfinposs),c(rep(1, times = length(event_sub$XPFfinposs)), rep(input$ponderation, times = length(event_ponder$XPFfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPFfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPFfinposs))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(mean_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
    }
    div(
      style = paste0("color: white; background-color:", color_value(mean_XPD), ";"),
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      max_XPD <- round(weighted.mean(c(event_sub$XPDmaxfinposs,event_ponder$XPDmaxfinposs,event_ponder_adv$XPDmaxfinposs, event_ponder_double$XPDmaxfinposs),c(rep(1, times = length(event_sub$XPDmaxfinposs)), rep(input$ponderation, times = length(event_ponder$XPDmaxfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDmaxfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDmaxfinposs))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      max_XPD <- -round(weighted.mean(c(event_sub$XPDmaxfinposs,event_ponder$XPDmaxfinposs,event_ponder_adv$XPDmaxfinposs, event_ponder_double$XPDmaxfinposs),c(rep(1, times = length(event_sub$XPDmaxfinposs)), rep(input$ponderation, times = length(event_ponder$XPDmaxfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPDmaxfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPDmaxfinposs))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(max_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      max_XPD <- round(weighted.mean(c(event_sub$XPFmaxfinposs,event_ponder$XPFmaxfinposs,event_ponder_adv$XPFmaxfinposs, event_ponder_double$XPFmaxfinposs),c(rep(1, times = length(event_sub$XPFmaxfinposs)), rep(input$ponderation, times = length(event_ponder$XPFmaxfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPFmaxfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPFmaxfinposs))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      max_XPD <- round(weighted.mean(c(event_sub$XPAminfinposs,event_ponder$XPAminfinposs,event_ponder_adv$XPAminfinposs, event_ponder_double$XPAminfinposs),c(rep(1, times = length(event_sub$XPAminfinposs)), rep(input$ponderation, times = length(event_ponder$XPAminfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPAminfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPAminfinposs))), na.rm = TRUE), 3)
    }
    # si tous les event sont NA
    if (is.na(max_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
    }
    div(
      style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
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
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team & name2 != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team_adv & name == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name != input$ponder_team & name2 != input$ponder_team_adv)
      max_XPD <- round(weighted.mean(c(event_sub$XPAminfinposs,event_ponder$XPAminfinposs,event_ponder_adv$XPAminfinposs, event_ponder_double$XPAminfinposs),c(rep(1, times = length(event_sub$XPAminfinposs)), rep(input$ponderation, times = length(event_ponder$XPAminfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPAminfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPAminfinposs))), na.rm = TRUE), 3)

    }else if(input$role == 2){
      event_ponder <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 == input$ponder_team & name != input$ponder_team_adv)
      event_ponder_adv <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 != input$ponder_team)
      event_ponder_double <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name == input$ponder_team_adv & name2 == input$ponder_team)
      event_sub <- event_sub %>%  filter(X.sens/10 >= input$X[1] & X.sens/10 <= input$X[2] & Y.sens/10 >= input$Y[1] & Y.sens/10 <= input$Y[2]) %>% filter(name2 != input$ponder_team & name != input$ponder_team_adv)
      max_XPD <- round(weighted.mean(c(event_sub$XPFmaxfinposs,event_ponder$XPFmaxfinposs,event_ponder_adv$XPFmaxfinposs, event_ponder_double$XPFmaxfinposs),c(rep(1, times = length(event_sub$XPFmaxfinposs)), rep(input$ponderation, times = length(event_ponder$XPFmaxfinposs)), rep(input$ponderation_adv, times = length(event_ponder_adv$XPFmaxfinposs)), rep(input$ponderation_adv*input$ponderation, times = length(event_ponder_double$XPFmaxfinposs))), na.rm = TRUE), 3)

    }
    # si tous les event sont NA
    if (is.na(max_XPD) )  {
      # code à exécuter si event_subset est vide, par exemple :
      return(div(style = "color: red;", "No data for the action"))
    }
    div(
      style = paste0("color: white; background-color:", color_value(max_XPD), ";"),
      as.character(max_XPD)
    )

  })

}
