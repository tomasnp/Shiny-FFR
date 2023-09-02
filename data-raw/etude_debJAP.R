library(readxl)

#event_red_xpts <- read_excel("~/Desktop/TStats/FFR/event_red_update.xlsx")
library(dplyr)

event_sub <- event_sub %>% mutate(diff_XPD = XPDseq - XPtsDiff)
event_sub <- event_sub %>% mutate(diff_XPF = XPFseq - xPtsFor)
event_sub <- event_sub %>% mutate(diff_XPA = XPAseq - xPtsAgainst)

xZone <- 2
yZone <- 2

generate_x_zones <- function(xZone, XMAX) {
  # La première limite est toujours -86
  x_breaks <- c(-100)

  # Si xZone est 1, la seule zone est de -86 à XMAX
  if(xZone == 1) {
    x_breaks <- c(x_breaks, XMAX)
  } else {
    # Sinon, diviser l'intervalle 0 à XMAX en (xZone) sous-intervalles
    interval_length <- XMAX / (xZone)
    x_breaks <- c(x_breaks, round(seq(from = interval_length, to = XMAX, by = interval_length)))
  }

  # Si la dernière limite est inférieure à XMAX en raison d'erreurs d'arrondi, l'ajuster à XMAX
  if(tail(x_breaks, n = 1) < XMAX) {
    x_breaks[length(x_breaks)] <- XMAX
  }

  # Générer les labels pour les zones
  x_labels <- paste(head(x_breaks, -1), tail(x_breaks, -1), sep = "-")

  # Renommer le premier label en "0-"
  x_labels[1] <- paste("0", round(tail(x_breaks, -1)[1]), sep = "-")

  return(list("breaks" = x_breaks, "labels" = x_labels))
}


terr_df <- event_sub[event_sub$Type == "Territorial",]
box_df <- event_sub[event_sub$Type == "Box",]
bomb_df <-event_sub[event_sub$Type == "Bomb",]
chip_df <-event_sub[event_sub$Type == "Chip",]
crosspitch_df <-event_sub[event_sub$Type == "Cross Pitch",]
low_df <-event_sub[event_sub$Type == "Low",]
touchkick_df <- event_sub[event_sub$Type == "Touch Kick",]

# input_df <- terr_df
# origine <- "Ruck"
process_df <- function(input_df, origine, XMAX) {

  # Attention origineRuck ou OrigineJAP
  df <- input_df[which(input_df$OrigineJAP == origine), ]

  if(identical(input_df,terr_df) ){t <- "Territorial" }
  if(identical(input_df,touchkick_df) ){t <- "TouchKick" }
  if(identical(input_df,box_df) ){t <- "Box" }


  # "Coup d'envoi adv"
  # "Jeu au pied adv"
  # "Turnover gagne"

  x_breaks <- generate_x_zones(xZone, XMAX)$breaks
  # Générer les labels pour X
  x_labels <- generate_x_zones(xZone, XMAX)$labels

  # Générer les limites pour Y
  y_breaks <- seq(from = 0, to = 700, length.out = yZone + 1)
  # Générer les labels pour Y
  y_labels <- paste(head(y_breaks, -1), tail(y_breaks, -1), sep = "-")

  print(x_labels)
  print(y_labels)

  # Appliquer les coupes
  df$X_ZoneF <- cut(df$X.end, breaks = x_breaks, labels = x_labels, include.lowest = TRUE)
  df$Y_ZoneF <- cut(df$Y.end, breaks = y_breaks, labels = y_labels, include.lowest = TRUE)

  # View(df[c("X.sens", "Y.sens", "OrigineJAP", "OrigineRuck","X_ZoneF","Y_ZoneF")])

  # Calculer la moyenne de XPD pour chaque groupe d'intervalles
  avg_XPD <- aggregate(df$diff_XPD, by=list(X_ZoneF=df$X_ZoneF, Y_ZoneF=df$Y_ZoneF), FUN=mean, na.rm = TRUE)


  count_XPD <- aggregate(df$diff_XPD, by=list(X_ZoneF=df$X_ZoneF, Y_ZoneF=df$Y_ZoneF), FUN=length)


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


  my_colors <- colorRampPalette(c("red", "orange", "green"))
  cy <- seq(0, 1, length.out = yZone)
  cx <- seq(0, 1, length.out = xZone)




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

  #View(matrix_result)


  smoothed <- matrix_smooth(matrix_result, window_size = 1, smoothing_factor = 4)

  map_colors <- function(x) {
    if (is.na(x)) {
      return("black")
    } else if (x > 2) {
      return("#276221")
    } else if (x > 1 & x <= 2) {
      return("#5bb450")
    } else if (x > 0 & x <= 1) {
      return("#acd8a7")
    } else if (x <= 0 & x > -1) {
      return("#f69697")
    } else if (x <= -1 & x > -2) {
      return("#ff2c2c")
    } else {  # x <= -2
      return("#c30010")
    }
  }
  # Faire une transposition (rotation de 90 degrés vers la droite)
  matrix_result <- t(matrix_result)

  # Inverser l'ordre des colonnes (pour obtenir une rotation de 90 degrés vers la gauche)
  matrix_result <- matrix_result[, seq_len(ncol(matrix_result))]

  # Ensuite, vous pouvez continuer avec votre code de tracé comme précédemment



  xmin <- 0
  xmax <- XMAX/10
  ymin <- 0
  ymax <- 70

  plot(1,1, type = "n", xlab = "", ylab = "", xlim = c(ymin, ymax), ylim = c(xmin, xmax),
        main = paste("diffXPD deb", t , "après", origine))

  y_seq <- seq(from = xmin, to = xmax, length.out = nrow(matrix_result) + 1)
  x_seq <- seq(from = ymin, to = ymax, length.out = ncol(matrix_result) + 1)

  for (i in 1:nrow(matrix_result)) {
    for (j in 1:ncol(matrix_result)) {
      xleft <- x_seq[j]
      ybottom <- y_seq[i]
      xright <- x_seq[j + 1]
      ytop <- y_seq[i + 1]
      value <- matrix_result[i, j]
      if (is.na(value)) {
        col <- "white"  # Color in white if the value is NA
        label <- ""  # Display "NA" instead of the average value
      } else {
        col <- map_colors(value)
        label <- round(value, 2)
      }
      rect(xleft, ybottom, xright, ytop, col = col, border = NA)  # bordure supprimée
      text(x = mean(c(xleft, xright)), y = mean(c(ybottom, ytop)), labels = label)
    }
  }



  lines(c(0,0), c(0,100), col = "black", lwd = 2)
  lines(c(70,70), c(0,100), col = "black", lwd = 2)
  lines(c(0,70), c(50,50), col = "black", lwd = 2)
  lines(c(0,70), c(100,100), col = "black", lwd = 2)
  lines(c(0,70), c(0,0), col = "black", lwd = 2)

  # Draw lines
  lines(c(0,70), c(22,22), col = "black", lwd = 2)
  lines(c(0,70), c(78,78), col = "black", lwd = 2)


  # Poteaux
  lines(c(33,32.5), c(100,107.5), col = "black", lwd = 2)
  lines(c(37,37.5), c(100,107.5), col = "black", lwd = 2)
  lines(c(33,37), c(102.5,102.5), col = "black", lwd = 2)
  lines(c(33,32.5), c(0,-7.5), col = "black", lwd = 2)
  lines(c(37,37.5), c(0,-7.5), col = "black", lwd = 2)
  lines(c(33,37), c(-2.5,-2.5), col = "black", lwd = 2)

  return(NULL)


  # for (i in 1:(yZone)) {
  #   for (j in 1:(xZone)) {
  #     print(paste(i,j))
  #     xleft <- cx[i]
  #     ybottom <- cy[j]
  #     xright <- cx[i + 1]
  #     ytop <- cy[j + 1]
  #     value <- matrix_result[i, j]
  #     if (is.na(value)) {
  #       col <- "white"  # Colorier en noir si la valeur est NA
  #       label <- "-"  # Afficher "NA" à la place de la valeur moyenne
  #     } else {
  #       col <- "white"
  #       label <- round(value, 3)
  #     }
  #     rect(xleft = xleft, ybottom = ybottom, xright = xright, ytop = ytop, col = map_colors(round(value, 3)))
  #     font <- 2
  #     text(x = mean(c(xleft, xright)), y = mean(c(ybottom, ytop)), labels = label, col = col, font = font)
  #   }
  # }



}



#
# process_df(terr_df, "Coup d'envoi adv")
# process_df(box_df, "Coup d'envoi adv")
# process_df(touchkick_df, "Coup d'envoi adv")
#
# process_df(terr_df, "Jeu au pied adv")
# process_df(box_df, "Jeu au pied adv")
# process_df(touchkick_df, "Jeu au pied adv")
#
# process_df(terr_df, "Turnover gagne")
# process_df(box_df, "Turnover gagne")
# process_df(touchkick_df, "Turnover gagne")


process_df(terr_df, 'Jeu au pied adv', 1100)
# process_df(box_df, "Ruck")
# process_df(touchkick_df, "Ruck")




#
# write_xlsx(df_mat, "~/Desktop/TStats/FFR/Etude/diffXPD_Deb_Box.xlsx")





#
# filtered_df <- event_sub %>%
#   filter(OrigineRuck %in% c("Ruck", "Passe")) %>%
#   filter(Type %in% c("Territorial", "Box", "Touch Kick"))
#
# # Charger les packages nécessaires
# library(dplyr)
# library(tidyverse)
#
# # Créer un tableau récapitulatif avec les moyennes pour chaque type
# summary_table <- filtered_df %>%
#   group_by(Type) %>%
#   summarise(
#     avg_xPtsFor = mean(xPtsFor, na.rm = TRUE),
#     avg_xPtsAgainst = mean(xPtsAgainst, na.rm = TRUE),
#     avg_xPtsDiff = mean(XPtsDiff, na.rm = TRUE),
#     avg_XPFseq = mean(XPFseq, na.rm = TRUE),
#     avg_XPAseq = mean(XPAseq, na.rm = TRUE),
#     avg_XPDseq = mean(XPDseq, na.rm = TRUE),
#     avg_diffXPD = mean(diff_XPD, na.rm = TRUE),
#     avg_diffXPF = mean(diff_XPF, na.rm = TRUE),
#     avg_diffXPA = mean(diff_XPA, na.rm = TRUE),
#     count = n()
#   )
#
# # Afficher le tableau récapitulatif
# print(summary_table)
#
#





