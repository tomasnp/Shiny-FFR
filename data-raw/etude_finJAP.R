

df <- box_df

# Définir les intervalles pour X et Y
df$X_ZoneF <- cut(df$X.end, breaks=c(-86, 100, 200,300,400,500,600,700,800,900,1000,1100), labels=c("-100-100", "100-200", "200-300","300-400","400-500","500-600","600-700","700-800","800-900","900-1000","1000+"), include.lowest = TRUE)
df$Y_ZoneF <- cut(df$Y.end, breaks=c(0, 100, 200,300,400,500,600,700), labels=c("0-100", "100-200", "200-300","300-400","400-500","500-600","600-700"), include.lowest = TRUE)

# Calculer la moyenne de XPD pour chaque groupe d'intervalles
avg_XPD <- aggregate(df$diff_XPD, by=list(X_ZoneF=df$X_ZoneF, Y_ZoneF=df$Y_ZoneF), FUN=mean, na.rm = TRUE)


count_XPD <- aggregate(df$diff_XPD, by=list(X_ZoneF=df$X_ZoneF, Y_ZoneF=df$Y_ZoneF), FUN=length)
# Calculer la moyenne de XPD pour chaque groupe d'intervalles
# Créer une grille complète avec toutes les combinaisons de X_ZoneF et Y_ZoneF
full_grid <- expand.grid(X_ZoneF=levels(df$X_ZoneF), Y_ZoneF=levels(df$Y_ZoneF))

# Fusionner le tableau complet avec les données moyennes
avg_XPD_full <- merge(full_grid, avg_XPD, by=c("X_ZoneF", "Y_ZoneF"), all.x=TRUE)

# Fusionner le tableau complet avec les comptes
count_XPD_full <- merge(full_grid, count_XPD, by=c("X_ZoneF", "Y_ZoneF"), all.x=TRUE)

# Remplacer les NAs par 0 dans le tableau de comptes
count_XPD_full$x[is.na(count_XPD_full$x)] <- 0


names(avg_XPD) <- c("X_ZoneF", "Y_ZoneF", "Avg_XPD")
names(count_XPD) <- c("X_ZoneF", "Y_ZoneF", "Count_XPD")

result <- merge(avg_XPD, count_XPD, by=c("X_ZoneF", "Y_ZoneF"))


mat_avg_XPD <- as.matrix(avg_XPD)


data <- avg_XPD_full
matrix_result <- matrix(nrow = 7, ncol = 11)

# Définir les niveaux pour X_ZoneF et Y_ZoneF
levels_X_ZoneF <- c("-100-100", "100-200", "200-300","300-400","400-500","500-600","600-700","700-800","800-900","900-1000","1000+")
levels_Y_ZoneF <- c("0-100", "100-200", "200-300","300-400","400-500","500-600","600-700")

# Convertir X_ZoneF et Y_ZoneF en facteurs avec les niveaux spécifiés
data$X_ZoneF <- factor(data$X_ZoneF, levels = levels_X_ZoneF)
data$Y_ZoneF <- factor(data$Y_ZoneF, levels = levels_Y_ZoneF)

# Créer une fonction pour déterminer l'indice dans la matrice pour une paire donnée de X_ZoneF et Y_ZoneF
get_matrix_index <- function(x, y) {
  x_index <- which(levels_X_ZoneF == x)
  y_index <- which(levels_Y_ZoneF == y)
  return(c(x_index, y_index))
}
# Remplir la matrice avec les valeurs Avg_XPD
for(i in 1:nrow(data)) {
  indices <- get_matrix_index(data$X_ZoneF[i], data$Y_ZoneF[i])
  matrix_result[indices[2], indices[1]] <- data$x[i]
}


my_colors <- colorRampPalette(c("red", "orange", "green"))
cy <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
cx <- c(0,0.16,0.33,0.5,0.66,0.83,1)

A<- 20


options('smoother.tails' = TRUE, 'smoother.gaussianwindow.alpha' = A) #plus alpha est grand, moins c'est lissé
matrix_result_for_gauss <- smth.gaussian(matrix_result)
matrix_result_for_gauss<- matrix(matrix_result_for_gauss, 7,11)





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

smoothed <- matrix_smooth(matrix_result, window_size = 1, smoothing_factor = 4)



#
# image(matrix_result, col = my_colors(11), axes=TRUE, main=paste("XPDseq fin JAP terr "))
#
# for (i in 1:7) {
#   for (j in 1:11) {
#     x <- cx[i]
#     y <- cy[j]
#     value <- matrix_result[i, j]
#     if (is.na(value)) {
#       col <- "black"  # Colorier en noir si la valeur est NA
#       label <- "-"  # Afficher "NA" à la place de la valeur moyenne
#     } else {
#       col <- "white"
#       label <- round(value, 3)
#     }
#     font <- 2
#     text(x = x, y = y, labels = label, col = col, font = font)
#   }
# }


image(smoothed, col = my_colors(11), axes=FALSE, main=paste("diffXPD fin JAP Box Gauss"))

dx = diff(range(cx))/length(cx)  # calculer la largeur de la case
dy = diff(range(cy))/length(cy)  # calculer la hauteur de la case

for (i in 1:7) {
  for (j in 1:11) {
    x <- cx[i]
    y <- cy[j]
    value <- smoothed[i, j]
    if (is.na(value)) {
      col <- "black"  # Colorier en noir si la valeur est NA
      label <- ""  # Afficher "NA" à la place de la valeur moyenne
    } else {
      col <- "white"
      label <- round(value, 3)
    }
    font <- 2
    text(x = x, y = y, labels = label, col = col, font = font)
  }
}



# cx <- c(0, 0.25, 0.5, 0.75, 1)
# cy <- c(0, 0.25, 0.5, 0.75, 1)
#
#
# # Créer une matrice vide de 5x5
# mat <- matrix(nrow = 5, ncol = 5)
#
# # Remplir chaque case de la matrice avec le numéro de la ligne
# for(i in 1:5){
#   mat[,i] <- 6-i
# }
#
# # Modification de la valeur à la position (2,5)
# mat[5, 4] <- 0
#
#
#
#
#
#
# image(mat, col = my_colors(11), axes=TRUE, main=paste("test gauss"))
#
# for (i in 1:5) {
#   for (j in 1:5) {
#     x <- cx[i]
#     y <- cy[j]
#     value <- mat[i, j]
#     if (is.na(value)) {
#       col <- "black"  # Colorier en noir si la valeur est NA
#       label <- "-"  # Afficher "NA" à la place de la valeur moyenne
#     } else {
#       col <- "white"
#       label <- round(value, 3)
#     }
#     font <- 2
#     text(x = x, y = y, labels = label, col = col, font = font)
#   }
# }
#
#
# A<- 3
#
#
# options('smoother.tails' = TRUE, 'smoother.gaussianwindow.alpha' = A) #plus alpha est grand, moins c'est lissé
# mat_for_gauss <- smth.gaussian(mat)
# mat_for_gauss<- matrix(mat_for_gauss, 5,5)
#
#
# image(mat_for_gauss, col = my_colors(11), axes=TRUE, main=paste("test gauss :", A))
#
# for (i in 1:5) {
#   for (j in 1:5) {
#     x <- cx[i]
#     y <- cy[j]
#     value <- mat_for_gauss[i, j]
#     if (is.na(value)) {
#       col <- "black"  # Colorier en noir si la valeur est NA
#       label <- "-"  # Afficher "NA" à la place de la valeur moyenne
#     } else {
#       col <- "white"
#       label <- round(value, 3)
#     }
#     font <- 2
#     text(x = x, y = y, labels = label, col = col, font = font)
#   }
# }





# Charger le package
library(reshape2)

# Créer une matrice


# Convertir la matrice en un data frame long
df <- melt(smoothed)

# Renommer les colonnes
colnames(df) <- c("Y", "X", "value")

# Remapper les valeurs dans la colonne "Y"
df$Y[df$Y == 1] <- 0
df$Y[df$Y == 2] <- 100
df$Y[df$Y == 3] <- 200
df$Y[df$Y == 4] <- 300
df$Y[df$Y == 5] <- 400
df$Y[df$Y == 6] <- 500
df$Y[df$Y == 7] <- 600

# Remapper les valeurs dans la colonne "X"
df$X[df$X == 1] <- 0
df$X[df$X == 2] <- 100
df$X[df$X == 3] <- 200
df$X[df$X == 4] <- 300
df$X[df$X == 5] <- 400
df$X[df$X == 6] <- 500
df$X[df$X == 7] <- 600
df$X[df$X == 8] <- 700
df$X[df$X == 9] <- 800
df$X[df$X == 10] <- 900
df$X[df$X == 11] <- 1000


write_xlsx(df, "~/Desktop/TStats/FFR/Etude/diffXPD_Fin_Box.xlsx")









