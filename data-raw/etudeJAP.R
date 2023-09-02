

df <- box_df

# Définir les intervalles pour X et Y
df$X_ZoneF <- cut(df$X.end, breaks=c(-86, 100, 200,300,400,500,600,700,800,900,1000,1100), labels=c("-100-100", "100-200", "200-300","300-400","400-500","500-600","600-700","700-800","800-900","900-1000","1000+"), include.lowest = TRUE)
df$Y_ZoneF <- cut(df$Y.end, breaks=c(0, 100, 200,300,400,500,600,700), labels=c("0-100", "100-200", "200-300","300-400","400-500","500-600","600-700"), include.lowest = TRUE)

# Calculer la moyenne de XPD pour chaque groupe d'intervalles
avg_XPD <- aggregate(df$XPDseq, by=list(X_ZoneF=df$X_ZoneF, Y_ZoneF=df$Y_ZoneF), FUN=mean, na.rm = TRUE)


count_XPD <- aggregate(df$XPDseq, by=list(X_ZoneF=df$X_ZoneF, Y_ZoneF=df$Y_ZoneF), FUN=length)
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


image(matrix_result_for_gauss, col = my_colors(11), axes=TRUE, main=paste("XPDseq fin JAP box filtre gaussien :", A))

for (i in 1:7) {
  for (j in 1:11) {
    x <- cx[i]
    y <- cy[j]
    value <- matrix_result_for_gauss[i, j]
    if (is.na(value)) {
      col <- "black"  # Colorier en noir si la valeur est NA
      label <- "-"  # Afficher "NA" à la place de la valeur moyenne
    } else {
      col <- "white"
      label <- round(value, 3)
    }
    font <- 2
    text(x = x, y = y, labels = label, col = col, font = font)
  }
}


image(matrix_result, col = my_colors(11), axes=TRUE, main=paste("XPDseq fin JAP box filtre gaussien"))

for (i in 1:7) {
  for (j in 1:11) {
    x <- cx[i]
    y <- cy[j]
    value <- matrix_result[i, j]
    if (is.na(value)) {
      col <- "black"  # Colorier en noir si la valeur est NA
      label <- "-"  # Afficher "NA" à la place de la valeur moyenne
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

df <- event[event$StatName == "Ruck", ]



df$X_ZoneF <- cut(df$X.sens, breaks=c(-86,0, 100, 200,300,400,500,600,700,800,900,1000,1100), labels=c("-100-0","0-100", "100-200", "200-300","300-400","400-500","500-600","600-700","700-800","800-900","900-1000","1000+"), include.lowest = TRUE)
df$Y_ZoneF <- cut(df$Y.sens, breaks=c(0, 100, 200,300,400,500,600,700), labels=c("0-100", "100-200", "200-300","300-400","400-500","500-600","600-700"), include.lowest = TRUE)

# Calculer la moyenne de XPD pour chaque groupe d'intervalles
avg_XPD <- aggregate(df$XPtsDiff, by=list(X_ZoneF=df$X_ZoneF, Y_ZoneF=df$Y_ZoneF), FUN=mean, na.rm = TRUE)


count_XPD <- aggregate(df$XPtsDiff, by=list(X_ZoneF=df$X_ZoneF, Y_ZoneF=df$Y_ZoneF), FUN=length)
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
matrix_result <- matrix(nrow = 7, ncol = 12)

# Définir les niveaux pour X_ZoneF et Y_ZoneF
levels_X_ZoneF <- c("-100-0","0-100", "100-200", "200-300","300-400","400-500","500-600","600-700","700-800","800-900","900-1000","1000+")
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
cy <- seq(0, 1, length.out = 12)

cx <- c(0,0.16,0.33,0.5,0.66,0.83,1)

image(matrix_result, col = my_colors(11), axes=FALSE, main=paste("XPDseq fin JAP box filtre gaussien"))

for (i in 1:7) {
  for (j in 1:12) {
    x <- cx[i]
    y <- cy[j]
    value <- matrix_result[i, j]
    if (is.na(value)) {
      col <- "black"  # Colorier en noir si la valeur est NA
      label <- "-"  # Afficher "NA" à la place de la valeur moyenne
    } else {
      col <- "white"
      label <- round(value, 3)
    }
    font <- 2
    text(x = x, y = y, labels = label, col = col, font = font)
  }
}






