# RUN fetch.R puis jap_22.R puis HeatMap.R


# PARTIE CREATION HEAT MAP


library(ggplot2)

event_sub <- event_sub %>% mutate(diff_XPD = XPDseq - XPtsDiff)



# intervalles des coordonées X et Y des zones de début du JAP ("0-70", "70-150", "150-220")
event_sub$X_ZoneD <- cut(event_sub$X.sens, breaks=c(-86, 70, 150, 220,300, 400), labels=c("0-70", "70-150", "150-220","220-300","300-400"), include.lowest = TRUE)
event_sub$Y_ZoneD <- cut(event_sub$Y.sens, breaks=c(0, 150,250,350,450, 550, 700), labels=c("0-150", "150-250","250-350","350-450","450-550", "550-700"), include.lowest = TRUE)


event_sub$X_ZoneD <- as.factor(event_sub$X_ZoneD)
event_sub$Y_ZoneD <- as.factor(event_sub$Y_ZoneD)

## coordonnées zones de fin JAP sous forme intervalle : (10 x 7 zones )
event_sub$X_ZoneF <- cut(event_sub$X.end, breaks=c(-86, 100, 200,300,400,500,600,700,800,900,1000,1100), labels=c("-100", "100-200", "200-300","300-400","400-500","500-600","600-700","700-800","800-900","900-1000","1000+"), include.lowest = TRUE)
event_sub$Y_ZoneF <- cut(event_sub$Y.end, breaks=c(0, 100, 200,300,400,500,600,700), labels=c("0-100", "100-200", "200-300","300-400","400-500","500-600","600-700"), include.lowest = TRUE)


event_sub$X_ZoneF <- as.factor(event_sub$X_ZoneF)
event_sub$Y_ZoneF <- as.factor(event_sub$Y_ZoneF)

# # on applique ca sur 2eme df
# jap_compet$X_ZoneD <- cut(jap_compet$X.sens, breaks=c(-86, 100, 220, 400), labels=c("0-100", "100-220","220-400"), include.lowest = TRUE)
# jap_compet$Y_ZoneD <- cut(jap_compet$Y.sens, breaks=c(0, 150, 550, 700), labels=c("0-150", "150-550", "550-700"), include.lowest = TRUE)
#
#
# jap_compet$X_ZoneD <- as.factor(jap_compet$X_ZoneD)
# jap_compet$Y_ZoneD <- as.factor(jap_compet$Y_ZoneD)



# dataframe des JAP TERRITORIAL
terr_df <- event_sub[event_sub$Type == "Territorial",]

# dataframe des JAP sous Press
# pressure_df <-jap_compet[jap_compet$Type == "Pressure",]

# dataframe de tous JAP de Press
box_df <-event_sub[event_sub$Type == "Box",]

bomb_df <-event_sub[event_sub$Type == "Bomb",]
chip_df <-event_sub[event_sub$Type == "Chip",]

crosspitch_df <-event_sub[event_sub$Type == "Cross Pitch",]
low_df <-event_sub[event_sub$Type == "Low",]

# dataframe des Touch Kick
touchkick_df <- event_sub[event_sub$Type == "Touch Kick",]


jap_compet_tsjap <- event_sub


create_heatmap <- function(typejap) {
  tjap <- NA
  if(identical(typejap, touchkick_df)){
    tjap <- "Touch Kick"
  # }else if(identical(typejap, pressure_df)){
  #   tjap <- "Pressure"
  }else if(identical(typejap, low_df)){
    tjap <- "Low"
  }else if(identical(typejap, bomb_df)){
    tjap <- "Bomb"
  }else if(identical(typejap, crosspitch_df)){
    tjap <- "Cross Pitch"
  }else if(identical(typejap, chip_df)){
    tjap <- "Chip"
  }else if(identical(typejap, box_df)){
    tjap <- "Box"
  }else{
    tjap <- "Territorial"
  }
  zone_df <- aggregate(typejap[["XPDseq"]] ~ X_ZoneD + Y_ZoneD, typejap, function(x) mean(na.omit(x)))
  colnames(zone_df)[3] <- "value"

  # calculate number of data in each zone
  count_df <- aggregate(typejap[["XPDseq"]] ~ X_ZoneD + Y_ZoneD, typejap, function(x) length(na.omit(x)))
  colnames(count_df)[3] <- "count"

  # combine zone_df and count_df
  zone_df <- merge(zone_df, count_df, by = c("X_ZoneD", "Y_ZoneD"))

  p <- ggplot(zone_df, aes(x = Y_ZoneD, y = X_ZoneD, fill = value)) +
    geom_tile() +
    geom_text(aes(label = paste(round(value, 4), " \n(", count, ")"), color = "red")) +
    scale_fill_gradient(low = "white", high = "black") +
    theme_minimal() + theme(legend.position = "none") +
    labs(x = "Y position", y = "X position", fill = paste("Avg", "XPDseq"),
         title = paste("Avg", "XPD (3TJ)", "pour Début JAP", tjap))

  return(p)
}


p <- create_heatmap(terr_df)
print(p)
# p <- create_heatmap(pressure_df)
# print(p)
#
# p <- create_heatmap(touchkick_df)
# print(p)
# p <- create_heatmap(low_df)
# print(p)
# p <- create_heatmap(crosspitch_df)
# print(p)
# p <- create_heatmap(chip_df)
# print(p)
# p <- create_heatmap(bomb_df)
# print(p)
# p <- create_heatmap(box_df)
# print(p)
#


####### ZONES D'ARRIVEES JAP ######





# jap_compet$X_ZoneF <- cut(jap_compet$X.end, breaks=c(-86, 100, 200,300,400,500,600,700,800,900,1100), labels=c("-100", "100-200", "200-300","300-400","400-500","500-600","600-700","700-800","900-1000","1000+"), include.lowest = TRUE)
# jap_compet$Y_ZoneF <- cut(jap_compet$Y.end, breaks=c(0, 100, 200,300,400,500,600,700), labels=c("0-100", "100-200", "200-300","300-400","400-500","500-600","600-700"), include.lowest = TRUE)
#
#
# jap_compet$X_ZoneF <- as.factor(jap_compet$X_ZoneF)
# jap_compet$Y_ZoneF <- as.factor(jap_compet$Y_ZoneF)



# dataframe des JAP TERRITORIAL
terr_df <- jap_compet_tsjap[jap_compet_tsjap$Type == "Territorial",]

# dataframe des JAP sous Press
# pressure_df <-jap_compet[jap_compet$Type == "Pressure",]

# dataframe de tous JAP de Press
box_df <-jap_compet_tsjap[jap_compet_tsjap$Type == "Box",]

bomb_df <-jap_compet_tsjap[jap_compet_tsjap$Type == "Bomb",]
chip_df <-jap_compet_tsjap[jap_compet_tsjap$Type == "Chip",]

crosspitch_df <-jap_compet_tsjap[jap_compet_tsjap$Type == "Cross Pitch",]
low_df <-jap_compet_tsjap[jap_compet_tsjap$Type == "Low",]

# dataframe des Touch Kick
touchkick_df <- jap_compet_tsjap[jap_compet_tsjap$Type == "Touch Kick",]



create_heatmap <- function(typejap) {
  tjap <- NA
  if(identical(typejap, touchkick_df)){
    tjap <- "Touch Kick"
  }else if(identical(typejap, low_df)){
    tjap <- "Low"
  # }else if(identical(typejap, pressure_df)){
  #   tjap <- "Pressure"
  }else if(identical(typejap, bomb_df)){
    tjap <- "Bomb"
  }else if(identical(typejap, crosspitch_df)){
    tjap <- "Cross Pitch"
  }else if(identical(typejap, chip_df)){
    tjap <- "Chip"
  }else if(identical(typejap, box_df)){
    tjap <- "Box"
  }else{
    tjap <- "Territorial"
  }
  zone_df <- aggregate(typejap[["XPDseq"]] ~ X_ZoneF + Y_ZoneF, typejap, function(x) mean(na.omit(x)))
  colnames(zone_df)[3] <- "value"

  # calculate number of data in each zone
  count_df <- aggregate(typejap[["XPDseq"]] ~ X_ZoneF + Y_ZoneF, typejap, function(x) length(na.omit(x)))
  colnames(count_df)[3] <- "count"

  # combine zone_df and count_df
  zone_df <- merge(zone_df, count_df, by = c("X_ZoneF", "Y_ZoneF"))

  p <- ggplot(zone_df, aes(x = Y_ZoneF, y = X_ZoneF, fill = value)) +
    geom_tile() +
    geom_text(aes(label = paste(round(value, 2), " \n(", count, ")"), color = "red")) +
    scale_fill_gradient(low = "white", high = "black") +
    theme_minimal() + theme(legend.position = "none") +
    labs(x = "Y position", y = "X position", fill = paste("Avg", "XPDseq"),
         title = paste("Avg", "XPD (3TJ)", "pour Fin JAP", tjap))

  return(p)
}


# p <- create_heatmap(terr_df)
# print(p)
# # p <- create_heatmap(pressure_df)
# # print(p)
#
# p <- create_heatmap(touchkick_df)
# print(p)
# p <- create_heatmap(low_df)
# print(p)
# p <- create_heatmap(crosspitch_df)
# print(p)
# p <- create_heatmap(chip_df)
# print(p)
# p <- create_heatmap(bomb_df)
# print(p)
# p <- create_heatmap(box_df)
# print(p)






b <- "diff_XPD"





### Heat MAP diff_XPD début JAP

create_heatmap <- function(typejap) {
  tjap <- NA
  if(identical(typejap, touchkick_df)){
    tjap <- "Touch Kick"
  }else if(identical(typejap, low_df)){
    tjap <- "Low"
  # }else if(identical(typejap, pressure_df)){
  #   tjap <- "Pressure"
  }else if(identical(typejap, bomb_df)){
    tjap <- "Bomb"
  }else if(identical(typejap, crosspitch_df)){
    tjap <- "Cross Pitch"
  }else if(identical(typejap, chip_df)){
    tjap <- "Chip"
  }else if(identical(typejap, box_df)){
    tjap <- "Box"
  }else{
    tjap <- "Territorial"
  }

  zone_df <- aggregate(typejap[["diff_XPD"]] ~ X_ZoneD + Y_ZoneD, typejap, function(x) mean(na.omit(x)))
  colnames(zone_df)[3] <- "value"

  # calculate number of data in each zone
  count_df <- aggregate(typejap[["diff_XPD"]] ~ X_ZoneD + Y_ZoneD, typejap, function(x) length(na.omit(x)))
  colnames(count_df)[3] <- "count"

  # combine zone_df and count_df
  zone_df <- merge(zone_df, count_df, by = c("X_ZoneD", "Y_ZoneD"))

  p <- ggplot(zone_df, aes(x = Y_ZoneD, y = X_ZoneD, fill = value)) +
    geom_tile() +
    geom_text(aes(label = paste(round(value, 2), " \n(", count, ")"), color = "red")) +
    scale_fill_gradient(low = "white", high = "black") +
    theme_minimal() + theme(legend.position = "none") +
    labs(x = "Y position", y = "X position", fill = paste("Avg", "diff_XPD"),
         title = paste("Avg", "diff_XPD (3TJ)", "pour Début JAP", tjap))

  return(p)
}

# p <- create_heatmap(terr_df)
# print(p)
# # p <- create_heatmap(pressure_df)
# # print(p)
#
# p <- create_heatmap(touchkick_df)
# print(p)
# p <- create_heatmap(low_df)
# print(p)
# p <- create_heatmap(crosspitch_df)
# print(p)
# p <- create_heatmap(chip_df)
# print(p)
# p <- create_heatmap(bomb_df)
# print(p)
# p <- create_heatmap(box_df)
# print(p)


### Heat MAP diff_XPD fin JAP

create_heatmap <- function(typejap) {
  tjap <- NA
  if(identical(typejap, touchkick_df)){
    tjap <- "Touch Kick"
  }else if(identical(typejap, low_df)){
    tjap <- "Low"
  # }else if(identical(typejap, pressure_df)){
  #   tjap <- "Pressure"
  }else if(identical(typejap, bomb_df)){
    tjap <- "Bomb"
  }else if(identical(typejap, crosspitch_df)){
    tjap <- "Cross Pitch"
  }else if(identical(typejap, chip_df)){
    tjap <- "Chip"
  }else if(identical(typejap, box_df)){
    tjap <- "Box"
  }else{
    tjap <- "Territorial"
  }

  zone_df <- aggregate(typejap[["diff_XPD"]] ~ X_ZoneF + Y_ZoneF, typejap, function(x) mean(na.omit(x)))
  colnames(zone_df)[3] <- "value"

  # calculate number of data in each zone
  count_df <- aggregate(typejap[["diff_XPD"]] ~ X_ZoneF + Y_ZoneF, typejap, function(x) length(na.omit(x)))
  colnames(count_df)[3] <- "count"

  # combine zone_df and count_df
  zone_df <- merge(zone_df, count_df, by = c("X_ZoneF", "Y_ZoneF"))

  p <- ggplot(zone_df, aes(x = Y_ZoneF, y = X_ZoneF, fill = value)) +
    geom_tile() +
    geom_text(aes(label = paste(round(value, 2), " \n(", count, ")"), color = "red")) +
    scale_fill_gradient(low = "white", high = "black") +
    theme_minimal() + theme(legend.position = "none") +
    labs(x = "Y position", y = "X position", fill = paste("Avg", "diff_XPD"),
         title = paste("Avg", "diff_XPD (3TJ)", "pour Fin JAP", tjap))

  return(p)
}

p <- create_heatmap(terr_df)
print(p)
# p <- create_heatmap(pressure_df)
# print(p)

# p <- create_heatmap(touchkick_df)
# print(p)
# p <- create_heatmap(low_df)
# print(p)
# p <- create_heatmap(crosspitch_df)
# print(p)
# p <- create_heatmap(chip_df)
# print(p)
# p <- create_heatmap(bomb_df)
# print(p)
# p <- create_heatmap(box_df)
# print(p)

