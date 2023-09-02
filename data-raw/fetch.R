# RUN fetch.R puis jap_22.R puis HeatMap.R

library(readr)
library(readxl)
library(dplyr)

path = unlist(strsplit(getwd(), "/"))[1]
username = unlist(strsplit(unlist(strsplit(getwd(), "Users/"))[2], "/"))[[1]]
mac_ou_pc = ifelse(substr(getwd(),1,1)!="/", "PC", "mac")

if (mac_ou_pc=="PC"){
  chemin = paste0(path, "/Users/", username, "/ffr.fr/Accompagnement Perf - Dashboard - Documents/General/Data/")
  event <- read_excel(file.path(chemin, "Event.xlsx"), na = "NA")
}else if(username == "thomassinapi"){
  chemin = "~/Desktop/TStats/FFR/data/"
  event <- read_delim(file.path(chemin,"event_update_XV.csv"),delim = ";", escape_double = FALSE, trim_ws = TRUE)
}


event <- event %>% filter(substr(GameId, 1,8) %in% c("20238100", "20233010", "20221910") | GameId %in% c("20232918010","20232932110","20232412910","20231811810"))

event <- event %>% mutate(Type = if_else(Type == "Territorial", "Occupation", Type))
event <- event %>% mutate(Type = if_else(Type == "Box", "Chapeau", Type))
event <- event %>% mutate(Type = if_else(Type == "Cross Pitch", "Fleche", Type))
event <- event %>% mutate(Type = if_else(Type == "Low", "Chasse", Type))
event <- event %>% mutate(Type = if_else(Type == "Bomb", "Wilco", Type))



circuit <- read_excel(file.path(chemin, "Db Circuit.xlsx"), na = "NA")

joueur <- read_excel(file.path(chemin, "Db STATS.xlsx"),sheet = 1, na = "NA")


match<- read_excel(file.path(chemin, "Db STATS.xlsx"),sheet = 2, na = "NA")

equipe<- read_excel(file.path(chemin, "Db STATS.xlsx"),sheet = 4, na = "NA")

equipe <- equipe %>% mutate(name = if_else(name == "La Rochelle", "Stade Rochelais", name))

#indiv<- read_excel("C:/Users/abelb/Documents/ENSAI/2A/Stage/Db_STATS.xlsx", sheet = 5, na = "NA")

event <- merge(event, joueur[, c("PlayerId", "Joueur")], by = "PlayerId", all.x = TRUE)

# Jointure avec le dataframe "equipe" pour ajouter la colonne "name" à "event"
event <- merge(event, equipe[, c("TeamId", "name")], by = "TeamId", all.x = TRUE)

# Jointure avec le dataframe "match" pour ajouter les colonnes "compétition", "date", "ScoreHome" et "ScoreAway" à "event"
event <- merge(event, match[, c("GameId", "Competition", "ScoreHome", "ScoreAway", "TeamHome", "TeamAway", "TeamidHome","TeamidAway")], by = "GameId", all.x = TRUE)

event$TeamId_adv <- ifelse(event$TeamidAway == event$TeamId, event$TeamidHome, event$TeamidAway)

event$name2 <- ifelse(event$TeamAway == event$name, event$TeamHome, event$TeamAway)


event$HomeAway <- ifelse(event$name == event$TeamHome, 1, 0)

event <- subset(event, select = -c(TeamHome, TeamAway,TeamidHome, TeamidAway))


event <- event %>%
  mutate(season = substr(GameId, 1, 4))

# for (i in c(5:8,11:14,17,19,23,24,29,31)) {
#   event[,i] <- as.numeric(event[,i])
# }
event$GameId <- as.character(event$GameId)
event <- event %>% arrange(GameId, TimeVideo) %>% group_by(GameId)


# Indentifiez les indices où GameId est égal à 20232412910
indices <- which(event$GameId == 20232412910)

# Remplacez  par "Top 14" pour ces indices
event$Competition[indices] <- "TOP 14"


event$TimeVidRecep <- NA
for (i in which (event$StatName == "Jeu au pied")) {
  #Récupération des temps vidéos de ce qui se passe juste après le coup de pied
  if(event$StatName[i+1] != "Touche"){
    event$TimeVidRecep[i] <- event$TimeVideo[i+1]
  }
}

event <- event %>% filter(StatName != "Reception" )

event$Type <- ifelse(event$Type == "Box" & event$Result %in% c("Kick in Touch (Full)", "Kick in Touch (Bounce)"), "Touch Kick", event$Type) #Box kicks qui vont en touche deviennent des touch kicks




############Ajout de turnovers gagnés après turnovers concédés##########
stockteamid <- NA
stockXPF <- NA
concede <- event %>%  filter(StatName == "Turnover concedee")
for (i in which(concede$StatName == "Turnover concedee")) {
  concede$StatName[i] <- "Turnover gagne"
  concede$X.sens[i] <- 1000 - concede$X.sens[i]
  concede$Y.sens[i] <- 700 - concede$Y.sens[i]
  stockteamid[i] <- concede$TeamId[i]
  concede$TeamId[i] <- concede$TeamId_adv[i]
  concede$TeamId_adv[i] <- stockteamid[i]
  stockXPF[i] <- concede$xPtsFor[i]
  concede$xPtsFor[i] <- concede$xPtsAgainst[i]
  concede$xPtsAgainst[i] <- stockXPF[i]
  #Gestion des XPtsFor et Against à faire si on change pour les turnovers concedés
}
event <- event %>%  rbind(concede) %>% arrange(GameId, TimeVideo) %>% group_by(GameId)
event$XPtsDiff <- event$xPtsFor - event$xPtsAgainst




circuit$Cellule <- ifelse(circuit$Cellule == "1", "+1", circuit$Cellule)
circuit$Cellule2 <- ifelse(circuit$Cellule2 == "1", "+1", circuit$Cellule2)
circuit$Cellule3 <- ifelse(circuit$Cellule3 == "1", "+1", circuit$Cellule3)
circuit$Cellule4 <- ifelse(circuit$Cellule4 == "1", "+1", circuit$Cellule4)


circuit$Circuit <- ifelse(is.na(circuit$Cellule4),
                          ifelse(is.na(circuit$Cellule3),
                                 ifelse(is.na(circuit$Cellule2), circuit$Cellule, paste(circuit$Cellule,circuit$Cellule2,sep = "/") ),
                                 paste(circuit$Cellule,circuit$Cellule2,circuit$Cellule3,sep = "/")),
                          paste(circuit$Cellule,circuit$Cellule2,circuit$Cellule3,circuit$Cellule4,sep = "/"))

event <- rename(event, CircuitAuto = Circuit)
#Création de TimeVideo2 pour avoir les temps équivalents entre les deux circuit et event
circuit$TimeVideo2<-NA
event$TimeVideo2<-NA
for (g in unique(circuit$GameId)){
  # On fait débuter à 0 le Timevideo de event
  event$TimeVideo2[which(event$GameId==g)]<-event$TimeVideo[which(event$GameId==g)]-event$TimeVideo[which(event$GameId==g)[1]]+event$GameSeconds[which(event$GameId==g)[1]]
  # On met sur le même temps celui de circuit
  circuit$TimeVideo2[which(circuit$GameId==g)]<-circuit$TimeVideo[which(circuit$GameId==g)]-circuit$TimeVideo[which(circuit$GameId==g)[1]]+event$TimeVideo2[rev(which(event$GameId==g & event$GameSeconds<=circuit$GameSeconds[which(circuit$GameId==g)][1]))[1]]
}


event$Circuit<-NA
event$idCircuit<-NA
event$Cellule<-NA
circuit$Join<-0
for(i in 1:nrow(circuit)){
  # Si on a une reception qui tombe à la même seconde du meme match avec le meme X alors on va le joindre au ruck précédent
  t<-which(event$GameId==circuit$GameId[i] & event$StatName%in%c("Reception") & event$GameSeconds==circuit$GameSeconds[i] & event$PlayerId==circuit$PlayerId[i] & event$X.sens==circuit$nX[i])
  if(length(t)>0){
    r<-ifelse(event$StatName[t-1]%in%c("Maul","Ruck"),t-1,t)
    event$Cellule[r]<-circuit$Cellule[i]
    event$Circuit[r]<-circuit$Circuit[i]
    event$idCircuit[r]<-circuit$indexCircuitCellule[i]
    circuit$Join[i]<-1
  }else{
    # Si on a une reception qui tombe dans les 30 secondes du meme match avec le meme X alors on va le joindre au ruck précédent
    t<-which(event$GameId==circuit$GameId[i] & event$StatName%in%c("Reception") & abs(event$GameSeconds-circuit$GameSeconds[i])<30 & event$PlayerId==circuit$PlayerId[i] & event$X.sens==circuit$nX[i])
    if(length(t)>0){
      r<-ifelse(event$StatName[t-1]%in%c("Maul","Ruck"),t-1,t)
      event$Cellule[r]<-circuit$Cellule[i]
      event$Circuit[r]<-circuit$Circuit[i]
      event$idCircuit[r]<-circuit$indexCircuitCellule[i]
      circuit$Join[i]<-1
    }else{
      # Si on a toujours pas fait le lien on l'attache au ruck le plus proche
      Temps<-which.min(abs(event$TimeVideo2[event$GameId==circuit$GameId[i] & event$StatName%in%c("Ruck","Maul") & event$GameSeconds<=circuit$GameSeconds[i]  & is.na(event$Cellule)]-circuit$TimeVideo2[i]))
      t<-which(event$GameId==circuit$GameId[i]& event$StatName%in%c("Ruck","Maul") & event$GameSeconds<=circuit$GameSeconds[i] & is.na(event$Cellule))[Temps]
      event$Cellule[t]<-circuit$Cellule[i]
      event$Circuit[t]<-circuit$Circuit[i]
      event$idCircuit[t]<-circuit$indexCircuitCellule[i]
    }
  }
}

# Prend CircuitAuto si tous les Circuit d'un match sont NA
for(i in unique(event$GameId)){
  if(all(is.na(event$Circuit[which(event$GameId == i)]))){
    event$Circuit[event$GameId == i] <- event$CircuitAuto[event$GameId == i]
  }else{
    event$Circuit[event$GameId == i] <- event$Circuit[event$GameId == i]
  }
}
event <- event %>% arrange(GameId, TimeVideo) %>% group_by(GameId)

event$ArretPen<-NA
for(i in which(event$StatName%in%c("Faute off","Faute def"))){
  event$ArretPen[i]<-ifelse(event$TimeVideo[i+1]>event$TimeVideo[i]+10 & event$StatName[i+1]!="Penalite",1,0)
}

# IdPossession
event$IdPossession<-0
for(i in 2:nrow(event)){
  event$IdPossession[i]<-ifelse((event$TeamId[i]!=event$TeamId[i-1] & !event$StatName[i] %in% c("Faute off","Faute def") & !event$StatName[i-1] %in% c("Faute def")) || (event$StatName[i-1] %in% c("Faute off") & !event$StatName[i] %in% c("Penalite")) || event$StatName[i]%in%c("Coup d'envoi") || (event$StatName[i]=="Faute def" & event$TeamId[i]==event$TeamId[i-1] & event$StatName[i-1]!="Faute def"), event$IdPossession[i-1]+1, event$IdPossession[i-1])
}

event$ExclAvantage<-0
for(i in which(event$StatName=="Penalite" | (event$StatName=="Touche" & event$Origine=="Pénalité"))){
  Fautedef<-rev(which(event$IdPossession==event$IdPossession[i] & event$TimeVideo<event$TimeVideo[i] & event$TeamId!=event$TeamId[i] & event$StatName%in%"Faute def" & event$ArretPen==0))[1]
  Fauteoff<-rev(which(event$IdPossession==event$IdPossession[i]-1 & event$TimeVideo<event$TimeVideo[i] & event$TeamId!=event$TeamId[i] & event$StatName%in%"Faute off" & event$ArretPen==0))[1]
  LastStop <- rev(which(event$IdPossession==event$IdPossession[i] & event$TimeVideo<event$TimeVideo[i] & (event$StatName%in%c("Touche","Melee","Coup d'envoi") | event$ArretPen==1)))[1]
  if(!is.na(Fautedef) & (is.na(LastStop)|LastStop<Fautedef)) {
    event$ExclAvantage[which(event$IdPossession==event$IdPossession[i] & event$TimeVideo<event$TimeVideo[i] & event$TimeVideo>event$TimeVideo[Fautedef])]<-1
  }else{
    if(!is.na(Fauteoff) & (is.na(LastStop)|LastStop<Fauteoff)){
      event$ExclAvantage[which(event$IdPossession%in%c(event$IdPossession[i]-1,event$IdPossession[i]) & event$TimeVideo<event$TimeVideo[i] & event$TimeVideo>event$TimeVideo[Fauteoff])]<-1
    }
  }
}
for(i in which(event$StatName=="Melee")){
  Fautedef<-rev(which(event$IdPossession==event$IdPossession[i] & event$TimeVideo<event$TimeVideo[i] & event$TeamId!=event$TeamId[i] & event$StatName%in%"Faute def" & event$ArretPen==0))[1]
  Fauteoff<-rev(which(event$IdPossession==event$IdPossession[i]-1 & event$TimeVideo<event$TimeVideo[i] & event$TeamId!=event$TeamId[i] & event$StatName%in%"Faute off" & event$ArretPen==0))[1]
  Tv<-rev(which(event$IdPossession==event$IdPossession[i]-1 & event$TimeVideo<event$TimeVideo[i] & event$TeamId!=event$TeamId[i] & event$StatName%in%"Faute off" & event$ArretPen==0))[1]
  LastStop <- rev(which(event$IdPossession==event$IdPossession[i] & event$TimeVideo<event$TimeVideo[i] & (event$StatName%in%c("Touche","Melee","Coup d'envoi") | event$ArretPen==1)))[1]
  if(!is.na(Tv) & (is.na(LastStop)|LastStop<Tv)){
    event$ExclAvantage[which(event$IdPossession==event$IdPossession[i]-1 & event$TimeVideo<event$TimeVideo[i] & event$TimeVideo>event$TimeVideo[Tv])]<-2
  }else{
    if(!is.na(Fautedef) & (is.na(LastStop)|LastStop<Fautedef)) {
      event$ExclAvantage[which(event$IdPossession==event$IdPossession[i] & event$TimeVideo<event$TimeVideo[i] & event$TimeVideo>event$TimeVideo[Fautedef])]<-2
    }else{
      if(!is.na(Fauteoff) & (is.na(LastStop)|LastStop<Fauteoff)){
        event$ExclAvantage[which(event$IdPossession%in%c(event$IdPossession[i]-1,event$IdPossession[i]) & event$TimeVideo<event$TimeVideo[i] & event$TimeVideo>event$TimeVideo[Fauteoff])]<-2
      }
    }
  }
}




event$Choix <- NA
for (i in which(event$StatName %in% c("Ruck", "Maul"))) {
  tj <- which(event$GameId == event$GameId[i] & event$TimeVideo[i] < event$TimeVideo & event$StatName != "Passe")[1]
  if (!is.na(event$Cellule[i])) {
    event$Choix[i] <- "Cellule"
    event$Type[i] <- event$Cellule[i]
  }
  else if (event$StatName[tj] == "Jeu au pied" & !is.na(event$StatName[tj]))
  {
    event$Choix[i] <- event$StatName[tj]
    event$Type[i] <- event$Type[tj]
  }
}

event$OrigineJAP <- NA
event$OrigineRuck <- NA

for (i in which (event$StatName %in% c("Jeu au pied","Melee","Touche", "Ruck","Turnover gagne","Turnover concedee","Essai","Faute def", "Penalite"))) {

  Origine<-rev(which(event$StatName %in% c("Coup d'envoi","Melee","Touche","Turnover gagne","Ruck") & event$GameId==event$GameId[i] & event$TimeVideo<event$TimeVideo[i]))[1]
  orig_large <- rev(which(event$StatName %in% c("Coup d'envoi","Melee","Touche","Turnover gagne","Jeu au pied") & event$GameId==event$GameId[i] & event$TimeVideo<event$TimeVideo[i]))[1]

  origineRuck<-rev(which(event$StatName %in% c("Jeu au pied","Coup d'envoi","Melee","Touche","Turnover gagne", "Ruck") & event$GameId==event$GameId[i] & event$TimeVideo<event$TimeVideo[i]))[1]

  if(event$StatName[i] == "Jeu au pied"){
    if(event$TeamId[i] != event$TeamId[Origine]){
      event$xPtsFor[i]<- as.numeric(event$xPtsAgainst[Origine])

      event$xPtsAgainst[i]<- as.numeric(event$xPtsFor[Origine])

      event$XPtsDiff[i]<- -as.numeric(event$XPtsDiff[Origine])
    }else{
      event$xPtsFor[i]<- as.numeric(event$xPtsFor[Origine])

      event$xPtsAgainst[i]<- as.numeric(event$xPtsAgainst[Origine])

      event$XPtsDiff[i]<- as.numeric(event$XPtsDiff[Origine])
    }
  }
  ifelse(event$TeamId[i] != event$TeamId[orig_large],
         event$OrigineJAP[i]<- paste0(event$StatName[orig_large], " adv"),
         event$OrigineJAP[i]<- event$StatName[orig_large])
  ifelse(event$TeamId[i] != event$TeamId[origineRuck],
         event$OrigineRuck[i]<- paste0(event$StatName[origineRuck], " adv"),
         event$OrigineRuck[i]<- event$StatName[origineRuck])
}





event_red <- event %>% filter(StatName != "Passe" & StatName != "Offload" & StatName != "Reception")





usethis::use_data(event, overwrite = TRUE)


















#
# #Création des colonnes utiles
# event_red_xpts$Time1TJ <- NA
# event_red_xpts$Time3tj <- NA
# event_red_xpts$TimeVideo3Max <- NA
# event_red_xpts$TimeFinSeq <- NA
# event_red_xpts$TeamId_First_Event <-NA
# event_red_xpts$StatName_First_Event <-NA
# event_red_xpts$Xsens_First_Event <-NA
# event_red_xpts$Ysens_First_Event <-NA
# event_red_xpts$XPF_First_Event <-NA
# event_red_xpts$XPA_First_Event <-NA
# event_red_xpts$StatName_Short_Seq <-NA
# event_red_xpts$TeamId_Short_Seq <-NA
# event_red_xpts$Xsens_Short_Seq <-NA
# event_red_xpts$Ysens_Short_Seq <-NA
# event_red_xpts$XPF_Short_Seq <-NA
# event_red_xpts$XPA_Short_Seq <-NA
# event_red_xpts$StatName_End_Seq <-NA
# event_red_xpts$TeamId_End_Seq <-NA
# event_red_xpts$Xsens_End_Seq <-NA
# event_red_xpts$Ysens_End_Seq <-NA
# event_red_xpts$XPF_End_Seq <-NA
# event_red_xpts$XPA_End_Seq <-NA
#
#
# for (i in which (event_red_xpts$StatName == "Jeu au pied")) {
#   #Récupération des temps vidéos des différents événements de la séquence
#   event_red_xpts$TimeFinSeq[i] <- event_red_xpts$TimeVideo[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo[i] < event_red_xpts$TimeVideo & event_red_xpts$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def"))[1]]
#   event_red_xpts$Time3tj[i] <- event_red_xpts$TimeVideo[which(event_red_xpts$GameId == event_red_xpts$GameId[i] & event_red_xpts$TimeVideo > event_red_xpts$TimeVideo[i] & event_red_xpts$StatName %in% c("Ruck"))[3]]
#   event_red_xpts$TimeVideo3Max[i] <- ifelse(event_red_xpts$TimeFinSeq[i] < event_red_xpts$Time3tj[i],event_red_xpts$TimeFinSeq[i], event_red_xpts$Time3tj[i])
#   event_red_xpts$Time1TJ[i] <- event_red_xpts$TimeVideo[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo[i] < event_red_xpts$TimeVideo & event_red_xpts$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def", "Ruck"))[1]]
#
#   #Affectation des caractéristiques des différents événements pointés par les temps vidéos
#   event_red_xpts$TeamId_Short_Seq[i] <- event_red_xpts$TeamId[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$TimeVideo3Max[i])][1]
#   event_red_xpts$StatName_Short_Seq[i] <- event_red_xpts$StatName[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$TimeVideo3Max[i])][1]
#   event_red_xpts$XPF_Short_Seq[i] <- event_red_xpts$xPtsFor[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$TimeVideo3Max[i])][1]
#   event_red_xpts$XPA_Short_Seq[i] <- event_red_xpts$xPtsAgainst[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$TimeVideo3Max[i])][1]
#   event_red_xpts$Xsens_Short_Seq[i] <- event_red_xpts$X.sens[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$TimeVideo3Max[i])][1]
#   event_red_xpts$Ysens_Short_Seq[i] <- event_red_xpts$Y.sens[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$TimeVideo3Max[i])][1]
#
#   event_red_xpts$TeamId_First_Event[i] <- event_red_xpts$TeamId[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$Time1TJ[i])][1]
#   event_red_xpts$StatName_First_Event[i] <- event_red_xpts$StatName[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$Time1TJ[i])][1]
#   event_red_xpts$XPF_First_Event[i] <- event_red_xpts$xPtsFor[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$Time1TJ[i])][1]
#   event_red_xpts$XPA_First_Event[i] <- event_red_xpts$xPtsAgainst[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$Time1TJ[i])][1]
#   event_red_xpts$Xsens_First_Event[i] <- event_red_xpts$X.sens[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$Time1TJ[i])][1]
#   event_red_xpts$Ysens_First_Event[i] <- event_red_xpts$Y.sens[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$Time1TJ[i])][1]
#
#   event_red_xpts$TeamId_End_Seq[i] <- event_red_xpts$TeamId[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$TimeFinSeq[i])][1]
#   event_red_xpts$StatName_End_Seq[i] <- event_red_xpts$StatName[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$TimeFinSeq[i])][1]
#   event_red_xpts$XPF_End_Seq[i] <- event_red_xpts$xPtsFor[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$TimeFinSeq[i])][1]
#   event_red_xpts$XPA_End_Seq[i] <- event_red_xpts$xPtsAgainst[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$TimeFinSeq[i])][1]
#   event_red_xpts$Xsens_End_Seq[i] <- event_red_xpts$X.sens[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$TimeFinSeq[i])][1]
#   event_red_xpts$Ysens_End_Seq[i] <- event_red_xpts$Y.sens[which(event_red_xpts$GameId[i] == event_red_xpts$GameId & event_red_xpts$TimeVideo == event_red_xpts$TimeFinSeq[i])][1]
# }
#
# event_red_xpts$a_supp <- NA
# for (i in rev(which(event_red_xpts$StatName == "Jeu au pied"))) {
#   if (!is.na(event_red_xpts$StatName[i+1])) {
#     if (event_red_xpts$StatName[i+1] == "Coup d'envoi" ||
#         (event_red_xpts$StatName[i+1] == "Touche" & event_red_xpts$Origine[i+1] == "Pénalité")) {event_red_xpts$a_supp[i] = 1}
#     else {event_red_xpts$a_supp[i] = 0}
#   }
#   if (!is.na(event_red_xpts$StatName[i+1])) {
#     if(event_red_xpts$StatName[i+1] == "Jeu au pied" & event_red_xpts$a_supp[i+1] == 1) {event_red_xpts$a_supp[i] = 1}
#   }
# }
#
#
# event_red_xpts <- event_red_xpts %>% mutate(XPD = xPtsFor - xPtsAgainst)


# ## Origine
# event_red_xpts$OrigineJAP<-NA
#
# event_red_xpts$OrigineTeam<-NA
#
# event_red_xpts$XPD_prevRuck<-NA
#
# event_red_xpts$Team_prevRuck<-NA
#
# event_red_xpts$StatName_prevRuck <-NA
#
# for (i in which (event_red_xpts$StatName == "Jeu au pied")) {
#
#   Origine<-rev(which(event_red_xpts$StatName %in% c("Jeu au pied","Coup d'envoi","Goal Line Restart","Melee","Touche","Turnover concedee") & event_red_xpts$GameId==event_red_xpts$GameId[i] & event_red_xpts$TimeVideo<event_red_xpts$TimeVideo[i]))[1]
#
#   event_red_xpts$OrigineJAP[i]<-event_red_xpts$StatName[Origine]
#
#   event_red_xpts$OrigineTeam[i]<-event_red_xpts$TeamId[Origine]
#
#   event_red_xpts$OrigineJAP[i]<-ifelse(event_red_xpts$TeamId[i]==event_red_xpts$OrigineTeam[i],event_red_xpts$OrigineJAP[i],paste(event_red_xpts$OrigineJAP[i],"adv"))
#

#
#
# }











# Ruck avant JAP



# for (i in which (event_red_xpts$StatName == "Jeu au pied")) {
#
#   OrigineRuck<-rev(which(event_red_xpts$StatName %in% c("Jeu au pied","Coup d'envoi","Goal Line Restart","Melee","Touche","Turnover concedee", "Ruck") & event_red_xpts$GameId==event_red_xpts$GameId[i] & event_red_xpts$TimeVideo<event_red_xpts$TimeVideo[i]))[1]
#
#   event_red_xpts$XPD_prevRuck[i]<-event_red_xpts$XPD[OrigineRuck]
#
#   event_red_xpts$Team_prevRuck[i]<-event_red_xpts$TeamId[OrigineRuck]
#
#   event_red_xpts$StatName_prevRuck[i]<-event_red_xpts$StatName[OrigineRuck]
#
#   #event_red_xpts$OrigineJAP[i]<-ifelse(event_red_xpts$TeamId[i]==event_red_xpts$OrigineTeam[i],event_red_xpts$OrigineJAP[i],paste(event_red_xpts$OrigineJAP[i],"adv"))
#
# }




#geom_segment(data=JAP[!is.na(JAP$X.end),],aes(x=Y.sens, y=X.sens, xend=Y.end, yend=X.end, colour=TypeJAP2), size = 1.1)+
#geom_point(data=JAP[!is.na(JAP$X.end) & JAP$Rebond==1,],aes(x=Y.end, y=X.end),color="#0f8a40",size=5,shape=19)+
#geom_segment(data=JAP[!is.na(JAP$X.end) & !is.na(JAP$X.prev) & !is.na(JAP$X.sens),],aes(x=Y.prev, y=X.prev, xend=Y.sens, yend=X.sens),color="#b5b5b5", size = 1, linetype="dashed")+
#geom_point(data=JAP[!is.na(JAP$X.end) & !is.na(JAP$X.prev) & !is.na(JAP$X.sens),],aes(x=Y.prev, y=X.prev),fill="#edc628",size=5,shape=23)+
#geom_text(data=JAP[!is.na(JAP$X.end) & !is.na(JAP$Poste),],aes(label=Poste,x=Y.sens, y=X.sens-3),hjust=-0.5, vjust=1,colour="#52514f", fontface = "bold")+
#scale_colour_manual(values = c("Occupation" = "#2f1e87","NA" = "#2f1e87","Pression" = "#41ccfa", "Manque" = "#f5221b","Recuperation" = "#27ab5a"))









par(mar = c(0, 0, 0, 0))  # Supprime les marges

plot(1, type = "n", xlab = "", ylab = "", xlim = c(0,700), ylim = c(-100,1100), xaxt = 'n', yaxt = 'n')

# Draw rectangles
rect(0, -100, 700, 0, col = "#ffe3e3",lwd = 1.5)
rect(0, 0, 700, 220, col = "#ffe3e3",lwd = 1.5)
rect(0, 220, 700, 400, col = "#dfe0f0",border = NA)
rect(0, 400, 700, 780, col = "#f1f0fc",border = NA)
rect(0, 780, 700, 1000, col = "#dfede4",lwd = 1.5)
rect(0, 1000, 700, 1100, col = "#dfede4",lwd = 1.5)
lines(c(0,0), c(0,1000), col = "black", lwd = 1.5)
lines(c(700,700), c(0,1000), col = "black", lwd = 1.5)
lines(c(0,700), c(500,500), col = "black", lwd = 1.5)


# Draw lines

lines(c(0,700), c(220,220), col = "black", lwd = 0.8)
lines(c(0,700), c(780,780), col = "black", lwd = 0.8)

lines(c(0,700), c(400,400), col = "black", lty = 3, lwd = 0.75)
lines(c(0,700), c(600,600), col = "black", lty = 3, lwd = 0.75)
lines(c(0,700), c(950,950), col = "black", lty = 3, lwd = 0.75)
lines(c(0,700), c(50,50), col = "black", lty = 3, lwd = 0.75)

lines(c(50,50), c(0,1000), col = "black", lty = 3, lwd = 0.75)
lines(c(650,650), c(0,1000), col = "black", lty = 3, lwd = 0.75)

lines(c(233,233), c(0,1000), col = "red", lwd = 0.4)
lines(c(466,466), c(0,1000), col = "red", lwd = 0.4)

lines(c(150,150), c(925,975), col = "black", lwd = 0.8)
lines(c(150,150), c(755,805), col = "black", lwd = 0.8)
lines(c(150,150), c(475,525), col = "black", lwd = 0.8)
lines(c(150,150), c(195,245), col = "black", lwd = 0.8)
lines(c(150,150), c(25,75), col = "black", lwd = 0.8)
lines(c(550,550), c(925,975), col = "black", lwd = 0.8)
lines(c(550,550), c(755,805), col = "black", lwd = 0.8)
lines(c(550,550), c(475,525), col = "black", lwd = 0.8)
lines(c(550,550), c(195,245), col = "black", lwd = 0.8)
lines(c(550,550), c(25,75), col = "black", lwd = 0.8)

lines(c(0,100), c(730,730), col = "#7289d4", lwd = 0.4)
lines(c(600,700), c(730,730), col = "#7289d4", lwd = 0.4)
lines(c(600,600), c(730,830), col = "#7289d4", lwd = 0.4)
lines(c(100,100), c(730,830), col = "#7289d4", lwd = 0.4)
lines(c(100,600), c(830,830), col = "#7289d4", lwd = 0.4)



# Draw points
points(c(150,550), c(150,150), pch = 1, cex = 3)

lines(c(330,325), c(1000,1075), col = "black", lwd = 2)
lines(c(370,375), c(1000,1075), col = "black", lwd = 2)
lines(c(330,370), c(1025,1025), col = "black", lwd = 2)
lines(c(330,325), c(0,-75), col = "black", lwd = 2)
lines(c(370,375), c(0,-75), col = "black", lwd = 2)
lines(c(330,370), c(-25,-25), col = "black", lwd = 2)












line_colour <- "#000000"
library(ggplot2)
library(ggpubr)
p<- ggplot() +
  geom_rect(aes(xmin=0, xmax=700, ymin=-100, ymax=0), fill = "#ffe3e3", colour = NA) +
  geom_rect(aes(xmin=0, xmax=700, ymin=0, ymax=220), fill = "#ffe3e3", colour = NA) +
  geom_rect(aes(xmin=0, xmax=700, ymin=220, ymax=400), fill = "#dfe0f0", colour = NA) +
  geom_rect(aes(xmin=0, xmax=700, ymin=400, ymax=780), fill = "#f1f0fc", colour = NA) +
  geom_rect(aes(xmin=0, xmax=700, ymin=780, ymax=1000), fill = "#dfede4", colour = NA) +
  geom_rect(aes(xmin=0, xmax=700, ymin=1000, ymax=1100), fill = "#dfede4", colour = NA) +
  geom_rect(aes(xmin=0, xmax=700, ymin=-100, ymax=1100), fill = NA, colour = line_colour,size=0.8) +
  geom_rect(aes(xmin=0, xmax=700, ymin=0, ymax=1000), fill = NA, colour = line_colour,size=0.8) +
  geom_segment(aes(x = 0, y = 500, xend =700, yend = 500),colour = line_colour,size=0.8) +
  geom_segment(aes(x = 0, y = 220, xend = 700, yend = 220),colour = line_colour,size=0.8)+
  geom_segment(aes(x = 0, y = 780, xend = 700, yend = 780),colour = line_colour,size=0.8)+
  geom_segment(aes(x =0, y = 400, xend = 700, yend = 400),colour = line_colour,linetype="dotted",size=0.75)+
  geom_segment(aes(x = 0, y = 600, xend = 700, yend = 600),colour = line_colour,linetype="dotted",size=0.75)+
  geom_segment(aes(x = 0, y = 50, xend = 700, yend = 50),colour = line_colour,linetype="dotted",size=0.75)+
  geom_segment(aes(x = 0, y = 950, xend = 700, yend = 950),colour = line_colour,linetype="dotted",size=0.75)+
  geom_segment(aes(x = 50, y = 0, xend = 50, yend = 1000),colour = line_colour,linetype="dotted",size=0.75)+
  geom_segment(aes(x = 650, y = 0, xend = 650, yend = 1000),colour = line_colour,linetype="dotted",size=0.75)+
  geom_segment(aes(x = 150, y = 925, xend =150, yend = 975),colour = line_colour,size=0.6) +
  geom_segment(aes(x = 550, y = 925, xend =550, yend = 975),colour = line_colour,size=0.6) +
  geom_segment(aes(x = 150, y = 755, xend =150, yend = 805),colour = line_colour,size=0.6) +
  geom_segment(aes(x = 550, y = 755, xend =550, yend = 805),colour = line_colour,size=0.6) +
  geom_segment(aes(x = 150, y = 475, xend =150, yend = 525),colour = line_colour,size=0.6) +
  geom_segment(aes(x = 550, y = 475, xend =550, yend = 525),colour = line_colour,size=0.6) +
  geom_segment(aes(x = 150, y = 195, xend =150, yend = 245),colour = line_colour,size=0.6) +
  geom_segment(aes(x = 550, y = 195, xend =550, yend = 245),colour = line_colour,size=0.6) +
  geom_segment(aes(x = 150, y = 25, xend =150, yend = 75),colour = line_colour,size=0.6) +
  geom_segment(aes(x = 550, y = 25, xend =550, yend = 75),colour = line_colour,size=0.6) +
  # Bleu
  geom_rect(aes(xmin=100, xmax=600, ymin=100, ymax=900), fill = "NA", colour = "#3fa9eb",size=0.5)+
  geom_rect(aes(xmin=0, xmax=700, ymin=400, ymax=600), fill = "NA", colour = "#7c3896",size=0.5)+
  # Tiers
  geom_segment(aes(x = 233, y = 220, xend = 233, yend = 780),colour = "red",size=0.5)+
  geom_segment(aes(x = 466, y = 220, xend = 466, yend = 780),colour = "red",size=0.5)+
  geom_segment(aes(x = 291, y = 220, xend = 291, yend = 780),colour = "#d9a925",size=0.3)+
  geom_segment(aes(x = 350, y = 220, xend = 350, yend = 780),colour = "#d9a925",size=0.3)+
  geom_segment(aes(x = 408, y = 220, xend = 408, yend = 780),colour = "#d9a925",size=0.3)+
  # Les poteaux
  geom_segment(aes(x = 330, y = 1000, xend =325, yend = 1075),colour = line_colour,size=1) +
  geom_segment(aes(x = 370, y = 1000, xend =375, yend = 1075),colour = line_colour,size=1) +
  geom_segment(aes(x = 330, y = 1025, xend =370, yend = 1025),colour = line_colour,size=1) +
  geom_segment(aes(x = 330, y = 0, xend =325, yend = -75),colour = line_colour,size=1) +
  geom_segment(aes(x = 370, y = 0, xend =375, yend = -75),colour = line_colour,size=1) +
  geom_segment(aes(x = 330, y = -25, xend =370, yend = -25),colour = line_colour,size=1) +
  theme_void()+
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill =  "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA)
  )+theme(legend.position = "none")






path = unlist(strsplit(getwd(), "/"))[1]
username = unlist(strsplit(unlist(strsplit(getwd(), "Users/"))[2], "/"))[[1]]
mac_ou_pc = ifelse(substr(getwd(),1,1)!="/", "PC", "mac")

cat("\n")
print("Chargement des données Event")
if (mac_ou_pc=="PC"){
  chemin = paste0(path, "/Users/", username, "/ffr.fr/Accompagnement Perf - Dashboard - Documents/General/Data/")
  event <- read_excel(file.path(chemin, "Event.xlsx"), na = "NA")
}else if(username == "thomassinapi"){
  chemin = "~/Desktop/TStats/FFR/data/"
  #event <- read_delim(file.path(chemin,"event_update_XV.csv"),delim = ";", escape_double = FALSE, trim_ws = TRUE)
  event00 <- read_excel(file.path(chemin, "Event.xlsx"), na = "NA")
}




# #JAP_df <- event_red_xpts[event_red_xpts$StatName == "Jeu au pied" & event_red_xpts$a_supp == 0 & event_red_xpts$X.sens <= 220 , c("GameId","StatName","TimeVideo","TeamId","X.sens","Y.sens","Type","Result",
#                                                                                                                                   "StatName_First_Event","TeamId_First_Event",
#                                                                                                                                   "Xsens_First_Event","Ysens_First_Event",
#                                                                                                                                   "XPF_First_Event","XPA_First_Event",
#                                                                                                                                   "StatName_Short_Seq","TeamId_Short_Seq",
#                                                                                                                                   "Xsens_Short_Seq","Ysens_Short_Seq",
#                                                                                                                                   "XPF_Short_Seq","XPA_Short_Seq",
#                                                                                                                                   "StatName_End_Seq","TeamId_End_Seq",
#                                                                                                                                   "Xsens_End_Seq","Ysens_End_Seq",
#                                                                                                                                   "XPF_End_Seq","XPA_End_Seq","OrigineJAP","OrigineTeam")]
#

