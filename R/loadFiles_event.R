#
# path = unlist(strsplit(getwd(), "/"))[1]
# username = unlist(strsplit(unlist(strsplit(getwd(), "Users/"))[2], "/"))[[1]]
# mac_ou_pc = ifelse(substr(getwd(),1,1)!="/", "PC", "mac")
#
# cat("\n")
# print("Chargement des données Event")
# if (mac_ou_pc=="PC"){
#   chemin = paste0(path, "/Users/", username, "/ffr.fr/Accompagnement Perf - Dashboard - Documents/General/Data/")
#   event <- read_excel(file.path(chemin, "Event.xlsx"), na = "NA")
# }else if(username == "thomassinapi"){
#   chemin = "~/Desktop/TStats/FFR/data/"
#   #event <- read_delim(file.path(chemin,"event_update_XV.csv"),delim = ";", escape_double = FALSE, trim_ws = TRUE)
#   event <- read_excel(file.path(chemin, "Event.xlsx"), na = "NA")
# }
#
#
#
# #event <- event %>% filter(substr(GameId, 1,8) %in% c("20238100", "20233010", "20221910") | GameId %in% c("20232918010","20232932110","20232412910","20231811810"))
# event <- event %>% mutate(season = substr(GameId, 1, 4))
# event$TimeVideo <- as.numeric(event$TimeVideo)
# event$TeamId <- as.numeric(event$TeamId)
#
# event$distance <- sqrt((event$X.end - event$X.sens)^2 + (event$Y.end - event$Y.sens)^2)
#
# event <- event %>% mutate(Type = if_else(Type == "Territorial", "Long", Type))
# event <- event %>% mutate(Type = if_else(Type == "Box" & distance > 300, "Chapeau Long", if_else(Type == "Box" & distance <= 300, "Chapeau Court", Type)))
#
# event <- event %>% mutate(Type = if_else(Type == "Cross Pitch", "Fleche", Type))
# event <- event %>% mutate(Type = if_else(Type == "Low", "Chasse", Type))
# event <- event %>% mutate(Type = if_else(Type == "Bomb", "Wilko", Type))
# event <- event %>% mutate(Type = if_else(Type == "Chip", "PD", Type))
#
#
# circuit <- read_excel(file.path(chemin, "Db Circuit.xlsx"), na = "NA")
#
# joueur <- read_excel(file.path(chemin, "Db STATS.xlsx"),sheet = 1, na = "NA")
#
#
# match<- read_excel(file.path(chemin, "Db STATS.xlsx"),sheet = 2, na = "NA")
#
# equipe<- read_excel(file.path(chemin, "Db STATS.xlsx"),sheet = 4, na = "NA")
#
# competition <- read_excel(file.path(chemin, "Db STATS.xlsx"),sheet = 7, na = "NA")
#
# competition$CompetID <- as.character(competition$CompetID)
# competition <- competition[, c("CompetID", "Competition")]
#
# event <- event %>% mutate(CompetID = substr(GameId, 5, 7)) %>% left_join(competition, by = "CompetID", relationship = "many-to-many")
#
# equipe <- equipe %>% mutate(name = if_else(name == "La Rochelle", "Stade Rochelais", name))
#
# #indiv<- read_excel("C:/Users/abelb/Documents/ENSAI/2A/Stage/Db_STATS.xlsx", sheet = 5, na = "NA")
#
# event <- merge(event, joueur[, c("PlayerId", "Joueur")], by = "PlayerId", all.x = TRUE)
#
# # Jointure avec le dataframe "equipe" pour ajouter la colonne "name" à "event"
# event <- merge(event, equipe[, c("TeamId", "name")], by = "TeamId", all.x = TRUE)
#
# # Jointure avec le dataframe "match" pour ajouter les colonnes "compétition", "date", "ScoreHome" et "ScoreAway" à "event"
# event <- merge(event, match[, c("GameId", "ScoreHome", "ScoreAway", "TeamHome", "TeamAway", "TeamidHome","TeamidAway")], by = "GameId", all.x = TRUE)
#
# event$TeamId_adv <- ifelse(event$TeamidAway == event$TeamId, event$TeamidHome, event$TeamidAway)
#
# event$name2 <- ifelse(event$TeamAway == event$name, event$TeamHome, event$TeamAway)
#
#
# event$HomeAway <- ifelse(event$name == event$TeamHome, 1, 0)
# event <- event %>% mutate(ScoreEquipe = if_else(HomeAway == 1, ScoreHome, ScoreAway))
#
# event <- subset(event, select = -c(TeamHome, TeamAway,TeamidHome, TeamidAway))
#
# indices <- which(event$GameId == 20232412910) # Final 2023 TOP 14
#
# event$Competition[indices] <- "TOP 14"
#
#
# event$GameId <- as.character(event$GameId)
# event <- event %>% arrange(GameId, TimeVideo) %>% group_by(GameId)
#
#
#
#
# event$TimeVidRecep <- NA
# for (i in which (event$StatName == "Jeu au pied")) {
#   #Récupération des temps vidéos de ce qui se passe juste après le coup de pied
#   if(event$StatName[i+1] != "Touche"){
#     event$TimeVidRecep[i] <- event$TimeVideo[i+1]
#   }
# }
#
# # event <- event %>% filter(StatName != "Reception" )
#
# event$Type <- ifelse(event$Type == "Chapeau" & event$Result %in% c("Kick in Touch (Full)", "Kick in Touch (Bounce)"), "Touch Kick", event$Type) #Box kicks qui vont en touche deviennent des touch kicks
#
#
#
#
# ############Ajout de turnovers gagnés après turnovers concédés##########
# stockteamid <- NA
# stockname <- NA
# stockXPF <- NA
# concede <- event %>%  filter(StatName == "Turnover concedee")
# for (i in which(concede$StatName == "Turnover concedee")) {
#   concede$StatName[i] <- "Turnover gagne"
#   concede$X.sens[i] <- 1000 - concede$X.sens[i]
#   concede$Y.sens[i] <- 700 - concede$Y.sens[i]
#   stockteamid[i] <- concede$TeamId[i]
#   concede$TeamId[i] <- concede$TeamId_adv[i]
#   concede$TeamId_adv[i] <- stockteamid[i]
#   stockname[i] <- concede$name[i]
#   concede$name[i] <- concede$name2[i]
#   concede$name2[i] <- stockname[i]
#   stockXPF[i] <- concede$xPtsFor[i]
#   concede$xPtsFor[i] <- concede$xPtsAgainst[i]
#   concede$xPtsAgainst[i] <- stockXPF[i]
#   #Gestion des XPtsFor et Against à faire si on change pour les turnovers concedés
# }
# event <- event %>%  rbind(concede) %>% arrange(GameId, TimeVideo) %>% group_by(GameId)
#
# event$XPtsDiff <- as.numeric(event$xPtsFor) -  as.numeric(event$xPtsAgainst)
#
#
#
#
# circuit$Cellule <- ifelse(circuit$Cellule == "1", "+1", circuit$Cellule)
# circuit$Cellule2 <- ifelse(circuit$Cellule2 == "1", "+1", circuit$Cellule2)
# circuit$Cellule3 <- ifelse(circuit$Cellule3 == "1", "+1", circuit$Cellule3)
# circuit$Cellule4 <- ifelse(circuit$Cellule4 == "1", "+1", circuit$Cellule4)
#
#
# circuit$Circuit <- ifelse(is.na(circuit$Cellule4),
#                           ifelse(is.na(circuit$Cellule3),
#                                  ifelse(is.na(circuit$Cellule2), circuit$Cellule, paste(circuit$Cellule,circuit$Cellule2,sep = "/") ),
#                                  paste(circuit$Cellule,circuit$Cellule2,circuit$Cellule3,sep = "/")),
#                           paste(circuit$Cellule,circuit$Cellule2,circuit$Cellule3,circuit$Cellule4,sep = "/"))
#
# event <- rename(event, CircuitAuto = Circuit)
# #Création de TimeVideo2 pour avoir les temps équivalents entre les deux circuit et event
# circuit$TimeVideo2<-NA
# event$TimeVideo2<-NA
# for (g in unique(circuit$GameId)){
#   # On fait débuter à 0 le Timevideo de event
#   event$TimeVideo2[which(event$GameId==g)]<-event$TimeVideo[which(event$GameId==g)]-event$TimeVideo[which(event$GameId==g)[1]]+event$GameSeconds[which(event$GameId==g)[1]]
#   # On met sur le même temps celui de circuit
#   circuit$TimeVideo2[which(circuit$GameId==g)]<-circuit$TimeVideo[which(circuit$GameId==g)]-circuit$TimeVideo[which(circuit$GameId==g)[1]]+event$TimeVideo2[rev(which(event$GameId==g & event$GameSeconds<=circuit$GameSeconds[which(circuit$GameId==g)][1]))[1]]
# }
#
# cat("\n")
#
# print("Filtrage de la base de données 1/2")
# event$Circuit<-NA
# event$idCircuit<-NA
# event$Cellule<-NA
# circuit$Join<-0
# for(i in 1:nrow(circuit)){
#   # Si on a une reception qui tombe à la même seconde du meme match avec le meme X alors on va le joindre au ruck précédent
#   t<-which(event$GameId==circuit$GameId[i] & event$StatName%in%c("Reception") & event$GameSeconds==circuit$GameSeconds[i] & event$PlayerId==circuit$PlayerId[i] & event$X.sens==circuit$nX[i])
#   if(length(t)>0){
#     r<-ifelse(event$StatName[t-1]%in%c("Maul","Ruck"),t-1,t)
#     event$Cellule[r]<-circuit$Cellule[i]
#     event$Circuit[r]<-circuit$Circuit[i]
#     event$idCircuit[r]<-circuit$indexCircuitCellule[i]
#     circuit$Join[i]<-1
#   }else{
#     # Si on a une reception qui tombe dans les 30 secondes du meme match avec le meme X alors on va le joindre au ruck précédent
#     t<-which(event$GameId==circuit$GameId[i] & event$StatName%in%c("Reception") & abs(event$GameSeconds-circuit$GameSeconds[i])<30 & event$PlayerId==circuit$PlayerId[i] & event$X.sens==circuit$nX[i])
#     if(length(t)>0){
#       r<-ifelse(event$StatName[t-1]%in%c("Maul","Ruck"),t-1,t)
#       event$Cellule[r]<-circuit$Cellule[i]
#       event$Circuit[r]<-circuit$Circuit[i]
#       event$idCircuit[r]<-circuit$indexCircuitCellule[i]
#       circuit$Join[i]<-1
#     }else{
#       # Si on a toujours pas fait le lien on l'attache au ruck le plus proche
#       Temps<-which.min(abs(event$TimeVideo2[event$GameId==circuit$GameId[i] & event$StatName%in%c("Ruck","Maul") & event$GameSeconds<=circuit$GameSeconds[i]  & is.na(event$Cellule)]-circuit$TimeVideo2[i]))
#       t<-which(event$GameId==circuit$GameId[i]& event$StatName%in%c("Ruck","Maul") & event$GameSeconds<=circuit$GameSeconds[i] & is.na(event$Cellule))[Temps]
#       event$Cellule[t]<-circuit$Cellule[i]
#       event$Circuit[t]<-circuit$Circuit[i]
#       event$idCircuit[t]<-circuit$indexCircuitCellule[i]
#     }
#   }
# }
#
# # Prend CircuitAuto si tous les Circuit d'un match sont NA
# for(i in unique(event$GameId)){
#   if(all(is.na(event$Circuit[which(event$GameId == i)]))){
#     event$Circuit[event$GameId == i] <- event$CircuitAuto[event$GameId == i]
#   }else{
#     event$Circuit[event$GameId == i] <- event$Circuit[event$GameId == i]
#   }
# }
# event <- event %>% arrange(GameId, TimeVideo) %>% group_by(GameId)
#
# event$ArretPen<-NA
# for(i in which(event$StatName%in%c("Faute off","Faute def"))){
#   event$ArretPen[i]<-ifelse(event$TimeVideo[i+1]>event$TimeVideo[i]+10 & event$StatName[i+1]!="Penalite",1,0)
# }
#
# # IdPossession
# event$IdPossession<-0
# for(i in 2:nrow(event)){
#   event$IdPossession[i]<-ifelse((event$TeamId[i]!=event$TeamId[i-1] & !event$StatName[i] %in% c("Faute off","Faute def") & !event$StatName[i-1] %in% c("Faute def")) || (event$StatName[i-1] %in% c("Faute off") & !event$StatName[i] %in% c("Penalite")) || event$StatName[i]%in%c("Coup d'envoi") || (event$StatName[i]=="Faute def" & event$TeamId[i]==event$TeamId[i-1] & event$StatName[i-1]!="Faute def"), event$IdPossession[i-1]+1, event$IdPossession[i-1])
# }
#
# event$ExclAvantage<-0
# for(i in which(event$StatName=="Penalite" | (event$StatName=="Touche" & event$Origine=="Pénalité"))){
#   Fautedef<-rev(which(event$IdPossession==event$IdPossession[i] & event$TimeVideo<event$TimeVideo[i] & event$TeamId!=event$TeamId[i] & event$StatName%in%"Faute def" & event$ArretPen==0))[1]
#   Fauteoff<-rev(which(event$IdPossession==event$IdPossession[i]-1 & event$TimeVideo<event$TimeVideo[i] & event$TeamId!=event$TeamId[i] & event$StatName%in%"Faute off" & event$ArretPen==0))[1]
#   LastStop <- rev(which(event$IdPossession==event$IdPossession[i] & event$TimeVideo<event$TimeVideo[i] & (event$StatName%in%c("Touche","Melee","Coup d'envoi") | event$ArretPen==1)))[1]
#   if(!is.na(Fautedef) & (is.na(LastStop)|LastStop<Fautedef)) {
#     event$ExclAvantage[which(event$IdPossession==event$IdPossession[i] & event$TimeVideo<event$TimeVideo[i] & event$TimeVideo>event$TimeVideo[Fautedef])]<-1
#   }else{
#     if(!is.na(Fauteoff) & (is.na(LastStop)|LastStop<Fauteoff)){
#       event$ExclAvantage[which(event$IdPossession%in%c(event$IdPossession[i]-1,event$IdPossession[i]) & event$TimeVideo<event$TimeVideo[i] & event$TimeVideo>event$TimeVideo[Fauteoff])]<-1
#     }
#   }
# }
# for(i in which(event$StatName=="Melee")){
#   Fautedef<-rev(which(event$IdPossession==event$IdPossession[i] & event$TimeVideo<event$TimeVideo[i] & event$TeamId!=event$TeamId[i] & event$StatName%in%"Faute def" & event$ArretPen==0))[1]
#   Fauteoff<-rev(which(event$IdPossession==event$IdPossession[i]-1 & event$TimeVideo<event$TimeVideo[i] & event$TeamId!=event$TeamId[i] & event$StatName%in%"Faute off" & event$ArretPen==0))[1]
#   Tv<-rev(which(event$IdPossession==event$IdPossession[i]-1 & event$TimeVideo<event$TimeVideo[i] & event$TeamId!=event$TeamId[i] & event$StatName%in%"Faute off" & event$ArretPen==0))[1]
#   LastStop <- rev(which(event$IdPossession==event$IdPossession[i] & event$TimeVideo<event$TimeVideo[i] & (event$StatName%in%c("Touche","Melee","Coup d'envoi") | event$ArretPen==1)))[1]
#   if(!is.na(Tv) & (is.na(LastStop)|LastStop<Tv)){
#     event$ExclAvantage[which(event$IdPossession==event$IdPossession[i]-1 & event$TimeVideo<event$TimeVideo[i] & event$TimeVideo>event$TimeVideo[Tv])]<-2
#   }else{
#     if(!is.na(Fautedef) & (is.na(LastStop)|LastStop<Fautedef)) {
#       event$ExclAvantage[which(event$IdPossession==event$IdPossession[i] & event$TimeVideo<event$TimeVideo[i] & event$TimeVideo>event$TimeVideo[Fautedef])]<-2
#     }else{
#       if(!is.na(Fauteoff) & (is.na(LastStop)|LastStop<Fauteoff)){
#         event$ExclAvantage[which(event$IdPossession%in%c(event$IdPossession[i]-1,event$IdPossession[i]) & event$TimeVideo<event$TimeVideo[i] & event$TimeVideo>event$TimeVideo[Fauteoff])]<-2
#       }
#     }
#   }
# }
#
#
#
#
# event$Choix <- NA
# for (i in which(event$StatName %in% c("Ruck", "Maul"))) {
#   tj <- which(event$GameId == event$GameId[i] & event$TimeVideo[i] < event$TimeVideo & event$StatName != "Passe")[1]
#   if (!is.na(event$Cellule[i])) {
#     event$Choix[i] <- "Cellule"
#     event$Type[i] <- event$Cellule[i]
#   }
#   else if (event$StatName[tj] == "Jeu au pied" & !is.na(event$StatName[tj]))
#   {
#     event$Choix[i] <- event$StatName[tj]
#     event$Type[i] <- event$Type[tj]
#   }
# }
#
# cat("\n")
# print("Filtrage de la base de données 2/2")
#
# event$OrigineJAP <- NA
# event$OrigineRuck <- NA
#
# for (i in which (event$StatName %in% c("Jeu au pied","Melee","Touche", "Ruck","Turnover gagne","Turnover concedee","Essai","Faute def", "Penalite"))) {
#
#   Origine<-rev(which(event$StatName %in% c("Coup d'envoi","Melee","Touche","Turnover gagne","Ruck") & event$GameId==event$GameId[i] & event$TimeVideo<event$TimeVideo[i]))[1]
#   orig_large <- rev(which(event$StatName %in% c("Coup d'envoi","Melee","Touche","Turnover gagne","Jeu au pied") & event$GameId==event$GameId[i] & event$TimeVideo<event$TimeVideo[i]))[1]
#
#   origineRuck<-rev(which(event$StatName %in% c("Jeu au pied","Coup d'envoi","Melee","Touche","Turnover gagne", "Ruck") & event$GameId==event$GameId[i] & event$TimeVideo<event$TimeVideo[i]))[1]
#
#   if(event$StatName[i] == "Jeu au pied"){
#     if(event$TeamId[i] != event$TeamId[Origine]){
#       event$xPtsFor[i]<- as.numeric(event$xPtsAgainst[Origine])
#
#       event$xPtsAgainst[i]<- as.numeric(event$xPtsFor[Origine])
#
#       event$XPtsDiff[i]<- -as.numeric(event$XPtsDiff[Origine])
#     }else{
#       event$xPtsFor[i]<- as.numeric(event$xPtsFor[Origine])
#
#       event$xPtsAgainst[i]<- as.numeric(event$xPtsAgainst[Origine])
#
#       event$XPtsDiff[i]<- as.numeric(event$XPtsDiff[Origine])
#     }
#   }
#   ifelse(event$TeamId[i] != event$TeamId[orig_large],
#          event$OrigineJAP[i]<- paste0(event$StatName[orig_large], " adv"),
#          event$OrigineJAP[i]<- event$StatName[orig_large])
#   ifelse(event$TeamId[i] != event$TeamId[origineRuck],
#          event$OrigineRuck[i]<- paste0(event$StatName[origineRuck], " adv"),
#          event$OrigineRuck[i]<- event$StatName[origineRuck])
# }
#
#
# event$PerfxPF<-event$ScorePour-event$xPtsFor
# event$PerfxPA<-event$ScoreContre-event$xPtsAgainst
# event$PerfxPD<-event$ScorePour-event$ScoreContre-event$XPtsDiff
#
# #event_red <- event %>% filter(StatName != "Passe" & StatName != "Offload" & StatName != "Reception")
#
#
#
#
# usethis::use_data(event, overwrite = TRUE)
#
# cat("\n")
# print("Ouverture du Shiny")
#
#
