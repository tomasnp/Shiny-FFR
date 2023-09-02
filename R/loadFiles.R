#
# path = unlist(strsplit(getwd(), "/"))[1]
# username = unlist(strsplit(unlist(strsplit(getwd(), "Users/"))[2], "/"))[[1]]
# mac_ou_pc = ifelse(substr(getwd(),1,1)!="/", "PC", "mac")
#
# cat("\n")
# print("Chargement des données Event")
#
# if (mac_ou_pc=="PC"){
#   chemin = paste0(path, "/Users/", username, "/ffr.fr/Accompagnement Perf - Dashboard - Documents/General/Data/")
#   Event <- read.csv(file.path(chemin, "liveMap.csv"), sep = ";", header = TRUE, stringsAsFactors = FALSE)
# }else if(username == "thomassinapi"){
#   chemin = "~/Desktop/TStats/FFR/data/"
#   #Event <- read_delim(file.path(chemin,"liveMap.csv"),delim = ";", escape_double = FALSE, trim_ws = TRUE)
#   Event <- read.csv(file.path(chemin, "liveMap.csv"), sep = ";", header = TRUE, stringsAsFactors = FALSE)
# }
#
# load("data/event.rda")
#
#
# Event$TimeVideo <- gsub(",", ".", Event$TimeVideo)
# Event$Vitesse <- gsub(",", ".", Event$Vitesse)
# Event$Gain1 <- gsub(",", ".", Event$Gain1)
# Event$GainTot <- gsub(",", ".", Event$GainTot)
# Event$Duree <- gsub(",", ".", Event$Duree)
# Event$xPtsFor <- gsub(",", ".", Event$xPtsFor)
# Event$xPtsAgainst <- gsub(",", ".", Event$xPtsAgainst)
# Event$StartVideo <- gsub(",", ".", Event$StartVideo)
# Event$EndVideo <- gsub(",", ".", Event$EndVideo)
# Event$ScorePour <- gsub(",", ".", Event$ScorePour)
# Event$ScoreContre <- gsub(",", ".", Event$ScoreContre)
#
# Event$ScoreContre <- as.numeric(Event$ScoreContre)
# Event$ScorePour <- as.numeric(Event$ScorePour)
# Event$EndVideo <- as.numeric(Event$EndVideo)
# Event$StartVideo <- as.numeric(Event$StartVideo)
# Event$TimeVideo <- as.numeric(Event$TimeVideo)
# Event$Vitesse <- as.numeric(Event$Vitesse)
# Event$Gain1 <- as.numeric(Event$Gain1)
# Event$GainTot <- as.numeric(Event$GainTot)
# Event$Duree <- as.numeric(Event$Duree)
# Event$xPtsFor <- as.numeric(Event$xPtsFor)
# Event$xPtsAgainst <- as.numeric(Event$xPtsAgainst)
# # Event <- Event %>% filter(substr(GameId, 1,8) %in% c("20238100", "20233010", "20221910") | GameId %in% c("20232918010","20232932110","20232412910","20231811810"))
# Event <- Event %>% mutate(season = substr(GameId, 1, 4))
#
# Event$distance <- sqrt((Event$X.end - Event$X.sens)^2 + (Event$Y.end - Event$Y.sens)^2)
#
# Event <- Event %>% mutate(Type = if_else(Type == "Territorial", "Long", Type))
# Event <- Event %>% mutate(Type = if_else(Type == "Box" & distance > 300, "Chapeau Long", if_else(Type == "Box" & distance <= 300, "Chapeau Court", Type)))
#
# Event <- Event %>% mutate(Type = if_else(Type == "Cross Pitch", "Fleche", Type))
# Event <- Event %>% mutate(Type = if_else(Type == "Low", "Chasse", Type))
# Event <- Event %>% mutate(Type = if_else(Type == "Bomb", "Wilko", Type))
# Event <- Event %>% mutate(Type = if_else(Type == "Chip", "PD", Type))
#
#
#
# unique_gameIds_event <- unique(event$GameId)
#
# Event <- Event[!(Event$GameId %in% unique_gameIds_event), ]
#
# if (nrow(Event) == 0) {
#   print("Tous les GameId de Event existent déjà dans event, le processus d'intégration est donc ignoré.")
# } else {
#
#   circuit <- read_excel(file.path(chemin, "Db Circuit.xlsx"), na = "NA")
#
#   joueur <- read_excel(file.path(chemin, "Db STATS.xlsx"),sheet = 1, na = "NA")
#
#
#   match<- read_excel(file.path(chemin, "Db STATS.xlsx"),sheet = 2, na = "NA")
#
#   equipe<- read_excel(file.path(chemin, "Db STATS.xlsx"),sheet = 4, na = "NA")
#
#   competition <- read_excel(file.path(chemin, "Db STATS.xlsx"),sheet = 7, na = "NA")
#
#   competition$CompetID <- as.character(competition$CompetID)
#   competition <- competition[, c("CompetID", "Competition")]
#
#   Event <- Event %>% mutate(CompetID = substr(GameId, 5, 7)) %>% left_join(competition, by = "CompetID", relationship = "many-to-many")
#
#   equipe <- equipe %>% mutate(name = if_else(name == "La Rochelle", "Stade Rochelais", name))
#
#   #indiv<- read_excel("C:/Users/abelb/Documents/ENSAI/2A/Stage/Db_STATS.xlsx", sheet = 5, na = "NA")
#
#   Event <- merge(Event, joueur[, c("PlayerId", "Joueur")], by = "PlayerId", all.x = TRUE)
#
#   # Jointure avec le dataframe "equipe" pour ajouter la colonne "name" à "Event"
#   Event <- merge(Event, equipe[, c("TeamId", "name")], by = "TeamId", all.x = TRUE)
#
#   # Jointure avec le dataframe "match" pour ajouter les colonnes "compétition", "date", "ScoreHome" et "ScoreAway" à "Event"
#   Event <- merge(Event, match[, c("GameId", "ScoreHome", "ScoreAway", "TeamHome", "TeamAway", "TeamidHome","TeamidAway")], by = "GameId", all.x = TRUE)
#
#   Event$TeamId_adv <- ifelse(Event$TeamidAway == Event$TeamId, Event$TeamidHome, Event$TeamidAway)
#
#   Event$name2 <- ifelse(Event$TeamAway == Event$name, Event$TeamHome, Event$TeamAway)
#
#
#   Event$HomeAway <- ifelse(Event$name == Event$TeamHome, 1, 0)
#   Event <- Event %>% mutate(ScoreEquipe = if_else(HomeAway == 1, ScoreHome, ScoreAway))
#
#   Event <- subset(Event, select = -c(TeamHome, TeamAway,TeamidHome, TeamidAway))
#   Event$GameId <- as.character(Event$GameId)
#   Event <- Event %>% arrange(GameId, TimeVideo) %>% group_by(GameId)
#
#   Event$TimeVidRecep <- NA
#   for (i in which (Event$StatName == "Jeu au pied")) {
#     #Récupération des temps vidéos de ce qui se passe juste après le coup de pied
#     if(Event$StatName[i+1] != "Touche"){
#       Event$TimeVidRecep[i] <- Event$TimeVideo[i+1]
#     }
#   }
#
#   Event <- Event %>% filter(StatName != "Reception" )
#
#   Event$Type <- ifelse(Event$Type == "Box" & Event$Result %in% c("Kick in Touch (Full)", "Kick in Touch (Bounce)"), "Touch Kick", Event$Type) #Box kicks qui vont en touche deviennent des touch kicks
#
#
#   ############Ajout de turnovers gagnés après turnovers concédés##########
#   stockname <- NA
#   stockteamid <- NA
#   stockXPF <- NA
#   concede <- Event %>%  filter(StatName == "Turnover concedee")
#   for (i in which(concede$StatName == "Turnover concedee")) {
#     concede$StatName[i] <- "Turnover gagne"
#     concede$X.sens[i] <- 1000 - concede$X.sens[i]
#     concede$Y.sens[i] <- 700 - concede$Y.sens[i]
#     stockteamid[i] <- concede$TeamId[i]
#     concede$TeamId[i] <- concede$TeamId_adv[i]
#     concede$TeamId_adv[i] <- stockteamid[i]
#     stockname[i] <- concede$name[i]
#     concede$name[i] <- concede$name2[i]
#     concede$name2[i] <- stockname[i]
#     stockXPF[i] <- concede$xPtsFor[i]
#     concede$xPtsFor[i] <- concede$xPtsAgainst[i]
#     concede$xPtsAgainst[i] <- stockXPF[i]
#     #Gestion des XPtsFor et Against à faire si on change pour les turnovers concedés
#   }
#   Event <- Event %>%  rbind(concede) %>% arrange(GameId, TimeVideo) %>% group_by(GameId)
#
#   Event$XPtsDiff <- as.numeric(Event$xPtsFor) -  as.numeric(Event$xPtsAgainst)
#   Event$XPtsDiff <- as.numeric(Event$XPtsDiff)
#
#
#
#
#   circuit$Cellule <- ifelse(circuit$Cellule == "1", "+1", circuit$Cellule)
#   circuit$Cellule2 <- ifelse(circuit$Cellule2 == "1", "+1", circuit$Cellule2)
#   circuit$Cellule3 <- ifelse(circuit$Cellule3 == "1", "+1", circuit$Cellule3)
#   circuit$Cellule4 <- ifelse(circuit$Cellule4 == "1", "+1", circuit$Cellule4)
#
#
#   circuit$Circuit <- ifelse(is.na(circuit$Cellule4),
#                             ifelse(is.na(circuit$Cellule3),
#                                    ifelse(is.na(circuit$Cellule2), circuit$Cellule, paste(circuit$Cellule,circuit$Cellule2,sep = "/") ),
#                                    paste(circuit$Cellule,circuit$Cellule2,circuit$Cellule3,sep = "/")),
#                             paste(circuit$Cellule,circuit$Cellule2,circuit$Cellule3,circuit$Cellule4,sep = "/"))
#
#   Event <- rename(Event, CircuitAuto = Circuit)
#   #Création de TimeVideo2 pour avoir les temps équivalents entre les deux circuit et Event
#   circuit$TimeVideo2<-NA
#   Event$TimeVideo2<-NA
#   for (g in unique(circuit$GameId)){
#     # On fait débuter à 0 le Timevideo de Event
#     Event$TimeVideo2[which(Event$GameId==g)]<-Event$TimeVideo[which(Event$GameId==g)]-Event$TimeVideo[which(Event$GameId==g)[1]]+Event$GameSeconds[which(Event$GameId==g)[1]]
#     # On met sur le même temps celui de circuit
#     circuit$TimeVideo2[which(circuit$GameId==g)]<-circuit$TimeVideo[which(circuit$GameId==g)]-circuit$TimeVideo[which(circuit$GameId==g)[1]]+Event$TimeVideo2[rev(which(Event$GameId==g & Event$GameSeconds<=circuit$GameSeconds[which(circuit$GameId==g)][1]))[1]]
#   }
#
#   cat("\n")
#
#   print("Filtrage de la base de données 1/2")
#   Event$Circuit<-NA
#   Event$idCircuit<-NA
#   Event$Cellule<-NA
#   circuit$Join<-0
#   for(i in 1:nrow(circuit)){
#     # Si on a une reception qui tombe à la même seconde du meme match avec le meme X alors on va le joindre au ruck précédent
#     t<-which(Event$GameId==circuit$GameId[i] & Event$StatName%in%c("Reception") & Event$GameSeconds==circuit$GameSeconds[i] & Event$PlayerId==circuit$PlayerId[i] & Event$X.sens==circuit$nX[i])
#     if(length(t)>0){
#       r<-ifelse(Event$StatName[t-1]%in%c("Maul","Ruck"),t-1,t)
#       Event$Cellule[r]<-circuit$Cellule[i]
#       Event$Circuit[r]<-circuit$Circuit[i]
#       Event$idCircuit[r]<-circuit$indexCircuitCellule[i]
#       circuit$Join[i]<-1
#     }else{
#       # Si on a une reception qui tombe dans les 30 secondes du meme match avec le meme X alors on va le joindre au ruck précédent
#       t<-which(Event$GameId==circuit$GameId[i] & Event$StatName%in%c("Reception") & abs(Event$GameSeconds-circuit$GameSeconds[i])<30 & Event$PlayerId==circuit$PlayerId[i] & Event$X.sens==circuit$nX[i])
#       if(length(t)>0){
#         r<-ifelse(Event$StatName[t-1]%in%c("Maul","Ruck"),t-1,t)
#         Event$Cellule[r]<-circuit$Cellule[i]
#         Event$Circuit[r]<-circuit$Circuit[i]
#         Event$idCircuit[r]<-circuit$indexCircuitCellule[i]
#         circuit$Join[i]<-1
#       }else{
#         # Si on a toujours pas fait le lien on l'attache au ruck le plus proche
#         Temps<-which.min(abs(Event$TimeVideo2[Event$GameId==circuit$GameId[i] & Event$StatName%in%c("Ruck","Maul") & Event$GameSeconds<=circuit$GameSeconds[i]  & is.na(Event$Cellule)]-circuit$TimeVideo2[i]))
#         t<-which(Event$GameId==circuit$GameId[i]& Event$StatName%in%c("Ruck","Maul") & Event$GameSeconds<=circuit$GameSeconds[i] & is.na(Event$Cellule))[Temps]
#         Event$Cellule[t]<-circuit$Cellule[i]
#         Event$Circuit[t]<-circuit$Circuit[i]
#         Event$idCircuit[t]<-circuit$indexCircuitCellule[i]
#       }
#     }
#   }
#
#   # Prend CircuitAuto si tous les Circuit d'un match sont NA
#   for(i in unique(Event$GameId)){
#     if(all(is.na(Event$Circuit[which(Event$GameId == i)]))){
#       Event$Circuit[Event$GameId == i] <- Event$CircuitAuto[Event$GameId == i]
#     }else{
#       Event$Circuit[Event$GameId == i] <- Event$Circuit[Event$GameId == i]
#     }
#   }
#   Event <- Event %>% arrange(GameId, TimeVideo) %>% group_by(GameId)
#
#   Event$ArretPen<-NA
#   for(i in which(Event$StatName%in%c("Faute off","Faute def"))){
#     Event$ArretPen[i]<-ifelse(Event$TimeVideo[i+1]>Event$TimeVideo[i]+10 & Event$StatName[i+1]!="Penalite",1,0)
#   }
#
#   # IdPossession
#   Event$IdPossession<-0
#   for(i in 2:nrow(Event)){
#     Event$IdPossession[i]<-ifelse((Event$TeamId[i]!=Event$TeamId[i-1] & !Event$StatName[i] %in% c("Faute off","Faute def") & !Event$StatName[i-1] %in% c("Faute def")) || (Event$StatName[i-1] %in% c("Faute off") & !Event$StatName[i] %in% c("Penalite")) || Event$StatName[i]%in%c("Coup d'envoi") || (Event$StatName[i]=="Faute def" & Event$TeamId[i]==Event$TeamId[i-1] & Event$StatName[i-1]!="Faute def"), Event$IdPossession[i-1]+1, Event$IdPossession[i-1])
#   }
#
#   Event$ExclAvantage<-0
#   for(i in which(Event$StatName=="Penalite" | (Event$StatName=="Touche" & Event$Origine=="Pénalité"))){
#     Fautedef<-rev(which(Event$IdPossession==Event$IdPossession[i] & Event$TimeVideo<Event$TimeVideo[i] & Event$TeamId!=Event$TeamId[i] & Event$StatName%in%"Faute def" & Event$ArretPen==0))[1]
#     Fauteoff<-rev(which(Event$IdPossession==Event$IdPossession[i]-1 & Event$TimeVideo<Event$TimeVideo[i] & Event$TeamId!=Event$TeamId[i] & Event$StatName%in%"Faute off" & Event$ArretPen==0))[1]
#     LastStop <- rev(which(Event$IdPossession==Event$IdPossession[i] & Event$TimeVideo<Event$TimeVideo[i] & (Event$StatName%in%c("Touche","Melee","Coup d'envoi") | Event$ArretPen==1)))[1]
#     if(!is.na(Fautedef) & (is.na(LastStop)|LastStop<Fautedef)) {
#       Event$ExclAvantage[which(Event$IdPossession==Event$IdPossession[i] & Event$TimeVideo<Event$TimeVideo[i] & Event$TimeVideo>Event$TimeVideo[Fautedef])]<-1
#     }else{
#       if(!is.na(Fauteoff) & (is.na(LastStop)|LastStop<Fauteoff)){
#         Event$ExclAvantage[which(Event$IdPossession%in%c(Event$IdPossession[i]-1,Event$IdPossession[i]) & Event$TimeVideo<Event$TimeVideo[i] & Event$TimeVideo>Event$TimeVideo[Fauteoff])]<-1
#       }
#     }
#   }
#   for(i in which(Event$StatName=="Melee")){
#     Fautedef<-rev(which(Event$IdPossession==Event$IdPossession[i] & Event$TimeVideo<Event$TimeVideo[i] & Event$TeamId!=Event$TeamId[i] & Event$StatName%in%"Faute def" & Event$ArretPen==0))[1]
#     Fauteoff<-rev(which(Event$IdPossession==Event$IdPossession[i]-1 & Event$TimeVideo<Event$TimeVideo[i] & Event$TeamId!=Event$TeamId[i] & Event$StatName%in%"Faute off" & Event$ArretPen==0))[1]
#     Tv<-rev(which(Event$IdPossession==Event$IdPossession[i]-1 & Event$TimeVideo<Event$TimeVideo[i] & Event$TeamId!=Event$TeamId[i] & Event$StatName%in%"Faute off" & Event$ArretPen==0))[1]
#     LastStop <- rev(which(Event$IdPossession==Event$IdPossession[i] & Event$TimeVideo<Event$TimeVideo[i] & (Event$StatName%in%c("Touche","Melee","Coup d'envoi") | Event$ArretPen==1)))[1]
#     if(!is.na(Tv) & (is.na(LastStop)|LastStop<Tv)){
#       Event$ExclAvantage[which(Event$IdPossession==Event$IdPossession[i]-1 & Event$TimeVideo<Event$TimeVideo[i] & Event$TimeVideo>Event$TimeVideo[Tv])]<-2
#     }else{
#       if(!is.na(Fautedef) & (is.na(LastStop)|LastStop<Fautedef)) {
#         Event$ExclAvantage[which(Event$IdPossession==Event$IdPossession[i] & Event$TimeVideo<Event$TimeVideo[i] & Event$TimeVideo>Event$TimeVideo[Fautedef])]<-2
#       }else{
#         if(!is.na(Fauteoff) & (is.na(LastStop)|LastStop<Fauteoff)){
#           Event$ExclAvantage[which(Event$IdPossession%in%c(Event$IdPossession[i]-1,Event$IdPossession[i]) & Event$TimeVideo<Event$TimeVideo[i] & Event$TimeVideo>Event$TimeVideo[Fauteoff])]<-2
#         }
#       }
#     }
#   }
#
#
#
#
# Event$Choix <- NA
# for (i in which(Event$StatName %in% c("Ruck", "Maul"))) {
#   tj <- which(Event$GameId == Event$GameId[i] & Event$TimeVideo[i] < Event$TimeVideo & Event$StatName != "Passe")[1]
#   if (!is.na(Event$Cellule[i])) {
#     Event$Choix[i] <- "Cellule"
#     Event$Type[i] <- Event$Cellule[i]
#   }
#   else if (Event$StatName[tj] == "Jeu au pied" & !is.na(Event$StatName[tj]))
#   {
#     Event$Choix[i] <- Event$StatName[tj]
#     Event$Type[i] <- Event$Type[tj]
#   }
# }
#
#   cat("\n")
#   print("Filtrage de la base de données 2/2")
#
#   Event$OrigineJAP <- NA
#   Event$OrigineRuck <- NA
#
#   for (i in which (Event$StatName %in% c("Jeu au pied","Melee","Touche", "Ruck","Turnover gagne","Turnover concedee","Essai","Faute def", "Penalite"))) {
#
#     Origine<-rev(which(Event$StatName %in% c("Coup d'envoi","Melee","Touche","Turnover gagne","Ruck") & Event$GameId==Event$GameId[i] & Event$TimeVideo<Event$TimeVideo[i]))[1]
#     orig_large <- rev(which(Event$StatName %in% c("Coup d'envoi","Melee","Touche","Turnover gagne","Jeu au pied") & Event$GameId==Event$GameId[i] & Event$TimeVideo<Event$TimeVideo[i]))[1]
#
#     origineRuck<-rev(which(Event$StatName %in% c("Jeu au pied","Coup d'envoi","Melee","Touche","Turnover gagne", "Ruck") & Event$GameId==Event$GameId[i] & Event$TimeVideo<Event$TimeVideo[i]))[1]
#
#     if(Event$StatName[i] == "Jeu au pied"){
#       if (!is.na(Event$TeamId[i]) & !is.na(Event$TeamId[Origine]) & Event$TeamId[i] != Event$TeamId[Origine]) {
#         Event$xPtsFor[i]<- as.numeric(Event$xPtsAgainst[Origine])
#
#         Event$xPtsAgainst[i]<- as.numeric(Event$xPtsFor[Origine])
#
#         Event$XPtsDiff[i]<- -as.numeric(Event$XPtsDiff[Origine])
#       }else{
#         Event$xPtsFor[i]<- as.numeric(Event$xPtsFor[Origine])
#
#         Event$xPtsAgainst[i]<- as.numeric(Event$xPtsAgainst[Origine])
#
#         Event$XPtsDiff[i]<- as.numeric(Event$XPtsDiff[Origine])
#       }
#     }
#     ifelse(Event$TeamId[i] != Event$TeamId[orig_large],
#            Event$OrigineJAP[i]<- paste0(Event$StatName[orig_large], " adv"),
#            Event$OrigineJAP[i]<- Event$StatName[orig_large])
#     ifelse(Event$TeamId[i] != Event$TeamId[origineRuck],
#            Event$OrigineRuck[i]<- paste0(Event$StatName[origineRuck], " adv"),
#            Event$OrigineRuck[i]<- Event$StatName[origineRuck])
#   }
#
#   Event$PerfxPF<-Event$ScorePour-Event$xPtsFor
#   Event$PerfxPA<-Event$ScoreContre-Event$xPtsAgainst
#   Event$PerfxPD<-Event$ScorePour-Event$ScoreContre-Event$XPtsDiff
#
#   #event_red <- event %>% filter(StatName != "Passe" & StatName != "Offload" & StatName != "Reception")
#
#   event <- rbind(event, Event)
#   usethis::use_data(event, overwrite = TRUE)
#
# }
#
#
# cat("\n")
# print("Ouverture du Shiny")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
