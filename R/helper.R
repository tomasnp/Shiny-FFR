library(dplyr)
library(ggplot2)
library(shinydashboard)
library(DT)

prepare_event_subset <- function(input, event, vals){

  # Créer un sous-ensemble de event basé sur les inputs en fonction du role de l'équipe
  event_subset <- event %>% filter(name %in% c(vals$equipeAdv, vals$equipe),name2 %in% c(vals$equipeAdv, vals$equipe),season %in% vals$annee,Competition %in% vals$compet)

  #View(event_subset[, c("StatName","TimeVideo","name", "X.sens","Y.sens","XPtsDiff","StatNameseq","nameseq","Xseq","Yseq","XPDseq")])

  #input non vides
  if(nrow(event_subset) > 0 ){

    event_subset$XPDseq <- NA
    event_subset$StatNameseq <- NA
    event_subset$Xseq <- NA
    event_subset$Yseq <- NA
    event_subset$nameseq <- NA
    event_subset$maxXPDseq <- NA

    seq_time <- input$sliderInput

    if(seq_time == 0){
      event_subset$XPDseq <- event_subset$XPtsDiff
      event_subset$StatNameseq <- event_subset$StatName
      event_subset$Xseq <- event_subset$X.sens
      event_subset$Yseq <- event_subset$Y.sens
      event_subset$nameseq <- event_subset$name
    }else{

      if(input$echeance == "Durée"){

        for (i in which( !(event_subset$StatName %in% c("Essai", "Conversion", "Penalite")) & (event_subset$StatName %in% input$selected_event ) )) {

          arrjeu <- which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def", "Coup d'envoi"))[1]
          meltouche <- which(event_subset$GameId == event_subset$GameId[i] & event_subset$TimeVideo > event_subset$TimeVideo[i] + seq_time)[1]
          rucks <- which(event_subset$GameId == event_subset$GameId[i] & event_subset$StatName == "Ruck" & event_subset$TimeVideo > event_subset$TimeVideo[i] & event_subset$TimeVideo <= event_subset$TimeVideo[i] + seq_time)

          event_subset$XPDseq[i] <- #Gestion du cas où on a un arrêt de jeu dans les 30 secondes
            ifelse(event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] < seq_time,
                   #Echange des XP pour les arrêts de jeu
                   ifelse((event_subset$TeamId[arrjeu] == event_subset$TeamId[i] & event_subset$StatName[arrjeu] !=  "Faute def") | (event_subset$TeamId[arrjeu] != event_subset$TeamId[i] & event_subset$StatName[arrjeu] ==  "Faute def"),
                          event_subset$XPtsDiff[which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def"))][1],
                          -event_subset$XPtsDiff[which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def"))][1]),

                   #Cas où on a touche ou mêlée après la fin des 30sec mais que ça a été sifflé avant
                   ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee"),
                          #Echange des XP pour ce cas
                          ifelse(event_subset$TeamId[meltouche] == event_subset$TeamId[i],
                                 event_subset$XPtsDiff[meltouche],
                                 -event_subset$XPtsDiff[meltouche]),
                          #Cas où on a un ruck pour terminer les 30 sec
                          ifelse(length(rucks) == 0,
                                 event_subset$XPtsDiff[i],
                                 ifelse(event_subset$TeamId[rucks][which.max(event_subset$TimeVideo[rucks])] == event_subset$TeamId,
                                        event_subset$XPtsDiff[rucks][which.max(event_subset$TimeVideo[rucks])],
                                        -event_subset$XPtsDiff[rucks][which.max(event_subset$TimeVideo[rucks])]
                                 ))))
          event_subset$XPDseq[i] <- #Gestion du cas où on a un arrêt de jeu dans les 30 secondes
            ifelse(event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] < seq_time,
                   #Echange des XP pour les arrêts de jeu
                   ifelse((event_subset$TeamId[arrjeu] == event_subset$TeamId[i] & event_subset$StatName[arrjeu] !=  "Faute def") | (event_subset$TeamId[arrjeu] != event_subset$TeamId[i] & event_subset$StatName[arrjeu] ==  "Faute def"),
                          event_subset$XPtsDiff[which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def"))][1],
                          -event_subset$XPtsDiff[which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def"))][1]),

                   #Cas où on a touche ou mêlée après la fin des 30sec mais que ça a été sifflé avant
                   ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee"),
                          #Echange des XP pour ce cas
                          ifelse(event_subset$TeamId[meltouche] == event_subset$TeamId[i],
                                 event_subset$XPtsDiff[meltouche],
                                 -event_subset$XPtsDiff[meltouche]),
                          #Cas où on a un ruck pour terminer les 30 sec
                          ifelse(length(rucks) == 0,
                                 event_subset$XPtsDiff[i],
                                 ifelse(event_subset$TeamId[rucks][which.max(event_subset$TimeVideo[rucks])] == event_subset$TeamId,
                                        event_subset$XPtsDiff[rucks][which.max(event_subset$TimeVideo[rucks])],
                                        -event_subset$XPtsDiff[rucks][which.max(event_subset$TimeVideo[rucks])]
                                 ))))



          event_subset$StatNameseq[i] <- #Gestion du cas où on a un arrêt de jeu dans les 30 secondes
            ifelse(event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] < seq_time,
                   event_subset$StatName[which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def"))][1],

                   #Cas où on a touche ou mêlée après la fin des 30sec mais que ça a été sifflé avant
                   ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee"),
                          #Echange des XP pour ce cas
                          event_subset$StatName[meltouche],
                          #Cas où on a un ruck pour terminer les 30 sec
                          ifelse(length(rucks) == 0,
                                 event_subset$StatName[i],
                                 event_subset$StatName[rucks][which.max(event_subset$TimeVideo[rucks])]
                          )))

          event_subset$nameseq[i] <- #Gestion du cas où on a un arrêt de jeu dans les 30 secondes
            ifelse(event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] < seq_time,
                   event_subset$name[which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def"))][1],

                   #Cas où on a touche ou mêlée après la fin des 30sec mais que ça a été sifflé avant
                   ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee"),
                          #Echange des XP pour ce cas
                          event_subset$name[meltouche],
                          #Cas où on a un ruck pour terminer les 30 sec
                          ifelse(length(rucks) == 0,
                                 event_subset$name[i],
                                 event_subset$name[rucks][which.max(event_subset$TimeVideo[rucks])]
                          )))

          event_subset$Xseq[i] <- #Gestion du cas où on a un arrêt de jeu dans les 30 secondes
            ifelse(event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] < seq_time,
                   event_subset$X.sens[which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def"))][1],

                   ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee"),
                          #Echange des XP pour ce cas
                          event_subset$X.sens[meltouche],
                          ifelse(length(rucks) == 0,
                                 event_subset$X.sens[i],
                                 event_subset$X.sens[rucks][which.max(event_subset$TimeVideo[rucks])]
                          )))

          event_subset$Yseq[i] <- #Gestion du cas où on a un arrêt de jeu dans les 30 secondes
            ifelse(event_subset$TimeVideo[arrjeu] - event_subset$TimeVideo[i] < seq_time,
                   event_subset$Y.sens[which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def"))][1],

                   ifelse(event_subset$StatName[meltouche] %in% c("Touche", "Melee"),
                          #Echange des XP pour ce cas
                          event_subset$Y.sens[meltouche],
                          ifelse(length(rucks) == 0,
                                 event_subset$Y.sens[i],
                                 event_subset$Y.sens[rucks][which.max(event_subset$TimeVideo[rucks])]
                          )))


          event_subset$Yseq[i] <-
            ifelse(event_subset$TimeVideo[which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def", "Coup d'envoi"))][1] - event_subset$TimeVideo[i] < seq_time,
                   event_subset$Y.sens[which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def", "Coup d'envoi"))][1],

                   ifelse(event_subset$StatName[which(event_subset$GameId == event_subset$GameId[i] & event_subset$TimeVideo > event_subset$TimeVideo[i] + seq_time)][1] %in% c("Touche", "Melee"),
                          event_subset$Y.sens[which(event_subset$GameId == event_subset$GameId[i] & event_subset$TimeVideo > event_subset$TimeVideo[i] + seq_time)][1],
                          ifelse(length(which(event_subset$StatName == "Ruck" & event_subset$GameId == event_subset$GameId[i] & event_subset$GameSeconds > event_subset$GameSeconds[i] & event_subset$GameSeconds <= event_subset$GameSeconds[i] + seq_time)) == 0,
                                 event_subset$Y.sens[i],
                                 event_subset$Y.sens[which(event_subset$GameId == event_subset$GameId[i] & event_subset$StatName == "Ruck" & event_subset$TimeVideo > event_subset$TimeVideo[i] & event_subset$TimeVideo <= event_subset$TimeVideo[i] + seq_time)][which.max(event_subset$TimeVideo[which(event_subset$GameId == event_subset$GameId[i] & event_subset$StatName == "Ruck" & event_subset$TimeVideo > event_subset$TimeVideo[i] & event_subset$TimeVideo <= event_subset$TimeVideo[i] + seq_time)])]
                          ))

            )
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
        for (i in which( !(event_subset$StatName %in% c("Essai", "Conversion", "Penalite")) & (event_subset$StatName %in% input$selected_event ) )) {

          arrjeu <- which(event_subset$GameId[i] == event_subset$GameId & event_subset$TimeVideo[i] < event_subset$TimeVideo & event_subset$TimeVideo < event_subset$TimeVideo[i] + 900 & event_subset$StatName %in% c("Essai", "Touche", "Melee", "Faute off", "Faute def"))[1]
          nruck <- which(event_subset$GameId == event_subset$GameId[i] & event_subset$TimeVideo > event_subset$TimeVideo[i] & event_subset$TimeVideo < event_subset$TimeVideo[i] + 900 & event_subset$StatName == "Ruck")[seq_time]
          evt_mmteam <- which(event_subset$GameId == event_subset$GameId[i] & !(event_subset$StatName %in% c("Conversion", "Penalite", "Jeu au pied", "Maul", "Turnover concedee", "Franchissement")) & event_subset$TeamId == event_subset$TeamId[i] & event_subset$TimeVideo > event_subset$TimeVideo[i])
          evt_diffteam <- which(event_subset$GameId == event_subset$GameId[i] & event_subset$TeamId != event_subset$TeamId[i] & !(event_subset$StatName %in% c("Conversion", "Penalite", "Jeu au pied", "Maul", "Turnover concedee", "Franchissement")) & event_subset$TimeVideo > event_subset$TimeVideo[i])
          infarrjeu <- which(event_subset$TimeVideo < event_subset$TimeVideo[arrjeu])
          infegarrjeu <- which(event_subset$TimeVideo <= event_subset$TimeVideo[arrjeu])
          infegnruck <- which(event_subset$TimeVideo <= event_subset$TimeVideo[nruck])
          event_subset$XPDseq[i] <- ifelse(event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck],
                                           #Inversion des XP dans le cas d'arrêts de jeu
                                           ifelse(event_subset$TeamId[arrjeu] == event_subset$TeamId[i] & event_subset$StatName[arrjeu] != "Faute def" | event_subset$TeamId[arrjeu] != event_subset$TeamId[i] & event_subset$StatName[arrjeu] == "Faute def",
                                                  event_subset$XPtsDiff[arrjeu],
                                                  -event_subset$XPtsDiff[arrjeu]),
                                           #Inversion dans le cas du 5ème ruck
                                           ifelse(event_subset$TeamId[nruck] == event_subset$TeamId[i] & event_subset$StatName[nruck] != "Faute def" | event_subset$TeamId[nruck] != event_subset$TeamId[i] & event_subset$StatName[nruck] == "Faute def",
                                                  event_subset$XPtsDiff[nruck],
                                                  -event_subset$XPtsDiff[nruck])

          )

          event_subset$StatNameseq[i] <- ifelse(event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck],
                                                event_subset$StatName[arrjeu],
                                                event_subset$StatName[nruck]

          )
          event_subset$nameseq[i] <- ifelse(event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck],
                                            event_subset$name[arrjeu],
                                            event_subset$name[nruck]

          )

          event_subset$Xseq[i] <- ifelse(event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck],
                                         event_subset$X.sens[arrjeu],
                                         event_subset$X.sens[nruck]

          )
          event_subset$Yseq[i] <- ifelse(event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck],
                                         event_subset$Y.sens[arrjeu],
                                         event_subset$Y.sens[nruck]

          )

          event_subset$maxXPDseq[i] <- ifelse(event_subset$TimeVideo[arrjeu] < event_subset$TimeVideo[nruck],
                                              #Inversion des XP dans le cas d'arrêts de jeu
                                              ifelse(event_subset$StatName[arrjeu] != "Faute def",
                                                     max(event_subset$XPtsDiff[intersect(evt_mmteam,infegarrjeu)], -event_subset$XPtsDiff[intersect(evt_diffteam,infegarrjeu)]),
                                                     ifelse(event_subset$TeamId[arrjeu] == event_subset$TeamId[i],
                                                            max(event_subset$XPtsDiff[intersect(evt_mmteam,infarrjeu)], -event_subset$XPtsDiff[arrjeu],-event_subset$XPtsDiff[intersect(evt_diffteam,infarrjeu)]),
                                                            max(event_subset$XPtsDiff[intersect(evt_mmteam,infarrjeu)], event_subset$XPtsDiff[arrjeu],-event_subset$XPtsDiff[intersect(evt_diffteam,infarrjeu)]))),
                                              max(event_subset$XPtsDiff[intersect(evt_mmteam,infegnruck)], -event_subset$XPtsDiff[intersect(evt_diffteam,infegnruck)])
          )
          # si l'équipe garde la balle on garde les coordonnées, si elle l'a perd on inverse
          if(!(event_subset$nameseq[i] %in% event_subset$name[i]) || !(event_subset$name[i] %in% event_subset$nameseq[i])){
            event_subset$Xseq[i] <- 1000 - event_subset$Xseq[i]
            event_subset$Yseq[i] <- 700 - event_subset$Yseq[i]
          }else if(event_subset$nameseq[i] %in% event_subset$name[i] || (event_subset$name[i] %in% event_subset$nameseq[i])){
            event_subset$Xseq[i] <- event_subset$Xseq[i]
            event_subset$Yseq[i] <- event_subset$Yseq[i]
          }
        }

      }
    }
  }else{
    #cas pour les input vides
    event_subset <- head(event_subset, 0)
    return(event_subset)
  }
  #View(event_subset[, c("StatName","TimeVideo","name", "X.sens","Y.sens","XPtsDiff","StatNameseq","nameseq","Xseq","Yseq","XPDseq")])
  return(event_subset)
}
