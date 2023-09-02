# RUN fetch.R puis jap_22.R puis HeatMap.R
usethis::use_data(jap_40, overwrite = TRUE)

library(readr)
library(readxl)
library(ggplot2)
library(dplyr)

JAP_df <- event_red_xpts[event_red_xpts$StatName == "Jeu au pied" & event_red_xpts$a_supp == 0 & event_red_xpts$X.sens <= 400 , c("GameId","StatName","TimeVideo","GameSeconds","TeamId","X.sens","Y.sens","Type","Result",
                                                                                                                                  "StatName_First_Event","TeamId_First_Event",
                                                                                                                                  "Time1TJ",
                                                                                                                                  "Xsens_First_Event","Ysens_First_Event",
                                                                                                                                  "XPF_First_Event","XPA_First_Event",
                                                                                                                                  "StatName_Short_Seq","TeamId_Short_Seq",
                                                                                                                                  "Xsens_Short_Seq","Ysens_Short_Seq",
                                                                                                                                  "XPF_Short_Seq","XPA_Short_Seq",
                                                                                                                                  "StatName_End_Seq","TeamId_End_Seq",
                                                                                                                                  "Xsens_End_Seq","Ysens_End_Seq",
                                                                                                                                  "XPF_End_Seq","XPA_End_Seq","OrigineJAP","OrigineTeam","X.end","Y.end","tpsjap","Duree", "StatName_prevRuck", "XPD_prevRuck", "Team_prevRuck")]

JAP_df$X.end <- as.numeric(JAP_df$X.end)
JAP_df$Y.end <- as.numeric(JAP_df$Y.end)

JAP_df$distance <- sqrt((JAP_df$X.end - JAP_df$X.sens)^2 + (JAP_df$Y.end - JAP_df$Y.sens)^2)
JAP_df <- JAP_df %>% mutate(diff_JAP_1TJ = Time1TJ - TimeVideo)
JAP_df$distX <- JAP_df$X.end - JAP_df$X.sens
JAP_df <- JAP_df %>% mutate(hauteur = distance / tpsjap)

JAP_df$CompetId <- stringr::str_sub(JAP_df$GameId,1,8) #Extraction id compétition
JAP_df$Type <- ifelse(JAP_df$Type == "Box" & JAP_df$Result %in% c("Kick in Touch (Full)", "Kick in Touch (Bounce)"), "Touch Kick", JAP_df$Type) #Box kicks qui vont en touche deviennent des touch kicks

#Attribution XPA et XPF selon l'équipe qui joue le coup de pied
XPF_First_Eventnew <- NA
XPF_Short_Seqnew <- NA
#XPF_End_Seqnew <- NA

XPF_First_Eventnew <- ifelse((JAP_df$TeamId != JAP_df$TeamId_First_Event & JAP_df$StatName_First_Event != "Faute def") |
                               (JAP_df$TeamId == JAP_df$TeamId_First_Event & JAP_df$StatName_First_Event == "Faute def"), JAP_df$XPA_First_Event, JAP_df$XPF_First_Event)
JAP_df$XPA_First_Event <- ifelse((JAP_df$TeamId != JAP_df$TeamId_First_Event & JAP_df$StatName_First_Event != "Faute def") |
                                   (JAP_df$TeamId == JAP_df$TeamId_First_Event & JAP_df$StatName_First_Event == "Faute def"), JAP_df$XPF_First_Event, JAP_df$XPA_First_Event)
JAP_df$XPF_First_Event <- XPF_First_Eventnew

XPF_Short_Seqnew <- ifelse((JAP_df$TeamId != JAP_df$TeamId_Short_Seq & JAP_df$StatName_Short_Seq != "Faute def") |
                             (JAP_df$TeamId == JAP_df$TeamId_Short_Seq & JAP_df$StatName_Short_Seq == "Faute def"), JAP_df$XPA_Short_Seq, JAP_df$XPF_Short_Seq)
JAP_df$XPA_Short_Seq <- ifelse((JAP_df$TeamId != JAP_df$TeamId_Short_Seq & JAP_df$StatName_Short_Seq != "Faute def") |
                                 (JAP_df$TeamId == JAP_df$TeamId_Short_Seq & JAP_df$StatName_Short_Seq == "Faute def"), JAP_df$XPF_Short_Seq, JAP_df$XPA_Short_Seq)
JAP_df$XPF_Short_Seq <- XPF_Short_Seqnew

# XPF_End_Seqnew <- ifelse((JAP_df$TeamId != JAP_df$TeamId_End_Seq & JAP_df$StatName_End_Seq != "Faute def") |
#                              (JAP_df$TeamId == JAP_df$TeamId_End_Seq & JAP_df$StatName_End_Seq == "Faute def"), JAP_df$XPA_End_Seq, JAP_df$XPF_End_Seq)
# JAP_df$XPA_End_Seq <- ifelse((JAP_df$TeamId != JAP_df$TeamId_End_Seq & JAP_df$StatName_End_Seq != "Faute def") |
#                                      (JAP_df$TeamId == JAP_df$TeamId_End_Seq & JAP_df$StatName_End_Seq == "Faute def"), JAP_df$XPF_End_Seq, JAP_df$XPA_End_Seq)
# JAP_df$XPF_End_Seq <- XPF_End_Seqnew



#Création variable zone X 22m
JAP_df <- JAP_df %>% mutate(zones40 = case_when(
  X.sens <= 70 ~ "Zone en-but - 7m",
  X.sens > 150 & X.sens <=220 ~ "Zone 15m - 22m",
  X.sens > 70 & X.sens <= 150 ~ "Zone 7m - 15m",
  X.sens > 220 & X.sens <= 310 ~ "Zone 22m - 31m",
  TRUE ~ "Zone 31m - 40m"
)) %>% mutate(zone2240 = case_when(
  X.sens <= 220 ~ "22m",
  TRUE ~ "22-40m"
))

#Création zones latérales
JAP_df <- JAP_df %>%  mutate(zone_lat_fin_jap = case_when(
  Y.end >= 0 & Y.end <= 100 ~ "10m gauche",
  Y.end >100 & Y.end <= 600 ~ "Centre terrain",
  Y.end >600 & Y.end <= 700 ~ "10m droite"
))


#Création nouvelle variable indiquant à qui profite l'évenement
JAP_df$StatName_Poss_First_Event <- ifelse(JAP_df$TeamId != JAP_df$TeamId_First_Event, paste0(JAP_df$StatName_First_Event,"adv"), JAP_df$StatName_First_Event)
JAP_df$StatName_Poss_Short_Seq <- ifelse(JAP_df$TeamId != JAP_df$TeamId_Short_Seq, paste0(JAP_df$StatName_Short_Seq,"adv"), JAP_df$StatName_Short_Seq)
#JAP_df$StatName_Poss_End_Seq <- ifelse(JAP_df$TeamId != JAP_df$TeamId_End_Seq, paste0(JAP_df$StatName_End_Seq,"adv"), JAP_df$StatName_End_Seq)

#Calcul XPD de l'équipe jouant le coup de pied
JAP_df <- JAP_df %>% mutate(XPD_First_Event = XPF_First_Event - XPA_First_Event) %>% mutate(XPD_Short_Seq = XPF_Short_Seq - XPA_Short_Seq) %>% mutate(diff_XPD_jap = XPD_Short_Seq - XPD_prevRuck)
#%>% mutate(XPD_End_Seq = XPF_End_Seq - XPA_End_Seq)


#Création indicateur adversaire
JAP_df$TeamId_adv <- NA

for(i in 1:nrow(JAP_df)){

  current_game <- JAP_df$GameId[i]
  current_team <- JAP_df$TeamId[i]

  opponent_rows <- which(JAP_df$GameId == current_game & JAP_df$TeamId != current_team)

  if(length(opponent_rows) >= 1){
    JAP_df$TeamId_adv[JAP_df$GameId == current_game & JAP_df$TeamId == current_team] <- JAP_df$TeamId[opponent_rows[1]]
  }
}

JAP_df$CompetId <- stringr::str_sub(JAP_df$GameId,1,8) #Extraction id compétition
JAP_df_tsjap <- JAP_df
JAP_df$Type <- ifelse(JAP_df$Type == "Box" & JAP_df$Result %in% c("Kick in Touch (Full)", "Kick in Touch (Bounce)"), "Touch Kick", JAP_df$Type) #Box kicks qui vont en touche deviennent des touch kicks

#Choix des compétitions qui nous intéressent
jap_compet <- JAP_df %>% filter(CompetId %in% c("20238100", "20233010","20221910") | GameId %in% c(20232918010,20232932110)) %>% filter(Type != "NA")
jap_inter <- JAP_df %>% filter(CompetId %in% c("20238100", "20233010","20221910", "20228100", "20223010") | GameId %in% c(20232918010,20232932110))  %>% filter(Type != "NA")
jap_comp_2023 <- JAP_df %>% filter(CompetId %in% c("20238100", "20233010","20221910", "20232913", "20232411") | GameId %in% c(20232918010,20232932110)) %>% filter(Type != "NA")


average_XPD_Short_Seq_by_type <- aggregate(jap_compet$XPD_Short_Seq ~ jap_compet$TeamId_adv + jap_compet$Type, FUN=mean)



# Compute the count for each group
count_XPD_Short_Seq_by_type <- aggregate(jap_compet$XPD_Short_Seq ~ jap_compet$TeamId_adv + jap_compet$Type, FUN=length)

# Rename the new column to something meaningful
names(count_XPD_Short_Seq_by_type)[3] <- "Count"

# Merge the count back into your original data.frame
average_XPD_Short_Seq_by_type <- merge(average_XPD_Short_Seq_by_type, count_XPD_Short_Seq_by_type, by=c("jap_compet$TeamId_adv","jap_compet$Type"))


#avg_Time_JAP_by_team_def <- aggregate(jap_compet$diff_JAP_1TJ[which(jap_compet$StatName_Poss_First_Event == "Ruck")] ~ jap_compet$TeamId_adv +jap_compet$Type , FUN=mean)

avg_Time_JAP_by_team_off <- aggregate(jap_compet$diff_JAP_1TJ ~ jap_compet$TeamId+jap_compet$Type, FUN=mean)

avg_Time_JAP_by_team_alltype <- aggregate(jap_compet$diff_JAP_1TJ ~ jap_compet$TeamId, FUN=mean)




subset_jap_compet <- subset(jap_compet, StatName_Poss_First_Event == "Ruck" & diff_JAP_1TJ < 90 )

#avg_TimeJAP_beam_def <- aggregate(subset_jap_compet$diff_JAP_1TJ ~ subset_jap_compet$TeamId_adv + subset_jap_compet$Type , FUN=mean)

avg_TimeJAP_beam_def <- aggregate(subset_jap_compet$diff_JAP_1TJ ~ subset_jap_compet$TeamId + subset_jap_compet$Type , FUN=mean)

avg_TimeJAP_team_alltype <- aggregate(subset_jap_compet$diff_JAP_1TJ ~ subset_jap_compet$TeamId, FUN=mean)


# ###Parties calculs###
# #Calcul fréquence chaque type de coups de pieds
# prop_table <- prop.table(table(jap_compet$Type))*100
# print(prop_table)
# int_table <- prop.table(table(jap_inter$Type))*100
# print(int_table)
#
#
# #XP moyen par type de coup de pied
# mean_XP <- aggregate(cbind(XPF_End_Seq,XPA_End_Seq, XPD_End_Seq, XPF_Short_Seq, XPD_Short_Seq, XPA_Short_Seq, XPF_First_Event, XPA_First_Event, XPD_First_Event ) ~ Type, jap_compet, mean)
# print(mean_XP)
# mean_XP_First_Event <- aggregate(cbind(XPF_First_Event, XPA_First_Event, XPD_First_Event ) ~ Type, jap_compet, mean)
# print(mean_XP_First_Event)
# mean_XP_Short_Seq <- aggregate(cbind(XPF_Short_Seq, XPA_Short_Seq, XPD_Short_Seq) ~ Type, jap_compet, mean)
# print(mean_XP_Short_Seq)
# mean_XP_End_Seq <- aggregate(cbind(XPF_End_Seq,XPA_End_Seq, XPD_End_Seq) ~ Type, jap_compet, mean)
# print(mean_XP_End_Seq)
#
# #XP moyen par type de coup de pied dans les matchs internationaux
# mean_XP_int <- aggregate(cbind(XPF_End_Seq,XPA_End_Seq, XPD_End_Seq, XPF_Short_Seq, XPD_Short_Seq, XPA_Short_Seq, XPF_First_Event, XPA_First_Event, XPD_First_Event ) ~ Type, jap_inter, mean)
# print(mean_XP_int)
# mean_XP_First_Event_int <- aggregate(cbind(XPF_First_Event, XPA_First_Event, XPD_First_Event ) ~ Type, jap_inter, mean)
# print(mean_XP_First_Event_int)
# mean_XP_Short_Seq_int <- aggregate(cbind(XPF_Short_Seq, XPA_Short_Seq, XPD_Short_Seq) ~ Type, jap_inter, mean)
# print(mean_XP_Short_Seq_int)
# mean_XP_End_Seq_int <- aggregate(cbind(XPF_End_Seq,XPA_End_Seq, XPD_End_Seq) ~ Type, jap_inter, mean)
# print(mean_XP_End_Seq_int)
#
# #XP moyen par type de coup de pied dans tous les matchs choisis puis seulement internationaux
# #mean_both <- rbind(mean_df, mean_inter)
#
# #Fréquence des évenements après coups de pied
# event_short_seq <- prop.table(table(jap_compet$StatName_Poss_Short_Seq))*100
# event_end_seq <- prop.table(table(jap_compet$StatName_Poss_End_Seq))*100
# first_event <- prop.table(table(jap_compet$StatName_Poss_First_Event))*100
# data.frame(event_short_seq)
# event_end_seq
# first_event
#
# #Fréquence des évenements après coups de pied hors touch kick
# jap_compet_hstouche <- jap_compet %>%  filter(Type != "Touch Kick")
# event_short_seq_hstouche <- prop.table(table(jap_compet_hstouche$StatName_Poss_Short_Seq))*100
# event_end_seq_hstouche <- prop.table(table(jap_compet_hstouche$StatName_Poss_End_Seq))*100
# first_event_hstouche <- prop.table(table(jap_compet_hstouche$StatName_Poss_First_Event))*100
# event_short_seq_hstouche
# event_end_seq_hstouche
# first_event_hstouche
#
# #Fréquence des évenements après coups de pied de pression
# jap_compet_pression <- jap_compet %>%  filter(Type == "Pressure")
# event_short_seq_pression <- prop.table(table(jap_compet_pression$StatName_Poss_Short_Seq))*100
# event_end_seq_pression <- prop.table(table(jap_compet_pression$StatName_Poss_End_Seq))*100
# first_event_pression <- prop.table(table(jap_compet_pression$StatName_Poss_First_Event))*100
# data.frame(event_short_seq_pression)
# event_end_seq_pression
# first_event_pression
#
# #Fréquence des évenements après coups de pied territorial
# jap_compet_terr <- jap_compet %>%  filter(Type == "Territorial")
# event_short_seq_terr <- prop.table(table(jap_compet_terr$StatName_Poss_Short_Seq))*100
# event_end_seq_terr <- prop.table(table(jap_compet_terr$StatName_Poss_End_Seq))*100
# first_event_terr <- prop.table(table(jap_compet_terr$StatName_Poss_First_Event))*100
# data.frame(event_short_seq_terr)
# event_end_seq_terr
# first_event_terr
#
# #Fréquence des évenements après coups de pied en touche
# jap_compet_touch <- jap_compet %>%  filter(Type == "Touch Kick")
# event_short_seq_touch <- prop.table(table(jap_compet_touch$StatName_Poss_Short_Seq))*100
# event_short_seq_touch
#
#
# #Création tables selon zone 22
# jap_compet_enbut <- jap_compet %>%  filter(zonex22 == "Zone en-but - 7m")
# jap_compet_mi22 <- jap_compet %>%  filter(zonex22 == "Zone 7m - 15m")
# jap_compet_avant22 <- jap_compet %>%  filter(zonex22 == "Zone 15m - 22m")
#
# #Calculs des XPD après une séquence courte selon la zone des 22
# table_moy_zones <- aggregate(XPD_Short_Seq ~ Type + zonex22, jap_compet, mean)
# table_contingence <- xtabs(XPD_Short_Seq ~ Type + zonex22, table_moy_zones)
# table_contingence
#
# table_moy_zones_int <- aggregate(XPD_Short_Seq ~ Type + zonex22, jap_inter, mean)
# table_contingence_int <- xtabs(XPD_Short_Seq ~ Type + zonex22, table_moy_zones_int)
# table_contingence_int
#
# mean_XP_Short_Seq_enbut <- aggregate(cbind(XPF_Short_Seq, XPA_Short_Seq, XPD_Short_Seq) ~ Type, jap_compet_enbut, mean)
# print(mean_XP_Short_Seq_enbut)
# mean_XP_Short_Seq_mi22 <- aggregate(cbind(XPF_Short_Seq, XPA_Short_Seq, XPD_Short_Seq) ~ Type, jap_compet_mi22, mean)
# print(mean_XP_Short_Seq_mi22)
# mean_XP_Short_Seq_avant22 <- aggregate(cbind(XPF_Short_Seq, XPA_Short_Seq, XPD_Short_Seq) ~ Type, jap_compet_avant22, mean)
# print(mean_XP_Short_Seq_avant22)
#
# #Création tables selon zone 22 pour matchs internationaux
# jap_inter_enbut <- jap_inter %>%  filter(zonex22 == "Zone en-but - 7m")
# jap_inter_mi22 <- jap_inter %>%  filter(zonex22 == "Zone 7m - 15m")
# jap_inter_avant22 <- jap_inter %>%  filter(zonex22 == "Zone 15m - 22m")
#
# #Calculs des XP après une séquence courte selon la zone des 22 pour les matchs internationaux
# mean_XP_Short_Seq_enbut_int <- aggregate(cbind(XPF_Short_Seq, XPA_Short_Seq, XPD_Short_Seq) ~ Type, jap_inter_enbut, mean)
# print(mean_XP_Short_Seq_enbut_int)
# mean_XP_Short_Seq_mi22_int <- aggregate(cbind(XPF_Short_Seq, XPA_Short_Seq, XPD_Short_Seq) ~ Type, jap_inter_mi22, mean)
# print(mean_XP_Short_Seq_mi22_int)
# mean_XP_Short_Seq_avant22_int <- aggregate(cbind(XPF_Short_Seq, XPA_Short_Seq, XPD_Short_Seq) ~ Type, jap_inter_avant22, mean)
# print(mean_XP_Short_Seq_avant22_int)

mean(jap_compet_enbut$XPD_Short_Seq[jap_compet_enbut$Type == "Territorial"], na.rm = TRUE)



compte <- event_red_xpts %>% group_by(GameId) %>% count(a_supp)
compte <-compte[which(compte$a_supp == 1),]







jap_compet_tsjap$Duree <- as.numeric(jap_compet$Duree)

jap_compet <- jap_compet %>% mutate(diff_XPD = XPD_Short_Seq - XPD_prevRuck)




# Create a subset of the dataframe where Duree is not equal to 0
subset_jap_compet <- subset(jap_compet, Duree != 0 & StatName_prevRuck == "Ruck" )#& Type =="Pressure" )

# Run the aggregate function on the subset dataframe
avg_XPD_by_duree <- aggregate(Duree ~ diff_XPD + StatName_prevRuck + Type, subset_jap_compet, FUN=mean)



ggplot(data = subset_jap_compet, aes(x = Duree, y = diff_XPD)) +
  geom_point() +
  labs(x = "Duree", y = "diff_XPD", title = "Nuage de points de diff_XPD en fonction de Duree")



