jap <- subset(event_sub, StatName == "Jeu au pied" & X.sens <= 400)
terr <- subset(jap, Type == "Territorial" & Duree != 0)
terr <- terr %>% filter(Duree > 1.24)   %>% arrange(GameId, TimeVideo) %>% mutate(diff_XPD = XPFseq - xPtsFor, XPA_diff = XPAseq - xPtsAgainst, distX = X.end - X.sens, distance = sqrt((X.end - X.sens)^2 + (Y.end - Y.sens)^2), hauteur = distance/Duree) %>% filter(distance >100)
nbterr <- data.frame(table(terr$Joueur))
#nbterr <- nbterr[which(nbterr$Freq >= 10),]
nbterr
eff_terr <- data.frame(aggregate(cbind(Duree, distance/10,hauteur*0.36, diff_XPD) ~ Joueur, data = terr, FUN = function(x) c(mean = mean(x, na.rm = TRUE))))
eff_terr <- eff_terr %>%  left_join(nbterr, by = join_by("Joueur" == "Var1")) %>% filter(Freq >= 13) %>%  rename(NbBox = Freq, vitesse = V3, distance = V2) %>%  arrange(desc(distance)) %>% mutate(Duree = round(Duree,2)) %>% mutate(distance = round(distance,2)) %>% mutate(vitesse = round(vitesse,2)) %>% mutate(diff_XPD = round(diff_XPD,2))
eff_terr <- eff_terr[,c(1,3,2,4,5,6)]
eff_terr




