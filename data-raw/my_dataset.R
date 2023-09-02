## ------------------ Chemins ------------------------------------------------------------------

# Récupérations d'infos
path = unlist(strsplit(getwd(), "/"))[1]
username = unlist(strsplit(unlist(strsplit(getwd(), "Users/"))[2], "/"))[[1]]
mac_ou_pc = ifelse(substr(getwd(),1,1)!="/", "PC", "mac")
# Chemins (depot, archives et bdd)
if (mac_ou_pc=="PC"){
  chemin_depot = paste0(path, "/Users/", username, "/ffr.fr/BDD_Power BI_Master Compte_France Rugby - Documents/General/France Jeunes Masculin/Depot donnees Jeunes/GPS")
  chemin_archives = paste0(path, "/Users/", username, "/ffr.fr/BDD_Power BI_Master Compte_France Rugby - Documents/General/France Jeunes Masculin/Archives donnees Jeunes/GPS")
  chemin_bdd = paste0(path, "/Users/", username, "/ffr.fr/BDD_Power BI_Master Compte_France Rugby - Documents/General/France Jeunes Masculin/Bases de donnees Jeunes")
  if (identical(list.files(chemin_depot), character(0))){
    chemin_depot = "D:/ffr.fr/BDD_Power BI_Master Compte_France Rugby - Documents/General/France Jeunes Masculin/Depot donnees Jeunes/GPS"
    chemin_archives = "D:/ffr.fr/BDD_Power BI_Master Compte_France Rugby - Documents/General/France Jeunes Masculin/Archives donnees Jeunes/GPS"
    chemin_bdd = "D:/ffr.fr/BDD_Power BI_Master Compte_France Rugby - Documents/General/France Jeunes Masculin/Bases de donnees Jeunes"
  }
}
else {
  chemin_depot = paste0("/Users/", username, "/Library/CloudStorage/OneDrive-Bibliothèquespartagées-ffr.fr/BDD_Power BI_Master Compte_France Rugby - Documents/General/France Jeunes Masculin/Depot donnees Jeunes/GPS")
  chemin_archives = paste0("/Users/", username, "/Library/CloudStorage/OneDrive-Bibliothèquespartagées-ffr.fr/BDD_Power BI_Master Compte_France Rugby - Documents/General/France Jeunes Masculin/Archives donnees Jeunes/GPS")
  chemin_bdd = paste0("/Users/", username, "/Library/CloudStorage/OneDrive-Bibliothèquespartagées-ffr.fr/BDD_Power BI_Master Compte_France Rugby - Documents/General/France Jeunes Masculin/Bases de donnees Jeunes")
}
