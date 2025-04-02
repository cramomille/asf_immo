
  
# Ce script est adapaté pour le cours d'initiation à R du M2 Géoprisme
# (R. Le Goix, Université Paris Cité, Sept. 2024)
# 
# Source : https://github.com/bmericskay/Script1-DVF/blob/main/Script1.Rmd
# 
# Boris Mericskay et Florent Demoraes, « Préparer et analyser les données de "Demandes
# de valeurs foncières" en open data : proposition d’une méthodologie reproductible »,
# Cybergeo: European Journal of Geography [En ligne], Cartographie, Imagerie, SIG,
# document 1031, mis en ligne le 28 septembre 2022, consulté le 25 septembre 2024.
# URL : http://journals.openedition.org/cybergeo/39583 ;
# DOI : https://doi.org/10.4000/cybergeo.39583


# 1 - PRÉPARATION DES DONNÉES DVF OPENDATA

# Cette première partie du script a comme objectif de préparer les données DVF en
# opendata avant de commencer les analyses (nettoyage, filtrage, agrégation,...).
# 
# Seul le package `tidyverse` dédié à la manipulation de données est nécessaire.
#
# Les étapes de la préparation des données DVF. Source : Mericskay and Demoraes (2022)


library(tidyverse)

# Lecture et assemblage des fichiers DVF
DVF <- read.csv("data/dvf/full_2024.csv", sep = ",", fileEncoding = "UTF-8")

# Etape1 > Sélection des mutations de type "Ventes" de "Maison" et "Appartement'
etape1 <- DVF %>% filter(nature_mutation == "Vente")
etape1bis <- etape1 %>% filter(type_local == "Maison" | type_local == "Appartement")

# Etape2 > Sélection et renommage des variables
etape2 <- etape1bis %>% select(
  id = id_mutation, 
  disposition = numero_disposition, 
  parcelle = id_parcelle, 
  date = date_mutation, 
  nature = nature_mutation, 
  codecommune = code_commune, 
  departement = code_departement, 
  type = type_local, 
  surface = surface_reelle_bati, 
  piece = nombre_pieces_principales, 
  prix = valeur_fonciere, 
  latitude, 
  longitude)

# Etape3 > Remplacement des cellules vides par des NA et suppression des NA
etape2[etape2 == ""] <- NA
etape3 <- etape2 %>% na.omit()

# Regrouper les transactions selon l'ID, la surface et la vente
unique <- etape3 %>% distinct(id, prix, surface)
nbunique <- unique %>% group_by(id) %>% summarise(nb = n())

# Etape4 > Sélections des mutations simples
etape4 <- nbunique %>% filter(nb == 1)

# Etape5 > Jointure attributaire pour récupérer les informations de la mutation
merge <- merge(etape4,etape3, by = "id")
etape5 <- merge %>% 
  distinct(id, .keep_all = TRUE) %>% 
  select(id, date, type, nature, codecommune, prix, surface, piece, latitude, longitude)

# Modification des formats des colonnes
etape5$prix <- as.numeric(etape5$prix)
etape5$surface <- as.numeric(etape5$surface)
etape5$piece <- as.numeric(etape5$piece)

## Création du prix au m² et filtre des valeurs extrêmes et aberrantes 
# Etape6 > Création de la variable prix/m²
etape6 <- etape5 %>% mutate(prixm2 = prix/surface)

maison <- etape6 %>% filter(type == 'Maison')
appart <- etape6 %>% filter (type == 'Appartement')

q_m_prix <- quantile(maison$prix, probs = c(0.005, 0.995))
q_a_prix <- quantile(appart$prix, probs = c(0.005, 0.995))

q_m_surf <- quantile(maison$surface, probs = c(0.005, 0.995))
q_a_surf <- quantile(appart$surface, probs = c(0.005, 0.995))

# Etape 7 > Sélection des bornes de prix (1er et dernier percentile) et de surface
etape7 <- etape6 %>% 
  filter(case_when(type == 'Appartement' ~  between(prix, q_a_prix[1], q_a_prix[2])) | 
           case_when(type == 'Maison' ~  between(prix, q_m_prix[1], q_m_prix[2]))) %>% 
  filter(case_when(type == 'Appartement' ~  between(surface, q_a_surf[1], q_a_surf[2])) | 
           case_when(type == 'Maison' ~  between(surface, q_m_surf[1], q_m_surf[2])))

# Transformation de la date en année
etape7$date <- as.character(etape7$date)
etape7 <- etape7 %>% mutate(ANNEE = substr(etape7$date, 1, 4))

# Arrondir les variables numériques
etape7$prix <- round(etape7$prix)
etape7$prixm2 <- round(etape7$prixm2)

# Etape8 > Structuration du jeu de données final
DVFOK <- etape7 %>% select(id, date, annee = ANNEE, type, prix, surface, prixm2, codecommune, latitude, longitude)

# Ecrire le jeu de données final en csv
write.csv(DVFOK, 'data/dvf_post/dvf_2024.csv')