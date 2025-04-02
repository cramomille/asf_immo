
#                                 EXPLORATIONS POUR LA PLANCHE SUR L'IMMOBILIER
#
#                                                                antoine beroud
#                                                                  aliette roux

library(sf)
library(mapsf)
library(RColorBrewer)

library(readxl)
library(data.table)

library(asf)
library(dplyr)
library(ggplot2)


###############################################################################
############################################################### FONDS D'ALIETTE

# com$dep <- substr(com$id_multi, 1,2)
# dep <- aggregate(com, by = list(com$dep), FUN = function(x) x[1])
# dep <- simplify_geom(dep, keep = 0.05)
# dep <- dep[!grepl("^97|^98", dep$dep), ]
# mf_map(dep)

# Recuperation des fonds de reference
load("C:/Users/Antoine Beroud/Desktop/Atlas/rexplo/data/aliette_roux/donnees/AR01_geog_constante.RData")
tabl_com <- d.comf.pass
com <- sf.comf
com <- st_as_sf(com)
com <- st_transform(com, 2154)

rm(d.comf.app, d.comf.pass, d.irisf.pass, sf.comf, sf.irisf)

tabl_com <- tabl_com[, c(1,4)]


###############################################################################
############################################################## DONNEES FISOSOFI

# Traitement du fichier filosofi (carroyage 1km)
filosofi <- st_read("data/filo/filo_2019.gpkg")

# On conserve le nombre d'individus et la somme des revenus pour calculer les salaires
# et en faisant 'logements' - 'menages proprietaires' - 'logements sociaux'
# on decide d'estimer le parc locatif prive
filo <- filosofi[, c(3,8,10,14:19)]
filo$log <- filo$log_av45 + filo$log_45_70 + filo$log_70_90 + filo$log_ap90 + filo$log_inc 
filo <- filo[, c(1,3,2,11,9)]

# Calcul de la surface des communes
com$area_com <- as.numeric(st_area(com))

# Calcul de la surface des carreaux
filo$area_caro <- as.numeric(st_area(filo))

# Decoupage des carreaux en fonction des communes
carro_decoup <- st_intersection(filo, com)

# Calcul de la surface de chaque morceau de carreau decoupe
carro_decoup$area_boucarro <- as.numeric(st_area(carro_decoup))

# Liste des colonnes a ajuster
cols <- head(names(filo), -1)

# Fonction pour ajuster les valeurs selon la part de recouvrement
by_area <- function(col) {
  carro_decoup[[col]] <- carro_decoup[[col]] * (carro_decoup$area_boucarro / carro_decoup$area_caro)
}

# Application de la fonction a toutes les colonnes de la liste
lapply(cols, by_area)

# Convertion du data.frame en un data.table pour reduire le temps de calcul
setDT(carro_decoup)

# Agregation (sum) sur toutes les colonnes numeriques sauf celles specifiees (id, geom)
carro_agreg <- carro_decoup[, lapply(.SD, sum), 
                            by = COMFC_CODE, 
                            .SDcols = !c("COMFC_CODE", "COMFA_CODE", "COMF_LIB", "geom")]

carro_agreg <- carro_agreg[, c(1:6)]

# Jointure entre le fond geographique et les donnees agregees au niveau communal
fond <- merge(com[, c(1:3)], carro_agreg, by.x = "COMFC_CODE", by.y = "COMFC_CODE", all.x = TRUE)

# Nettoyage et export
rm(filo, filosofi, carro_decoup, carro_agreg, cols, by_area)
st_write(fond, "export/fond_filo_com.gpkg")


###############################################################################
######################################################### INDICE D'ABORDABILITE

# Chargement du fichier filosofi
filo <- st_read("export/fond_filo_com.gpkg")
filo <- filo[, c(1,4:8)]
filo$geom <- NULL

filo$loc_priv <- round(filo$log - filo$log_soc - filo$men_prop, 1)
filo <- filo[, -c(4:6)]


# Chargement des fichiers dvf
dvf2019 <- read.csv("data/dvf_prep/dvf_2019.csv")
dvf2020 <- read.csv("data/dvf_prep/dvf_2020.csv")
dvf <- rbind(dvf2019, dvf2020)

dvf <- dvf[, -c(1,3,4,10,11)]

mar <- merge(tabl_com, dvf, by.x = "COM_CODE", by.y = "codecommune")
mar <- mar[, -c(1)]

# Fonction pour traiter un type de bien (Maison/Appartement)
filter_dvf <- function(dvf, type_bien) {

  # Filtrer les données pour le type de bien
  dvf_filtered <- dvf[dvf$type == type_bien, ]
  
  # Calcul des agregations par commune
  com_prix <- tapply(dvf_filtered$prix, dvf_filtered$COMF_CODE, median, na.rm = TRUE)
  com_nomb <- tapply(dvf_filtered$prix, dvf_filtered$COMF_CODE, length)
  
  # Convertir en data.frame
  result <- data.frame(commune = names(com_prix),
                       prix = as.vector(com_prix),
                       nb = as.vector(com_nomb))
  
  # Renommer les colonnes selon le type de bien
  colnames(result)[which(names(result) == "prix")] <- paste0("median_prix_", tolower(type_bien))
  colnames(result)[which(names(result) == "nb")] <- paste0("nb_", tolower(type_bien))
  
  return(result)
}
  
maison <- filter_dvf(mar, "Maison")
appart <- filter_dvf(mar, "Appartement")

dvf_mar <- merge(maison, appart, by = "commune", all = TRUE)
rm(dvf2019, dvf2020, dvf, mar, maison, appart)


# Chargement du fichier sur les loyers
loyer <- read.csv("data/loyer/indicateurs-loyers-appartements.csv", sep = ';')

loyer <- loyer[, c(2,8)]
loyer$loy_m2  <- as.numeric(gsub(",", ".", loyer$loypredm2))
loyer$INSEE <- ifelse(nchar(loyer$INSEE) == 4, paste0("0", loyer$INSEE), loyer$INSEE)
loyer <- loyer[, c(1,3)]

mar <- merge(tabl_com, loyer, by.x = "COM_CODE", by.y = "INSEE")
mar <- mar[, -c(1)]

loyer_mar <- aggregate(loy_m2 ~ COMF_CODE, data = mar, FUN = mean)
rm(loyer, mar)


# Tableau de donnees final
data <- merge(filo, loyer_mar, by.x = "COMFC_CODE", by.y = "COMF_CODE", all.x = TRUE)
data <- merge(data, dvf_mar, by.x = "COMFC_CODE", by.y = "commune", all.x = TRUE)


###############################################################################
################################################################### PACKAGE ASF
data <- data
fond <- st_read("export/fond_filo_com.gpkg")
fond <- fond[, c(1,3)]
fond <- fond[!grepl("^97|^98", fond$COMFC_CODE), ]

zoom_created <- create_zoom(fond = fond,
                            villes = c("Marseille", "Lyon", "Toulouse", "Nantes", "Montpellier",
                                       "Bordeaux", "Lille", "Rennes", "Reims", "Dijon",
                                       "Angers", "Grenoble", "Clermont-Ferrand", "Tours", "Perpignan",
                                       "Besancon", "Rouen", "La Rochelle", "Le Havre", "Nice"
                                       ),
                            buffer = 10000)

zooms <- zoom_created$zooms
labels <- zoom_created$labels

fond <- simplify_geom(fond, keep = 0.1)

fondata <- merge_fondata(data = data,
                         fond = fond,
                         zoom = zooms,
                         id = c("COMFC_CODE", "COMFC_CODE"))



# Nombre d'annee de revenu pour acheter un bien
fondata$maison_abord <- (fondata$median_prix_maison * 0.9) / (fondata$ind_snv / fondata$ind)
fondata$appart_abord <- (fondata$median_prix_appart * 0.9) / (fondata$ind_snv / fondata$ind)

# Abordabilite du loyer (pourcentage du salaire)
fondata$location_abord <- (fondata$loy_m2 * 49) / (fondata$ind_snv / fondata$ind / 12) * 100

# Indice final
# Calcul des quartiles
q_m <- quantile(fondata$maison_abord, probs = c(0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)
q_a <- quantile(fondata$appart_abord, probs = c(0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)

# Création de la colonne 'typo'
fondata$typo_m <- with(fondata, 
                       ifelse(maison_abord < q_m[1] & location_abord < 30, 1,
                       ifelse(maison_abord < q_m[1] & location_abord > 30, 2,
                       ifelse(maison_abord >= q_m[1] & maison_abord < q_m[2] & location_abord < 30, 3,
                       ifelse(maison_abord >= q_m[1] & maison_abord < q_m[2] & location_abord > 30, 4,
                       ifelse(maison_abord >= q_m[2] & maison_abord < q_m[3] & location_abord < 30, 5,
                       ifelse(maison_abord >= q_m[2] & maison_abord < q_m[3] & location_abord > 30, 6,
                       ifelse(maison_abord >= q_m[3] & maison_abord < q_m[4] & location_abord < 30, 7,
                       ifelse(maison_abord >= q_m[3] & maison_abord < q_m[4] & location_abord > 30, 8,
                       ifelse(maison_abord >= q_m[4] & maison_abord < q_m[5] & location_abord < 30, 9,
                       ifelse(maison_abord >= q_m[4] & maison_abord < q_m[5] & location_abord > 30, 10,
                       ifelse(maison_abord >= q_m[5] & location_abord < 30, 11,
                       ifelse(maison_abord >= q_m[5] & location_abord > 30, 12, 
                       NA)))))))))))))

fondata$typo_a <- with(fondata, 
                       ifelse(appart_abord < q_a[1] & location_abord < 30, 1,
                       ifelse(appart_abord < q_a[1] & location_abord > 30, 2,
                       ifelse(appart_abord >= q_a[1] & appart_abord < q_a[2] & location_abord < 30, 3,
                       ifelse(appart_abord >= q_a[1] & appart_abord < q_a[2] & location_abord > 30, 4,
                       ifelse(appart_abord >= q_a[2] & appart_abord < q_a[3] & location_abord < 30, 5,
                       ifelse(appart_abord >= q_a[2] & appart_abord < q_a[3] & location_abord > 30, 6,
                       ifelse(appart_abord >= q_a[3] & appart_abord < q_a[4] & location_abord < 30, 7,
                       ifelse(appart_abord >= q_a[3] & appart_abord < q_a[4] & location_abord > 30, 8,
                       ifelse(appart_abord >= q_a[4] & appart_abord < q_a[5] & location_abord < 30, 9,
                       ifelse(appart_abord >= q_a[4] & appart_abord < q_a[5] & location_abord > 30, 10,
                       ifelse(appart_abord >= q_a[5] & location_abord < 30, 11,
                       ifelse(appart_abord >= q_a[5] & location_abord > 30, 12, 
                       NA)))))))))))))

palette <- c(
  "1" = "#6561a9",
  "2" = "#00a183",
  "3" = "#9b99cc",
  "4" = "#8ccaae",
  "5" = "#d3d5ed",
  "6" = "#cce5d8",
  
  "7" = "#ffe8b6",
  "8" = "#f9c4a7",
  "9" = "#fdc75f",
  "10" = "#f08159",
  "11" = "#f59c00",
  "12" = "#dc0d15"
)

mf_map(fondata,
       var = "typo_a", 
       type = "typo",
       pal = palette,
       border = NA)

st_write(fondata, "test.gpkg")

































###############################################################################
####################################################################### MATRICE

# Definition des fichiers et des valeurs de division
data <- c("data/dvf_prep/dvf_2014.csv", 
          "data/dvf_prep/dvf_2015.csv", 
          "data/dvf_prep/dvf_2016.csv",
          "data/dvf_prep/dvf_2017.csv", 
          "data/dvf_prep/dvf_2018.csv", 
          "data/dvf_prep/dvf_2019.csv",
          "data/dvf_prep/dvf_2020.csv", 
          "data/dvf_prep/dvf_2021.csv", 
          "data/dvf_prep/dvf_2022.csv",
          "data/dvf_prep/dvf_2023.csv",
          "data/dvf_prep/dvf_2024.csv"
          )

rd1 <- c(5720, #2013
         5560, #2014
         5401, #2015
         5416, #2016
         5430, #2017
         5518, #2018
         5605, #2019
         6285, #2020
         6966, #2021
         7285, #2022
         7604  #2023
         )

rd5 <- c(16979, #2013
         17214, #2014
         17448, #2015
         17609, #2016
         17770, #2017
         18070, #2018
         18370, #2019
         18811, #2020
         19252, #2021
         19893, #2022
         20534  #2023
         )

rd9 <- c(36068, #2013
         36506, #2014
         36943, #2015
         37286, #2016
         37628, #2017
         38323, #2018
         39018, #2019
         39787, #2020
         40555, #2021
         42029, #2022
         43503  #2023
         )


# Initialisation d'une liste pour stocker les resultats
result <- list()

# Boucle sur les fichiers et les valeurs de division
for (i in seq_along(data)) {
  # Chargement du fichier
  dvf <- read.csv(data[i])
  dvf <- dvf[, c(2,5,6)]
  
  # Filtrage du type de bien (Maison ou Appartement)
  type <- dvf[dvf$type == "Appartement", ]
  
  # Calcul des deciles
  decile <- quantile(type$prix, probs = seq(0.1, 0.9, 0.1))
  
  # Stockage des resultats
  result[[i]] <- data.frame(
    # decile = decile,
    abord = round(decile / rd1[i], 1)
  )
  
  # Renommer les colonnes pour chaque annee
  colnames(result[[i]]) <- paste0(colnames(result[[i]]), "_", substr(data[i], 18, 22))
}

# Fusionner tous les tableaux par la colonne des deciles
tableau_a_d1 <- do.call(cbind, result)











library(ggplot2)
library(reshape2)

tableau <- tableau_a_d1

tableau$decile = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")

# Transformation des donnees en format long
tableau_long <- melt(tableau, id.vars = "decile")

# Renommer les colonnes pour ggplot
colnames(tableau_long) <- c("decile", "annee", "valeur")

# Création d'une colonne categorielle pour les classes
tableau_long$classe <- cut(tableau_long$valeur,
                           breaks = c(0, 5, 10, 15, 30, Inf),
                           labels = c("1", "2", "3", "4", "5"),
                           right = FALSE
)

# Couleurs associées aux classes
couleurs_classe <- c("1" = "#bce4fa",
                     "2" = "#e3e3e3",
                     "3" = "#f8c9df",
                     "4" = "#f088b6",
                     "5" = "#e8308a")

# Création du heatmap avec ggplot
ggplot(tableau_long, aes(x = annee, y = decile, fill = classe)) +
  geom_tile(color = "white") +  # creation des carres
  scale_fill_manual(values = couleurs_classe) +  # couleurs
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # rotation des labels
  labs(title = "Évolution des valeurs par décile et année", 
       fill = "Classe de Valeur")








