
#                                 EXPLORATIONS POUR LA PLANCHE SUR L'IMMOBILIER
#
#                                                                antoine beroud
#                                                                  aliette roux

library(sf)
library(mapsf)

# library(readxl)
# library(data.table)

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
rm(d.comf.app, d.comf.pass, d.irisf.pass, sf.comf, sf.irisf)

tabl_com <- tabl_com[, c(1,4)]

# Recuperation des communes regroupees
load("C:/Users/Antoine Beroud/Desktop/Atlas/rexplo/data/aliette_roux/donnees/AR02_maille_IRISr.RData")
iris <- sf.irisr
rm(d.irisr.app, d.irisr.etapes, d.irisr.pass, sf.irisr)

# Agregation en communes
com <- aggregate(iris, by = list(iris$COMF_CODE_MULTI), FUN = function(x) x[1])
com <- com[, c(1,7,10)]

colnames(com)[1] <- "id_multi"

com <- st_as_sf(com)
com <- st_transform(com, 2154)

summary(nchar(com$id_multi))

# Decomposition des identifiants agreges en une liste
list_id <- strsplit(com$id_multi, " \\| ")

# Creation d'une table d'association entre chaque commune et son id_aggreg
tabl_id <- data.frame(
  id_comf = unlist(list_id),
  id_multi = rep(com$id_multi, sapply(list_id, length))
)

summary(nchar(tabl_id$id_multi))
rm(iris, list_id)



###############################################################################
######################################################### INDICE D'ABORDABILITE

# Chargement du fichier sur les donnees FILOCOM -------------------------------
mar_revenu <- read.csv("data/decile_revucm_ar.csv")
mar_revenu <- mar_revenu[, c(2,127)]


# Chargement des fichiers dvf -------------------------------------------------
dvfa <- read.csv("data/dvf_prep/dvf_2023.csv")
dvfb <- read.csv("data/dvf_prep/dvf_2024.csv")
dvf <- rbind(dvfa, dvfb)

dvf <- dvf[, -c(1,3,4,10,11)]

mar <- merge(tabl_com, dvf, by.x = "COM_CODE", by.y = "codecommune")
mar <- mar[, -c(1)]
mar <- merge(tabl_id, mar, by.x = "id_comf", by.y = "COMF_CODE")
mar <- mar[, -c(1)]

# Fonction pour traiter un type de bien (Maison/Appartement)
filter_dvf <- function(dvf, type_bien) {
  
  # Filtrer les données pour le type de bien
  dvf_filtered <- dvf[dvf$type == type_bien, ]
  
  # Calcul des agregations par commune
  com_prix <- tapply(dvf_filtered$prix, dvf_filtered$id_multi, median, na.rm = TRUE)
  com_nomb <- tapply(dvf_filtered$prix, dvf_filtered$id_multi, length)
  
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

mar_dvf <- merge(maison, appart, by = "commune", all = TRUE)

rm(dvfa, dvfb, dvf, mar, maison, appart)


# Chargement du fichier de l'Observatoire des Territoires ---------------------
loyer <- read.csv("data/loyer/base_OT_2024.csv", skip = 2, header = TRUE, sep = ";")

loyer[, 3] <- as.numeric(loyer[, 3])
loyer[, 4] <- as.numeric(loyer[, 4])

colnames(loyer)[3:4] <- c("loyer_mai", "loyer_app")

mar <- merge(tabl_com, loyer, by.x = "COM_CODE", by.y = "Code")
mar <- mar[, -c(1)]
mar <- merge(tabl_id, mar, by.x = "id_comf", by.y = "COMF_CODE")
mar <- mar[, -c(1)]

mar_loyer <- aggregate(cbind(loyer_mai, loyer_app) ~ id_multi, data = mar, FUN = mean)
mar_loyer[, -1] <- round(mar_loyer[, -1], 2)

rm(loyer, mar)



###############################################################################
################################################################### PACKAGE ASF

# Traitement sur les donnees --------------------------------------------------


# Creation du fond et des zooms -----------------------------------------------
iris <- st_read("C:/Users/Antoine Beroud/Desktop/Atlas/rexplo/data/aliette_roux/donnees/fond_irisf.gpkg")
com <- aggregate(iris, by = list(iris$COMF_CODE), FUN = function(x) x[1])


fond <- 
  fond <- fond[!grepl("^98", fond$id_multi), ]

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
                         id = c("id_multi", "id_multi"))



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



