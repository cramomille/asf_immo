
#                                 EXPLORATIONS POUR LA PLANCHE SUR L'IMMOBILIER
#
#                                                                antoine beroud
#                                                                renaud le goix
#                                                                  aliette roux

library(sf)
library(mapsf)

# library(readxl)
# library(data.table)

library(asf)
# library(dplyr)
# library(ggplot2)


###############################################################################
############################################################### FONDS D'ALIETTE

# Recuperation des fonds de reference
load("C:/Users/Antoine Beroud/Desktop/rexplo/input/mar/donnees/AR01_geog_constante.RData")
tabl_com <- d.comf.pass
rm(d.comf.app, d.comf.pass, d.irisf.pass, sf.comf, sf.irisf)

tabl_com <- tabl_com[, c(1,4)]

# Recuperation des communes regroupees
load("C:/Users/Antoine Beroud/Desktop/rexplo/input/mar/donnees/AR02_maille_IRISr.RData")
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
mar_revenu <- read.csv("input/decile_revucm_comar.csv")
mar_revenu <- mar_revenu[, c(2,127)]


# Chargement des fichiers dvf -------------------------------------------------
dvf_2014 <- "https://sharedocs.huma-num.fr/wl/?id=vp4DTsuh5ctsBwGTzCSzgdKvZ3HnreAf&mode=grid&download=1"
dvf_2015 <- "https://sharedocs.huma-num.fr/wl/?id=QJ3AiWOCYVCkYN6Z0FzqI2yMM7Fu0Jhp&mode=grid&download=1"
dvf_2016 <- "https://sharedocs.huma-num.fr/wl/?id=OshgupIqPkg70hEMB7DdSpbsFDuTeAMN&mode=grid&download=1"
dvf_2017 <- "https://sharedocs.huma-num.fr/wl/?id=E0Xc2Ahyb0UUGoHFL6JR704dpQnUE7wK&mode=grid&download=1"
dvf_2018 <- "https://sharedocs.huma-num.fr/wl/?id=I22yu03q8W53XEFSb0voebmdi0ORUzUl&mode=grid&download=1"
dvf_2019 <- "https://sharedocs.huma-num.fr/wl/?id=6muOpXEStHm1Y56YUNv93n14zx2QSQ9i&mode=grid&download=1"
dvf_2020 <- "https://sharedocs.huma-num.fr/wl/?id=iq9S63LevHYxoDMCkL01bNQBeE4YlWYx&mode=grid&download=1"
dvf_2021 <- "https://sharedocs.huma-num.fr/wl/?id=XYI1SDuWfYRXfCtuz0jvcz7C4LuLi5Qg&mode=grid&download=1"
dvf_2022 <- "https://sharedocs.huma-num.fr/wl/?id=5sYwnTlHiFAiTtgD9ZqUeNuEAUTo5T7F&mode=grid&download=1"
dvf_2023 <- "https://sharedocs.huma-num.fr/wl/?id=4l09Pfh8OGPICchf9PQEw4X4kdvjOR5P&mode=grid&download=1"
dvf_2024 <- "https://sharedocs.huma-num.fr/wl/?id=oSMYbBxT6OWaePSLPydnXOJYoQE3tOID&mode=grid&download=1"

a <- read.csv(dvf_2023)
b <- read.csv(dvf_2024)

dvf <- rbind(a, b)

dvf <- dvf[, -c(1,3,4,10,11)]

dvf <- dvf[dvf$type == "Appartement", ]
mean(dvf$surface)

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

rm(a, b, dvf, mar, maison, appart)


# Chargement du fichier de l'Observatoire des Territoires ---------------------
loyer <- read.csv("input/base_OT_2024.csv", skip = 2, header = TRUE, sep = ";")

loyer[, 3] <- as.numeric(loyer[, 3])
loyer[, 4] <- as.numeric(loyer[, 4])

colnames(loyer)[3:4] <- c("loyer_mai", "loyer_app")

# Comme il manque les arrondissements on dedouble les communes concernees
arr_com <- tabl_com[grepl("^751|^132|^6938", tabl_com$COM_CODE), ]
arr_com$id <- substr(arr_com$COM_CODE, 1, 2)

arr_loyer <- loyer[grepl("75056|13055|69123", loyer$Code), ]
arr_loyer$id <- substr(arr_loyer$Code, 1, 2)

arr <- merge(arr_com, arr_loyer, by = "id")
arr <- arr[, c(2,5:7)]
colnames(arr)[1] <- "Code"

loyer <- loyer[!grepl("75056|13055|69123", loyer$Code), ]

loyer <- rbind(arr, loyer)


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
data <- merge(mar_revenu, mar_loyer, by.x = "comar", by.y = "id_multi", all = TRUE)
data <- merge(data, mar_dvf, by.x = "comar", by.y = "commune", all = TRUE)

# Creation du fond et des zooms -----------------------------------------------
comar <- "https://sharedocs.huma-num.fr/wl/?id=ompFGNW6dYfyBv2pk2HxLA0x7yA3fc8C&mode=grid&download=1"

fond <- st_read(comar)

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
                         id = c("comar", "id_multi"))



# Nombre d'annee de revenu pour acheter un bien
fondata$abord_mai <- (fondata$median_prix_maison * 0.9) / fondata$d5_2022 
fondata$abord_app <- (fondata$median_prix_appart * 0.9) / fondata$d5_2022

# Abordabilite du loyer (pourcentage du salaire)
fondata$abord_mai_loc <- (fondata$loyer_mai * 98) / (fondata$d5_2022 / 12) * 100
fondata$abord_app_loc <- (fondata$loyer_app * 49) / (fondata$d5_2022 / 12) * 100

# Indice final
# Calcul des quartiles
q_m <- quantile(fondata$abord_mai, probs = c(0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)
q_a <- quantile(fondata$abord_app, probs = c(0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)

# Création de la colonne 'typo'
fondata$typo_m <- with(fondata, 
                       ifelse(abord_mai < q_m[1] & abord_mai_loc < 30, 1,
                       ifelse(abord_mai < q_m[1] & abord_mai_loc > 30, 2,
                       ifelse(abord_mai >= q_m[1] & abord_mai < q_m[2] & abord_mai_loc < 30, 3,
                       ifelse(abord_mai >= q_m[1] & abord_mai < q_m[2] & abord_mai_loc > 30, 4,
                       ifelse(abord_mai >= q_m[2] & abord_mai < q_m[3] & abord_mai_loc < 30, 5,
                       ifelse(abord_mai >= q_m[2] & abord_mai < q_m[3] & abord_mai_loc > 30, 6,
                       ifelse(abord_mai >= q_m[3] & abord_mai < q_m[4] & abord_mai_loc < 30, 7,
                       ifelse(abord_mai >= q_m[3] & abord_mai < q_m[4] & abord_mai_loc > 30, 8,
                       ifelse(abord_mai >= q_m[4] & abord_mai < q_m[5] & abord_mai_loc < 30, 9,
                       ifelse(abord_mai >= q_m[4] & abord_mai < q_m[5] & abord_mai_loc > 30, 10,
                       ifelse(abord_mai >= q_m[5] & abord_mai_loc < 30, 11,
                       ifelse(abord_mai >= q_m[5] & abord_mai_loc > 30, 12, 
                       NA)))))))))))))

fondata$typo_a <- with(fondata, 
                       ifelse(abord_app < q_a[1] & abord_app_loc < 30, 1,
                       ifelse(abord_app < q_a[1] & abord_app_loc > 30, 2,
                       ifelse(abord_app >= q_a[1] & abord_app < q_a[2] & abord_app_loc < 30, 3,
                       ifelse(abord_app >= q_a[1] & abord_app < q_a[2] & abord_app_loc > 30, 4,
                       ifelse(abord_app >= q_a[2] & abord_app < q_a[3] & abord_app_loc < 30, 5,
                       ifelse(abord_app >= q_a[2] & abord_app < q_a[3] & abord_app_loc > 30, 6,
                       ifelse(abord_app >= q_a[3] & abord_app < q_a[4] & abord_app_loc < 30, 7,
                       ifelse(abord_app >= q_a[3] & abord_app < q_a[4] & abord_app_loc > 30, 8,
                       ifelse(abord_app >= q_a[4] & abord_app < q_a[5] & abord_app_loc < 30, 9,
                       ifelse(abord_app >= q_a[4] & abord_app < q_a[5] & abord_app_loc > 30, 10,
                       ifelse(abord_app >= q_a[5] & abord_app_loc < 30, 11,
                       ifelse(abord_app >= q_a[5] & abord_app_loc > 30, 12, 
                       NA)))))))))))))

palette <- c(
  "1" = "#00a183",
  "2" = "#6561a9",
  "3" = "#8ccaae",
  "4" = "#9b99cc",
  "5" = "#cce5d8",
  "6" = "#d3d5ed",
  
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



palette <- c(
  "2" = "#6561a9",
  "3" = "#8ccaae",
  "4" = "#9b99cc",
  "6" = "#d3d5ed",
  
  "7" = "#ffe8b6",
  "8" = "#f9c4a7",
  "10" = "#f08159",
  "12" = "#dc0d15"
)

mf_map(fondata,
       var = "typo_m", 
       type = "typo",
       pal = palette,
       border = NA)
