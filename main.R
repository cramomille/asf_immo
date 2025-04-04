
#                                 EXPLORATIONS POUR LA PLANCHE SUR L'IMMOBILIER
#
#                                                                antoine beroud
#                                                                renaud le goix
#                                                                  aliette roux

library(sf)
library(asf)
library(mapsf)

###############################################################################
############################################################### FONDS D'ALIETTE

# Recuperation des fonds de reference
load("C:/Users/Antoine Beroud/Desktop/rexplo/input/mar/donnees/AR01_geog_constante.RData")
tabl_com <- d.comf.pass
tabl_com <- tabl_com[, c(1,4)]

# Recuperation des communes regroupees
load("C:/Users/Antoine Beroud/Desktop/rexplo/input/mar/donnees/AR02_maille_IRISr.RData")
iris <- sf.irisr

# Agregation en communes
com <- aggregate(iris, by = list(iris$COMF_CODE_MULTI), FUN = function(x) x[1])
com <- com[, c(1,7,10)]
com <- st_as_sf(com)
com <- st_transform(com, 2154)

colnames(com)[1] <- "comar"

summary(nchar(com$comar))

# Decomposition des identifiants agreges en une liste
list_id <- strsplit(com$comar, " \\| ")

# Creation d'une table d'association entre chaque commune et son regroupement de communes
tabl_id <- data.frame(
  COMF_CODE = unlist(list_id),
  comar = rep(com$comar, sapply(list_id, length))
)

summary(nchar(tabl_id$comar))

# Creation d'une table de passage globale
tabl <- merge(tabl_com, tabl_id, by = "COMF_CODE", all = TRUE)
tabl <- tabl[, c(2,1,3)]
# tabl <- tabl[!grepl("75056|13055|69123", tabl$COMF_CODE), ]

# Suppression des donnees inutilisees
rm(d.comf.app, d.comf.pass, d.irisf.pass, sf.comf, sf.irisf)
rm(d.irisr.app, d.irisr.etapes, d.irisr.pass, sf.irisr)
rm(iris, com, list_id, tabl_com, tabl_id)


###############################################################################
############################################ TRAITEMENT DES FICHIERS DE DONNEES

# Chargement du fichier FILOCOM -----------------------------------------------
mar_revenu <- read.csv("input/decile_revucm_comar.csv")
mar_revenu <- mar_revenu[, c("comar", "d5_2022")]


# Chargement des fichiers DVF -------------------------------------------------
dvf <- list(dvf_2014 = "https://sharedocs.huma-num.fr/wl/?id=vp4DTsuh5ctsBwGTzCSzgdKvZ3HnreAf&mode=grid&download=1",
            dvf_2015 = "https://sharedocs.huma-num.fr/wl/?id=QJ3AiWOCYVCkYN6Z0FzqI2yMM7Fu0Jhp&mode=grid&download=1",
            dvf_2016 = "https://sharedocs.huma-num.fr/wl/?id=OshgupIqPkg70hEMB7DdSpbsFDuTeAMN&mode=grid&download=1",
            dvf_2017 = "https://sharedocs.huma-num.fr/wl/?id=E0Xc2Ahyb0UUGoHFL6JR704dpQnUE7wK&mode=grid&download=1",
            dvf_2018 = "https://sharedocs.huma-num.fr/wl/?id=I22yu03q8W53XEFSb0voebmdi0ORUzUl&mode=grid&download=1",
            dvf_2019 = "https://sharedocs.huma-num.fr/wl/?id=6muOpXEStHm1Y56YUNv93n14zx2QSQ9i&mode=grid&download=1",
            dvf_2020 = "https://sharedocs.huma-num.fr/wl/?id=iq9S63LevHYxoDMCkL01bNQBeE4YlWYx&mode=grid&download=1",
            dvf_2021 = "https://sharedocs.huma-num.fr/wl/?id=XYI1SDuWfYRXfCtuz0jvcz7C4LuLi5Qg&mode=grid&download=1",
            dvf_2022 = "https://sharedocs.huma-num.fr/wl/?id=5sYwnTlHiFAiTtgD9ZqUeNuEAUTo5T7F&mode=grid&download=1",
            dvf_2023 = "https://sharedocs.huma-num.fr/wl/?id=4l09Pfh8OGPICchf9PQEw4X4kdvjOR5P&mode=grid&download=1",
            dvf_2024 = "https://sharedocs.huma-num.fr/wl/?id=oSMYbBxT6OWaePSLPydnXOJYoQE3tOID&mode=grid&download=1"
)

a <- read.csv(dvf[[9]])
b <- read.csv(dvf[[10]])

dvf <- rbind(a, b)

dvf <- dvf[, -c(1,3,4,10,11)]

# # Explo pour voir les surfaces medianes des biens pour les loyers
# ma <- dvf[dvf$type == "Maison", ]
# quantile(ma$surface, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
# ap <- dvf[dvf$type == "Appartement", ]
# quantile(ap$surface, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

mar <- merge(tabl, dvf, by.x = "COM_CODE", by.y = "codecommune")
mar <- mar[, -c(1,2)]

# Fonction pour traiter un type de bien (Maison/Appartement)
filter_dvf <- function(dvf, type_bien) {
  
  # Filtrer les données pour le type de bien
  dvf_filtered <- dvf[dvf$type == type_bien, ]
  
  # Calcul des agregations par regroupement de communes
  com_prix <- tapply(dvf_filtered$prix, dvf_filtered$comar, median, na.rm = TRUE)
  com_nomb <- tapply(dvf_filtered$prix, dvf_filtered$comar, length)
  
  # Convertir en data.frame
  result <- data.frame(comar = names(com_prix),
                       prix = as.vector(com_prix),
                       nb = as.vector(com_nomb))
  
  # Renommer les colonnes selon le type de bien
  colnames(result)[which(names(result) == "prix")] <- paste0("median_prix_", tolower(type_bien))
  colnames(result)[which(names(result) == "nb")] <- paste0("nb_", tolower(type_bien))
  
  return(result)
}

maison <- filter_dvf(mar, "Maison")
appart <- filter_dvf(mar, "Appartement")

mar_dvf <- merge(maison, appart, by = "comar", all = TRUE)

rm(a, b, dvf, mar, maison, appart)


# Chargement du fichier de l'Observatoire des Territoires ---------------------
loyer <- read.csv("input/base_OT_2024.csv", skip = 2, header = TRUE, sep = ";")

loyer[, 3] <- as.numeric(loyer[, 3])
loyer[, 4] <- as.numeric(loyer[, 4])

colnames(loyer)[3:4] <- c("loyer_mai", "loyer_app")

# Comme il manque les arrondissements on dedouble les communes concernees
arr_com <- tabl[grepl("^751|^132|^6938", tabl$COM_CODE), ]
arr_com$id <- substr(arr_com$COM_CODE, 1, 2)

arr_loyer <- loyer[grepl("75056|13055|69123", loyer$Code), ]
arr_loyer$id <- substr(arr_loyer$Code, 1, 2)

arr <- merge(arr_com, arr_loyer, by = "id")
arr <- arr[, c(2,6:8)]
colnames(arr)[1] <- "Code" 

loyer <- loyer[!grepl("75056|13055|69123", loyer$Code), ]

loyer <- rbind(arr, loyer)

# Utilisation de l'Indice de reference des loyers pour estimer les loyers de 2022
loyer$loyer_mai <- round(loyer$loyer_mai / 1.0247 / 1.035, 2)
loyer$loyer_app <- round(loyer$loyer_app / 1.0247 / 1.035, 2)

# Chargement du nombre de menages par commune pour ponderer les loyers entre communes lors du regroupement
pop <- read.csv("C:/Users/Antoine Beroud/Desktop/casd/export/TREVPOP_export_02/donnees/fra/filocom_2022_decile.csv")
pop <- pop[, c(2,13)]

loyer <- merge(loyer, pop, by.x = "Code", by.y = "COM", all.x = TRUE)
loyer$TOT[is.na(loyer$TOT)] <- 5
loyer$TOTB <- loyer$TOT

# Agregation des communes en calculant une moyenne ponderee des loyers
mar_loyer <- aggreg_data(tabl = tabl,
                         data = loyer,
                         vars = c(3:6),
                         funs = c("prod1", "prod2", "coef1", "coef2"),
                         id = c("COM_CODE", "Code"),
                         maille = "comar")

rm(arr, arr_com, arr_loyer, pop, loyer)


###############################################################################
################################################################### PACKAGE ASF

# Traitement sur les donnees --------------------------------------------------
data <- merge(mar_revenu, mar_loyer, by = "comar", all = TRUE)
data <- merge(data, mar_dvf, by = "comar", all = TRUE)

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


###############################################################################
############################################# CALCUL DE L'INDICE D'ABORDABILITE

# Nombre d'annee de revenu pour acheter un bien
fondata$abord_mai <- (fondata$median_prix_maison * 0.9) / fondata$d5_2022 
fondata$abord_app <- (fondata$median_prix_appart * 0.9) / fondata$d5_2022

# Abordabilite du loyer (pourcentage du salaire)
fondata$abord_mai_loc <- (fondata$loyer_mai * 90) / (fondata$d5_2022 / 12) * 100
fondata$abord_app_loc <- (fondata$loyer_app * 55) / (fondata$d5_2022 / 12) * 100

# Indice final
# Calcul des quartiles
q_m <- quantile(fondata$abord_mai, probs = c(0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)
q_a <- quantile(fondata$abord_app, probs = c(0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)

# Création de la colonne 'typo'
fondata$typo_m <- with(fondata, 
                       ifelse(abord_mai < q_m[1] & abord_app_loc < 30, 1,
                       ifelse(abord_mai < q_m[1] & abord_app_loc > 30, 2,
                       ifelse(abord_mai >= q_m[1] & abord_mai < q_m[2] & abord_app_loc < 30, 3,
                       ifelse(abord_mai >= q_m[1] & abord_mai < q_m[2] & abord_app_loc > 30, 4,
                       ifelse(abord_mai >= q_m[2] & abord_mai < q_m[3] & abord_app_loc < 30, 5,
                       ifelse(abord_mai >= q_m[2] & abord_mai < q_m[3] & abord_app_loc > 30, 6,
                       ifelse(abord_mai >= q_m[3] & abord_mai < q_m[4] & abord_app_loc < 30, 7,
                       ifelse(abord_mai >= q_m[3] & abord_mai < q_m[4] & abord_app_loc > 30, 8,
                       ifelse(abord_mai >= q_m[4] & abord_mai < q_m[5] & abord_app_loc < 30, 9,
                       ifelse(abord_mai >= q_m[4] & abord_mai < q_m[5] & abord_app_loc > 30, 10,
                       ifelse(abord_mai >= q_m[5] & abord_app_loc < 30, 11,
                       ifelse(abord_mai >= q_m[5] & abord_app_loc > 30, 12, 
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
  "#00a183","#6561a9",
  "#8ccaae","#9b99cc",
  "#cce5d8","#d3d5ed",
  "#ffe8b6","#f9c4a7",
  "#fdc75f","#f08159",
  "#f59c00","#dc0d15"
)

mf_map(fondata,
       var = "typo_a", 
       type = "typo",
       pal = palette,
       border = NA)

mf_map(fondata,
       var = "typo_m", 
       type = "typo",
       pal = palette,
       border = NA)


###############################################################################
############################################################# GRAPHIQUE MATRICE

# library(readxl)
# library(data.table)

# library(dplyr)
# library(ggplot2)

# Definition des fichiers et des valeurs de division
data <- list(dvf_2014 = "https://sharedocs.huma-num.fr/wl/?id=vp4DTsuh5ctsBwGTzCSzgdKvZ3HnreAf&mode=grid&download=1",
             dvf_2015 = "https://sharedocs.huma-num.fr/wl/?id=QJ3AiWOCYVCkYN6Z0FzqI2yMM7Fu0Jhp&mode=grid&download=1",
             dvf_2016 = "https://sharedocs.huma-num.fr/wl/?id=OshgupIqPkg70hEMB7DdSpbsFDuTeAMN&mode=grid&download=1",
             dvf_2017 = "https://sharedocs.huma-num.fr/wl/?id=E0Xc2Ahyb0UUGoHFL6JR704dpQnUE7wK&mode=grid&download=1",
             dvf_2018 = "https://sharedocs.huma-num.fr/wl/?id=I22yu03q8W53XEFSb0voebmdi0ORUzUl&mode=grid&download=1",
             dvf_2019 = "https://sharedocs.huma-num.fr/wl/?id=6muOpXEStHm1Y56YUNv93n14zx2QSQ9i&mode=grid&download=1",
             dvf_2020 = "https://sharedocs.huma-num.fr/wl/?id=iq9S63LevHYxoDMCkL01bNQBeE4YlWYx&mode=grid&download=1",
             dvf_2021 = "https://sharedocs.huma-num.fr/wl/?id=XYI1SDuWfYRXfCtuz0jvcz7C4LuLi5Qg&mode=grid&download=1",
             dvf_2022 = "https://sharedocs.huma-num.fr/wl/?id=5sYwnTlHiFAiTtgD9ZqUeNuEAUTo5T7F&mode=grid&download=1",
             dvf_2023 = "https://sharedocs.huma-num.fr/wl/?id=4l09Pfh8OGPICchf9PQEw4X4kdvjOR5P&mode=grid&download=1",
             dvf_2024 = "https://sharedocs.huma-num.fr/wl/?id=oSMYbBxT6OWaePSLPydnXOJYoQE3tOID&mode=grid&download=1"
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
  dvf <- read.csv(data[[i]])
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
  colnames(result[[i]]) <- paste0(colnames(result[[i]]), "_", substr(data[[i]], 18, 22))
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