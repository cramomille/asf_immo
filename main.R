
#                                 EXPLORATIONS POUR LA PLANCHE SUR L'IMMOBILIER
#
#                                                                antoine beroud
#                                                                renaud le goix
#                                                                  aliette roux

library(asf)
library(mapsf)

library(ggplot2)
library(reshape2)


###############################################################################
############################################################### FONDS D'ALIETTE

asf_mar()

# Creation du fond geographique des communes regroupees
com <- asf_fond(sf.irisf, 
                df.irisr, 
                by = "IRISF_CODE", 
                maille = "COMR_CODE")

com <- com[, c(10:28)]


###############################################################################
############################################ TRAITEMENT DES FICHIERS DE DONNEES

# Chargement du fichier FILOCOM -----------------------------------------------
mar_revenu <- read.csv("input/decile_revucm_comar.csv")
mar_revenu <- mar_revenu[, c("comar", "d5_2022")]
names(mar_revenu)[1] <- "COMR_CODE"


# Chargement des fichiers DVF -------------------------------------------------
download <- "&mode=grid&download=1"
                       
dvf <- list(dvf_2014 = paste0("https://sharedocs.huma-num.fr/wl/?id=vp4DTsuh5ctsBwGTzCSzgdKvZ3HnreAf", download),
            dvf_2015 = paste0("https://sharedocs.huma-num.fr/wl/?id=QJ3AiWOCYVCkYN6Z0FzqI2yMM7Fu0Jhp", download),
            dvf_2016 = paste0("https://sharedocs.huma-num.fr/wl/?id=OshgupIqPkg70hEMB7DdSpbsFDuTeAMN", download),
            dvf_2017 = paste0("https://sharedocs.huma-num.fr/wl/?id=E0Xc2Ahyb0UUGoHFL6JR704dpQnUE7wK", download),
            dvf_2018 = paste0("https://sharedocs.huma-num.fr/wl/?id=I22yu03q8W53XEFSb0voebmdi0ORUzUl", download),
            dvf_2019 = paste0("https://sharedocs.huma-num.fr/wl/?id=6muOpXEStHm1Y56YUNv93n14zx2QSQ9i", download),
            dvf_2020 = paste0("https://sharedocs.huma-num.fr/wl/?id=iq9S63LevHYxoDMCkL01bNQBeE4YlWYx", download),
            dvf_2021 = paste0("https://sharedocs.huma-num.fr/wl/?id=XYI1SDuWfYRXfCtuz0jvcz7C4LuLi5Qg", download),
            dvf_2022 = paste0("https://sharedocs.huma-num.fr/wl/?id=5sYwnTlHiFAiTtgD9ZqUeNuEAUTo5T7F", download),
            dvf_2023 = paste0("https://sharedocs.huma-num.fr/wl/?id=4l09Pfh8OGPICchf9PQEw4X4kdvjOR5P", download),
            dvf_2024 = paste0("https://sharedocs.huma-num.fr/wl/?id=oSMYbBxT6OWaePSLPydnXOJYoQE3tOID", download)
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

# Jointure avec le maillage composite d'Aliette Roux
tabl <- df.comr[, c(2,4,5,17)]
mar <- merge(tabl, dvf, by.x = "COM_CODE", by.y = "codecommune")

# Fonction pour traiter un type de bien (Maison/Appartement)
filter_dvf <- function(dvf, id, type_bien) {
  
  # Filtrer les données pour le type de bien
  dvf_filtered <- dvf[dvf$type == type_bien, ]
  
  # Calcul des agregations par regroupement de communes
  com_prix <- tapply(dvf_filtered$prix, dvf_filtered[[id]], median, na.rm = TRUE)
  com_nomb <- tapply(dvf_filtered$prix, dvf_filtered[[id]], length)
  
  # Convertir en data.frame
  result <- data.frame(COMR_CODE = names(com_prix),
                       prix = as.vector(com_prix),
                       nb = as.vector(com_nomb))
  
  # Renommer les colonnes selon le type de bien
  colnames(result)[which(names(result) == "prix")] <- paste0("median_prix_", tolower(type_bien))
  colnames(result)[which(names(result) == "nb")] <- paste0("nb_", tolower(type_bien))
  
  return(result)
}

maison <- filter_dvf(mar, "COMR_CODE", "Maison")
appart <- filter_dvf(mar, "COMR_CODE", "Appartement")

mar_dvf <- merge(maison, appart, by = "COMR_CODE", all = TRUE)

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
mar_loyer <- asf_data(d = loyer,
                      t = tabl, 
                      by.x = "Code",
                      by.y = "COM_CODE",
                      maille = "COMR_CODE",
                      vars = c(3:6),
                      funs = c("prod1", "prod2", "coef1", "coef2"))

rm(arr, arr_com, arr_loyer, pop, loyer)


###############################################################################
################################################################### PACKAGE ASF

# Traitement sur les donnees --------------------------------------------------
data <- merge(mar_revenu, mar_loyer, by = "COMR_CODE", all = TRUE)
data <- merge(data, mar_dvf, by = "COMR_CODE", all = TRUE)

# Creation du fond et des zooms -----------------------------------------------
fond <- com
fond <- asf_drom(fond, id = "COMR_CODE")

z <- asf_zoom(fond,
              places = c("Marseille", "Lyon", "Toulouse", "Nantes", "Montpellier",
                         "Bordeaux", "Lille", "Rennes", "Reims", "Dijon",
                         "Angers", "Grenoble", "Clermont-Ferrand", "Tours", "Perpignan",
                         "Besancon", "Rouen", "La Rochelle", "Le Havre", "Nice"
              ),
              r = 15000)

zoom <- z$zooms
label <- z$labels
point <- z$points

fond <- asf_simplify(fond, keep = 0.1)

fondata <- asf_fondata(f = fond,
                       z = zoom,
                       d = data,
                       by = "COMR_CODE")


###############################################################################
############################################# CALCUL DE L'INDICE D'ABORDABILITE

# Nombre d'annee de revenu pour acheter un bien
fondata$abord_mai <- (fondata$median_prix_maison * 0.9) / fondata$d5_2022 
fondata$abord_app <- (fondata$median_prix_appart * 0.9) / fondata$d5_2022

# Abordabilite du loyer (pourcentage du salaire)
fondata$abord_mai_loc <- (fondata$loyer_mai * 98) / (fondata$d5_2022 / 12) * 100
fondata$abord_app_loc <- (fondata$loyer_app * 49) / (fondata$d5_2022 / 12) * 100

# Indice final
# Calcul des Q6
q_m <- quantile(fondata$abord_mai, probs = c(0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)
q_a <- quantile(fondata$abord_app, probs = c(0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = TRUE)

# Arrondis des Q6
q_m <- c(4, 7, 9, 12, 18)
q_a <- c(2, 4, 6, 8, 10)

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

dep <- asf_borders(fond, 
                   by = "DEP",
                   keep = 0.1)

mf_map(fondata,
       var = "typo_m",
       type = "typo",
       pal = palette,
       border = NA)
mf_map(dep, 
       col = "white", 
       lwd = 1, 
       add = TRUE)
mf_map(point,
       add = TRUE)
mf_label(label, 
         var = "label")

mf_map(fondata,
       var = "typo_a", 
       type = "typo",
       pal = palette,
       border = NA)

# # Export pour la carte web de Nicolas
# test <- st_transform(fondata, crs = "EPSG:4326")
# st_write(test, "web_immo.geojson")


###############################################################################
################################################################ GRAPHIQUES AAV

# asf_mar()
# tmp <- fondata[, c(1, 2, 11, 12)]
# com <- mar$ar01$sf.comf
# com <- com[, c(1)]
# tabl <- mar$ar01$d.comf.app
# tabl <- tabl[, c(2, 16)]
# 
# aav <- merge(tabl, com, by.x = "COMF_CODE", by.y = "COMFA_CODE")


# Fonction pour recuperer les typologies associees aux id d'une cellule
x <- function(id_multi, df) {
  # Separation des id
  id <- trimws(unlist(strsplit(id_multi, "\\|")))
  # Recuperation des typologies associees a ces id
  typo <- df$TAAV2017[match(id, df$COMF_CODE)]
  # Retour des typologies distinctes
  paste(sort(unique(typo)), collapse = " | ")
}

# Application
tmp$aav <- sapply(tmp$COMF_CODE_MULTI, x, df = aav)


# Fonction pour recuperer la typologie majoritaire
y <- function(id_multi, df) {
  # Separation des id
  id <- trimws(unlist(strsplit(id_multi, "\\|")))
  # Recuperation des typologies associees a ces id
  typo <- df$TAAV2017[match(id, df$COMF_CODE)]
  typo <- typo[!is.na(typo)]
  
  # Si vide (aucun code trouve), retourner NA
  if (length(typo) == 0) return(NA)
  
  # Tableau de fréquence
  freq <- table(typo)
  freq_max <- max(freq)
  maj <- as.numeric(names(freq)[freq == freq_max])
  maj <- max(maj)
  
  return(maj)
}

# Application
tmp$aav_maj <- sapply(tmp$COMF_CODE_MULTI, y, df = aav)


###############################################################################

tabl <-  df.comr[, c("COMR_CODE", "TAAV2017")]
tabl <- tabl[!duplicated(tabl$COMR_CODE), ]


tmp <- merge(fondata, tabl, by = "COMR_CODE")

# Calcul des q6
tmp <- tmp[!is.na(tmp$abord_mai), ]
nb <- round(quantile(tmp$abord_mai, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)))
nb <- c(0, 4, 7, 9, 12, 18, 63)

# Definition pour chaque entite de son q6 d'appartenance
tmp$class <- cut(tmp$abord_mai,
                 breaks = nb,
                 include.lowest = TRUE,
                 labels = paste0("q", 1:6))

tmp$TAAV2017 <- as.character(tmp$TAAV2017)
tmp$class <- as.character(tmp$class)

asf_plot_typo(d = tmp, 
              vars = c("class"), 
              typo = "TAAV2017",
              order.v = c("q1", "q2", "q3", "q4", "q5", "q6"), 
              order.t = c("0", "1", "2", "3", "4", "5"))


asf_plot_typo(d = tmp, 
              vars = c("TAAV2017"), 
              typo = "class",
              order.v = c("0", "1", "2", "3", "4", "5"), 
              order.t = c("q1", "q2", "q3", "q4", "q5", "q6"))


###############################################################################
############################################################ GRAPHIQUES MATRICE

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

asf_mar()
tabl <- df.comr[, c(2,4,5,17)]

# Initialisation d'une liste pour stocker les resultats
maison <- list()
appart <- list()

# Boucle sur les fichiers et les valeurs de division
for (i in seq_along(data)) {

  # Chargement du fichier
  dvf <- read.csv(data[[i]])
  dvf <- dvf[, c(9,5,6)]
  
  mar <- merge(tabl, dvf, by.x = "COM_CODE", by.y = "codecommune")
  mar <- mar[, c("COMR_CODE", "type", "prix")]
  
  # Filtrage du type de bien (Maison ou Appartement)
  mai <- mar[mar$type == "Maison", ]
  app <- mar[mar$type == "Appartement", ]

  # Agregation par groupe de communes
  mai <- aggregate(prix ~ COMR_CODE, mai, FUN = median, na.rm = TRUE)
  app <- aggregate(prix ~ COMR_CODE, app, FUN = median, na.rm = TRUE)
  
  # Calcul des deciles
  decile_mai <- quantile(mai$prix, probs = seq(0.1, 0.9, 0.1))
  decile_app <- quantile(app$prix, probs = seq(0.1, 0.9, 0.1))
  
  # Stockage des resultats
  maison[[i]] <- data.frame(
    # decile = decile,
    abord = round(decile_mai / rd9[i], 1)
  )
  appart[[i]] <- data.frame(
    # decile = decile,
    abord = round(decile_app / rd9[i], 1)
  )
  
  # Renommer les colonnes pour chaque annee
  colnames(maison[[i]]) <- paste0(colnames(maison[[i]]), "_", substr(names(data)[i], 5, 8))
  colnames(appart[[i]]) <- paste0(colnames(appart[[i]]), "_", substr(names(data)[i], 5, 8))
  
  print(".")
}

# Fusionner tous les tableaux par la colonne des deciles
maison_d9 <- do.call(cbind, maison)
appart_d9 <- do.call(cbind, appart)

# Dessin des graphiques -------------------------------------------------------
# tableau <- maison_d1
tableau <- appart_d9

tableau$decile = c("10%","20%","30%","40%","50%","60%","70%","80%","90%")

# Transformation des donnees en format long
tableau_long <- melt(tableau, id.vars = "decile")

# Renommer les colonnes pour ggplot
colnames(tableau_long) <- c("decile", "annee", "valeur")

# Création d'une colonne categorielle pour les classes
tableau_long$classe <- cut(tableau_long$valeur,
                           breaks = c(0, 2, 4, 6, 8, 10, Inf),
                           # breaks = c(0, 4, 7, 9, 12, 18, Inf),
                           labels = c("1", "2", "3", "4", "5", "6"),
                           right = FALSE
)

palette <- c("1" = "#00a183",
             "2" = "#8ccaae",
             "3" = "#cce5d8",
             "4" = "#fbceb4",
             "5" = "#f28d65",
             "6" = "#dc0d15"
             )

# Creation du heatmap avec ggplot
ggplot(tableau_long, aes(x = annee, y = decile, fill = classe)) +
  geom_tile(color = "white") +  # creation des carres
  scale_fill_manual(values = palette) +  # couleurs
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # rotation des labels
  labs(title = "Évolution des valeurs par décile et année", 
       fill = "Classe de Valeur")
  
