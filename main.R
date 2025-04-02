
#                                 EXPLORATIONS POUR LA PLANCHE SUR L'IMMOBILIER
#
#                                                                antoine beroud
#                                                                renaud le goix

library(sf)
library(mapsf)
library(RColorBrewer)

library(readxl)
library(data.table)

library(asf)
library(dplyr)
library(ggplot2)


################################################################################
###################################################################### FONCTIONS

# Fonction pour charger des fichiers
load_files <- function(dir, ext, fun = read.csv, list = FALSE, var = NULL) {
  
  # Fonction interne pour lire et selectionner les colonnes
  read <- function(file) {
    data <- fun(file)
    if (!is.null(var)) {
      data <- data[, var, drop = FALSE]
    }
    return(data)
  }
  
  # Creation de la liste des fichiers avec l'extension specifiee
  files <- list.files(path = dir, pattern = paste0("\\.", ext, "$"), full.names = TRUE)
  
  # Si list = TRUE les fichiers sont regroupes dans une liste
  # sinon on cree un objet par fichier
  if (list) {
    file_list <- lapply(files, read)
    names(file_list) <- gsub(paste0("\\.", ext, "$"), "", basename(files))
    return(file_list)
    
  } else {
    for (file in files) {
      file_name <- gsub(paste0("\\.", ext), "", basename(file))
      assign(file_name, read(file), envir = .GlobalEnv)
    }
  }
}


# Fonction pour calculer les valeurs a partir des tables dvf
process_dvf <- function(dvf) {
  
  # Fonction pour traiter un type de bien (Maison/Appartement)
  filter_dvf <- function(dvf, type_bien) {
    
    # Filtrer les données pour le type de bien
    dvf_filtered <- dvf[dvf$type == type_bien, ]
    
    # Calcul des agregations par commune
    com_prix <- tapply(dvf_filtered$prix, dvf_filtered$codecommune, median, na.rm = TRUE)
    com_nomb <- tapply(dvf_filtered$prix, dvf_filtered$codecommune, length)
    
    # Convertir en data.frame
    result <- data.frame(commune = names(com_prix),
                         prix = as.vector(com_prix),
                         nb = as.vector(com_nomb))
    
    # Renommer les colonnes selon le type de bien
    colnames(result)[which(names(result) == "prix")] <- paste0("median_prix_", tolower(type_bien))
    colnames(result)[which(names(result) == "nb")] <- paste0("nb_", tolower(type_bien))
    
    return(result)
  }
  
  # Creation de la liste final
  result <- list()
  
  for (name in names(dvf)) {
    
    x <- dvf[[name]]
    
    maison <- filter_dvf(x, "Maison")
    appart <- filter_dvf(x, "Appartement")
    
    tmp <- merge(maison, appart, by = "commune", all = TRUE)
    
    result[[name]] <- tmp
  }
  
  return(result)  
}


################################################################################
############################################################### DONNEES FISOSOFI

# Recuperation du fond iris du package asf 
iris <- st_read("data/fond_2019.gpkg")

# Agregation en communes
com <- aggregate(iris, by = list(iris$INSEE_COM), FUN = function(x) x[1])
colnames(com)[1] <- "CODE_COM"
com <- com[, c(1,3)]

# Traitement du fichier filosofi (carroyage 1km)
filosofi <- st_read("data/filo/filo_2019.gpkg")

filo <- filosofi[, c(3:10, 14:30)]
filo$log <- filo$log_av45 + filo$log_45_70 + filo$log_70_90 + filo$log_ap90 + filo$log_inc 
filo$log_av90 <- filo$log - filo$log_ap90
filo$ind_25m <- filo$ind_0_3 + filo$ind_4_5 + filo$ind_6_10 + filo$ind_11_17 + filo$ind_18_24
filo <- filo[, -c(9:11, 13, 15:23, 25)]

# On conserve le nombre d'individus et la somme des revenus pour calculer les salaires
# et en faisant 'logements' - 'menages proprietaires' - 'logements sociaux'
# on decide d'estimer le parc locatif prive
filo <- filo[, c("ind", "ind_snv", "men_prop", "log", "log_soc")]

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
                            by = CODE_COM, 
                            .SDcols = !c("CODE_COM", "NOM_COM", "geom")]
com <- com[, c(1,2)]
carro_agreg <- carro_agreg[, c(1:6)]

# Jointure entre le fond geographique et les donnees agregees au niveau communal
fond <- merge(com, carro_agreg, by.x = "CODE_COM", by.y = "CODE_COM", all.x = TRUE)

# Nettoyage et export
rm(iris, com, filosofi, filo, carro_decoup, carro_agreg, cols, by_area)
st_write(fond, "export/fond_filo.gpkg")


################################################################################
######################################################### INDICES D'ABORDABILITE
fond <- st_read("export/fond_filo.gpkg")
fond$geom <- NULL

file_list <- load_files("data/dvf_prep/", "csv", list = TRUE)

dvf <- file_list[c(6)]

dvf_prep <- process_dvf(dvf)
dvf_prep <- dvf_prep[[1]]

fond <- merge(fond, dvf_prep, by.x = "CODE_COM", by.y = "commune", all.x = TRUE)

loyer <- read.csv("data/loyer/indicateurs-loyers-appartements.csv", sep = ';')

loyer <- loyer[, c(2,8)]
loyer$loy_m2  <- as.numeric(gsub(",", ".", loyer$loypredm2))
loyer$INSEE <- ifelse(nchar(loyer$INSEE) == 4, paste0("0", loyer$INSEE), loyer$INSEE)
loyer <- loyer[, c(1,3)]

data <- merge(fond, loyer, by.x = "CODE_COM", by.y = "INSEE", all.x = TRUE)

data$loc_priv <- round(data$log - data$log_soc - data$men_prop, 1)

data <- data[, c(1:4, 8:13)]


################################################################################
#################################################################### PACKAGE ASF
rm(dvf, dvf_prep, file_list, loyer)

tabl <- read.csv("data/tabl_2019.csv")
fond <- st_read("data/fond_2019.gpkg")
fond <- fond[, c(4,7)]

data_aggreg <- aggreg_data(tabl = tabl,
                           data = data, 
                           vars = c(3:10),
                           funs = c("sum", "sum", "prod1", "coef1", "prod2", "coef2", "prod3", "coef3"),
                           id = c("INSEE_COM", "CODE_COM"),
                           maille = "ce")

fond_aggreg <- aggreg_fond(tabl = tabl,
                           fond = fond,
                           id = c("CODE_IRIS", "CODE_IRIS"),
                           maille = "ce")

zoom_created <- create_zoom(fond = fond_aggreg,
                            villes = c("Paris", "Marseille", "Lyon", "Lille", "Nantes",
                                       "Bordeaux", "Toulouse", "Clermont-Ferrand", "Angers",
                                       "Perpignan", "Le Havre", "Rouen", "Rennes",
                                       "Tours", "Dijon", "Reims", "Grenoble",
                                       "Nice", "Montpellier", "La Rochelle", "Besancon"),
                            buffer = 10000)

zooms <- zoom_created$zooms
labels <- zoom_created$labels

fond <- simplify_geom(fond, keep = 0.1)

fondata <- merge_fondata(data = data_aggreg,
                         fond = fond_aggreg,
                         zoom = zooms,
                         id = c("ce", "ce"))

# Nombre d'annee de revenu pour acheter un bien
fondata$maison_abord <- (fondata$median_prix_maison * 0.9) / (fondata$ind_snv / fondata$ind)
fondata$appart_abord <- (fondata$median_prix_appart * 0.9) / (fondata$ind_snv / fondata$ind)

# Abordabilite du loyer (pourcentage du salaire)
fondata$location_abord <- (fondata$loy_m2 * 49) / (fondata$ind_snv / fondata$ind / 12) * 100

palette <- rev(brewer.pal(10, "RdGy"))

mf_map(fondata,
       var = "maison_abord",
       type = "choro",
       breaks = "quantile",
       nbreaks = 10,
       pal = palette,
       border = NA)

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
























################################################################################
##################################################################### GRAPHIQUES
aav <- read.csv("data/aav_2020.csv")

file_list <- load_files("data/dvf_prep/", "csv", list = TRUE, var = c(5:9))

dvf_prep <- process_dvf(file_list)



files <- dvf_prep

calc_decile <- function(files, var) {
  
  result_list <- list()
  
  # Crée les noms de colonnes dynamiques (1 + 10 déciles)
  colnames_row <- c("name", paste0("d", 1:9, "_", var))
  
  for (i in seq_along(files)) {
    data <- files[[i]]
    annee <- names(files)[i]
    
    decile <- round(quantile(data[[var]], probs = seq(0, 1, by = 0.1), na.rm = TRUE), 0)
    
    row <- c(annee, decile[2:10])
    
    result_list[[length(result_list) + 1]] <- row
  }
  
  # Conversion en data.frame
  result_df <- do.call(rbind, result_list)
  
  # Convertir correctement les colonnes déciles en numeric
  result_df <- as.data.frame(result_df, stringsAsFactors = FALSE)
  
  # Convertir les colonnes des déciles en numérique (en excluant la première colonne)
  result_df[, -1] <- lapply(result_df[, -1], as.numeric)
  
  # Affecte les noms de colonnes
  colnames(result_df) <- colnames_row
  
  return(result_df)
}

m <- calc_decile(files, "median_prix_maison")
a <- calc_decile(files, "median_prix_appartement")




ggplot() +
  geom_point(data = m, aes(x = name, y = d1_median_prix_maison, color = 'blue'), size = 3) +
  geom_point(data = m, aes(x = name, y = d5_median_prix_maison, color = 'blue'), size = 3) +
  geom_point(data = m, aes(x = name, y = d9_median_prix_maison, color = 'blue'), size = 3) +
  geom_point(data = a, aes(x = name, y = d1_median_prix_appartement, color = 'red'), size = 3) +
  geom_point(data = a, aes(x = name, y = d5_median_prix_appartement, color = 'red'), size = 3) +
  geom_point(data = a, aes(x = name, y = d9_median_prix_appartement, color = 'red'), size = 3) +
  labs(
    title = "Évolution du prix médian des maisons et des appartements",
    x = "Nom",
    y = "Prix médian"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


















calc_decile <- function(files, var, typo) {
  
  result_list <- list()
  
  # Crée les noms de colonnes dynamiques (1 + 10 déciles)
  colnames_row <- c("name", paste0("d", 1:9, "_", var))
  
  # Parcours des fichiers
  for (i in seq_along(files)) {
    data <- files[[i]]
    annee <- names(files)[i]
    
    data <- merge(aav, data, by.x = "CODGEO", by.y = "commune")
    
    # Liste des classes distinctes dans la colonne typo
    typo_classes <- unique(data[[typo]])
    
    for (class in typo_classes) {
      # Filtrer les données selon la classe de la variable 'typo'
      subset_data <- data[data[[typo]] == class, ]
      
      # Calcul des déciles pour le sous-ensemble de données
      decile <- round(quantile(subset_data[[var]], probs = seq(0, 1, by = 0.1), na.rm = TRUE), 0)
      
      # Ajouter les résultats avec les noms de colonnes dynamiques
      row <- c(paste(annee, class, sep = "_"), decile[2:10])
      
      # Ajouter les résultats dans la liste
      result_list[[length(result_list) + 1]] <- row
    }
  }
  
  # Conversion en data.frame
  result_df <- do.call(rbind, result_list)
  
  # Convertir correctement les colonnes déciles en numeric
  result_df <- as.data.frame(result_df, stringsAsFactors = FALSE)
  
  # Convertir les colonnes des déciles en numérique (en excluant la première colonne)
  result_df[, -1] <- lapply(result_df[, -1], as.numeric)
  
  # Affecte les noms de colonnes (ajustés en fonction de la variable typo)
  typo_classes <- unique(unlist(lapply(files, function(data) unique(data[[typo]]))))
  colnames(result_df) <- colnames_row
  
  return(result_df)
}

m <- calc_decile(files, "median_prix_maison", "CATEAAV2020")
a <- calc_decile(files, "median_prix_appartement", "CATEAAV2020")






m$group <- substr(m$name, nchar(m$name) - 1, nchar(m$name))
a$group <- substr(a$name, nchar(a$name) - 1, nchar(a$name))

m$name <- substr(m$name, 1, nchar(m$name) - 3)
a$name <- substr(a$name, 1, nchar(a$name) - 3)

m$d9d1 <- round(m$d9_median_prix_maison / m$d1_median_prix_maison, 2)
m$d9d5 <- round(m$d9_median_prix_maison / m$d5_median_prix_maison, 2)
m$d5d1 <- round(m$d5_median_prix_maison / m$d1_median_prix_maison, 2)

a$d9d1 <- round(a$d9_median_prix_appartement / a$d1_median_prix_appartement, 2)
a$d9d5 <- round(a$d9_median_prix_appartement / a$d5_median_prix_appartement, 2)
a$d5d1 <- round(a$d5_median_prix_appartement / a$d1_median_prix_appartement, 2)




# Affichage du graphique avec ggplot2, en séparant les lignes par 'group'
ggplot(m, aes(x = name, y = d5_median_prix_maison, color = group)) +
  geom_line(aes(group = group), size = 1) +  # Ajoute une ligne pour chaque groupe
  geom_point(size = 3) +  # Ajoute des points aux données
  labs(
    title = "Évolution du prix médian des maisons",
    x = "Nom",
    y = "Prix médian"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquette

# Affichage du graphique avec ggplot2, en séparant les lignes par 'group'
ggplot(a, aes(x = name, y = d5_median_prix_appartement, color = group)) +
  geom_line(aes(group = group), size = 1) +  # Ajoute une ligne pour chaque groupe
  geom_point(size = 3) +  # Ajoute des points aux données
  labs(
    title = "Évolution du prix médian des appartements",
    x = "Nom",
    y = "Prix médian"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotat

# Création du graphique combiné
ggplot() +
  # Ajoute les lignes pour les maisons
  geom_line(data = m, aes(x = name, y = d5_median_prix_maison, color = group, group = group), size = 1) +
  # Ajoute les points pour les maisons
  geom_point(data = m, aes(x = name, y = d5_median_prix_maison, color = group), size = 3) +
  # Ajoute les lignes pour les appartements avec des pointillés
  geom_line(data = a, aes(x = name, y = d5_median_prix_appartement, color = group, group = group), size = 1, linetype = "dotted") +
  # Ajoute les points pour les appartements
  geom_point(data = a, aes(x = name, y = d5_median_prix_appartement, color = group), size = 3) +
  # Ajoute les labels et le titre
  labs(
    title = "Évolution du prix médian des maisons et des appartements",
    x = "Nom",
    y = "Prix médian"
  ) +
  # Utilise un thème minimal
  theme_minimal() +
  # Rotation des étiquettes de l'axe x
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

 











library(ggplot2)
library(tidyr)

# Exemple de tableau des revenus par décile et département
df <- data.frame(
  Departement = c("01", "02", "03", "04"),
  D1 = c(12000, 11500, 11800, 12200),
  D2 = c(14000, 13500, 13800, 14200),
  D3 = c(16000, 15500, 15800, 16200),
  D4 = c(18000, 17500, 17800, 18200),
  D5 = c(20000, 19500, 19800, 20200),
  D6 = c(22000, 21500, 21800, 22200),
  D7 = c(24000, 23500, 23800, 24200),
  D8 = c(26000, 25500, 25800, 26200),
  D9 = c(28000, 27500, 27800, 28200),
  D10 = c(30000, 29500, 29800, 30200)
)

# Transformation des données en format long
df_long <- df %>%
  pivot_longer(cols = -Departement, names_to = "Decile", values_to = "Revenu")

# Création du graphique avec des points
ggplot(df_long, aes(x = Departement, y = Revenu, color = Decile)) +
  geom_point(size = 2, alpha = 0.7) +  # Points sans ligne
  labs(title = "Revenus par décile et par département",
       x = "Département",
       y = "Revenu (€)") +
  theme_minimal()




# Conversion des noms de départements en indices numériques pour l'axe des abscisses
x <- 1:nrow(df)

# Extraction des valeurs des déciles
y <- as.matrix(df[, -1])  # On retire la colonne des départements

# Création du graphique avec des points uniquement
matplot(x, y, pch = 16, col = rainbow(ncol(y)), xaxt = "n", xlab = "Département", ylab = "Revenu (€)", main = "Revenus par décile et par département")

# Ajout des noms de départements sur l'axe des x
axis(1, at = x, labels = df$Departement)

# Ajout de la légende
legend("topleft", legend = colnames(df)[-1], col = rainbow(ncol(y)), pch = 16, title = "Déciles")


















# Chargement des bibliothèques nécessaires
library(ggplot2)
library(dplyr)

# Transformation des dataframes en format long
data <- data.frame(
  year = c(2010, 2011, 2012, 2013, 2014, 2015),
  median_price = c(100000, 110000, 120000, 130000, 140000, 150000)
)

# Affichage du graphique avec ggplot2
ggplot(data, aes(x = year, y = median_price)) +
  geom_line(size = 1) +  # Ajoute une ligne
  geom_point(size = 3) +  # Ajoute des points aux données
  labs(
    title = "Évolution du prix médian des maisons",
    x = "Année",
    y = "Prix médian"
  ) +
  theme_minimal()


data <- data.frame(
  type = c("q1", "q2", "q3", "q4"),
  y2010 = c(10, 20, 30, 40),
  y2011 = c(20, 30, 40, 50),
  y2012 = c(25, 40, 55, 60),
  y2013 = c(15, 30, 60, 65)
)




