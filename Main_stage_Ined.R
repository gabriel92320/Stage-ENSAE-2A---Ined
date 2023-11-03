# Programme principal qui génère la totalité du code R relatif au stage d'application de l'Ensae réalisé à l'Ined sur
# le sujet "Comparaisons internationales des durées de veuvage" (juin-août 2023)

# Auteur: Gabriel Sklénard.

# IMPORTANT: 3 paramètres à renseigner par chaque utilisateur avant d'excécuter ce programme:

# 1) Indiquez votre WD (Working Directory):
mon_WD="/Users/sklenard/Documents/Stage_INED/WD_Gabriel" # WD de Gabriel.

setwd(mon_WD)
# c'est à cet endroit que devront être enregistrés les différents fichiers servant  d'"inputs" pour notre travail
# (cf. la liste juste rappelée ci-dessous); et c'est à cet endroit que seront stocké tous les outputs.


# 2) Préciser le chemin pointant vers le dossier où sont enregistrés les scripts R: le Main.R + les scripts associés:
#scripts_path="/Users/sklenard/Documents" # scripts_path de Gabriel (ordi perso)
scripts_path="/Users/sklenard/Documents/Stage_INED/WD_Gabriel/Scripts_R_Stage_Ined_Gabriel" # scripts_path de Gabriel.

# WARNING: il faut penser à enregistrer sous votre WD l'ensemble des life tables du HMD avant de compiler ce code!!

# Etape 0 - Les packages utilisés dans ce projet:
source(paste0(scripts_path,"/Chargement_packages.R"),encoding="UTF-8")

# Etape 1 - Le chargement des différentes sources de données utilisées dans ce projet:
source(paste0(scripts_path,"/Chargement_data.R"),encoding="UTF-8")

# Etape 2: Construction d'une base de données internationale sur les indicateurs relatifs à la durée de veuvage
# Précisez les paramètres suivants:
# 1) Choix de l'âge pour le calcul de la durée de veuvage:
choix_age=60
# 2) Choix de l'écart d'âge pour le calcul de la durée de veuvage:
choix_ecart_age=0
# 3) Choix de la fenêtre temporelle à retenir pour la base internationale: 
choix_annee_deb=1950
choix_annee_fin=2020

source(paste0(scripts_path,"/Construction_base_internationale_WD.R"),encoding="UTF-8")

# Sauvegarde sous forme d'un fichier Rdata des principales bases de données utilisées dans la suite des analyses:
#save(base_WD_international,base_WD_international_1960_2018,panel_WD_pays_1960_1990_2018,file = "bases_WD_international.RData")
# NB: Il faudra penser à rajouer le panel complet sur la période 1960-2018 quand il aura tourné!
#save(base_WD_international,base_WD_international_1960_2018,panel_WD_pays_1960_1990_2018,estimation_param_Gompertz_pays_1960_2015,
#     file = "bases_WD_international_110823.RData")
#load("bases_WD_international_110823.RData")

# Etape 3: Principaux faits stylisés sur la proba et la durée espérée de veuvage
source(paste0(scripts_path,"/Faits_stylisés.R"),encoding="UTF-8")

# Etape 4: Analyse de certains déterminants de la durée de veuvage (Evolution des paramètres de la Gompertz par pays
# + ACP-CAH sur ces paramètres)
source(paste0(scripts_path,"/Analyse_déterminants.R"),encoding="UTF-8")

# Etape 5: Variantes - impact d'une modification d'un paramètre de calcul (âge, écart d'âge entre conjoints) sur la
# mesure de la proba et de la durée de veuvage des femmes à 60 ans.
source(paste0(scripts_path,"/Calcul_PW_WD_variantes.R"),encoding="UTF-8")


