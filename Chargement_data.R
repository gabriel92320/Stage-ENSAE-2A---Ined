
# Chargement des différentes sources de données utilisées dans ce projet:

# 1) Les life tables du HMD:

# I. Chargement des lifetables (female/male) pour chaque pays:

upload_data_HMD_f <- function(pays){
  
  # Nom du fichier "female":  
  chemin_TB_f <-paste0("/Users/sklenard/Documents/Stage_INED/Data/fltper_1x1/",pays,".fltper_1x1.txt")
  
  # Importation des fichiers HMD au format dataframe:
  LT_PC_female <- readHMD(chemin_TB_f)
  
  LT_PC_female <- as_tibble(LT_PC_female)
  
  return(LT_PC_female)
}

upload_data_HMD_m <- function(pays){
  
  # Nom du fichier "male":  
  chemin_TB_m <-paste0("/Users/sklenard/Documents/Stage_INED/Data/mltper_1x1/",pays,".mltper_1x1.txt")
  
  # Importation des fichiers HMD au format dataframe:
  LT_PC_male <- readHMD(chemin_TB_m)
  
  LT_PC_male <- as_tibble(LT_PC_male)
  
  return(LT_PC_male)
}

# France (Civilian Population): 1816-2020
LT_PC_female_FRACNP <-upload_data_HMD_f(pays = "FRACNP")
LT_PC_male_FRACNP <-upload_data_HMD_m(pays = "FRACNP")

# France (Total Population): 1816-2020
LT_PC_female_FRATNP <-upload_data_HMD_f(pays = "FRATNP")
LT_PC_male_FRATNP <-upload_data_HMD_m(pays = "FRATNP")

# Germany (Total Population): 1990-2020
LT_PC_female_DEUTNP <-upload_data_HMD_f(pays = "DEUTNP")
LT_PC_male_DEUTNP <-upload_data_HMD_m(pays = "DEUTNP")

# Germany (West): 1956-2020
LT_PC_female_DEUTW <-upload_data_HMD_f(pays = "DEUTW")
LT_PC_male_DEUTW <-upload_data_HMD_m(pays = "DEUTW")

# Germany (East): 1956-2020
LT_PC_female_DEUTE <-upload_data_HMD_f(pays = "DEUTE")
LT_PC_male_DEUTE <-upload_data_HMD_m(pays = "DEUTE")

# USA: 1933-2020
LT_PC_female_USA <-upload_data_HMD_f(pays = "USA")
LT_PC_male_USA <-upload_data_HMD_m(pays = "USA")

# Spain: 1908-2020
LT_PC_female_ESP <-upload_data_HMD_f(pays = "ESP")
LT_PC_male_ESP <-upload_data_HMD_m(pays = "ESP")

# Italy: 1872-2019
LT_PC_female_ITA <-upload_data_HMD_f(pays = "ITA")
LT_PC_male_ITA <-upload_data_HMD_m(pays = "ITA")

# Japan: 1947-2021
LT_PC_female_JPN <-upload_data_HMD_f(pays = "JPN")
LT_PC_male_JPN <-upload_data_HMD_m(pays = "JPN")

# Sweden: 1751-2021
LT_PC_female_SWE <-upload_data_HMD_f(pays = "SWE")
LT_PC_male_SWE <-upload_data_HMD_m(pays = "SWE")

# Russia: 1959-2014
LT_PC_female_RUS <-upload_data_HMD_f(pays = "RUS")
LT_PC_male_RUS <-upload_data_HMD_m(pays = "RUS")

# UK (Total Population): 1922-2020
LT_PC_female_GBR_NP <-upload_data_HMD_f(pays = "GBR_NP")
LT_PC_male_GBR_NP <-upload_data_HMD_m(pays = "GBR_NP")

# Canada: 1921-2020
LT_PC_female_CAN <-upload_data_HMD_f(pays = "CAN")
LT_PC_male_CAN <-upload_data_HMD_m(pays = "CAN")

# Netherlands: 1850-2019
LT_PC_female_NLD <-upload_data_HMD_f(pays = "NLD")
LT_PC_male_NLD <-upload_data_HMD_m(pays = "NLD")

# Ireland: 1950-2020
LT_PC_female_IRL <-upload_data_HMD_f(pays = "IRL")
LT_PC_male_IRL <-upload_data_HMD_m(pays = "IRL")

# Austria: 1947-2019
LT_PC_female_AUT <-upload_data_HMD_f(pays = "AUT")
LT_PC_male_AUT <-upload_data_HMD_m(pays = "AUT")

# Finland: 1878-2022
LT_PC_female_FIN <-upload_data_HMD_f(pays = "FIN")
LT_PC_male_FIN <-upload_data_HMD_m(pays = "FIN")

# Norway: 1846-2022
LT_PC_female_NOR <-upload_data_HMD_f(pays = "NOR")
LT_PC_male_NOR <-upload_data_HMD_m(pays = "NOR")

# Australia: 1921-2020
LT_PC_female_AUS <-upload_data_HMD_f(pays = "AUS")
LT_PC_male_AUS <-upload_data_HMD_m(pays = "AUS")

# Belarus: 1959-2018
LT_PC_female_BLR <-upload_data_HMD_f(pays = "BLR")
LT_PC_male_BLR <-upload_data_HMD_m(pays = "BLR")

# Belgium: 1841-2021
LT_PC_female_BEL <-upload_data_HMD_f(pays = "BEL")
LT_PC_male_BEL <-upload_data_HMD_m(pays = "BEL")

# Bulgaria: 1947-2021
LT_PC_female_BGR <-upload_data_HMD_f(pays = "BGR")
LT_PC_male_BGR <-upload_data_HMD_m(pays = "BGR")

# Chile: 1992-2020
LT_PC_female_CHL <-upload_data_HMD_f(pays = "CHL")
LT_PC_male_CHL <-upload_data_HMD_m(pays = "CHL")

# Croatia: 2001-2020
LT_PC_female_HRV <-upload_data_HMD_f(pays = "HRV")
LT_PC_male_HRV <-upload_data_HMD_m(pays = "HRV")

# Czechia: 1950-2021
LT_PC_female_CZE <-upload_data_HMD_f(pays = "CZE")
LT_PC_male_CZE <-upload_data_HMD_m(pays = "CZE")

# Denmark: 1835-2022
LT_PC_female_DNK <-upload_data_HMD_f(pays = "DNK")
LT_PC_male_DNK <-upload_data_HMD_m(pays = "DNK")

# Estonia: 1959-2019
LT_PC_female_EST <-upload_data_HMD_f(pays = "EST")
LT_PC_male_EST <-upload_data_HMD_m(pays = "EST")

# Greece: 1981-2019
LT_PC_female_GRC <-upload_data_HMD_f(pays = "GRC")
LT_PC_male_GRC <-upload_data_HMD_m(pays = "GRC")

# Honk Kong: 1986-2020
LT_PC_female_HKG <-upload_data_HMD_f(pays = "HKG")
LT_PC_male_HKG <-upload_data_HMD_m(pays = "HKG")

# Hungary: 1950-2020
LT_PC_female_HUN <-upload_data_HMD_f(pays = "HUN")
LT_PC_male_HUN <-upload_data_HMD_m(pays = "HUN")

# Iceland: 1838-2020
LT_PC_female_ISL <-upload_data_HMD_f(pays = "ISL")
LT_PC_male_ISL <-upload_data_HMD_m(pays = "ISL")

# Israël: 1983-2016
LT_PC_female_ISR <-upload_data_HMD_f(pays = "ISR")
LT_PC_male_ISR <-upload_data_HMD_m(pays = "ISR")

# Latvia: 1959-2019
LT_PC_female_LVA <-upload_data_HMD_f(pays = "LVA")
LT_PC_male_LVA <-upload_data_HMD_m(pays = "LVA")

# Lithuania: 1959-2019
LT_PC_female_LTU <-upload_data_HMD_f(pays = "LTU")
LT_PC_male_LTU <-upload_data_HMD_m(pays = "LTU")

# Luxembourg: 1960-2021
LT_PC_female_LUX <-upload_data_HMD_f(pays = "LUX")
LT_PC_male_LUX <-upload_data_HMD_m(pays = "LUX")

# New Zealand: 1948-2021
LT_PC_female_NZL_NP <-upload_data_HMD_f(pays = "NZL_NP")
LT_PC_male_NZL_NP <-upload_data_HMD_m(pays = "NZL_NP")

# Poland: 1958-2019
LT_PC_female_POL <-upload_data_HMD_f(pays = "POL")
LT_PC_male_POL <-upload_data_HMD_m(pays = "POL")

# Portugal: 1940-2021
LT_PC_female_PRT <-upload_data_HMD_f(pays = "PRT")
LT_PC_male_PRT <-upload_data_HMD_m(pays = "PRT")

# Republic of Corea: 2003-2020
LT_PC_female_KOR <-upload_data_HMD_f(pays = "KOR")
LT_PC_male_KOR <-upload_data_HMD_m(pays = "KOR")

# Sloviaka: 1950-2019
LT_PC_female_SVK<-upload_data_HMD_f(pays = "SVK")
LT_PC_male_SVK <-upload_data_HMD_m(pays = "SVK")

# Slovenia: 1983-2019
LT_PC_female_SVN<-upload_data_HMD_f(pays = "SVN")
LT_PC_male_SVN <-upload_data_HMD_m(pays = "SVN")

# Switzerland: 1876-2021
LT_PC_female_CHE<-upload_data_HMD_f(pays = "CHE")
LT_PC_male_CHE <-upload_data_HMD_m(pays = "CHE")

# Taiwan: 1970-2019
LT_PC_female_TWN<-upload_data_HMD_f(pays = "TWN")
LT_PC_male_TWN <-upload_data_HMD_m(pays = "TWN")

# Ukraine: 1959-2013
LT_PC_female_UKR<-upload_data_HMD_f(pays = "UKR")
LT_PC_male_UKR <-upload_data_HMD_m(pays = "UKR")
