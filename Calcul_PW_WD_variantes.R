
# Variantes - tests de sensibilité dans la mesure de la durée de veuvage au choix de certains paramètres de calcul:

# Paramètre 1: écart d'âge entre conjoints (homme-femme)
# Paramètre 2: âge de référence pour le calcul (60 ans dans le Rapport)

# 1) On recalcule la probabilité de veuvage et la durée de veuvage à 60 ans mais avec un écart d'âge entre conjoints
# de 2 ans (au lieu de 0):

# 1) Choix de l'âge pour le calcul de la durée de veuvage:
choix_age=60
# 2) Choix de l'écart d'âge pour le calcul de la durée de veuvage:
choix_ecart_age=2
# 3) Choix de la fenêtre temporelle à retenir pour la base internationale: 
choix_annee_deb=1950
choix_annee_fin=2020

# France (population civile): 1816-2020
WD_f_FRACNP <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_FRACNP,LT_m = LT_PC_male_FRACNP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1816,annee_fin = 2020)

# France (population totale): 1816-2020
WD_f_FRATNP <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_FRATNP,LT_m = LT_PC_male_FRATNP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1816,annee_fin = 2020)

# Allemagne (population totale): 1990-2020
WD_f_DEUTNP <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_DEUTNP,LT_m = LT_PC_male_DEUTNP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1990,annee_fin = 2020)

# Allemagne (de l'Ouest): 1956-2020
WD_f_DEUTW <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_DEUTW,LT_m = LT_PC_male_DEUTW,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1956,annee_fin = 2020)

# Allemagne (de l'Est): 1956-2020
WD_f_DEUTE <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_DEUTE,LT_m = LT_PC_male_DEUTE,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1956,annee_fin = 2020)

# Etats-Unis: 1933-2020
WD_f_USA <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_USA,LT_m = LT_PC_male_USA,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1933,annee_fin = 2020)

# Espagne: 1908-2020
WD_f_ESP <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_ESP,LT_m = LT_PC_male_ESP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1908,annee_fin = 2020)

# Italie: 1872-2019
WD_f_ITA <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_ITA,LT_m = LT_PC_male_ITA,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1872,annee_fin = 2019)

# Japon: 1947-2021
WD_f_JPN <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_JPN,LT_m = LT_PC_male_JPN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1947,annee_fin = 2021)

# Suède: 1751-2021
WD_f_SWE <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_SWE,LT_m = LT_PC_male_SWE,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1751,annee_fin = 2021)

# Russie: 1959-2014
WD_f_RUS <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_RUS,LT_m = LT_PC_male_RUS,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2014)

# UK: 1922-2020
WD_f_UK <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_GBR_NP,LT_m = LT_PC_male_GBR_NP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1922,annee_fin = 2020)

# Canada: 1921-2020
WD_f_CAN <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_CAN,LT_m = LT_PC_male_CAN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1921,annee_fin = 2020)

# Pays-Bas: 1850-2019
WD_f_NLD <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_NLD,LT_m = LT_PC_male_NLD,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1850,annee_fin = 2019)

# Autriche: 1947-2019
WD_f_AUT <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_AUT,LT_m = LT_PC_male_AUT,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1947,annee_fin = 2019)

# Finlande: 1878-2021
WD_f_FIN <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_FIN,LT_m = LT_PC_male_FIN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1878,annee_fin = 2021)

# Ireland: 1950-2020
WD_f_IRL <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_IRL,LT_m = LT_PC_male_IRL,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1950,annee_fin = 2020)

# Norway: 1846-2020
WD_f_NOR <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_NOR,LT_m = LT_PC_male_NOR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1846,annee_fin = 2020)

# Australie: 1921-2020
WD_f_AUS <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_AUS,LT_m = LT_PC_male_AUS,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1921,annee_fin = 2020)

# Bielorussie: 1959-2018
WD_f_BLR <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_BLR,LT_m = LT_PC_male_BLR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2018)

# Belgique: 1841-2021
WD_f_BEL <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_BEL,LT_m = LT_PC_male_BEL,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1841,annee_fin = 2021)

# Bulgarie: 1947-2021
WD_f_BGR <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_BGR,LT_m = LT_PC_male_BGR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1947,annee_fin = 2021)

# Chili: 1992-2020
WD_f_CHL <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_CHL,LT_m = LT_PC_male_CHL,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1992,annee_fin = 2020)

# Croatie: 2001-2020
WD_f_HRV <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_HRV,LT_m = LT_PC_male_HRV,age=choix_age,ecart_age=choix_ecart_age,annee_deb =2001,annee_fin = 2020)

# Tchéquie: 2001-2020
WD_f_CZE <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_CZE,LT_m = LT_PC_male_CZE,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1950,annee_fin = 2021)

# Danemark: 1835-2021
WD_f_DNK <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_DNK,LT_m = LT_PC_male_DNK,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1835,annee_fin = 2021)

# Estonie: 1959-2019
WD_f_EST <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_EST,LT_m = LT_PC_male_EST,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2019)

# Grèce: 1981-2019
WD_f_GRC <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_GRC,LT_m = LT_PC_male_GRC,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1981,annee_fin = 2019)

# Honk Kong: 1986-2020
WD_f_HKG <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_HKG,LT_m = LT_PC_male_HKG,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1986,annee_fin = 2020)

# Hongrie: 1950-2020
WD_f_HUN <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_HUN,LT_m = LT_PC_male_HUN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1950,annee_fin = 2020)

# Islande: 1838-2020
WD_f_ISL <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_ISL,LT_m = LT_PC_male_ISL,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1838,annee_fin = 2020)

# Israël: 1983-2016
WD_f_ISR <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_ISR,LT_m = LT_PC_male_ISR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1983,annee_fin = 2016)

# Lettonie: 1959-2019
WD_f_LVA <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_LVA,LT_m = LT_PC_male_LVA,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2019)

# Lithuanie: 1959-2019
WD_f_LTU <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_LTU,LT_m = LT_PC_male_LTU,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2019)

# Luxembourg: 1960-2021
WD_f_LUX <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_LUX,LT_m = LT_PC_male_LUX,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1960,annee_fin = 2021)

# Nouvelle-Zélande: 1948-2021
WD_f_NZL_NP <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_NZL_NP,LT_m = LT_PC_male_NZL_NP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1948,annee_fin = 2021)

# Pologne: 1958-2019
WD_f_POL <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_POL,LT_m = LT_PC_male_POL,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1958,annee_fin = 2019)

# Portugal: 1940-2021
WD_f_PRT <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_PRT,LT_m = LT_PC_male_PRT,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1940,annee_fin = 2021)

# République de Corée: 2003-2020
WD_f_KOR <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_KOR,LT_m = LT_PC_male_KOR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =2003,annee_fin = 2020)

# Slovaquie: 1950-2019
WD_f_SVK <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_SVK,LT_m = LT_PC_male_SVK,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1950,annee_fin = 2019)

# Slovénie: 1983-2019
WD_f_SVN <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_SVN,LT_m = LT_PC_male_SVN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1983,annee_fin = 2019)

# Suisse: 1876-2021
WD_f_CHE <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_CHE,LT_m = LT_PC_male_CHE,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1876,annee_fin = 2021)

# Taiwan: 1970-2019
WD_f_TWN <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_TWN,LT_m = LT_PC_male_TWN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1970,annee_fin = 2019)

# Ukraine: 1959-2013
WD_f_UKR <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_UKR,LT_m = LT_PC_male_UKR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2013)

######################################################################################################

# 3. Construction d'une base de données synthétique avec les 42 pays rencencés par le HMD (au format "long"):

# Reformatage des bases-pays:

# On impose une structure temporelle fixe aux données: par ex, la période 1950-2020:
fenetre_temporelle_fixe <-WD_f_FRATNP %>% select(Year) %>% filter(Year>=choix_annee_deb & Year<=choix_annee_fin)

# On procède au reformatage: restriction des séries à la fenêtre temporelle définie + création d'une variable "pays":
reformatage_base <-function(data_f,data_m,nom_pays){
  
  data2<- fenetre_temporelle_fixe %>% left_join(data_f,by=c("Year"="Year")) %>% 
    left_join(data_m,by=c("Year"="Year")) %>%
    mutate(pays := {{nom_pays}}) %>% 
    relocate(pays) %>%
    select(-Age_m)
  return(data2)
}

FRATNP <- reformatage_base(WD_f_FRATNP,WD_m_FRATNP,"France")
DEUTW <- reformatage_base(WD_f_DEUTW,WD_m_DEUTW,"All. (de l'Ouest)")
DEUTE <- reformatage_base(WD_f_DEUTE,WD_m_DEUTE,"All. (de l'Est)")
AUT <- reformatage_base(WD_f_AUT,WD_m_AUT,"Autriche")
USA <- reformatage_base(WD_f_USA,WD_m_USA,"Etats-Unis")
UK <- reformatage_base(WD_f_UK,WD_m_UK,"Royaume-Uni")
IRL <- reformatage_base(WD_f_IRL,WD_m_IRL,"Irlande")
SWE <- reformatage_base(WD_f_SWE,WD_m_SWE,"Suède")
FIN <- reformatage_base(WD_f_FIN,WD_m_FIN,"Finlande")
NOR <- reformatage_base(WD_f_NOR,WD_m_NOR,"Norvège")
ESP <- reformatage_base(WD_f_ESP,WD_m_ESP,"Espagne")
ITA <- reformatage_base(WD_f_ITA,WD_m_ITA,"Italie")
JPN <- reformatage_base(WD_f_JPN,WD_m_JPN,"Japon")
RUS <- reformatage_base(WD_f_RUS,WD_m_RUS,"Russie")
CAN <- reformatage_base(WD_f_CAN,WD_m_CAN,"Canada")
NLD <- reformatage_base(WD_f_NLD,WD_m_NLD,"Pays-Bas")
AUS <- reformatage_base(WD_f_AUS,WD_m_AUS,"Australie")
BLR <- reformatage_base(WD_f_BLR,WD_m_BLR,"Biélorussie")
BEL <- reformatage_base(WD_f_BEL,WD_m_BEL,"Belgique")
BGR <- reformatage_base(WD_f_BGR,WD_m_BGR,"Bulgarie")
CHL <- reformatage_base(WD_f_CHL,WD_m_CHL,"Chili")
HRV <- reformatage_base(WD_f_HRV,WD_m_HRV,"Croatie")
CZE <- reformatage_base(WD_f_CZE,WD_m_CZE,"Tchéquie")
DNK <- reformatage_base(WD_f_DNK,WD_m_DNK,"Danemark")
EST <- reformatage_base(WD_f_EST,WD_m_EST,"Estonie")
GRC <- reformatage_base(WD_f_GRC,WD_m_GRC,"Grèce")
HKG <- reformatage_base(WD_f_HKG,WD_m_HKG,"Honk-Kong")
HUN <- reformatage_base(WD_f_HUN,WD_m_HUN,"Hongrie")
ISL <- reformatage_base(WD_f_ISL,WD_m_ISL,"Islande")
ISR <- reformatage_base(WD_f_ISR,WD_m_ISR,"Israël")
LVA <- reformatage_base(WD_f_LVA,WD_m_LVA,"Lettonie")
LTU <- reformatage_base(WD_f_LTU,WD_m_LTU,"Lituanie")
LUX <- reformatage_base(WD_f_LUX,WD_m_LUX,"Luxembourg")
NZL <- reformatage_base(WD_f_NZL_NP,WD_m_NZL_NP,"Nouvelle-Zélande")
POL <- reformatage_base(WD_f_POL,WD_m_POL,"Pologne")
PRT <- reformatage_base(WD_f_PRT,WD_m_PRT,"Portugal")
KOR <- reformatage_base(WD_f_KOR,WD_m_KOR,"République de Corée")
SVK <- reformatage_base(WD_f_SVK,WD_m_SVK,"Slovaquie")
SVN <- reformatage_base(WD_f_SVN,WD_m_SVN,"Slovénie")
CHE <- reformatage_base(WD_f_CHE,WD_m_CHE,"Suisse")
TWN <- reformatage_base(WD_f_TWN,WD_m_TWN,"Taïwan")
UKR <- reformatage_base(WD_f_UKR,WD_m_UKR,"Ukraine")

# Concaténation des bases-pays pour constituer une base en format panel donnant pour chacun des 42 pays
# les séries temporelles annuelles sur la période 1950-2020 relatives aux différents indicateurs mesurant la
# durée de veuvage féminin:
base_WD_international_ecart_age_2ans<-bind_rows(FRATNP,DEUTW,DEUTE,AUT,USA,UK,IRL,SWE,FIN,NOR,ESP,ITA,JPN,RUS,CAN, 
                                 NLD,AUS,BLR,BEL,BGR,CHL,HRV,CZE,DNK,EST,GRC,HKG,HUN,ISL,ISR,LVA,LTU,LUX,NZL,POL,PRT,KOR,SVK,SVN, 
                                 CHE,TWN,UKR )

# base des 32 pays du HMD pour lesquels on dispose de tables de mortalité renseignées sur la période 1960-2018:

base_WD_international_1960_2018_ecart_age_2ans<-base_WD_international_ecart_age_2ans%>%
  filter(pays %in% c("Australie","Autriche","Biélorussie","Belgique","Bulgarie","Canada","Tchéquie",
                     "Danemark","Estonie","Finlande","France","All. (de l'Ouest)","All. (de l'Est)",
                     "Hongrie","Islande","Irlande","Italie","Japon","Lettonie","Lituanie",
                     "Luxembourg","Pologne","Portugal","Pays-Bas","Nouvelle-Zélande","Norvège",
                     "Slovaquie","Espagne","Suède","Suisse","Royaume-Uni","Etats-Unis")
         & Year>1959 & Year<2019)

# On compare les mesures de P_W_f et WDc_f dans la base de référence, nommée base_WD_international_1960_2018 (calcul réalisé
# à 60 ans et sans écart d'âge) avec la base générée ci-dessus, nommée base_WD_international_1960_2018_ecart_age_2ans
# (calcul réalisé à 60 ans avec un écart d'âge de 2 ans):

impact_ecart_age <- base_WD_international_1960_2018_ecart_age_2ans %>%
                    select(pays,Year,P_W_f,WDc_f) %>%
                    rename(P_W_f_ecart_2ans=P_W_f,WDc_f_ecart_2ans=WDc_f) %>%
                    left_join(base_WD_international_1960_2018 %>% select(pays,Year,P_W_f,WDc_f),by=c("pays"="pays","Year"="Year"))

# Illustration graphique:

# a) Probabilité de veuvage des femmes à 60 ans : calcul sans écart d'âge vs avec écart d'âge de 2 ans.

impact_ecart_age_P_W_f <- impact_ecart_age %>% select(pays,Year,P_W_f_ecart_2ans,P_W_f) %>%
  filter(pays %in% c("France", "Etats-Unis", "Japon", "Lituanie", "Espagne", "Suède"
  )) %>% 
  pivot_longer(cols = starts_with("P_W"), 
               names_to = "ecart_age", 
               values_to = "P_W_f") %>%
  mutate(
    ecart_age = case_when(
      ecart_age == "P_W_f" ~ "Sans écart d'âge",
      ecart_age == "P_W_f_ecart_2ans" ~ "Avec 2 ans d'écart d'âge",
      .default = "other"
    )
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = Year, y = P_W_f, color = pays, linetype = ecart_age)) +
  theme_classic()+
  theme(legend.position = "left")+
  labs(x="",y="En %")

# b) Durée de veuvage des femmes à 60 ans : calcul sans écart d'âge vs avec écart d'âge de 2 ans.

impact_ecart_age_WDc_f <- impact_ecart_age %>% select(pays,Year,WDc_f_ecart_2ans,WDc_f) %>%
  filter(pays %in% c("France", "Etats-Unis", "Japon", "Lituanie", "Espagne", "Suède"
  )) %>% 
  pivot_longer(cols = starts_with("WD"), 
               names_to = "ecart_age", 
               values_to = "WDc_f") %>%
  mutate(
    ecart_age = case_when(
      ecart_age == "WDc_f" ~ "Sans écart d'âge",
      ecart_age == "WDc_f_ecart_2ans" ~ "Avec 2 ans d'écart d'âge",
      .default = "other"
    )
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = Year, y = WDc_f, color = pays, linetype = ecart_age)) +
  theme_classic()+
  theme(legend.position = "left")+
  labs(x="",y="En années")

#Export:

ggsave("impact_ecart_age_P_W_f.pdf",plot = impact_ecart_age_P_W_f,width = 8, height = 11)
ggsave("impact_ecart_age_WDc_f.pdf",plot = impact_ecart_age_WDc_f,width = 8, height = 11)


# 2) On recalcule la probabilité de veuvage et la durée de veuvage à 50 ans (au lieu de 60 ans) mais toujours sans
# écart d'âge entre conjoints:

# 1) Choix de l'âge pour le calcul de la durée de veuvage:
choix_age=50
# 2) Choix de l'écart d'âge pour le calcul de la durée de veuvage:
choix_ecart_age=0
# 3) Choix de la fenêtre temporelle à retenir pour la base internationale: 
choix_annee_deb=1950
choix_annee_fin=2020

# France (population civile): 1816-2020
WD_f_FRACNP <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_FRACNP,LT_m = LT_PC_male_FRACNP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1816,annee_fin = 2020)

# France (population totale): 1816-2020
WD_f_FRATNP <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_FRATNP,LT_m = LT_PC_male_FRATNP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1816,annee_fin = 2020)

# Allemagne (population totale): 1990-2020
WD_f_DEUTNP <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_DEUTNP,LT_m = LT_PC_male_DEUTNP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1990,annee_fin = 2020)

# Allemagne (de l'Ouest): 1956-2020
WD_f_DEUTW <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_DEUTW,LT_m = LT_PC_male_DEUTW,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1956,annee_fin = 2020)

# Allemagne (de l'Est): 1956-2020
WD_f_DEUTE <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_DEUTE,LT_m = LT_PC_male_DEUTE,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1956,annee_fin = 2020)

# Etats-Unis: 1933-2020
WD_f_USA <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_USA,LT_m = LT_PC_male_USA,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1933,annee_fin = 2020)

# Espagne: 1908-2020
WD_f_ESP <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_ESP,LT_m = LT_PC_male_ESP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1908,annee_fin = 2020)

# Italie: 1872-2019
WD_f_ITA <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_ITA,LT_m = LT_PC_male_ITA,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1872,annee_fin = 2019)

# Japon: 1947-2021
WD_f_JPN <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_JPN,LT_m = LT_PC_male_JPN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1947,annee_fin = 2021)

# Suède: 1751-2021
WD_f_SWE <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_SWE,LT_m = LT_PC_male_SWE,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1751,annee_fin = 2021)

# Russie: 1959-2014
WD_f_RUS <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_RUS,LT_m = LT_PC_male_RUS,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2014)

# UK: 1922-2020
WD_f_UK <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_GBR_NP,LT_m = LT_PC_male_GBR_NP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1922,annee_fin = 2020)

# Canada: 1921-2020
WD_f_CAN <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_CAN,LT_m = LT_PC_male_CAN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1921,annee_fin = 2020)

# Pays-Bas: 1850-2019
WD_f_NLD <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_NLD,LT_m = LT_PC_male_NLD,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1850,annee_fin = 2019)

# Autriche: 1947-2019
WD_f_AUT <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_AUT,LT_m = LT_PC_male_AUT,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1947,annee_fin = 2019)

# Finlande: 1878-2021
WD_f_FIN <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_FIN,LT_m = LT_PC_male_FIN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1878,annee_fin = 2021)

# Ireland: 1950-2020
WD_f_IRL <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_IRL,LT_m = LT_PC_male_IRL,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1950,annee_fin = 2020)

# Norway: 1846-2020
WD_f_NOR <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_NOR,LT_m = LT_PC_male_NOR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1846,annee_fin = 2020)

# Australie: 1921-2020
WD_f_AUS <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_AUS,LT_m = LT_PC_male_AUS,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1921,annee_fin = 2020)

# Bielorussie: 1959-2018
WD_f_BLR <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_BLR,LT_m = LT_PC_male_BLR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2018)

# Belgique: 1841-2021
WD_f_BEL <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_BEL,LT_m = LT_PC_male_BEL,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1841,annee_fin = 2021)

# Bulgarie: 1947-2021
WD_f_BGR <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_BGR,LT_m = LT_PC_male_BGR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1947,annee_fin = 2021)

# Chili: 1992-2020
WD_f_CHL <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_CHL,LT_m = LT_PC_male_CHL,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1992,annee_fin = 2020)

# Croatie: 2001-2020
WD_f_HRV <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_HRV,LT_m = LT_PC_male_HRV,age=choix_age,ecart_age=choix_ecart_age,annee_deb =2001,annee_fin = 2020)

# Tchéquie: 2001-2020
WD_f_CZE <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_CZE,LT_m = LT_PC_male_CZE,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1950,annee_fin = 2021)

# Danemark: 1835-2021
WD_f_DNK <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_DNK,LT_m = LT_PC_male_DNK,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1835,annee_fin = 2021)

# Estonie: 1959-2019
WD_f_EST <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_EST,LT_m = LT_PC_male_EST,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2019)

# Grèce: 1981-2019
WD_f_GRC <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_GRC,LT_m = LT_PC_male_GRC,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1981,annee_fin = 2019)

# Honk Kong: 1986-2020
WD_f_HKG <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_HKG,LT_m = LT_PC_male_HKG,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1986,annee_fin = 2020)

# Hongrie: 1950-2020
WD_f_HUN <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_HUN,LT_m = LT_PC_male_HUN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1950,annee_fin = 2020)

# Islande: 1838-2020
WD_f_ISL <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_ISL,LT_m = LT_PC_male_ISL,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1838,annee_fin = 2020)

# Israël: 1983-2016
WD_f_ISR <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_ISR,LT_m = LT_PC_male_ISR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1983,annee_fin = 2016)

# Lettonie: 1959-2019
WD_f_LVA <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_LVA,LT_m = LT_PC_male_LVA,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2019)

# Lithuanie: 1959-2019
WD_f_LTU <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_LTU,LT_m = LT_PC_male_LTU,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2019)

# Luxembourg: 1960-2021
WD_f_LUX <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_LUX,LT_m = LT_PC_male_LUX,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1960,annee_fin = 2021)

# Nouvelle-Zélande: 1948-2021
WD_f_NZL_NP <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_NZL_NP,LT_m = LT_PC_male_NZL_NP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1948,annee_fin = 2021)

# Pologne: 1958-2019
WD_f_POL <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_POL,LT_m = LT_PC_male_POL,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1958,annee_fin = 2019)

# Portugal: 1940-2021
WD_f_PRT <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_PRT,LT_m = LT_PC_male_PRT,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1940,annee_fin = 2021)

# République de Corée: 2003-2020
WD_f_KOR <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_KOR,LT_m = LT_PC_male_KOR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =2003,annee_fin = 2020)

# Slovaquie: 1950-2019
WD_f_SVK <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_SVK,LT_m = LT_PC_male_SVK,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1950,annee_fin = 2019)

# Slovénie: 1983-2019
WD_f_SVN <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_SVN,LT_m = LT_PC_male_SVN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1983,annee_fin = 2019)

# Suisse: 1876-2021
WD_f_CHE <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_CHE,LT_m = LT_PC_male_CHE,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1876,annee_fin = 2021)

# Taiwan: 1970-2019
WD_f_TWN <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_TWN,LT_m = LT_PC_male_TWN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1970,annee_fin = 2019)

# Ukraine: 1959-2013
WD_f_UKR <- calculs_ind_veuvage_f_pays(LT_f = LT_PC_female_UKR,LT_m = LT_PC_male_UKR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2013)

######################################################################################################

# 3. Construction d'une base de données synthétique avec les 42 pays rencencés par le HMD (au format "long"):

# Reformatage des bases-pays:

# On impose une structure temporelle fixe aux données: par ex, la période 1950-2020:
fenetre_temporelle_fixe <-WD_f_FRATNP %>% select(Year) %>% filter(Year>=choix_annee_deb & Year<=choix_annee_fin)

# On procède au reformatage: restriction des séries à la fenêtre temporelle définie + création d'une variable "pays":
reformatage_base <-function(data_f,data_m,nom_pays){
  
  data2<- fenetre_temporelle_fixe %>% left_join(data_f,by=c("Year"="Year")) %>% 
    left_join(data_m,by=c("Year"="Year")) %>%
    mutate(pays := {{nom_pays}}) %>% 
    relocate(pays) %>%
    select(-Age_m)
  return(data2)
}

FRATNP <- reformatage_base(WD_f_FRATNP,WD_m_FRATNP,"France")
DEUTW <- reformatage_base(WD_f_DEUTW,WD_m_DEUTW,"All. (de l'Ouest)")
DEUTE <- reformatage_base(WD_f_DEUTE,WD_m_DEUTE,"All. (de l'Est)")
AUT <- reformatage_base(WD_f_AUT,WD_m_AUT,"Autriche")
USA <- reformatage_base(WD_f_USA,WD_m_USA,"Etats-Unis")
UK <- reformatage_base(WD_f_UK,WD_m_UK,"Royaume-Uni")
IRL <- reformatage_base(WD_f_IRL,WD_m_IRL,"Irlande")
SWE <- reformatage_base(WD_f_SWE,WD_m_SWE,"Suède")
FIN <- reformatage_base(WD_f_FIN,WD_m_FIN,"Finlande")
NOR <- reformatage_base(WD_f_NOR,WD_m_NOR,"Norvège")
ESP <- reformatage_base(WD_f_ESP,WD_m_ESP,"Espagne")
ITA <- reformatage_base(WD_f_ITA,WD_m_ITA,"Italie")
JPN <- reformatage_base(WD_f_JPN,WD_m_JPN,"Japon")
RUS <- reformatage_base(WD_f_RUS,WD_m_RUS,"Russie")
CAN <- reformatage_base(WD_f_CAN,WD_m_CAN,"Canada")
NLD <- reformatage_base(WD_f_NLD,WD_m_NLD,"Pays-Bas")
AUS <- reformatage_base(WD_f_AUS,WD_m_AUS,"Australie")
BLR <- reformatage_base(WD_f_BLR,WD_m_BLR,"Biélorussie")
BEL <- reformatage_base(WD_f_BEL,WD_m_BEL,"Belgique")
BGR <- reformatage_base(WD_f_BGR,WD_m_BGR,"Bulgarie")
CHL <- reformatage_base(WD_f_CHL,WD_m_CHL,"Chili")
HRV <- reformatage_base(WD_f_HRV,WD_m_HRV,"Croatie")
CZE <- reformatage_base(WD_f_CZE,WD_m_CZE,"Tchéquie")
DNK <- reformatage_base(WD_f_DNK,WD_m_DNK,"Danemark")
EST <- reformatage_base(WD_f_EST,WD_m_EST,"Estonie")
GRC <- reformatage_base(WD_f_GRC,WD_m_GRC,"Grèce")
HKG <- reformatage_base(WD_f_HKG,WD_m_HKG,"Honk-Kong")
HUN <- reformatage_base(WD_f_HUN,WD_m_HUN,"Hongrie")
ISL <- reformatage_base(WD_f_ISL,WD_m_ISL,"Islande")
ISR <- reformatage_base(WD_f_ISR,WD_m_ISR,"Israël")
LVA <- reformatage_base(WD_f_LVA,WD_m_LVA,"Lettonie")
LTU <- reformatage_base(WD_f_LTU,WD_m_LTU,"Lituanie")
LUX <- reformatage_base(WD_f_LUX,WD_m_LUX,"Luxembourg")
NZL <- reformatage_base(WD_f_NZL_NP,WD_m_NZL_NP,"Nouvelle-Zélande")
POL <- reformatage_base(WD_f_POL,WD_m_POL,"Pologne")
PRT <- reformatage_base(WD_f_PRT,WD_m_PRT,"Portugal")
KOR <- reformatage_base(WD_f_KOR,WD_m_KOR,"République de Corée")
SVK <- reformatage_base(WD_f_SVK,WD_m_SVK,"Slovaquie")
SVN <- reformatage_base(WD_f_SVN,WD_m_SVN,"Slovénie")
CHE <- reformatage_base(WD_f_CHE,WD_m_CHE,"Suisse")
TWN <- reformatage_base(WD_f_TWN,WD_m_TWN,"Taïwan")
UKR <- reformatage_base(WD_f_UKR,WD_m_UKR,"Ukraine")

# Concaténation des bases-pays pour constituer une base en format panel donnant pour chacun des 42 pays
# les séries temporelles annuelles sur la période 1950-2020 relatives aux différents indicateurs mesurant la
# durée de veuvage féminin:
base_WD_international_age_50ans<-bind_rows(FRATNP,DEUTW,DEUTE,AUT,USA,UK,IRL,SWE,FIN,NOR,ESP,ITA,JPN,RUS,CAN, 
                                                NLD,AUS,BLR,BEL,BGR,CHL,HRV,CZE,DNK,EST,GRC,HKG,HUN,ISL,ISR,LVA,LTU,LUX,NZL,POL,PRT,KOR,SVK,SVN, 
                                                CHE,TWN,UKR )

# base des 32 pays du HMD pour lesquels on dispose de tables de mortalité renseignées sur la période 1960-2018:

base_WD_international_1960_2018_age_50ans<-base_WD_international_age_50ans%>%
  filter(pays %in% c("Australie","Autriche","Biélorussie","Belgique","Bulgarie","Canada","Tchéquie",
                     "Danemark","Estonie","Finlande","France","All. (de l'Ouest)","All. (de l'Est)",
                     "Hongrie","Islande","Irlande","Italie","Japon","Lettonie","Lituanie",
                     "Luxembourg","Pologne","Portugal","Pays-Bas","Nouvelle-Zélande","Norvège",
                     "Slovaquie","Espagne","Suède","Suisse","Royaume-Uni","Etats-Unis")
         & Year>1959 & Year<2019)


# On compare les mesures de P_W_f et WDc_f dans la base de référence, nommée base_WD_international_1960_2018 (calcul réalisé
# à 60 ans et sans écart d'âge) avec la base générée ci-dessus, nommée base_WD_international_1960_2018_age_50ans
# (calcul réalisé à 50 ans sans écart d'âge):

impact_age <- base_WD_international_1960_2018_age_50ans %>%
  select(pays,Year,P_W_f,WDc_f) %>%
  rename(P_W_f_50ans=P_W_f,WDc_f_50ans=WDc_f) %>%
  left_join(base_WD_international_1960_2018 %>% select(pays,Year,P_W_f,WDc_f),by=c("pays"="pays","Year"="Year"))

# Illustration graphique:

# a) Probabilité de veuvage des femmes sans écart d'âge : calcul à 60 ans vs à 50 ans.

impact_age_P_W_f <- impact_age %>% select(pays,Year,P_W_f_50ans,P_W_f) %>%
  filter(pays %in% c("France", "Etats-Unis", "Japon", "Lituanie", "Espagne", "Suède"
  )) %>% 
  pivot_longer(cols = starts_with("P_W"), 
               names_to = "age", 
               values_to = "P_W_f") %>%
  mutate(
    age = case_when(
      age == "P_W_f" ~ "à 60 ans",
      age == "P_W_f_50ans" ~ "à 50 ans",
      .default = "other"
    )
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = Year, y = P_W_f, color = pays, linetype = age)) +
  theme_classic()+
  theme(legend.position = "left")+
  labs(x="",y="En %")

# b) Durée de veuvage des femmes à 60 ans : calcul sans écart d'âge vs avec écart d'âge de 2 ans.

impact_age_WDc_f <- impact_age %>% select(pays,Year,WDc_f_50ans,WDc_f) %>%
  filter(pays %in% c("France", "Etats-Unis", "Japon", "Lituanie", "Espagne", "Suède"
  )) %>% 
  pivot_longer(cols = starts_with("WD"), 
               names_to = "age", 
               values_to = "WDc_f") %>%
  mutate(
    age = case_when(
      age == "WDc_f" ~ "à 60 ans",
      age == "WDc_f_50ans" ~ "à 50 ans",
      .default = "other"
    )
  ) %>%
  ggplot() +
  geom_line(mapping = aes(x = Year, y = WDc_f, color = pays, linetype = age)) +
  theme_classic()+
  theme(legend.position = "left")+
  labs(x="",y="En années")

#Export:

ggsave("impact_age_P_W_f.pdf",plot = impact_age_P_W_f,width = 8, height = 11)
ggsave("impact_age_WDc_f.pdf",plot = impact_age_WDc_f,width = 8, height = 11)

