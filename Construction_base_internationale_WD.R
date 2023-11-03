
# 1a. Codage d'une fonction qui génère et graphe pour chaque pays les indicateurs suivants sur le veuvage féminin:

# - Durée de veuvage féminin conditionnelle: WDc_f
# - Durée de veuvage féminin inconditionnelle: WD_f
# - Probabilité de veuvage féminin: P_W_f
# - Durée de veuvage féminin (mesure naïve): WD_naif_f (ex_f_age-ex_m_age+ecart_age)
# - Overlap des distributions de mortalité par âge des femmes et des hommes: overlap

calculs_ind_veuvage_f_pays <- function(LT_f,LT_m,age,ecart_age,annee_deb,annee_fin){
  
  LT_PC_female <-LT_f
  LT_PC_male <-LT_m
  
  # On peut restreindre la lifetable féminine aux âges supérieurs ou égaux à la valeur du paramètre age fixée en argument
  #de la fonction:
  LT_PC_female_2 <- LT_PC_female %>% filter(Age>=age)
  
  # On peut également sélectionner les variables utiles pour le calcul: 
  # - Year (Year or range of years - for both period & cohort data)
  # - Age (Age group for n-year interval from exact age x to just before exact age x+n,where n=1,4,5, or infinite-> open age interval)
  # - dx (Number of deaths between ages x and x+n)
  # - lx (Number of survivors at exact age x, assuming I(0)=100 000)
  # - ex (Life expectancy at exact age x, in years)
  
  LT_PC_female_2 <- LT_PC_female_2 %>% select(Year,Age,dx,qx,lx,ex)
  
  # On recalcule les proba de survie à partir de 60 ans (en normalisant lx_60=100000) ainsi que
  # le nombre de décès qui en découle (dx_60):
  
  renormalisation <-function(data,annee){
    data2 <- data %>% 
      filter(Year==annee) 
    
    data2$lx_renorm<-NA
    data2$lx_renorm[1]<-100000
    for (i in 2:nrow(data2)){
      data2$lx_renorm[i]<-data2$lx_renorm[i-1]*(1-data2$qx[i-1])
    }
    
    data2 <- data2 %>% mutate(dx_renorm = lx_renorm-lead(lx_renorm)) %>%
      mutate(dx_renorm2=dx_renorm/sum(dx_renorm,na.rm=T))
    
    # Calcul de l'espérance de vie à partir de 60 ans après renormalisation des probas de survie lx_60
    # (création de la var ex_60):
    # data2$ex_60<-NA
    # for (a in unique(data2$Age)){
    #   temp = data2 %>% filter(Age>a)
    #   sum_lx_a = sum(temp$lx_60, na.rm=T)
    #   data2[which(data2$Age==a),]$ex_60 = round(sum_lx_a/data2$lx_60[1],2)
    # }
    
    return(data2)
  }
  
  # On génère le tibble avec les variables sx et dx retraitées
  LT_PC_female_renorm<-NULL
  for (i in annee_deb:annee_fin){
    m<-renormalisation(LT_PC_female_2,i)
    LT_PC_female_renorm<-rbind(LT_PC_female_renorm,m)
  }
  
  # On calcule pour les variables lx et ex, les valeurs moyennes sur deux ages consécutifs a et a+1 (pour chaque période)
  
  LT_PC_female_renorm <-
    LT_PC_female_renorm %>%
    arrange(Year, Age) %>%
    group_by(Year) %>%
    mutate(moy_lx_f = (lx + lead(lx))/2,moy_ex_f = (ex + lead(ex))/2,moy_lx_renorm_f = (lx_renorm + lead(lx_renorm))/2)  %>%
    rename(Age_f=Age,dx_f=dx,lx_f=lx,ex_f=ex,dx_renorm_f=dx_renorm,dx_renorm2_f=dx_renorm2)
  
  # On souhaite récupérer l'info sur le nombre de décès dx des hommes à partir de
  # 60 + ecart_age ans (on retient habituellement un écart de 2 ans d'âge entre l'homme et la femme): on peut donc restreindre 
  #la lifetable des hommes aux âges supérieurs ou égaux à 60+ecart_age ans:
  
  # On commence par renormaliser à 60 ans les probas de survie (en posant lx_60=100000) comme on l'a fait
  # dans les lifetable des femmes:
  LT_PC_male_2 <- LT_PC_male %>% 
    filter(Age>=age+ecart_age) %>% 
    select(Year,Age,dx,qx,lx,ex)
  
  # On génère le tibble avec les variables sx et dx retraitées
  LT_PC_male_renorm<-NULL
  for (i in annee_deb:annee_fin){
    n<-renormalisation(LT_PC_male_2,i)
    LT_PC_male_renorm<-rbind(LT_PC_male_renorm,n)
  }
  
  LT_PC_male_renorm <- LT_PC_male_renorm %>% filter(Age>=age+ecart_age) %>% select(Year,Age,dx_renorm,dx_renorm2) %>%
    rename(Age_m=Age,dx_renorm_m=dx_renorm,dx_renorm2_m=dx_renorm2)
  
  # Puis on crée une variable Age_f correspondant à l'âge de la femme pour faire la jointure entre les deux
  # lifetables:
  
  LT_PC_male_renorm <- LT_PC_male_renorm %>% mutate(Age_f=Age_m-ecart_age)
  
  # Jointure entre les deux lifetables:
  
  Base_calcul_WD_f <- LT_PC_female_renorm %>%
    left_join(y=LT_PC_male_renorm,
              by = c("Year" = "Year","Age_f" = "Age_f"))
  
  # Calcul des produits: dx_renorm_m*moy_lx_renorm_f et dx_renorm_m*moy_lx_renorm_f*moy_ex_f:
  Base_calcul_WD_f <- Base_calcul_WD_f %>%
    mutate(produit_dx_m_moy_lx_f=dx_renorm_m*moy_lx_renorm_f,produit_dx_m_moy_lx_ex_f=dx_renorm_m*moy_lx_renorm_f*moy_ex_f)
  
  # Calcul de la durée de veuvage féminine conditionnelle WDc_f à 60 ans avec un écart d'âge homme-femme à renseigner
  # + durée de veuvage inconditionnelle (WD_f) + probabilité de veuvage (P_W_f) en %:
  calculs_WD_f <-Base_calcul_WD_f %>%
    group_by(Year) %>%
    summarise(sum_produit_dx_m_moy_lx_ex_f=sum(produit_dx_m_moy_lx_ex_f,na.rm = TRUE),
              sum_produit_dx_m_moy_lx_f=sum(produit_dx_m_moy_lx_f,na.rm = TRUE)) %>%
    mutate(WDc_f=sum_produit_dx_m_moy_lx_ex_f/sum_produit_dx_m_moy_lx_f,
           WD_f=sum_produit_dx_m_moy_lx_ex_f/100000^2,
           P_W_f=(sum_produit_dx_m_moy_lx_f/100000^2)*100) %>%
    select(Year,WDc_f,WD_f,P_W_f)
  
  # Calcul de l'overlap des distributions de mortalité femme/homme pour chaque année:
  base_calcul_overlap<-Base_calcul_WD_f%>%
    mutate(aire = pmin(dx_renorm2_f,dx_renorm2_m)) %>%
    drop_na() #permet d'ignorer les cas où il n'y a pas de décès/survivant aux âges extrêmes certaines années (ex: à 109, 110 ans!)
  
  calcul_overlap<-base_calcul_overlap%>%
    group_by(Year)%>%
    summarise(overlap = integrate.xy(Age_f,aire))
  
  # Calcul de la durée de veuvage féminine à l'âge égal au paramètre fixé "age" avec un écart d'âge homme-femme fixé
  # à la valeur du paramètre "ecart_age"
  
  ex_f_2 <-LT_PC_female %>% 
    filter(Age==age) %>%
    select(Year,ex) %>%
    rename(ex_f=ex)
  
  ex_m_2 <-LT_PC_male %>% 
    filter(Age==age) %>% 
    select(Year,Age,ex) %>%
    rename(Age_m=Age,ex_m=ex) 
  
  ex_f_m <- ex_f_2 %>%
    left_join(y=ex_m_2,
              by=c("Year"="Year"))
  
  calculs_WD_naif_f <- ex_f_m %>%
    mutate(WD_naif_f=ex_f-ex_m+ecart_age)
  
  # On regroupe dans une même table les deux indicateurs: WD_naif_f et WDc_f
  calculs_ind_veuvage_f <- calculs_WD_naif_f %>%
    left_join(y=calculs_WD_f,
              by=c("Year"="Year"))
  
  # Et on regroupe avec le calcul de l'overlap:
  calculs_ind_veuvage_f<-calculs_ind_veuvage_f %>%
    left_join(y=calcul_overlap,
              by=c("Year"="Year"))
  
  return(calculs_ind_veuvage_f)  
  
}

# 1b. Codage d'une fonction qui génère et graphe pour chaque pays les indicateurs suivants sur le veuvage masculin:

# - Durée de veuvage masculin conditionnelle: WDc_m
# - Durée de veuvage masculin inconditionnelle: WD_m
# - Probabilité de veuvage masculin: P_W_m
# - Durée de veuvage masculin (mesure naïve): WD_naif_f (ex_f_age-ex_m_age+ecart_age)
# - Overlap des distributions de mortalité par âge des femmes et des hommes: overlap

calculs_ind_veuvage_m_pays <- function(LT_f,LT_m,age,ecart_age,annee_deb,annee_fin){
  
  LT_PC_female <-LT_f
  LT_PC_male <-LT_m
  
  # On peut restreindre la lifetable masculine aux âges supérieurs ou égaux à la valeur du paramètre age fixée en argument
  #de la fonction:
  LT_PC_male_2 <- LT_PC_male %>% filter(Age>=age)
  
  # On peut également sélectionner les variables utiles pour le calcul: 
  # - Year (Year or range of years - for both period & cohort data)
  # - Age (Age group for n-year interval from exact age x to just before exact age x+n,where n=1,4,5, or infinite-> open age interval)
  # - dx (Number of deaths between ages x and x+n)
  # - lx (Number of survivors at exact age x, assuming I(0)=100 000)
  # - ex (Life expectancy at exact age x, in years)
  
  LT_PC_male_2 <- LT_PC_male_2 %>% select(Year,Age,dx,qx,lx,ex)
  
  # On recalcule les proba de survie à partir de 60 ans (en normalisant lx_60=100000) ainsi que
  # le nombre de décès qui en découle (dx_60):
  
  renormalisation <-function(data,annee){
    data2 <- data %>% 
      filter(Year==annee) 
    
    data2$lx_renorm<-NA
    data2$lx_renorm[1]<-100000
    for (i in 2:nrow(data2)){
      data2$lx_renorm[i]<-data2$lx_renorm[i-1]*(1-data2$qx[i-1])
    }
    
    data2 <- data2 %>% mutate(dx_renorm = lx_renorm-lead(lx_renorm)) %>%
      mutate(dx_renorm2=dx_renorm/sum(dx_renorm,na.rm=T))
    
    # Calcul de l'espérance de vie à partir de 60 ans après renormalisation des probas de survie lx_60
    # (création de la var ex_60):
    # data2$ex_60<-NA
    # for (a in unique(data2$Age)){
    #   temp = data2 %>% filter(Age>a)
    #   sum_lx_a = sum(temp$lx_60, na.rm=T)
    #   data2[which(data2$Age==a),]$ex_60 = round(sum_lx_a/data2$lx_60[1],2)
    # }
    
    return(data2)
  }
  
  # On génère le tibble avec les variables sx et dx retraitées
  LT_PC_male_renorm<-NULL
  for (i in annee_deb:annee_fin){
    m<-renormalisation(LT_PC_male_2,i)
    LT_PC_male_renorm<-rbind(LT_PC_male_renorm,m)
  }
  
  # On calcule pour les variables lx et ex, les valeurs moyennes sur deux ages consécutifs a et a+1 (pour chaque période)
  
  LT_PC_male_renorm <-
    LT_PC_male_renorm %>%
    arrange(Year, Age) %>%
    group_by(Year) %>%
    mutate(moy_lx_m = (lx + lead(lx))/2,moy_ex_m = (ex + lead(ex))/2,moy_lx_renorm_m = (lx_renorm + lead(lx_renorm))/2)  %>%
    rename(Age_m=Age,dx_m=dx,lx_m=lx,ex_m=ex,dx_renorm_m=dx_renorm,dx_renorm2_m=dx_renorm2)
  
  # On souhaite récupérer l'info sur le nombre de décès dx des femmes à partir de
  # 60 + ecart_age ans (on retient habituellement un écart de 2 ans d'âge entre l'homme et la femme): on peut donc restreindre 
  #la lifetable des femmes aux âges supérieurs ou égaux à 60+ecart_age ans:
  
  # On commence par renormaliser à 60 ans les probas de survie (en posant lx_60=100000) comme on l'a fait
  # dans les lifetable des femmes:
  LT_PC_female_2 <- LT_PC_female %>% 
    filter(Age>=age+ecart_age) %>% 
    select(Year,Age,dx,qx,lx,ex)
  
  # On génère le tibble avec les variables sx et dx retraitées
  LT_PC_female_renorm<-NULL
  for (i in annee_deb:annee_fin){
    n<-renormalisation(LT_PC_female_2,i)
    LT_PC_female_renorm<-rbind(LT_PC_female_renorm,n)
  }
  
  LT_PC_female_renorm <- LT_PC_female_renorm %>% filter(Age>=age+ecart_age) %>% select(Year,Age,dx_renorm,dx_renorm2) %>%
    rename(Age_f=Age,dx_renorm_f=dx_renorm,dx_renorm2_f=dx_renorm2)
  
  # Puis on crée une variable Age_m correspondant à l'âge de l'homme pour faire la jointure entre les deux
  # lifetables:
  
  LT_PC_female_renorm <- LT_PC_female_renorm %>% mutate(Age_m=Age_f-ecart_age)
  
  # Jointure entre les deux lifetables:
  
  Base_calcul_WD_m <- LT_PC_male_renorm %>%
    left_join(y=LT_PC_female_renorm,
              by = c("Year" = "Year","Age_m" = "Age_m"))
  
  # Calcul des produits: dx_renorm_f*moy_lx_renorm_m et dx_renorm_f*moy_lx_renorm_m*moy_ex_m:
  Base_calcul_WD_m <- Base_calcul_WD_m %>%
    mutate(produit_dx_f_moy_lx_m=dx_renorm_f*moy_lx_renorm_m,produit_dx_f_moy_lx_ex_m=dx_renorm_f*moy_lx_renorm_m*moy_ex_m)
  
  # Calcul de la durée de veuvage masculin conditionnelle WDc_m à 60 ans avec un écart d'âge homme-femme à renseigner
  # + durée de veuvage inconditionnelle (WD_m) + probabilité de veuvage (P_W_m) en %:
  calculs_ind_veuvage_m <-Base_calcul_WD_m %>%
    group_by(Year) %>%
    summarise(sum_produit_dx_f_moy_lx_ex_m=sum(produit_dx_f_moy_lx_ex_m,na.rm = TRUE),
              sum_produit_dx_f_moy_lx_m=sum(produit_dx_f_moy_lx_m,na.rm = TRUE)) %>%
    mutate(WDc_m=sum_produit_dx_f_moy_lx_ex_m/sum_produit_dx_f_moy_lx_m,
           WD_m=sum_produit_dx_f_moy_lx_ex_m/100000^2,
           P_W_m=(sum_produit_dx_f_moy_lx_m/100000^2)*100) %>%
    select(Year,WDc_m,WD_m,P_W_m)
  
  return(calculs_ind_veuvage_m)  
  
}



# 2a. Calcul des 4 indicateurs sur le veuvage féminin à 60 ans pour chaque pays (création d'une base générique pour 
# chaque pays)

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


# 2b. Calcul des 4 indicateurs sur le veuvage masculin à 60 ans pour chaque pays (création d'une base générique pour 
# chaque pays)

# France (population civile): 1816-2020
WD_m_FRACNP <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_FRACNP,LT_m = LT_PC_male_FRACNP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1816,annee_fin = 2020)

# France (population totale): 1816-2020
WD_m_FRATNP <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_FRATNP,LT_m = LT_PC_male_FRATNP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1816,annee_fin = 2020)

# Allemagne (population totale): 1990-2020
WD_m_DEUTNP <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_DEUTNP,LT_m = LT_PC_male_DEUTNP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1990,annee_fin = 2020)

# Allemagne (de l'Ouest): 1956-2020
WD_m_DEUTW <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_DEUTW,LT_m = LT_PC_male_DEUTW,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1956,annee_fin = 2020)

# Allemagne (de l'Est): 1956-2020
WD_m_DEUTE <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_DEUTE,LT_m = LT_PC_male_DEUTE,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1956,annee_fin = 2020)

# Etats-Unis: 1933-2020
WD_m_USA <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_USA,LT_m = LT_PC_male_USA,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1933,annee_fin = 2020)

# Espagne: 1908-2020
WD_m_ESP <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_ESP,LT_m = LT_PC_male_ESP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1908,annee_fin = 2020)

# Italie: 1872-2019
WD_m_ITA <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_ITA,LT_m = LT_PC_male_ITA,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1872,annee_fin = 2019)

# Japon: 1947-2021
WD_m_JPN <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_JPN,LT_m = LT_PC_male_JPN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1947,annee_fin = 2021)

# Suède: 1751-2021
WD_m_SWE <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_SWE,LT_m = LT_PC_male_SWE,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1751,annee_fin = 2021)

# Russie: 1959-2014
WD_m_RUS <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_RUS,LT_m = LT_PC_male_RUS,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2014)

# UK: 1922-2020
WD_m_UK <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_GBR_NP,LT_m = LT_PC_male_GBR_NP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1922,annee_fin = 2020)

# Canada: 1921-2020
WD_m_CAN <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_CAN,LT_m = LT_PC_male_CAN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1921,annee_fin = 2020)

# Pays-Bas: 1850-2019
WD_m_NLD <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_NLD,LT_m = LT_PC_male_NLD,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1850,annee_fin = 2019)

# Autriche: 1947-2019
WD_m_AUT <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_AUT,LT_m = LT_PC_male_AUT,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1947,annee_fin = 2019)

# Finlande: 1878-2021
WD_m_FIN <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_FIN,LT_m = LT_PC_male_FIN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1878,annee_fin = 2021)

# Ireland: 1950-2020
WD_m_IRL <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_IRL,LT_m = LT_PC_male_IRL,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1950,annee_fin = 2020)

# Norway: 1846-2020
WD_m_NOR <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_NOR,LT_m = LT_PC_male_NOR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1846,annee_fin = 2020)

# Australie: 1921-2020
WD_m_AUS <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_AUS,LT_m = LT_PC_male_AUS,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1921,annee_fin = 2020)

# Bielorussie: 1959-2018
WD_m_BLR <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_BLR,LT_m = LT_PC_male_BLR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2018)

# Belgique: 1841-2021
WD_m_BEL <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_BEL,LT_m = LT_PC_male_BEL,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1841,annee_fin = 2021)

# Bulgarie: 1947-2021
WD_m_BGR <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_BGR,LT_m = LT_PC_male_BGR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1947,annee_fin = 2021)

# Chili: 1992-2020
WD_m_CHL <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_CHL,LT_m = LT_PC_male_CHL,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1992,annee_fin = 2020)

# Croatie: 2001-2020
WD_m_HRV <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_HRV,LT_m = LT_PC_male_HRV,age=choix_age,ecart_age=choix_ecart_age,annee_deb =2001,annee_fin = 2020)

# Tchéquie: 2001-2020
WD_m_CZE <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_CZE,LT_m = LT_PC_male_CZE,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1950,annee_fin = 2021)

# Danemark: 1835-2021
WD_m_DNK <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_DNK,LT_m = LT_PC_male_DNK,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1835,annee_fin = 2021)

# Estonie: 1959-2019
WD_m_EST <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_EST,LT_m = LT_PC_male_EST,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2019)

# Grèce: 1981-2019
WD_m_GRC <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_GRC,LT_m = LT_PC_male_GRC,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1981,annee_fin = 2019)

# Honk Kong: 1986-2020
WD_m_HKG <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_HKG,LT_m = LT_PC_male_HKG,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1986,annee_fin = 2020)

# Hongrie: 1950-2020
WD_m_HUN <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_HUN,LT_m = LT_PC_male_HUN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1950,annee_fin = 2020)

# Islande: 1838-2020
WD_m_ISL <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_ISL,LT_m = LT_PC_male_ISL,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1838,annee_fin = 2020)

# Israël: 1983-2016
WD_m_ISR <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_ISR,LT_m = LT_PC_male_ISR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1983,annee_fin = 2016)

# Lettonie: 1959-2019
WD_m_LVA <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_LVA,LT_m = LT_PC_male_LVA,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2019)

# Lithuanie: 1959-2019
WD_m_LTU <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_LTU,LT_m = LT_PC_male_LTU,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2019)

# Luxembourg: 1960-2021
WD_m_LUX <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_LUX,LT_m = LT_PC_male_LUX,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1960,annee_fin = 2021)

# Nouvelle-Zélande: 1948-2021
WD_m_NZL_NP <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_NZL_NP,LT_m = LT_PC_male_NZL_NP,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1948,annee_fin = 2021)

# Pologne: 1958-2019
WD_m_POL <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_POL,LT_m = LT_PC_male_POL,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1958,annee_fin = 2019)

# Portugal: 1940-2021
WD_m_PRT <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_PRT,LT_m = LT_PC_male_PRT,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1940,annee_fin = 2021)

# République de Corée: 2003-2020
WD_m_KOR <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_KOR,LT_m = LT_PC_male_KOR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =2003,annee_fin = 2020)

# Slovaquie: 1950-2019
WD_m_SVK <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_SVK,LT_m = LT_PC_male_SVK,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1950,annee_fin = 2019)

# Slovénie: 1983-2019
WD_m_SVN <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_SVN,LT_m = LT_PC_male_SVN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1983,annee_fin = 2019)

# Suisse: 1876-2021
WD_m_CHE <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_CHE,LT_m = LT_PC_male_CHE,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1876,annee_fin = 2021)

# Taiwan: 1970-2019
WD_m_TWN <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_TWN,LT_m = LT_PC_male_TWN,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1970,annee_fin = 2019)

# Ukraine: 1959-2013
WD_m_UKR <- calculs_ind_veuvage_m_pays(LT_f = LT_PC_female_UKR,LT_m = LT_PC_male_UKR,age=choix_age,ecart_age=choix_ecart_age,annee_deb =1959,annee_fin = 2013)



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
base_WD_international<-bind_rows(FRATNP,DEUTW,DEUTE,AUT,USA,UK,IRL,SWE,FIN,NOR,ESP,ITA,JPN,RUS,CAN, 
NLD,AUS,BLR,BEL,BGR,CHL,HRV,CZE,DNK,EST,GRC,HKG,HUN,ISL,ISR,LVA,LTU,LUX,NZL,POL,PRT,KOR,SVK,SVN, 
CHE,TWN,UKR )

# Checks sur les pays pour lesquels on a des valeurs calculées sur la période 1960-2018:
paysHMD_WDc_f_calculee_2018<-base_WD_international %>% filter(Year==2018 & is.na(WDc_f)==F) %>% select(pays)
paysHMD_WDc_f_calculee_1960<-base_WD_international %>% filter(Year==1960 & is.na(WDc_f)==F) %>% select(pays)
paysHMD_WDc_f_calculee_1960_2018<-paysHMD_WDc_f_calculee_2018 %>%
  inner_join(paysHMD_WDc_f_calculee_1960)

# base des 32 pays du HMD pour lesquels on dispose de tables de mortalité renseignées sur la période 1960-2018:

base_WD_international_1960_2018<-base_WD_international%>%
                            filter(pays %in% c("Australie","Autriche","Biélorussie","Belgique","Bulgarie","Canada","Tchéquie",
                                               "Danemark","Estonie","Finlande","France","All. (de l'Ouest)","All. (de l'Est)",
                                               "Hongrie","Islande","Irlande","Italie","Japon","Lettonie","Lituanie",
                                               "Luxembourg","Pologne","Portugal","Pays-Bas","Nouvelle-Zélande","Norvège",
                                               "Slovaquie","Espagne","Suède","Suisse","Royaume-Uni","Etats-Unis")
                                   & Year>1959 & Year<2019)

# Création d'une variable "code pays":

base_WD_international_1960_2018<-base_WD_international_1960_2018 %>%
  mutate(
    code_pays=case_when(
      pays=="Australie"~"AUS",
      pays=="Autriche"~"AUT",
      pays=="Biélorussie"~"BLR",
      pays=="Belgique"~"BEL",
      pays=="Bulgarie"~"BGR",
      pays=="Canada"~"CAN",
      pays=="Tchéquie"~"CZE",
      pays=="Danemark"~"DNK",
      pays=="Estonie"~"EST",
      pays=="Finlande"~"FIN",
      pays=="France"~"FRATNP",
      pays=="All. (de l'Ouest)"~"DEUTW",
      pays=="All. (de l'Est)"~"DEUTE",
      pays=="Hongrie"~"HUN",
      pays=="Islande"~"ISL",
      pays=="Irlande"~"IRL",
      pays=="Italie"~"ITA",
      pays=="Japon"~"JPN",
      pays=="Lettonie"~"LVA",
      pays=="Lituanie"~"LTU",
      pays=="Luxembourg"~"LUX",
      pays=="Pologne"~"POL",
      pays=="Portugal"~"PRT",
      pays=="Pays-Bas"~"NLD",
      pays=="Nouvelle-Zélande"~"NZL_NP",
      pays=="Norvège"~"NOR",
      pays=="Slovaquie"~"SVK",
      pays=="Espagne"~"ESP",
      pays=="Suède"~"SWE",
      pays=="Suisse"~"CHE",
      pays=="Royaume-Uni"~"GBR_NP",
      pays=="Etats-Unis"~"USA"
    ))



# Estimation des paramètres (location,scale) d'une loi de Gompertz pour modéliser la distribution de mortalité par âge.
# Distincion homme/femme.
# Estimation par pays/années.
# WARNING: étant donné le temps de calcul très important que nécessite de telles estimations, nous réalisons ici 
# une estimation du quadruplet (location_f,scale_f,location_m,scale_m) pour chaque pays tous les 5 ans entre
# 1960 et 2015 (soit 12 années):

# Chargement des fonctions de calcul:
source(paste0(scripts_path,"/Compute_life_table.R"),encoding="UTF-8")
source(paste0(scripts_path,"/Estimation_param_Gompertz.R"),encoding="UTF-8")


# Australie:
estimation_param_Gompertz_AUS<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_AUS,base_HMD_m=LT_PC_male_AUS,annee=annee,pays="Australie",age=60)[[1]] 
  estimation_param_Gompertz_AUS = bind_rows(estimation_param_Gompertz_AUS, estim_annee)
}

# Autriche:
estimation_param_Gompertz_AUT<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_AUT,base_HMD_m=LT_PC_male_AUT,annee=annee,pays="Autriche",age=60)[[1]] 
  estimation_param_Gompertz_AUT = bind_rows(estimation_param_Gompertz_AUT, estim_annee)
}

# Biélorussie:
estimation_param_Gompertz_BLR<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_BLR,base_HMD_m=LT_PC_male_BLR,annee=annee,pays="Biélorussie",age=60)[[1]] 
  estimation_param_Gompertz_BLR = bind_rows(estimation_param_Gompertz_BLR, estim_annee)
}

# Belgique:
estimation_param_Gompertz_BEL<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_BEL,base_HMD_m=LT_PC_male_BEL,annee=annee,pays="Belgique",age=60)[[1]] 
  estimation_param_Gompertz_BEL = bind_rows(estimation_param_Gompertz_BEL, estim_annee)
}

# Bulgarie:
estimation_param_Gompertz_BGR<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_BGR,base_HMD_m=LT_PC_male_BGR,annee=annee,pays="Bulgarie",age=60)[[1]] 
  estimation_param_Gompertz_BGR = bind_rows(estimation_param_Gompertz_BGR, estim_annee)
}

# Canada:
estimation_param_Gompertz_CAN<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_CAN,base_HMD_m=LT_PC_male_CAN,annee=annee,pays="Canada",age=60)[[1]] 
  estimation_param_Gompertz_CAN = bind_rows(estimation_param_Gompertz_CAN, estim_annee)
}

# Tchéquie:
estimation_param_Gompertz_CZE<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_CZE,base_HMD_m=LT_PC_male_CZE,annee=annee,pays="Tchéquie",age=60)[[1]] 
  estimation_param_Gompertz_CZE = bind_rows(estimation_param_Gompertz_CZE, estim_annee)
}

# Danemark:
estimation_param_Gompertz_DNK<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_DNK,base_HMD_m=LT_PC_male_DNK,annee=annee,pays="Danemark",age=60)[[1]] 
  estimation_param_Gompertz_DNK = bind_rows(estimation_param_Gompertz_DNK, estim_annee)
}

# Estonie:
estimation_param_Gompertz_EST<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_EST,base_HMD_m=LT_PC_male_EST,annee=annee,pays="Estonie",age=60)[[1]] 
  estimation_param_Gompertz_EST = bind_rows(estimation_param_Gompertz_EST, estim_annee)
}

# Finlande:
estimation_param_Gompertz_FIN<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_FIN,base_HMD_m=LT_PC_male_FIN,annee=annee,pays="Finlande",age=60)[[1]] 
  estimation_param_Gompertz_FIN = bind_rows(estimation_param_Gompertz_FIN, estim_annee)
}

# France:
estimation_param_Gompertz_FRATNP<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_FRATNP,base_HMD_m=LT_PC_male_FRATNP,annee=annee,pays="France",age=60)[[1]] 
  estimation_param_Gompertz_FRATNP = bind_rows(estimation_param_Gompertz_FRATNP, estim_annee)
}

# Allemagne (de l'Est):
estimation_param_Gompertz_DEUTE<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_DEUTE,base_HMD_m=LT_PC_male_DEUTE,annee=annee,pays="All. (de l'Est)",age=60)[[1]] 
  estimation_param_Gompertz_DEUTE = bind_rows(estimation_param_Gompertz_DEUTE, estim_annee)
}

# Allemagne (de l'Ouest):
estimation_param_Gompertz_DEUTW<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_DEUTW,base_HMD_m=LT_PC_male_DEUTW,annee=annee,pays="All. (de l'Ouest)",age=60)[[1]] 
  estimation_param_Gompertz_DEUTW = bind_rows(estimation_param_Gompertz_DEUTW, estim_annee)
}

# Hongrie:
estimation_param_Gompertz_HUN<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_HUN,base_HMD_m=LT_PC_male_HUN,annee=annee,pays="Hongrie",age=60)[[1]] 
  estimation_param_Gompertz_HUN = bind_rows(estimation_param_Gompertz_HUN, estim_annee)
}

# Islande:
estimation_param_Gompertz_ISL<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_ISL,base_HMD_m=LT_PC_male_ISL,annee=annee,pays="Islande",age=60)[[1]] 
  estimation_param_Gompertz_ISL = bind_rows(estimation_param_Gompertz_ISL, estim_annee)
}

# Irlande:
estimation_param_Gompertz_IRL<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_IRL,base_HMD_m=LT_PC_male_IRL,annee=annee,pays="Irlande",age=60)[[1]] 
  estimation_param_Gompertz_IRL = bind_rows(estimation_param_Gompertz_IRL, estim_annee)
}

# Italie:
estimation_param_Gompertz_ITA<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_ITA,base_HMD_m=LT_PC_male_ITA,annee=annee,pays="Italie",age=60)[[1]] 
  estimation_param_Gompertz_ITA = bind_rows(estimation_param_Gompertz_ITA, estim_annee)
}

# Japon:
estimation_param_Gompertz_JPN<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_JPN,base_HMD_m=LT_PC_male_JPN,annee=annee,pays="Japon",age=60)[[1]] 
  estimation_param_Gompertz_JPN = bind_rows(estimation_param_Gompertz_JPN, estim_annee)
}

# Lettonie:
estimation_param_Gompertz_LVA<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_LVA,base_HMD_m=LT_PC_male_LVA,annee=annee,pays="Lettonie",age=60)[[1]] 
  estimation_param_Gompertz_LVA = bind_rows(estimation_param_Gompertz_LVA, estim_annee)
}

# Lituanie:
estimation_param_Gompertz_LTU<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_LTU,base_HMD_m=LT_PC_male_LTU,annee=annee,pays="Lituanie",age=60)[[1]] 
  estimation_param_Gompertz_LTU = bind_rows(estimation_param_Gompertz_LTU, estim_annee)
}

# Luxembourg:
estimation_param_Gompertz_LUX<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_LUX,base_HMD_m=LT_PC_male_LUX,annee=annee,pays="Luxembourg",age=60)[[1]] 
  estimation_param_Gompertz_LUX = bind_rows(estimation_param_Gompertz_LUX, estim_annee)
}

# Pologne:
estimation_param_Gompertz_POL<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_POL,base_HMD_m=LT_PC_male_POL,annee=annee,pays="Pologne",age=60)[[1]] 
  estimation_param_Gompertz_POL = bind_rows(estimation_param_Gompertz_POL, estim_annee)
}

# Portugal:
estimation_param_Gompertz_PRT<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_PRT,base_HMD_m=LT_PC_male_PRT,annee=annee,pays="Portugal",age=60)[[1]] 
  estimation_param_Gompertz_PRT = bind_rows(estimation_param_Gompertz_PRT, estim_annee)
}

# Pays-Bas:
estimation_param_Gompertz_NLD<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_NLD,base_HMD_m=LT_PC_male_NLD,annee=annee,pays="Pays-Bas",age=60)[[1]] 
  estimation_param_Gompertz_NLD = bind_rows(estimation_param_Gompertz_NLD, estim_annee)
}

# Nouvelle-Zélande:
estimation_param_Gompertz_NZL_NP<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_NZL_NP,base_HMD_m=LT_PC_male_NZL_NP,annee=annee,pays="Nouvelle-Zélande",age=60)[[1]] 
  estimation_param_Gompertz_NZL_NP = bind_rows(estimation_param_Gompertz_NZL_NP, estim_annee)
}

# Norvège:
estimation_param_Gompertz_NOR<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_NOR,base_HMD_m=LT_PC_male_NOR,annee=annee,pays="Norvège",age=60)[[1]] 
  estimation_param_Gompertz_NOR = bind_rows(estimation_param_Gompertz_NOR, estim_annee)
}

# Slovaquie:
estimation_param_Gompertz_SVK<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_SVK,base_HMD_m=LT_PC_male_SVK,annee=annee,pays="Slovaquie",age=60)[[1]] 
  estimation_param_Gompertz_SVK = bind_rows(estimation_param_Gompertz_SVK, estim_annee)
}

# Espagne:
estimation_param_Gompertz_ESP<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_ESP,base_HMD_m=LT_PC_male_ESP,annee=annee,pays="Espagne",age=60)[[1]] 
  estimation_param_Gompertz_ESP = bind_rows(estimation_param_Gompertz_ESP, estim_annee)
}

# Suède:
estimation_param_Gompertz_SWE<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_SWE,base_HMD_m=LT_PC_male_SWE,annee=annee,pays="Suède",age=60)[[1]] 
  estimation_param_Gompertz_SWE = bind_rows(estimation_param_Gompertz_SWE, estim_annee)
}

# Suisse:
estimation_param_Gompertz_CHE<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_CHE,base_HMD_m=LT_PC_male_CHE,annee=annee,pays="Suisse",age=60)[[1]] 
  estimation_param_Gompertz_CHE = bind_rows(estimation_param_Gompertz_CHE, estim_annee)
}

# Royaume-Uni:
estimation_param_Gompertz_GBR_NP<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_GBR_NP,base_HMD_m=LT_PC_male_GBR_NP,annee=annee,pays="Royaume-Uni",age=60)[[1]] 
  estimation_param_Gompertz_GBR_NP = bind_rows(estimation_param_Gompertz_GBR_NP, estim_annee)
}

# Etats-Unis:
estimation_param_Gompertz_USA<-tibble()
for (annee in seq(from=1960, to=2015, by=5)){
  estim_annee<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_USA,base_HMD_m=LT_PC_male_USA,annee=annee,pays="Etats-Unis",age=60)[[1]] 
  estimation_param_Gompertz_USA = bind_rows(estimation_param_Gompertz_USA, estim_annee)
}


# Concaténation verticale des bases-pays:

estimation_param_Gompertz_pays_1960_2015<-bind_rows(estimation_param_Gompertz_AUS,estimation_param_Gompertz_AUT,
                                          estimation_param_Gompertz_BLR,estimation_param_Gompertz_BEL,
                                          estimation_param_Gompertz_BGR,estimation_param_Gompertz_CAN,
                                          estimation_param_Gompertz_CZE,estimation_param_Gompertz_DNK,
                                          estimation_param_Gompertz_EST,estimation_param_Gompertz_FIN,
                                          estimation_param_Gompertz_FRATNP,estimation_param_Gompertz_DEUTE,
                                          estimation_param_Gompertz_DEUTW,estimation_param_Gompertz_HUN,
                                          estimation_param_Gompertz_ISL,estimation_param_Gompertz_IRL,
                                          estimation_param_Gompertz_ITA,estimation_param_Gompertz_JPN,
                                          estimation_param_Gompertz_LVA,estimation_param_Gompertz_LTU,
                                          estimation_param_Gompertz_LUX,estimation_param_Gompertz_POL,
                                          estimation_param_Gompertz_PRT,estimation_param_Gompertz_NLD,
                                          estimation_param_Gompertz_NZL_NP,estimation_param_Gompertz_NOR,
                                          estimation_param_Gompertz_SVK,estimation_param_Gompertz_ESP,
                                          estimation_param_Gompertz_SWE,estimation_param_Gompertz_CHE,
                                          estimation_param_Gompertz_GBR_NP,estimation_param_Gompertz_USA)

estimation_param_Gompertz_pays_1960_2015<-estimation_param_Gompertz_pays_1960_2015 %>%
                                          rename(overlap_simul=overlap,
                                                 phi_simul=phi) %>%
                                          left_join(base_WD_international_1960_2018,
                                                    by=c("pays"="pays","Year"="Year"))
                                          
# Création d'une variable "code pays":

estimation_param_Gompertz_pays_1960_2015<-estimation_param_Gompertz_pays_1960_2015 %>%
      mutate(
        code_pays=case_when(
        pays=="Australie"~"AUS",
        pays=="Autriche"~"AUT",
        pays=="Biélorussie"~"BLR",
        pays=="Belgique"~"BEL",
        pays=="Bulgarie"~"BGR",
        pays=="Canada"~"CAN",
        pays=="Tchéquie"~"CZE",
        pays=="Danemark"~"DNK",
        pays=="Estonie"~"EST",
        pays=="Finlande"~"FIN",
        pays=="France"~"FRATNP",
        pays=="All. (de l'Ouest)"~"DEUTW",
        pays=="All. (de l'Est)"~"DEUTE",
        pays=="Hongrie"~"HUN",
        pays=="Islande"~"ISL",
        pays=="Irlande"~"IRL",
        pays=="Italie"~"ITA",
        pays=="Japon"~"JPN",
        pays=="Lettonie"~"LVA",
        pays=="Lituanie"~"LTU",
        pays=="Luxembourg"~"LUX",
        pays=="Pologne"~"POL",
        pays=="Portugal"~"PRT",
        pays=="Pays-Bas"~"NLD",
        pays=="Nouvelle-Zélande"~"NZL_NP",
        pays=="Norvège"~"NOR",
        pays=="Slovaquie"~"SVK",
        pays=="Espagne"~"ESP",
        pays=="Suède"~"SWE",
        pays=="Suisse"~"CHE",
        pays=="Royaume-Uni"~"GBR_NP",
        pays=="Etats-Unis"~"USA"
      ))


