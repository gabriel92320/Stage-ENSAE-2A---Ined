
# On développe ici une fonction qui calcule pour chaque pays du HMD et chaque année une série d'indicateurs/paramètres
# démographiques résultant de l'estimation de la distribution de mortalité par une loi de Gompertz.

# Fonction principal: estimation des paramètres de la Gompertz pour chaque pays et chaque année +
# calculs d'indicateurs démographiques d'intérêt (overlap,phi,proba et durée de veuvage,espérance de vie) à 
# partir des probas de décès et de survie simulées à chaque âge (à partir d'un âge fixé via le paramètre "age" de 
# la fonction.). Usuellement on fixera "age" à 60 ans.
estimation_param_Gompertz<-function(base_HMD_f,base_HMD_m,annee,pays,age){
  
  # Les inputs: extrait des lifetables du HMD pour la France en 2016
  base_f<-base_HMD_f %>% filter(Year=={{annee}}) %>% select(Year,Age,dx) %>% rename(dx_f=dx) 
  base_m<-base_HMD_m %>% filter(Year=={{annee}}) %>% select(Year,Age,dx) %>% rename(dx_m=dx) 
  
  # Jointure:
  base_f_m <- base_f %>%
    left_join(base_m,by=c("Year","Age"))
  
  data_expanded_f = tibble()
  data_expanded_m = tibble()
  
  age_max=110-age
  
  ages = c(0:age_max)
  
  for (a in ages){
    data_temp = base_f_m %>% filter(Age == a+60)
    for_expanded_f = tibble(age_deces_f = rep(a, data_temp$dx_f))
    for_expanded_m = tibble(age_deces_m = rep(a, data_temp$dx_m))
    data_expanded_f = bind_rows(data_expanded_f, for_expanded_f)
    data_expanded_m = bind_rows(data_expanded_m, for_expanded_m)
  }
  data_estim_f = data_expanded_f %>% mutate(noind_f = row_number())
  data_estim_m = data_expanded_m %>% mutate(noind_m = row_number())
  
  age_min_f = min(data_estim_f$age_deces_f)
  age_max_f = max(data_estim_f$age_deces_f)
  age_min_m = min(data_estim_m$age_deces_m)
  age_max_m = max(data_estim_m$age_deces_m)
  
  table_f = tidyr::expand(data_estim_f, noind_f = noind_f, age_ext_f =c(age_min_f:age_max_f))
  table_m = tidyr::expand(data_estim_m, noind_m = noind_m, age_ext_m =c(age_min_m:age_max_m))
  
  data_estim_f = table_f %>% left_join(data_estim_f)
  data_estim_m = table_m %>% left_join(data_estim_m)
  
  data_estim_f = data_estim_f %>%
    filter(age_ext_f <= age_deces_f) %>%
    mutate(
      age_ent_f = age_ext_f - 1,
      event_f = ifelse(age_ext_f == age_deces_f, TRUE, FALSE)
    )
  data_estim_m = data_estim_m %>%
    filter(age_ext_m <= age_deces_m) %>%
    mutate(
      age_ent_m = age_ext_m - 1,
      event_m = ifelse(age_ext_m == age_deces_m, TRUE, FALSE)
    )
  
  
  temp_f = Surv(data_estim_f$age_ent_f, data_estim_f$age_ext_f, data_estim_f$event_f)
  temp_m = Surv(data_estim_m$age_ent_m, data_estim_m$age_ext_m, data_estim_m$event_m)
  
  estim_mle_f = phreg(formula = temp_f ~ 1, dist = "gompertz", param = "rate")
  estim_mle_m = phreg(formula = temp_m ~ 1, dist = "gompertz", param = "rate")
  
  # On récupère les valeurs simulées de la fonction de densité de la Gompertz pour chaque âge:
  
  # Réindexation des âges:
  Age_f=seq(age_min_f+age,age_max_f+age,1)
  Age_m=seq(age_min_m+age,age_max_m+age,1)
  
  density_f = tibble(d_f=eha::dgompertz(seq(age_min_f,age_max_f,1),param = "rate",
                                        rate = estim_mle_f$coefficients[1],
                                        shape = exp(estim_mle_f$coefficients[2])),Age=Age_f)
  density_m = tibble(d_m=eha::dgompertz(seq(age_min_m,age_max_m,1),param = "rate",
                                        rate = estim_mle_m$coefficients[1],
                                        shape = exp(estim_mle_m$coefficients[2])),Age=Age_m)
  
  # On récupère les valeurs simulées des proba de survies associées à la Gompertz pour chaque âge (normalisation à 1 à la valeur de "age"):
  
  survival_f = tibble(l_f=1 - eha::pgompertz(seq(age_min_f,age_max_f,1),param = "rate",
                                             rate = estim_mle_f$coefficients[1],
                                             shape = exp(estim_mle_f$coefficients[2])),Age=Age_f)
  survival_m = tibble(l_m=1 - eha::pgompertz(seq(age_min_m,age_max_m,1),param = "rate",
                                             rate = estim_mle_m$coefficients[1],
                                             shape = exp(estim_mle_m$coefficients[2])),Age=Age_m)
  
  # On en déduit les espérances de vie associées à ces proba de survie simulées pour chaque âge (normalisées à 1 à la valeur de "age") :
  life_expectation_f<-compute_life_table(data = survival_f %>% rename(y = l_f))
  life_expectation_m<-compute_life_table(data = survival_m %>% rename(y = l_m))
  
  # On calcule les indicateurs suivants à partir des valeurs simulées de "hazard" et de "survival" par âge:
  
  # 1) Overlap:
  table_aire = density_f %>% 
    left_join(density_m) %>%
    mutate(aire = pmin(d_f,d_m)) %>%
    drop_na() #permet d'ignorer les cas où il n'y a pas de décès/survivant aux âges extrêmes certaines années (ex: à 109, 110 ans!)
  
  overlap = integrate.xy(table_aire$Age, table_aire$aire)
  
  # 2) L'outsurvival (ou Phi)  à l'âge correspondant à la valeur de la variable "age" (sans écart d'âge entre conjoints):
  table_phi = density_f %>% left_join(survival_m) %>% mutate(product=d_f*l_m) %>%
    drop_na() #permet d'ignorer les cas où il n'y a pas de décès/survivant aux âges extrêmes certaines années (ex: à 109, 110 ans!)
  
  phi = integrate.xy(table_phi$Age, table_phi$product)
  
  # 3) Probabilité de veuvage féminin  à l'âge correspondant à la valeur de la variable "age" (sans écart d'âge entre conjoints):
  proba = density_m %>% left_join(survival_f) %>% mutate(product=d_m*l_f) %>%
    drop_na() #permet d'ignorer les cas où il n'y a pas de décès/survivant aux âges extrêmes certaines années (ex: à 109, 110 ans!)
  
  P_W_simul_f = integrate.xy(proba$Age, proba$product)
  
  # 4) Durée inconditionnelle de veuvage féminin à l'âge correspondant à la valeur de la variable "age" (sans écart d'âge entre conjoints):
  UWD = survival_f %>% left_join(survival_m) %>% mutate(product=l_f*l_m) %>%
    drop_na() #permet d'ignorer les cas où il n'y a pas de décès/survivant aux âges extrêmes certaines années (ex: à 109, 110 ans!)
  
  WD_simul_f = integrate.xy(survival_f$Age, survival_f$l_f)-integrate.xy(UWD$Age, UWD$product)
  
  # 5) Durée conditionnelle de veuvage féminin à l'âge correspondant à la valeur de la variable "age" (sans écart d'âge entre conjoints):
  WDc_simul_f=WD_simul_f/P_W_simul_f
  
  # 6) Espérance de vie des femmes à l'âge correspondant à la valeur de la variable "age" (sans écart d'âge entre conjoints):
  ex_simul_f<-life_expectation_f$e[1]+0.5
  # Note: on rajoute 0.5 pour tenir compte du fait que le quotient de mortalite de la ligne correspondant a l'age a represente
  #la proba de deceder entre l'age a et l'age a+1 sachant que l'on a survecu jusqu'a l'age a. Autrement dit l'EV moyenne d'une
  #personne correspondant a cette ligne est de a+0.5.
  
  # 7) Espérance de vie des hommes à l'âge correspondant à la valeur de la variable "age" (sans écart d'âge entre conjoints): 
  ex_simul_m<-life_expectation_m$e[1]+0.5
  # Note: on rajoute 0.5 pour tenir compte du fait que le quotient de mortalite de la ligne correspondant a l'age a represente
  #la proba de deceder entre l'age a et l'age a+1 sachant que l'on a survecu jusqu'a l'age a. Autrement dit l'EV moyenne d'une
  #personne correspondant a cette ligne est de a+0.5.
  
  # 8) Calcul de l'écart d'espérance de vie homme-femme à l'âge correspondant à la valeur de la variable "age":
  # -> cela correspond aussi à la mesure dite "naïve" de la durée de veuvage (sans écart d'âge entre conjoints).
  WD_naif_simul_f=ex_simul_f-ex_simul_m
  
  # Récupération des paramètres de la loi de Gompertz:
  rate_f = estim_mle_f$coefficients[1] # coeff de la covariate "rate" dans la sortie de l'estimation  avec la fonction "phreg()"
  shape_f = exp(estim_mle_f$coefficients[2]) # exp. du coeff de la covariate "log(level)" dans la sortie de l'estimation avec la fonction "phreg()"
  rate_m = estim_mle_m$coefficients[1] # coeff de la covariate "rate" dans la sortie de l'estimation  avec la fonction "phreg()"
  shape_m = exp(estim_mle_m$coefficients[2]) # exp. du coeff de la covariate "log(level)" dans la sortie de l'estimation avec la fonction "phreg()"
  
  # Calcul des paramètres "scale" et "location" associés plus interprétables (voir le papier de Besellini&al.,2019):
  scale_f = 1/rate_f
  location_f = scale_f*log(rate_f/shape_f)
  scale_m = 1/rate_m
  location_m = scale_m*log(rate_m/shape_m)
  
  # on peut stocker les estimations des paramètres de la loi de Gompertz (+ indicateurs utiles pour l'analyse démographique):
  parameters = tibble(pays=pays,Year=annee,rate_f=rate_f,shape_f=shape_f,location_f=location_f,scale_f=scale_f,
                      rate_m=rate_m,shape_m=shape_m,location_m=location_m,scale_m=scale_m,overlap=overlap,
                      phi=phi,P_W_simul_f=P_W_simul_f,WD_simul_f=WD_simul_f,WDc_simul_f=WDc_simul_f,
                      ex_simul_f=ex_simul_f,ex_simul_m=ex_simul_m,WD_naif_simul_f=WD_naif_simul_f)
  
  # on peut grapher la densité observée dans HMD et la densité simulée avec la Gompertz pour chaque âge (à partir de la valeur de "age"):
  table_estim_dx_Gompertz<-density_f %>%
    left_join(density_m) %>%
    mutate(type="Gompertz simulation") 
  
  table_obs_dx_Gompertz_f<-base_HMD_f%>%
    filter(Year==annee & Age>=age)%>%
    select(Age,dx)%>%
    mutate(dx = dx/sum(dx,na.rm=T))%>%
    rename(d_f=dx)
  table_obs_dx_Gompertz_m<-base_HMD_m%>%
    filter(Year==annee & Age>=age)%>%
    select(Age,dx)%>%
    mutate(dx = dx/sum(dx,na.rm=T))%>%
    rename(d_m=dx)
  table_obs_dx_Gompertz<-table_obs_dx_Gompertz_f%>%
    left_join(table_obs_dx_Gompertz_m,by="Age")%>%
    mutate(type="HMD life table")
  
  plot_data_density<-bind_rows(table_obs_dx_Gompertz,table_estim_dx_Gompertz)
  
  annee2<-as.character(annee)
  
  graph_density_f<- ggplot(plot_data_density,
                           aes(Age, d_f, col=type, lty=type)) + geom_line(size=1) + theme_bw() +
    scale_color_grey(start=0, end=0.66)+
    xlab("Age")+ylab("Density")+
    theme(legend.position = "bottom")+
    labs(col = "", lty = "", title = pays, subtitle = paste0("Femme-",annee2))+
    theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5)) +
    theme(plot.subtitle = element_text(size = 11L, face = "bold", hjust = 0.5))
  
  graph_density_m<- ggplot(plot_data_density,
                           aes(Age, d_m, col=type, lty=type)) + geom_line(size=1) + theme_bw() +
    scale_color_grey(start=0, end=0.66)+
    xlab("Age")+ylab("Density")+
    theme(legend.position = "bottom")+
    labs(col = "", lty = "", title = pays, subtitle = paste0("Homme-",annee2))+
    theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5)) +
    theme(plot.subtitle = element_text(size = 11L, face = "bold", hjust = 0.5))
  
  # liste des outputs de la fonction:
  estim_Gompertz<-list(parameters,graph_density_f,graph_density_m,density_f,density_m,survival_f,survival_m,table_aire,table_phi,proba,UWD)
  
  return(estim_Gompertz) 
}