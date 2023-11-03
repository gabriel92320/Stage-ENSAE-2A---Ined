
# Shifting vers la droite et rectangularisation, quels impacts sur la proba et la durée de veuvage?
# Modélisation des distributions de mortalité par âge: loi de Gompertz reparamétrisée à la Basellini (2019)

# Estimation des paramètres de la loi de Gompertz reparamétrisée pour chaque pays, chaque année (de 1960 à 2015 tous les
# cinq ans) et par sexe:
# Voir le programme "Construction_base_internationale_WD.R".
# Une base de données y est crée: estimation_param_Gompertz_pays_1960_2015

# En guise d'illustration, on peut montrer les densités observées et simulées pour la France en 1960 et en 2015:
# France:

estim_Gompertz_1960<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_FRATNP,base_HMD_m=LT_PC_male_FRATNP,annee=1960,pays="France",age=60) 
#estim_Gompertz_2015<-estimation_param_Gompertz(base_HMD_f=LT_PC_female_FRATNP,base_HMD_m=LT_PC_male_FRATNP,annee=2015,pays="France",age=60) 

graph_densite_f_FRA_1960<-estim_Gompertz_1960[[2]]
graph_densite_f_FRA_1960

graph_densite_m_FRA_1960<-estim_Gompertz_1960[[3]]
graph_densite_m_FRA_1960

# Graphiques à mettre en Annexe:
ggsave("graph_densite_f_FRA_1960.pdf",plot = graph_densite_f_FRA_1960)
ggsave("graph_densite_m_FRA_1960.pdf",plot = graph_densite_m_FRA_1960)


# Idée 1: on regarde l'évolution dans le temps du quadruplet (scale_f,scale_m,location_f,location_m) pour chaque pays 
# Rapprpocher ces évolutions des phénomènes démographiques connus sur la déformation des distributions de mortalité
# -> shifting vers la droite:
# -> compression (rectangularisation):

# Figure 7: Evolution de la moyenne et de la distribution des paramètres scale et location pour les femmes et les hommes
# sur la période 1960-2018

# Graph 7a: évolution de la moyenne et de la distribution des paramètres "scale (femme)" (box plot):
Graph7a <- ggplot(estimation_param_Gompertz_pays_1960_2015 %>% mutate(Year = as.factor(Year)), aes(x = Year, y = scale_f)) + 
  geom_boxplot(notch = TRUE, fill = "lightgray")+
  stat_summary(fun.y = mean, geom = "point",shape = 18, size = 2.5, color = "#FC4E07")+
  scale_x_discrete(limits=c("1960","1965","1970","1975","1980","1985","1990","1995","2000","2005","2010","2015"))+
  xlab("")+
  ylab("Paramètre scale (femme)")+
  theme_classic()

# Graph 7b: évolution de la moyenne et de la distribution des paramètres "scale (homme)" (box plot):
Graph7b <- ggplot(estimation_param_Gompertz_pays_1960_2015 %>% mutate(Year = as.factor(Year)), aes(x = Year, y = scale_m)) + 
  geom_boxplot(notch = TRUE, fill = "lightgray")+
  stat_summary(fun.y = mean, geom = "point",shape = 18, size = 2.5, color = "#FC4E07")+
  scale_x_discrete(limits=c("1960","1965","1970","1975","1980","1985","1990","1995","2000","2005","2010","2015"))+
  xlab("")+
  ylab("Paramètre scale (homme)")+
  theme_classic()

# Graph 7c: évolution de la moyenne et de la distribution des paramètres "location (femme)" (box plot):
# On rajoute 60 au paramètre location pour l'interpréter en terme d'âge modal de mortalité:

Graph7c <- ggplot(estimation_param_Gompertz_pays_1960_2015 %>% mutate(Year = as.factor(Year),location_f=location_f+60),
                  aes(x = Year, y = location_f)) + 
  geom_boxplot(notch = TRUE, fill = "lightgray")+
  stat_summary(fun.y = mean, geom = "point",shape = 18, size = 2.5, color = "#FC4E07")+
  scale_x_discrete(limits=c("1960","1965","1970","1975","1980","1985","1990","1995","2000","2005","2010","2015"))+
  xlab("")+
  ylab("Paramètre location (femme)")+
  theme_classic()

# Graph 7d: évolution de la moyenne et de la distribution des paramètres "location (homme)" (box plot):
Graph7d <- ggplot(estimation_param_Gompertz_pays_1960_2015 %>% mutate(Year = as.factor(Year),location_m=location_m+60),
                  aes(x = Year, y = location_m)) + 
  geom_boxplot(notch = TRUE, fill = "lightgray")+
  stat_summary(fun.y = mean, geom = "point",shape = 18, size = 2.5, color = "#FC4E07")+
  scale_x_discrete(limits=c("1960","1965","1970","1975","1980","1985","1990","1995","2000","2005","2010","2015"))+
  xlab("")+
  ylab("Paramètre location (homme)")+
  theme_classic()

#Export:
ggsave("Graph7a.pdf",plot = Graph7a)
ggsave("Graph7b.pdf",plot = Graph7b)
ggsave("Graph7c.pdf",plot = Graph7c)
ggsave("Graph7d.pdf",plot = Graph7d)

# On peut grapher par zone géographique les évolutions des paramètres ....


# TODO: on peut également regarder les écarts de scale/location femme-homme.


# Idée 2: examiner une éventuelle convergence de ces paramètres (de type sigma et/ou bêta)

# sigma-convergence (CV):
calcul_CV_param_1960_2015<-estimation_param_Gompertz_pays_1960_2015%>%
  filter(!pays %in% c("Islande","Luxembourg"))%>%
  group_by(Year)%>%
  summarise(
    cv_scale_f=cv(scale_f)*100,
    cv_scale_m=cv(scale_m)*100,
    cv_location_f=cv(location_f)*100,
    cv_location_m=cv(location_m)*100,
  )%>%
  pivot_longer(cols =starts_with("cv"),
               names_to = "type_indicateur",
               values_to = "coeff_variation")%>%
  mutate(type_indicateur=case_when(
    type_indicateur=="cv_scale_f" ~ "scale (femme)",
    type_indicateur=="cv_scale_m" ~ "scale (homme)",
    type_indicateur=="cv_location_f" ~ "location (femme)",
    type_indicateur=="cv_location_m" ~ "location (homme)"
  ))%>%
  arrange(type_indicateur,Year)

Graph8<-ggplot(calcul_CV_param_1960_2015,aes(x=Year, y=coeff_variation,color=type_indicateur)) +
  geom_line()+
  scale_color_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "bottom",legend.title = element_blank())+
  labs(x="",y="Coefficient de variation, en %" )

# bêta-convergence:

# Préparation des données: 1960-1990
# calcul de la variation moyenne entre 1960 et 1990 des 4 paramètres de la Gompertz
base_delta_scale_loc_1960_1990<-estimation_param_Gompertz_pays_1960_2015 %>%
  filter(Year %in% c(1960,1990) & !(pays=="Islande")) %>%
  group_by(pays) %>%
  mutate(delta_log_scale_f_1960_1990=(log(scale_f)-log(lag(scale_f)))/30*100,
         delta_log_scale_m_1960_1990=(log(scale_m)-log(lag(scale_m)))/30*100,
         delta_log_location_f_1960_1990=(log(location_f)-log(lag(location_f)))/30*100,
         delta_log_location_m_1960_1990=(log(scale_m)-log(lag(location_m)))/30*100
  ) %>%
  filter(Year==1990) %>%
  select(pays,delta_log_scale_f_1960_1990,delta_log_scale_m_1960_1990,delta_log_location_f_1960_1990,
         delta_log_location_m_1960_1990)

# jointure avec la base originelle restreinte à l'année 1960:
base_conv_scale_loc_1960_1990<- estimation_param_Gompertz_pays_1960_2015 %>%
  filter(Year==1960 & !(pays=="Islande")) %>%
  select(pays,scale_f,scale_m,location_f,location_m) %>%
  mutate(log_scale_f_1960=log(scale_f),
         log_scale_m_1960=log(scale_m),
         log_location_f_1960=log(location_f),
         log_location_m_1960=log(location_m)
  ) %>%
  left_join(base_delta_scale_loc_1960_1990,by="pays")
base_conv_scale_loc_1960_1990<-base_conv_scale_loc_1960_1990 %>%
  column_to_rownames("pays")

# Préparation des données: 1990-2015
# calcul de la variation moyenne entre 1990 et 2015 des 4 paramètres de la Gompertz
base_delta_scale_loc_1990_2015<-estimation_param_Gompertz_pays_1960_2015 %>%
  filter(Year %in% c(1990,2015) & !(pays=="Islande")) %>%
  group_by(pays) %>%
  mutate(delta_log_scale_f_1990_2015=(log(scale_f)-log(lag(scale_f)))/25*100,
         delta_log_scale_m_1990_2015=(log(scale_m)-log(lag(scale_m)))/25*100,
         delta_log_location_f_1990_2015=(log(location_f)-log(lag(location_f)))/25*100,
         delta_log_location_m_1990_2015=(log(scale_m)-log(lag(location_m)))/25*100
  ) %>%
  filter(Year==2015) %>%
  select(pays,delta_log_scale_f_1990_2015,delta_log_scale_m_1990_2015,delta_log_location_f_1990_2015,
         delta_log_location_m_1990_2015)

# jointure avec la base originelle restreinte à l'année 1990:
base_conv_scale_loc_1990_2015<- estimation_param_Gompertz_pays_1960_2015 %>%
  filter(Year==1990 & !(pays=="Islande")) %>%
  select(pays,scale_f,scale_m,location_f,location_m) %>%
  mutate(log_scale_f_1990=log(scale_f),
         log_scale_m_1990=log(scale_m),
         log_location_f_1990=log(location_f),
         log_location_m_1990=log(location_m)
  ) %>%
  left_join(base_delta_scale_loc_1990_2015,by="pays")
base_conv_scale_loc_1990_2015<-base_conv_scale_loc_1990_2015 %>%
  column_to_rownames("pays")

# Figure 9: bêta-convergence du paramètre "scale" (femme/homme) sur les deux sous-périodes
# 1960-1990 et 1990-2015

# Graph 9a: bêta-convergence du paramètre scale (femme) sur la période 1960-1990
# Régression OLS (pour avoir le bêta estimé et le R2 associé):
reg_OLS_Graph9a <- lm(delta_log_scale_f_1960_1990~log_scale_f_1960, data=base_conv_scale_loc_1960_1990) 
summary(reg_OLS_Graph9a)

Graph9a<-ggplot(base_conv_scale_loc_1960_1990, aes(y=delta_log_scale_f_1960_1990, x=log_scale_f_1960,label = rownames(base_conv_scale_loc_1960_1990)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1960 et 1990,en %")+
  xlab("Niveau en 1960,en log") +
  theme_classic()+
  annotate("text", x = 2.4, y = 0.1, label = "delta_log_scale = 3.2 -1.5*log_scale\n (R2=0.2)") +
  labs(title = "1960-1990")+
  theme(plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))+
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)

# Graph 9b: bêta-convergence du paramètre scale (femme) sur la période 1990-2018
# Régression OLS (pour avoir le bêta estimé et le R2 associé):
reg_OLS_Graph9b <- lm(delta_log_scale_f_1990_2015~log_scale_f_1990, data=base_conv_scale_loc_1990_2015) 
summary(reg_OLS_Graph9b)

Graph9b<-ggplot(base_conv_scale_loc_1990_2015, aes(y=delta_log_scale_f_1990_2015, x=log_scale_f_1990,label = rownames(base_conv_scale_loc_1990_2015)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1990 et 2015,en %")+
  xlab("Niveau en 1990,en log") +
  theme_classic()+
  annotate("text", x = 2.2, y = -0.8, label = "delta_log_scale = 2.6 -1.4*log_scale\n (R2=0.2)") +
  labs(title = "1990-2015")+
  theme(plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))+
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)

# Graph 9c: bêta-convergence du paramètre scale (homme) sur la période 1960-1990
# Régression OLS (pour avoir le bêta estimé et le R2 associé):
reg_OLS_Graph9c <- lm(delta_log_scale_m_1960_1990~log_scale_m_1960, data=base_conv_scale_loc_1960_1990) 
summary(reg_OLS_Graph9c)

Graph9c<-ggplot(base_conv_scale_loc_1960_1990, aes(y=delta_log_scale_m_1960_1990, x=log_scale_m_1960,label = rownames(base_conv_scale_loc_1960_1990)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1960 et 1990,en %")+
  xlab("Niveau en 1960,en log") +
  theme_classic()+
  annotate("text", x = 2.5, y = 0.6, label = "delta_log_scale = 3.2 -1.3*log_scale\n (R2=0.1)") +
  labs(title = "1960-1990")+
  theme(plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))+
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)

# Graph 9d: bêta-convergence du paramètre scale (homme) sur la période 1990-2015
# Régression OLS (pour avoir le bêta estimé et le R2 associé):
reg_OLS_Graph9d <- lm(delta_log_scale_m_1990_2015~log_scale_m_1990, data=base_conv_scale_loc_1990_2015) 
summary(reg_OLS_Graph9d)

Graph9d<-ggplot(base_conv_scale_loc_1990_2015, aes(y=delta_log_scale_m_1990_2015, x=log_scale_m_1990,label = rownames(base_conv_scale_loc_1990_2015)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1990 et 2015,en %")+
  xlab("Niveau en 1990,en log") +
  theme_classic()+
  annotate("text", x = 2.4, y = -0.2, label = "delta_log_scale = -4.1 +1.4*log_scale\n (R2=0.2)") +
  labs(title = "1990-2015")+
  theme(plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))+
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)


# Figure 10: bêta-convergence du paramètre "location" (femme/homme) sur les deux sous-périodes
# 1960-1990 et 1990-2015

# Graph 10a: bêta-convergence du paramètre location (femme) sur la période 1960-1990
# Régression OLS (pour avoir le bêta estimé et le R2 associé):
reg_OLS_Graph10a <- lm(delta_log_location_f_1960_1990~log_location_f_1960, data=base_conv_scale_loc_1960_1990) 
summary(reg_OLS_Graph10a)

Graph10a<-ggplot(base_conv_scale_loc_1960_1990, aes(y=delta_log_location_f_1960_1990, x=log_location_f_1960,label = rownames(base_conv_scale_loc_1960_1990)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1960 et 1990,en %")+
  xlab("Niveau en 1960,en log") +
  theme_classic()+
  annotate("text", x = 3.1, y = 0.9, label = "delta_log_loc = 7.9 -2.4*log_loc\n (R2=0.2)") +
  labs(title = "1960-1990")+
  theme(plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))+
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)

# Graph 10b: bêta-convergence du paramètre location (femme) sur la période 1990-2018
# Régression OLS (pour avoir le bêta estimé et le R2 associé):
reg_OLS_Graph10b <- lm(delta_log_location_f_1990_2015~log_location_f_1990, data=base_conv_scale_loc_1990_2015) 
summary(reg_OLS_Graph10b)

Graph10b<-ggplot(base_conv_scale_loc_1990_2015, aes(y=delta_log_location_f_1990_2015, x=log_location_f_1990,label = rownames(base_conv_scale_loc_1990_2015)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1990 et 2015,en %")+
  xlab("Niveau en 1990,en log") +
  theme_classic()+
  annotate("text", x = 3.2, y = 0.9, label = "delta_log_loc = 4.5 -1.2*log_loc\n (R2=0.1)") +
  labs(title = "1990-2015")+
  theme(plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))+
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)

# Graph 10c: bêta-convergence du paramètre location (homme) sur la période 1960-1990
# Régression OLS (pour avoir le bêta estimé et le R2 associé):
reg_OLS_Graph10c <- lm(delta_log_location_m_1960_1990~log_location_m_1960, data=base_conv_scale_loc_1960_1990) 
summary(reg_OLS_Graph10c)

Graph10c<-ggplot(base_conv_scale_loc_1960_1990, aes(y=delta_log_location_m_1960_1990, x=log_location_m_1960,label = rownames(base_conv_scale_loc_1960_1990)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1960 et 1990,en %")+
  xlab("Niveau en 1960,en log") +
  theme_classic()+
  annotate("text", x = 2.7, y = -1.6, label = "delta_log_loc = 7.7 -3.2*log_loc\n (R2=0.5)") +
  labs(title = "1960-1990")+
  theme(plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))+
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)

# Graph 10d: bêta-convergence du paramètre location (homme) sur la période 1990-2015
# Régression OLS (pour avoir le bêta estimé et le R2 associé):
reg_OLS_Graph10d <- lm(delta_log_location_m_1990_2015~log_location_m_1990, data=base_conv_scale_loc_1990_2015) 
summary(reg_OLS_Graph10d)

Graph10d<-ggplot(base_conv_scale_loc_1990_2015, aes(y=delta_log_location_m_1990_2015, x=log_location_m_1990,label = rownames(base_conv_scale_loc_1990_2015)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1990 et 2015,en %")+
  xlab("Niveau en 1990,en log") +
  theme_classic()+
  annotate("text", x = 2.9, y = -0.5, label = "delta_log_scale = 17.5 -6.9*log_scale\n (R2=0.9)") +
  labs(title = "1990-2015")+
  theme(plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))+
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)


# Idée 3: faire une ACP sur la base de ces paramètres pris en niveau et en évolution et projeter en supplémentaire les
# proba et durées de veuvage...

# En longitudinal: évolution sur les deux sous-périodes 1960-1990 et 1990-2015

# 1 - 1960-1990
# calcul de la variation moyenne entre 1960 et 1990 des 4 paramètres de la Gompertz
base_delta_1960_1990<-estimation_param_Gompertz_pays_1960_2015 %>%
  filter(Year %in% c(1960,1990) & !(pays=="Islande")) %>%
  group_by(pays) %>%
  mutate(delta_scale_f=(log(scale_f)-log(lag(scale_f)))/30,
         delta_scale_m=(log(scale_m)-log(lag(scale_m)))/30,
         delta_location_f=(log(location_f)-log(lag(location_f)))/30,
         delta_location_m=(log(location_m)-log(lag(location_m)))/30,
         delta_P_W_f=(P_W_simul_f-lag(P_W_simul_f))/30,
         delta_WDc_f=(WDc_simul_f-lag(WDc_simul_f))/30,
  ) %>%
  filter(Year==1990) %>%
  select(pays,delta_scale_f,delta_scale_m,delta_location_f,delta_location_m,
         delta_P_W_f,delta_WDc_f)

# jointure avec la base originelle restreinte à l'année 1960:
data.ACP_1<- estimation_param_Gompertz_pays_1960_2015 %>%
  filter(Year==1960 & !(pays=="Islande")) %>%
  select(pays,scale_f,scale_m,location_f,location_m) %>%
  rename(scale_f_1960=scale_f,
         scale_m_1960=scale_m,
         location_f_1960=location_f,
         location_m_1960=location_m
  ) %>%
  left_join(base_delta_1960_1990,by="pays") %>%
  column_to_rownames("pays")

# Phase exploratoire de l'ACP-CAH (choix des variables supplémentaires, nombre d'axes à retenir, nombre
# de clusters...)
# res.PCA_1_shiny = PCAshiny(data.ACP_1)

# Version finale de l'ACP: on met ici la Bielorussie (ind. 3), la Bulgarie (ind. 5) et la Lituanie (ind.19)
# en supplémentaire car il s'agit d'individus atypiques et influents.

# on réalise l'ACP sur les 8 variables actives + 2 variables suppélemntaires (delta_P_W_f,delta_WDc_f):
res.PCA1<-PCA(data.ACP_1,quanti.sup=c(9,10),ind.sup=c(3,5,19),graph=FALSE)

# on génère les figures associées: graph des individus (1er plan factoriel) + cercle des corrélations des variables:
plot.PCA(res.PCA1,choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA_1_shiny,invisible=c('quali'),habillage='delta_WDc_f',title="Graphe des individus de l'ACP",label =c('ind','ind.sup'))

# Version finale du code de la CAH:

# on réalise la CAH en retenant 4 clusters (choix à partir du graph de perte d'inertie) à partir des 5 premiers axes
# factoriels de l'ACP:
res.CAH1<-HCPC(res.PCA1,nb.clust=4,consol=FALSE,graph=FALSE)

# Choix du nombre de clusters à partir du graph des pertes d'inertie intra-classe en fonction du nombre de classes
# retenues pour la partition.
plot(res.CAH1, choice = "bar")
# on retient 4 clusters.

# on génère les figures associées:
plot.HCPC(res.CAH1,choice='tree',title='Arbre hiérarchique')
plot.HCPC(res.CAH1,choice='map',draw.tree=FALSE,title='Plan factoriel')
plot.HCPC(res.CAH1,choice='3D.map',ind.names=FALSE,centers.plot=FALSE,angle=60,title='Arbre hiérarchique sur le plan factoriel')


# Sauvegarder les graphiques au format PDF directement à partir de la sortie Quartz.

# 2 - 1990-2015
# calcul de la variation moyenne entre 1990 et 2015 des 4 paramètres de la Gompertz
base_delta_1990_2015<-estimation_param_Gompertz_pays_1960_2015 %>%
  filter(Year %in% c(1990,2015) & !(pays=="Islande")) %>%
  group_by(pays) %>%
  mutate(delta_scale_f=(log(scale_f)-log(lag(scale_f)))/25,
         delta_scale_m=(log(scale_m)-log(lag(scale_m)))/25,
         delta_location_f=(log(location_f)-log(lag(location_f)))/25,
         delta_location_m=(log(location_m)-log(lag(location_m)))/25,
         delta_P_W_f=(P_W_simul_f-lag(P_W_simul_f))/25,
         delta_WDc_f=(WDc_simul_f-lag(WDc_simul_f))/25,
  ) %>%
  filter(Year==2015) %>%
  select(pays,delta_scale_f,delta_scale_m,delta_location_f,delta_location_m,
         delta_P_W_f,delta_WDc_f)

# jointure avec la base originelle restreinte à l'année 1960:
data.ACP_2<- estimation_param_Gompertz_pays_1960_2015 %>%
  filter(Year==1990 & !(pays=="Islande")) %>%
  select(pays,scale_f,scale_m,location_f,location_m) %>%
  rename(scale_f_1990=scale_f,
         scale_m_1990=scale_m,
         location_f_1990=location_f,
         location_m_1990=location_m
  ) %>%
  left_join(base_delta_1990_2015,by="pays") %>%
  column_to_rownames("pays")

# Travail exploratoire:
#res.PCA_2_shiny = PCAshiny(data.ACP_2)

# Version finale de l'ACP: on met ici la Bielorussie (ind. 3), la Bulgarie (ind. 5) et la Lituanie (ind.19)
# en supplémentaire car il s'agit d'individus atypiques et influents. Pas souci de comparabilité avec
# l'ACP 1.

# on réalise l'ACP sur les 8 variables actives + 2 variables suppélemntaires (delta_P_W_f,delta_WDc_f):
res.PCA2<-PCA(data.ACP_2,quanti.sup=c(9,10),ind.sup=c(3,5,19),graph=FALSE)

# on génère les figures associées: graph des individus (1er plan factoriel) + cercle des corrélations des variables
plot.PCA(res.PCA,choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA,habillage='delta_WDc_f',title="Graphe des individus de l'ACP")

# Version finale du code de la CAH:

# on réalise la CAH en retenant 3 clusters (choix à partir du graph de perte d'inertie) à partir des 5 premiers axes
# factoriels de l'ACP:
res.CAH2<-HCPC(res.PCA2,nb.clust=3,consol=FALSE,graph=FALSE)

# Choix du nombre de clusters à partir du graph des pertes d'inertie intra-classe en fonction du nombre de classes
# retenues pour la partition.
plot(res.CAH2, choice = "bar")
# on retient 3 clusters.


# on génère les figures associées:
plot.HCPC(res.CAH2,choice='tree',title='Arbre hiérarchique')
plot.HCPC(res.CAH2,choice='map',draw.tree=FALSE,title='Plan factoriel')
plot.HCPC(res.CAH2,choice='3D.map',ind.names=FALSE,centers.plot=FALSE,angle=60,title='Arbre hiérarchique sur le plan factoriel')

# Sauvegarder les graphiques au format PDF directement à partir de la sortie Quartz.
