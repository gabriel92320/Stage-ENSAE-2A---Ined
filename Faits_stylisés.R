
# Partie "Faits stylisés" du Rapport de stage (version finale):

# Liste des figures générées pour cette partie:

# Figure 1:
# a. Niveaux de la probabilité de veuvage des femmes à 60 ans
# b. Niveaux de la durée de veuvage des femmes à 60 ans
# Note: Mettre en Annexe les deux mêmes graphiques pour les hommes.

# Figure 2:
# a. Corrélations entre la probabilité de veuvage des femmes et l'écart d'espérance de vie femme/homme
# (tracer les nuages de points pour différentes années: 1960,1990,2018)
# b. Corrélations entre la durée de veuvage des femmes et l'écart d'espérance de vie femme/homme
# (tracer les nuages de points pour différentes années: 1960,1990,2018)
# Mettre en Annexe les matrices de corrélations: coeff. de corrélation de Pearsons + Pvalue associée au test de significativité.

# Figure 3:
# a. Evolution moyenne de la probabilité de veuvage des femmes à 60 ans + boxplot pour avoir l'évolution de la distribution 
# b. Evolution moyenne de la durée de veuvage des femmes à 60 ans + boxplot pour avoir l'évolution de la distribution 
# Note: Mettre en Annexe les deux mêmes graphiques pour les hommes.
# R: cette figure permet d'indtroduire la sigma-convergence des proba/durées de veuvage.
# Pour compléter les box-plots, on peut rajouter en Annexe, un graphique donnant l'évolution des coefficients de variation
# pour la proba/durée de veuvage.

# Figure 4:
# Evolution du coefficient de variation (CV) de la probabilité et de la durée de veuvage des femmes de 60 ans

# Figure 5:
# a. Bêta-convergence absolue des probabilités de veuvage des femmes sur la période 1960-1990 (tracer le nuage de points)
# b. Bêta-convergence absolue des probabilités de veuvage des femmes sur la période 1990-2018 (tracer le nuage de points)
# Mettre en Annexe les mêmes graphiques pour la  durée de veuvage des femmes d'une part, et la durée de veuvage des hommes
# d'autre part.

# Figure 6:
# Tableau 6a: estimation en "cross section" de la bêta-convergence absolue de la proba/durée de veuvage
# Estimation MCO et distinguer différentes sous-périodes temporelles (une colonne du tableau correspond à une sous-période)
# Tableau 6b: estimation en "panel" de la bêta-convergence absolue de la proba/durée de veuvage
# Estimation LSDV (avec effet fixe pays) et distinguer différentes sous-périodes temporelles (une colonne du tableau
# correspond à une sous-période); éventuellement spécification avec un terme autoregressif (modèle dynamique) avec 
# estimation GMM-DIFF...

# Figures optionnelles: ACP
# Probablement pas, car on n'aura pas la place!!

############################################################################################################################

# Figure 1:
# a. Niveaux de la probabilité de veuvage des femmes à 60 ans
# b. Niveaux de la durée de veuvage des femmes à 60 ans
# Note: Mettre en Annexe les deux mêmes graphiques pour les hommes.

# Création de la Figure 1:

base_WD_international_2018<-base_WD_international_1960_2018%>%
  filter(Year==2018 &!(pays=="Islande"))%>%
  mutate(ind_FRA=if_else(pays=="France",TRUE,FALSE))

# Graphique 1a: les niveaux de proba de veuvage des différents pays en 2018
# Présentation par ordre croissant;
# Calcul sur la sous-population des femmes de 60 ans en 2018.

Graph1a <-ggplot(base_WD_international_2018) +
  aes(x = fct_rev(fct_reorder(pays,P_W_f)), y = P_W_f,color=ind_FRA) +
  geom_point() +
  xlab("") +
  ylab("En %") +
  theme(legend.position = "none")+
  scale_color_manual(values=c("black", "red"))+
  coord_flip()+
  labs(title = "Probabilité de veuvage") +
  theme(plot.title = element_text(size = 10L, face = "bold", hjust = 0.5))

Graph1a_solo <-ggplot(base_WD_international_2018) +
  aes(x = fct_rev(fct_reorder(pays,P_W_f)), y = P_W_f,color=ind_FRA) +
  geom_point(size=3,alpha=1) +
  xlab("") +
  ylab("En %") +
  theme(legend.position = "none")+
  scale_color_manual(values=c("black", "red"))+
  coord_flip()+
  theme(axis.text.y = element_text(size = rel(1.5),color = "black"))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "grey50"),
        panel.grid.major.y = element_line(colour = "grey90",linetype = "dashed"),
        panel.ontop = TRUE)+
  theme(axis.ticks.y = element_blank())

# Graphique 1b: les niveaux de durée de veuvage des différents pays en 2018:
# Présentation par ordre croissant;
# Calcul sur la sous-population des femmes âgées de 60 ans en 2018.

Graph1b <- ggplot(base_WD_international_2018) +
  aes(x = fct_rev(fct_reorder(pays,WDc_f)), y = WDc_f,color=ind_FRA) +
  geom_point() +
  xlab("") +
  ylab("En années") +
  theme(legend.position = "none")+
  scale_color_manual(values=c("black", "red"))+
  coord_flip()+
  labs(title = "Durée espérée de veuvage") +
  theme(plot.title = element_text(size = 10L, face = "bold", hjust = 0.5))

Graph1b_solo <-ggplot(base_WD_international_2018) +
  aes(x = fct_rev(fct_reorder(pays,WDc_f)), y = WDc_f,color=ind_FRA) +
  geom_point(size=3,alpha=1) +
  xlab("") +
  ylab("En années") +
  theme(legend.position = "none")+
  scale_color_manual(values=c("black", "red"))+
  coord_flip()+
  theme(axis.text.y = element_text(size = rel(1.5),color = "black"))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "grey50"),
        panel.grid.major.y = element_line(colour = "grey90",linetype = "dashed"),
        panel.ontop = TRUE)+
  theme(axis.ticks.y = element_blank())

Graph1a_b<-ggarrange(Graph1a,Graph1b,ncol = 2,nrow = 1)

#Export:
ggsave("Graph1a_b.pdf",plot = Graph1a_b,width = 11, height = 8)
ggsave("Graph1a.pdf",plot = Graph1a_solo)
ggsave("Graph1b.pdf",plot = Graph1b_solo)


# Figure 1 (en Annexe): réplication des deux graphiques pour les hommes

# Graphique 1a (Annexe): les niveaux de proba de veuvage des différents pays en 2018
# Présentation par ordre croissant;
# Calcul sur la sous-population des hommes de 60 ans en 2018.

Graph1a_Annexe <-ggplot(base_WD_international_2018) +
  aes(x = fct_rev(fct_reorder(pays,P_W_m)), y = P_W_m,color=ind_FRA) +
  geom_point() +
  xlab("") +
  ylab("En %") +
  theme(legend.position = "none")+
  scale_color_manual(values=c("black", "red"))+
  coord_flip()+
  labs(title = "Probabilité de veuvage") +
  theme(plot.title = element_text(size = 10L, face = "bold", hjust = 0.5))

Graph1a_solo_Annexe <-ggplot(base_WD_international_2018) +
  aes(x = fct_rev(fct_reorder(pays,P_W_m)), y = P_W_m,color=ind_FRA) +
  geom_point(size=3,alpha=1) +
  xlab("") +
  ylab("En %") +
  theme(legend.position = "none")+
  scale_color_manual(values=c("black", "red"))+
  coord_flip()+
  theme(axis.text.y = element_text(size = rel(1.5),color = "black"))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "grey50"),
        panel.grid.major.y = element_line(colour = "grey90",linetype = "dashed"),
        panel.ontop = TRUE)+
  theme(axis.ticks.y = element_blank())

# Graphique 1b: les niveaux de durée de veuvage des différents pays en 2018:
# Présentation par ordre croissant;
# Calcul sur la sous-population des hommes âgés de 60 ans en 2018.

Graph1b_Annexe <- ggplot(base_WD_international_2018) +
  aes(x = fct_rev(fct_reorder(pays,WDc_m)), y = WDc_m,color=ind_FRA) +
  geom_point() +
  xlab("") +
  ylab("En années") +
  theme(legend.position = "none")+
  scale_color_manual(values=c("black", "red"))+
  coord_flip()+
  labs(title = "Durée espérée de veuvage") +
  theme(plot.title = element_text(size = 10L, face = "bold", hjust = 0.5))

Graph1b_solo_Annexe <-ggplot(base_WD_international_2018) +
  aes(x = fct_rev(fct_reorder(pays,WDc_m)), y = WDc_m,color=ind_FRA) +
  geom_point(size=3,alpha=1) +
  xlab("") +
  ylab("En années") +
  theme(legend.position = "none")+
  scale_color_manual(values=c("black", "red"))+
  coord_flip()+
  theme(axis.text.y = element_text(size = rel(1.5),color = "black"))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "grey50"),
        panel.grid.major.y = element_line(colour = "grey90",linetype = "dashed"),
        panel.ontop = TRUE)+
  theme(axis.ticks.y = element_blank())

Graph1a_b_Annexe<-ggarrange(Graph1a_Annexe,Graph1b_Annexe,ncol = 2,nrow = 1)

#Export:
ggsave("Graph1a_b_Annexe.pdf",plot = Graph1a_b_Annexe,width = 11, height = 8)
ggsave("Graph1a_Annexe.pdf",plot = Graph1a_solo_Annexe)
ggsave("Graph1b_Annexe.pdf",plot = Graph1b_solo_Annexe)


# Figure 2:
# a. Corrélations entre la probabilité de veuvage des femmes et l'écart d'espérance de vie femme/homme
# (tracer les nuages de points pour différentes années: 1960; 1990; 2018)
# b. Corrélations entre la durée de veuvage des femmes et l'écart d'espérance de vie femme/homme
# (tracer les nuages de points pour différentes années: 1960; 1990; 2018)
# Mettre en Annexe les matrices de corrélations: coeff. de corrélation de Pearsons + Pvalue associée au test de significativité.

# Création de la Figure 2:

base_WD_international_1960_1990_2018<-base_WD_international_1960_2018%>%
  filter(Year %in% c(1960,1990,2018) &!(pays=="Islande"))%>%
  mutate(ind_FRA=if_else(pays=="France",TRUE,FALSE)) %>%
  mutate(Year=as.factor(Year)) %>%
  rename(ecart_EV_f_m=WD_naif_f)

# Graph 2a: Corrélations entre la probabilité de veuvage des femmes et l'écart d'espérance de vie femme/homme
# (tracer les nuages de points pour différentes années: 1960; 1990; 2018)
Graph2a<-ggplot(base_WD_international_1960_1990_2018, aes(y=P_W_f, x=ecart_EV_f_m,color=Year))+
  geom_point()+
  xlab("Ecart d'espérance de vie femme-homme, en années")+
  ylab("Probabilité de veuvage des femmes,en %") +
  labs(color="Années")+
  theme_classic()+
  theme(legend.position="bottom")
# Graph 2b: Corrélations entre la durée de veuvage des femmes et l'écart d'espérance de vie femme/homme
# (tracer les nuages de points pour différentes années: 1960; 1990; 2018)
Graph2b<-ggplot(base_WD_international_1960_1990_2018, aes(y=WDc_f, x=ecart_EV_f_m,color=Year))+
  geom_point()+
  xlab("Ecart d'espérance de vie femme-homme, en années")+
  ylab("Durée de veuvage des femmes,en années") +
  labs(color="Années")+
  theme_classic()+
  theme(legend.position="bottom")

Graph2a_b<-ggarrange(Graph2a,Graph2b,ncol = 2,nrow = 1)

#Export:
ggsave("Graph2a_b.pdf",plot = Graph2a_b,width = 11, height = 8)
ggsave("Graph2a.pdf",plot = Graph2a,width = 8, height = 11)
ggsave("Graph2b.pdf",plot = Graph2b,width = 8, height = 11)

ggsave("Graph2a.pdf",plot = Graph2a)
ggsave("Graph2b.pdf",plot = Graph2b)


# on réplique les mêmes graphiques pour les hommes (à mettre en Annexe):
# Graph 2a (Annexe): Corrélations entre la probabilité de veuvage des hommes et l'écart d'espérance de vie femme/homme
# (tracer les nuages de points pour différentes années: 1960; 1990; 2018)
Graph2a_Annexe<-ggplot(base_WD_international_1960_1990_2018, aes(y=P_W_m, x=ecart_EV_f_m,color=Year))+
  geom_point()+
  xlab("Ecart d'espérance de vie femme-homme, en années")+
  ylab("Probabilité de veuvage des hommes,en %") +
  labs(color="Années")+
  theme_classic()+
  theme(legend.position="bottom")
# Graph 2b: Corrélations entre la durée de veuvage des femmes et l'écart d'espérance de vie femme/homme
# (tracer les nuages de points pour différentes années: 1960; 1990; 2018)
Graph2b_Annexe<-ggplot(base_WD_international_1960_1990_2018, aes(y=WDc_m, x=ecart_EV_f_m,color=Year))+
  geom_point()+
  xlab("Ecart d'espérance de vie femme-homme, en années")+
  ylab("Durée de veuvage des hommes,en années") +
  labs(color="Années")+
  theme_classic()+
  theme(legend.position="bottom")

Graph2a_b_Annexe<-ggarrange(Graph2a_Annexe,Graph2b_Annexe,ncol = 2,nrow = 1)

#Export:
ggsave("Graph2a_b_Annexe.pdf",plot = Graph2a_b_Annexe,width = 11, height = 8)
ggsave("Graph2a_Annexe.pdf",plot = Graph2a_Annexe)
ggsave("Graph2b_Annexe.pdf",plot = Graph2b_Annexe)


# Rajout en Annexe de la matrice de corrélations entre les variables suivantes:
# P_W_f,P_W_m,WDc_f,WDc_m,WD_naif_f
Graph2c_Annexe <-chart.Correlation(base_WD_international_1960_1990_2018 %>% filter(Year =="2018") %>%select(P_W_f,P_W_m,ecart_EV_f_m,WDc_f,WDc_m,overlap), histogram=TRUE, pch=19)

# Export:
ggsave("Graph2c_Annexe.pdf",plot = Graph2c_Annexe,width = 11, height = 8)
# Vérifier que l'export a bien fonctionné...

# Figure 3:
# a. Evolution moyenne de la probabilité de veuvage des femmes à 60 ans + boxplot pour avoir l'évolution de la distribution 
# b. Evolution moyenne de la durée de veuvage des femmes à 60 ans + boxplot pour avoir l'évolution de la distribution 
# Note: Mettre en Annexe les deux mêmes graphiques pour les hommes.

# Graph 3a: évolution de la moyenne et de la distribution des proba de veuvage des femmes de 60 ans (box plot):
Graph3a <- ggplot(base_WD_international_1960_2018 %>% mutate(Year = as.factor(Year)), aes(x = Year, y = P_W_f)) + 
  geom_boxplot(notch = TRUE, fill = "lightgray")+
  stat_summary(fun.y = mean, geom = "point",shape = 18, size = 2.5, color = "#FC4E07")+
  scale_x_discrete(limits=c("1960","1965","1970","1975","1980","1985","1990","1995","2000","2005","2010","2015","2018"))+
  xlab("")+
  ylab("Probabilité de veuvage des femmes,en %")+
  theme_classic()

# Graph 3b: évolution de la moyenne et de la distribution des durées de veuvage des femmes de 60 ans (box plot):
Graph3b <- ggplot(base_WD_international_1960_2018 %>% mutate(Year = as.factor(Year)), aes(x = Year, y = WDc_f)) + 
  geom_boxplot(notch = TRUE, fill = "lightgray")+
  stat_summary(fun.y = mean, geom = "point",shape = 18, size = 2.5, color = "#FC4E07")+
  scale_x_discrete(limits=c("1960","1965","1970","1975","1980","1985","1990","1995","2000","2005","2010","2015","2018"))+
  xlab("")+
  ylab("Durée de veuvage des femmes,en années")+
  theme_classic()


#Export:
ggsave("Graph3a.pdf",plot = Graph3a)
ggsave("Graph3b.pdf",plot = Graph3b)

# Réplication des deux graphs pour les hommes, à mettre en Annexe:

Graph3a_Annexe <- ggplot(base_WD_international_1960_2018 %>% mutate(Year = as.factor(Year)), aes(x = Year, y = P_W_m)) + 
  geom_boxplot(notch = TRUE, fill = "lightgray")+
  stat_summary(fun.y = mean, geom = "point",shape = 18, size = 2.5, color = "#FC4E07")+
  scale_x_discrete(limits=c("1960","1965","1970","1975","1980","1985","1990","1995","2000","2005","2010","2015","2018"))+
  xlab("")+
  ylab("Probabilité de veuvage des hommes,en %")+
  theme_classic()

# Graph 3b: évolution de la moyenne et de la distribution des durées de veuvage des femmes de 60 ans (box plot):
Graph3b_Annexe <- ggplot(base_WD_international_1960_2018 %>% mutate(Year = as.factor(Year)), aes(x = Year, y = WDc_m)) + 
  geom_boxplot(notch = TRUE, fill = "lightgray")+
  stat_summary(fun.y = mean, geom = "point",shape = 18, size = 2.5, color = "#FC4E07")+
  scale_x_discrete(limits=c("1960","1965","1970","1975","1980","1985","1990","1995","2000","2005","2010","2015","2018"))+
  xlab("")+
  ylab("Durée de veuvage des hommes,en années")+
  theme_classic()

#Export:
ggsave("Graph3a_Annexe.pdf",plot = Graph3a_Annexe)
ggsave("Graph3b_Annexe.pdf",plot = Graph3b_Annexe)

# Graphiques à mettre en Annexe:

# a. Evolution de la proba de veuvage des femmes par zone géographique:

# Europe du Nord:
Graph_PWf_Europe_Nord <-ggplot(base_WD_international_1960_2018%>%
                        filter(pays %in% c("Finlande","Pays-Bas","Norvège","Suède",
                                           "Danemark"))) +
  aes(x = Year, y = P_W_f, colour = pays, group = pays) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "none",legend.title = element_blank())+
  labs(x="",y="En %")+
  geom_text_repel(
    aes(label = pays), data = base_WD_international_1960_2018%>%
      filter(pays %in% c("Finlande","Pays-Bas","Norvège","Suède",
                         "Danemark") & Year==2018), size = 5,fontface="bold")

# Europe de l'Ouest:
Graph_PWf_Europe_Ouest <-ggplot(base_WD_international_1960_2018%>%
                                 filter(pays %in% c("All. (de l'Ouest)","All. (de l'Est)","Autriche","Belgique",
                                                    "France","Irlande","Luxembourg","Pays-Bas","Royaume-Uni","Suisse"))) +
  aes(x = Year, y = P_W_f, colour = pays, group = pays) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "none",legend.title = element_blank())+
  labs(x="",y="En %")+
  geom_text_repel(
    aes(label = pays), data = base_WD_international_1960_2018%>%
      filter(pays %in% c("All. (de l'Ouest)","All. (de l'Est)","Autriche","Belgique",
                         "France","Irlande","Luxembourg","Pays-Bas","Royaume-Uni","Suisse") & Year==2018), size = 5,fontface="bold")

# Europe du Sud:
Graph_PWf_Europe_Sud <-ggplot(base_WD_international_1960_2018%>%
                                  filter(pays %in% c("Espagne","Italie","Portugal"))) +
  aes(x = Year, y = P_W_f, colour = pays, group = pays) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "none",legend.title = element_blank())+
  labs(x="",y="En %")+
  geom_text_repel(
    aes(label = pays), data = base_WD_international_1960_2018%>%
      filter(pays %in% c("Espagne","Italie","Portugal") & Year==2018), size = 5,fontface="bold")


# Europe centrale:
Graph_PWf_Europe_Centrale <-ggplot(base_WD_international_1960_2018%>%
                                filter(pays %in% c("Bulgarie","Hongrie","Pologne","Tchéquie","Slovaquie"))) +
  aes(x = Year, y = P_W_f, colour = pays, group = pays) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "none",legend.title = element_blank())+
  labs(x="",y="En %")+
  geom_text_repel(
    aes(label = pays), data = base_WD_international_1960_2018%>%
      filter(pays %in% c("Bulgarie","Hongrie","Pologne","Tchéquie","Slovaquie") & Year==2018), size = 5,fontface="bold")

# Pays de l'ex-URSS:
Graph_PWf_Europe_exURSS <-ggplot(base_WD_international_1960_2018%>%
                                     filter(pays %in% c("Biélorussie","Estonie","Lettonie","Lituanie"))) +
  aes(x = Year, y = P_W_f, colour = pays, group = pays) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "none",legend.title = element_blank())+
  labs(x="",y="En %")+
  geom_text_repel(
    aes(label = pays), data = base_WD_international_1960_2018%>%
      filter(pays %in% c("Biélorussie","Estonie","Lettonie","Lituanie") & Year==2018), size = 5,fontface="bold")

# Pays non européens:
Graph_PWf_Pays_hors_Europe <-ggplot(base_WD_international_1960_2018%>%
                                     filter(pays %in% c("Australie","Etats-Unis","Nouvelle-Zélande","Canada","Japon"))) +
  aes(x = Year, y = P_W_f, colour = pays, group = pays) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "none",legend.title = element_blank())+
  labs(x="",y="En %")+
  geom_text_repel(
    aes(label = pays), data = base_WD_international_1960_2018%>%
      filter(pays %in% c("Australie","Etats-Unis","Nouvelle-Zélande","Canada","Japon") & Year==2018), size = 5,fontface="bold")


# b. Evolution de la durée de veuvage des femmes par zone géographique:

# Europe du Nord:
Graph_WDf_Europe_Nord <-ggplot(base_WD_international_1960_2018%>%
                                 filter(pays %in% c("Finlande","Pays-Bas","Norvège","Suède",
                                                    "Danemark"))) +
  aes(x = Year, y = WDc_f, colour = pays, group = pays) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "none",legend.title = element_blank())+
  labs(x="",y="En années")+
  geom_text_repel(
    aes(label = pays), data = base_WD_international_1960_2018%>%
      filter(pays %in% c("Finlande","Pays-Bas","Norvège","Suède",
                         "Danemark") & Year==2018), size = 5,fontface="bold")

# Europe de l'Ouest:
Graph_WDf_Europe_Ouest <-ggplot(base_WD_international_1960_2018%>%
                                  filter(pays %in% c("All. (de l'Ouest)","All. (de l'Est)","Autriche","Belgique",
                                                     "France","Irlande","Luxembourg","Pays-Bas","Royaume-Uni","Suisse"))) +
  aes(x = Year, y = WDc_f, colour = pays, group = pays) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "none",legend.title = element_blank())+
  labs(x="",y="En années")+
  geom_text_repel(
    aes(label = pays), data = base_WD_international_1960_2018%>%
      filter(pays %in% c("All. (de l'Ouest)","All. (de l'Est)","Autriche","Belgique",
                         "France","Irlande","Luxembourg","Pays-Bas","Royaume-Uni","Suisse") & Year==2018), size = 5,fontface="bold")

# Europe du Sud:
Graph_WDf_Europe_Sud <-ggplot(base_WD_international_1960_2018%>%
                                filter(pays %in% c("Espagne","Italie","Portugal"))) +
  aes(x = Year, y = WDc_f, colour = pays, group = pays) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "none",legend.title = element_blank())+
  labs(x="",y="En années")+
  geom_text_repel(
    aes(label = pays), data = base_WD_international_1960_2018%>%
      filter(pays %in% c("Espagne","Italie","Portugal") & Year==2018), size = 5,fontface="bold")


# Europe centrale:
Graph_WDf_Europe_Centrale <-ggplot(base_WD_international_1960_2018%>%
                                     filter(pays %in% c("Bulgarie","Hongrie","Pologne","Tchéquie","Slovaquie"))) +
  aes(x = Year, y = WDc_f, colour = pays, group = pays) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "none",legend.title = element_blank())+
  labs(x="",y="En années")+
  geom_text_repel(
    aes(label = pays), data = base_WD_international_1960_2018%>%
      filter(pays %in% c("Bulgarie","Hongrie","Pologne","Tchéquie","Slovaquie") & Year==2018), size = 5,fontface="bold")

# Pays de l'ex-URSS:
Graph_WDf_Europe_exURSS <-ggplot(base_WD_international_1960_2018%>%
                                     filter(pays %in% c("Biélorussie","Estonie","Lettonie","Lituanie"))) +
  aes(x = Year, y = WDc_f, colour = pays, group = pays) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "none",legend.title = element_blank())+
  labs(x="",y="En années")+
  geom_text_repel(
    aes(label = pays), data = base_WD_international_1960_2018%>%
      filter(pays %in% c("Biélorussie","Estonie","Lettonie","Lituanie") & Year==2018), size = 5,fontface="bold")

# Pays non européens:
Graph_WDf_Pays_hors_Europe <-ggplot(base_WD_international_1960_2018%>%
                                      filter(pays %in% c("Australie","Etats-Unis","Nouvelle-Zélande","Canada","Japon"))) +
  aes(x = Year, y = WDc_f, colour = pays, group = pays) +
  geom_line() +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "none",legend.title = element_blank())+
  labs(x="",y="En années")+
  geom_text_repel(
    aes(label = pays), data = base_WD_international_1960_2018%>%
      filter(pays %in% c("Australie","Etats-Unis","Nouvelle-Zélande","Canada","Japon") & Year==2018), size = 5,fontface="bold")

# TODO: mettre à la même échelle l'axe des ordonnées de ces graphs.

#Export:

# Proba de veuvage des femmes:
ggsave("Graph_PWf_Europe_Nord.pdf",plot = Graph_PWf_Europe_Nord)
ggsave("Graph_PWf_Europe_Ouest.pdf",plot = Graph_PWf_Europe_Ouest)
ggsave("Graph_PWf_Europe_Sud.pdf",plot = Graph_PWf_Europe_Sud)
ggsave("Graph_PWf_Europe_Centrale.pdf",plot = Graph_PWf_Europe_Centrale)
ggsave("Graph_PWf_Europe_exURSS.pdf",plot = Graph_PWf_Europe_exURSS)
ggsave("Graph_PWf_Pays_hors_Europe.pdf",plot = Graph_PWf_Pays_hors_Europe)

# Durée de veuvage des femmes:
ggsave("Graph_WDf_Europe_Nord.pdf",plot = Graph_WDf_Europe_Nord)
ggsave("Graph_WDf_Europe_Ouest.pdf",plot = Graph_WDf_Europe_Ouest)
ggsave("Graph_WDf_Europe_Sud.pdf",plot = Graph_WDf_Europe_Sud)
ggsave("Graph_WDf_Europe_Centrale.pdf",plot = Graph_WDf_Europe_Centrale)
ggsave("Graph_WDf_Europe_exURSS.pdf",plot = Graph_WDf_Europe_exURSS)
ggsave("Graph_WDf_Pays_hors_Europe.pdf",plot = Graph_WDf_Pays_hors_Europe)

# Figure 4:
# Evolution du coefficient de variation (CV) de la probabilité et de la durée de veuvage des femmes de 60 ans

# Note: On exclue les deux pays pour lesquels les variables sont très bruitées: Islande et Luxembourg.
calcul_CV_1960_2018<-base_WD_international_1960_2018%>%
  filter(!pays %in% c("Islande","Luxembourg"))%>%
  group_by(Year)%>%
  summarise(
    cv_P_W_f=cv(P_W_f)*100,
    cv_WDc_f=cv(WDc_f)*100,
  )%>%
  pivot_longer(cols =starts_with("cv"),
               names_to = "type_indicateur",
               values_to = "coeff_variation")%>%
  mutate(type_indicateur=case_when(
    type_indicateur=="cv_P_W_f" ~ "Probabilité de veuvage",
    type_indicateur=="cv_WDc_f" ~ "Durée espérée de veuvage"
  ))%>%
  arrange(type_indicateur,Year)

Graph4<-ggplot(calcul_CV_1960_2018,aes(x=Year, y=coeff_variation,linetype=type_indicateur)) +
  geom_line()+
  scale_color_hue(direction = 1) +
  theme_minimal() +
  theme(legend.position = "bottom",legend.title = element_blank())+
  labs(x="",y="Coefficient de variation, en %" )

# Exports:
ggsave("Graph4.pdf",plot = Graph4)

# Figure 5:
# a. Bêta-convergence absolue des probabilités de veuvage des femmes sur la période 1960-1990 (tracer le nuage de points)
# b. Bêta-convergence absolue des probabilités de veuvage des femmes sur la période 1990-2018 (tracer le nuage de points)
# Mettre en Annexe les mêmes graphiques pour la  durée de veuvage des femmes d'une part, et la durée de veuvage des hommes
# d'autre part.

# Préparation des données: 1960-1990
# calcul de la variation moyenne entre 1960 et 1990 des différents indicateurs de veuvage.
base_delta_1960_1990<-base_WD_international_1960_2018 %>%
  filter(Year %in% c(1960,1990) & !(pays=="Islande")) %>%
  group_by(pays) %>%
  mutate(delta_WDc_f_1960_1990=(WDc_f-lag(WDc_f))/30,
         delta_WD_f_1960_1990=(WD_f-lag(WD_f))/30,
         delta_P_W_f_1960_1990=(P_W_f-lag(P_W_f))/30,
         delta_WD_naif_f_1960_1990=(WD_naif_f-lag(WD_naif_f))/30,
         delta_WDc_m_1960_1990=(WDc_m-lag(WDc_m))/30,
         delta_WD_m_1960_1990=(WD_m-lag(WD_m))/30,
         delta_P_W_m_1960_1990=(P_W_m-lag(P_W_m))/30
  ) %>%
  filter(Year==1990) %>%
  select(pays,delta_WDc_f_1960_1990,delta_WD_f_1960_1990,delta_P_W_f_1960_1990,delta_WD_naif_f_1960_1990,
         delta_WDc_m_1960_1990,delta_WD_m_1960_1990,delta_P_W_m_1960_1990)

# jointure avec la base originelle restreite à l'année 1960:
base_conv_1960_1990<- base_WD_international_1960_2018 %>%
  filter(Year==1960 & !(pays=="Islande")) %>%
  select(pays,WDc_f,WD_f,P_W_f,WD_naif_f,
         WDc_m,WD_m,P_W_m) %>%
  rename(WDc_f_1960=WDc_f,WD_f_1960=WD_f,P_W_f_1960=P_W_f,WD_naif_f_1960=WD_naif_f,
         WDc_m_1960=WDc_m,WD_m_1960=WD_m,P_W_m_1960=P_W_m    
  ) %>%
  left_join(base_delta_1960_1990,by="pays")
base_conv_1960_1990<-base_conv_1960_1990 %>%
  column_to_rownames("pays")

# Préparation des données: 1990-2018
# calcul de la variation moyenne entre 1990 et 2018 des différents indicateurs de veuvage.
base_delta_1990_2018<-base_WD_international_1960_2018 %>%
  filter(Year %in% c(1990,2018) & !(pays=="Islande")) %>%
  group_by(pays) %>%
  mutate(delta_WDc_f_1990_2018=(WDc_f-lag(WDc_f))/28,
         delta_WD_f_1990_2018=(WD_f-lag(WD_f))/28,
         delta_P_W_f_1990_2018=(P_W_f-lag(P_W_f))/28,
         delta_WD_naif_f_1990_2018=(WD_naif_f-lag(WD_naif_f))/28,
         delta_WDc_m_1990_2018=(WDc_m-lag(WDc_m))/28,
         delta_WD_m_1990_2018=(WD_m-lag(WD_m))/28,
         delta_P_W_m_1990_2018=(P_W_m-lag(P_W_m))/28
  ) %>%
  filter(Year==2018) %>%
  select(pays,delta_WDc_f_1990_2018,delta_WD_f_1990_2018,delta_P_W_f_1990_2018,delta_WD_naif_f_1990_2018,
         delta_WDc_m_1990_2018,delta_WD_m_1990_2018,delta_P_W_m_1990_2018)

# jointure avec la base originelle restreite à l'année 1990:
base_conv_1990_2018<- base_WD_international_1960_2018 %>%
  filter(Year==1990 & !(pays=="Islande")) %>%
  select(pays,WDc_f,WD_f,P_W_f,WD_naif_f,
         WDc_m,WD_m,P_W_m) %>%
  rename(WDc_f_1990=WDc_f,WD_f_1990=WD_f,P_W_f_1990=P_W_f,WD_naif_f_1990=WD_naif_f,
         WDc_m_1990=WDc_m,WD_m_1990=WD_m,P_W_m_1990=P_W_m
  ) %>%
  left_join(base_delta_1990_2018,by="pays")
base_conv_1990_2018<-base_conv_1990_2018 %>%
  column_to_rownames("pays")

# Graph 5a: bêta-convergence des proba de veuvage des femmes sur la période 1960-1990
# Régression OLS (pour avoir le bêta estimé et la P-value assciée):
reg_OLS_Graph5a <- lm(delta_P_W_f_1960_1990~P_W_f_1960, data=base_conv_1960_1990) 
summary(reg_OLS_Graph5a)

Graph5a<-ggplot(base_conv_1960_1990, aes(y=delta_P_W_f_1960_1990, x=P_W_f_1960,label = rownames(base_conv_1960_1990)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1960 et 1990,en pp")+
  xlab("Niveau en 1960,en %") +
  theme_classic()+
  annotate("text", x = 58, y = 0.02, label = "delta_proba = 1.9 -0.03*proba\n (R2=0.8)") +
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)

# Graph 5b: bêta-convergence des proba de veuvage des femmes sur la période 1990-2018
# Régression OLS (pour avoir le bêta estimé et la P-value assciée):
reg_OLS_Graph5b <- lm(delta_P_W_f_1990_2018~P_W_f_1990, data=base_conv_1990_2018) 
summary(reg_OLS_Graph5b)

Graph5b<-ggplot(base_conv_1990_2018, aes(y=delta_P_W_f_1990_2018, x=P_W_f_1990,label = rownames(base_conv_1990_2018)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1990 et 2018,en pp")+
  xlab("Niveau en 1990,en %") +
  theme_classic()+
  annotate("text", x = 63, y = -0.24, label = "delta_proba = 1.3 -0.02*proba\n (R2=0.04)") +
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)

# Graph 5c: bêta-convergence des durées de veuvage des femmes sur la période 1960-1990
# Régression OLS (pour avoir le bêta estimé et la P-value assciée):
reg_OLS_Graph5c <- lm(delta_WDc_f_1960_1990~WDc_f_1960, data=base_conv_1960_1990) 
summary(reg_OLS_Graph5c)

Graph5c<-ggplot(base_conv_1960_1990, aes(y=delta_WDc_f_1960_1990, x=WDc_f_1960,label = rownames(base_conv_1960_1990)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1960 et 1990,en années")+
  xlab("Niveau en 1960,en années") +
  theme_classic()+
  annotate("text", x = 10.5, y = -0.01, label = "delta_duree = 0.3 -0.02*duree\n (R2=0.6)") +
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)

# Graph 5d: bêta-convergence des proba de veuvage des femmes sur la période 1990-2018
# Régression OLS (pour avoir le bêta estimé et la P-value assciée):
reg_OLS_Graph5d <- lm(delta_WDc_f_1990_2018~WDc_f_1990, data=base_conv_1990_2018) 
summary(reg_OLS_Graph5d)

Graph5d<-ggplot(base_conv_1990_2018, aes(y=delta_WDc_f_1990_2018, x=WDc_f_1990,label = rownames(base_conv_1990_2018)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1990 et 2018,en années")+
  xlab("Niveau en 1990,en années") +
  theme_classic()+
  annotate("text", x = 11.2, y = -0.03, label = "delta_duree = 0.4 -0.03*duree\n (R2=0.2)") +
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)

#Export:
ggsave("Graph5a.pdf",plot = Graph5a)
ggsave("Graph5b.pdf",plot = Graph5b)
ggsave("Graph5c.pdf",plot = Graph5c)
ggsave("Graph5d.pdf",plot = Graph5d)


# En annexe: réplication de ces 4 graphiques pour la proba et la durée de veuvage des hommes:

# Graph 5a (Annexe): bêta-convergence des proba de veuvage des hommes sur la période 1960-1990
# Régression OLS (pour avoir le bêta estimé et la P-value assciée):
reg_OLS_Graph5a_Annexe <- lm(delta_P_W_m_1960_1990~P_W_m_1960, data=base_conv_1960_1990) 
summary(reg_OLS_Graph5a_Annexe)

Graph5a_Annexe<-ggplot(base_conv_1960_1990, aes(y=delta_P_W_m_1960_1990, x=P_W_m_1960,label = rownames(base_conv_1960_1990)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1960 et 1990,en pp")+
  xlab("Niveau en 1960,en %") +
  theme_classic()+
  annotate("text", x = 38, y = -0.3, label = "delta_proba = 1.0 -0.03*proba\n (R2=0.8)") +
  labs(title = "1960-1990")+
  theme(plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))+
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)

# Graph 5b (Annexe): bêta-convergence des proba de veuvage des hommes sur la période 1990-2018
# Régression OLS (pour avoir le bêta estimé et la P-value assciée):
reg_OLS_Graph5b_Annexe <- lm(delta_P_W_m_1990_2018~P_W_m_1990, data=base_conv_1990_2018) 
summary(reg_OLS_Graph5b_Annexe)

Graph5b_Annexe<-ggplot(base_conv_1990_2018, aes(y=delta_P_W_m_1990_2018, x=P_W_m_1990,label = rownames(base_conv_1990_2018)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1990 et 2018,en pp")+
  xlab("Niveau en 1990,en %") +
  theme_classic()+
  annotate("text", x = 34.5, y = -0.2, label = "delta_proba = 0.8 -0.02*proba\n (R2=0.04)") +
  labs(title = "1990-2018")+
  theme(plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))+
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)

# Graph 5c (Annexe): bêta-convergence des durées de veuvage des hommes sur la période 1960-1990
# Régression OLS (pour avoir le bêta estimé et la P-value assciée):
reg_OLS_Graph5c_Annexe <- lm(delta_WDc_m_1960_1990~WDc_m_1960, data=base_conv_1960_1990) 
summary(reg_OLS_Graph5c_Annexe)

Graph5c_Annexe<-ggplot(base_conv_1960_1990, aes(y=delta_WDc_m_1960_1990, x=WDc_m_1960,label = rownames(base_conv_1960_1990)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1960 et 1990,en années")+
  xlab("Niveau en 1960,en années") +
  theme_classic()+
  annotate("text", x = 8.75, y = -0.03, label = "delta_duree = 0.2 -0.02*duree\n (R2=0.5)") +
  labs(title = "1960-1990")+
  theme(plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))+
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)

# Graph 5d: bêta-convergence des proba de veuvage des femmes sur la période 1990-2018
# Régression OLS (pour avoir le bêta estimé et la P-value assciée):
reg_OLS_Graph5d_Annexe <- lm(delta_WDc_m_1990_2018~WDc_m_1990, data=base_conv_1990_2018) 
summary(reg_OLS_Graph5d_Annexe)

Graph5d_Annexe<-ggplot(base_conv_1990_2018, aes(y=delta_WDc_m_1990_2018, x=WDc_m_1990,label = rownames(base_conv_1990_2018)))+
  geom_point()+
  geom_smooth(colour="red", method="lm", fill="red") +
  ylab("Variation annuelle moyenne entre 1990 et 2018,en années")+
  xlab("Niveau en 1990,en années") +
  theme_classic()+
  annotate("text", x = 8.5, y = -0.005, label = "delta_duree = 0.2 -0.02*duree\n (R2=0.3)") +
  labs(title = "1990-2018")+
  theme(plot.title = element_text(size = 12L, face = "bold", hjust = 0.5))+
  geom_text(hjust=0.5,vjust=-1,check_overlap = T)

#Export:
ggsave("Graph5a_Annexe.pdf",plot = Graph5a_Annexe)
ggsave("Graph5b_Annexe.pdf",plot = Graph5b_Annexe)
ggsave("Graph5c_Annexe.pdf",plot = Graph5c_Annexe)
ggsave("Graph5d_Annexe.pdf",plot = Graph5d_Annexe)


# Tableaux des résultats économétriques sur la bêta-convergence (à mettre en Annexe)

# Tableau 1: estimation en "cross section" de la bêta-convergence absolue de la proba/durée de veuvage
# Estimation MCO et distinguer différentes sous-périodes temporelles (une colonne du tableau correspond à une sous-période)

# 1) Préparation des données:

# 1960-2018
# calcul de la variation moyenne entre 1960 et 2018 des différents indicateurs de veuvage.
base_delta_1960_2018<-base_WD_international_1960_2018 %>%
  filter(Year %in% c(1960,2018) & !(pays=="Islande")) %>%
  group_by(pays) %>%
  mutate(delta_WDc_f_1960_2018=(WDc_f-lag(WDc_f))/58,
         delta_WD_f_1960_2018=(WD_f-lag(WD_f))/58,
         delta_P_W_f_1960_2018=(P_W_f-lag(P_W_f))/58,
         delta_WD_naif_f_1960_2018=(WD_naif_f-lag(WD_naif_f))/58,
         delta_WDc_m_1960_2018=(WDc_m-lag(WDc_m))/58,
         delta_WD_m_1960_2018=(WD_m-lag(WD_m))/58,
         delta_P_W_m_1960_2018=(P_W_m-lag(P_W_m))/58
  ) %>%
  filter(Year==2018) %>%
  select(pays,delta_WDc_f_1960_2018,delta_WD_f_1960_2018,delta_P_W_f_1960_2018,delta_WD_naif_f_1960_2018,
         delta_WDc_m_1960_2018,delta_WD_m_1960_2018,delta_P_W_m_1960_2018)

# jointure avec la base originelle restreite à l'année 1960:
base_conv_1960_2018<- base_WD_international_1960_2018 %>%
  filter(Year==1960 & !(pays=="Islande")) %>%
  select(pays,WDc_f,WD_f,P_W_f,WD_naif_f,
         WDc_m,WD_m,P_W_m) %>%
  rename(WDc_f_1960=WDc_f,WD_f_1960=WD_f,P_W_f_1960=P_W_f,WD_naif_f_1960=WD_naif_f,
         WDc_m_1960=WDc_m,WD_m_1960=WD_m,P_W_m_1960=P_W_m
  ) %>%
  left_join(base_delta_1960_2018,by="pays")
base_conv_1960_2018<-base_conv_1960_2018 %>%
  column_to_rownames("pays")


# 1960-1990
# calcul de la variation moyenne entre 1960 et 1990 des différents indicateurs de veuvage.
base_delta_1960_1990<-base_WD_international_1960_2018 %>%
  filter(Year %in% c(1960,1990) & !(pays=="Islande")) %>%
  group_by(pays) %>%
  mutate(delta_WDc_f_1960_1990=(WDc_f-lag(WDc_f))/30,
         delta_WD_f_1960_1990=(WD_f-lag(WD_f))/30,
         delta_P_W_f_1960_1990=(P_W_f-lag(P_W_f))/30,
         delta_WD_naif_f_1960_1990=(WD_naif_f-lag(WD_naif_f))/30,
         delta_WDc_m_1960_1990=(WDc_m-lag(WDc_m))/30,
         delta_WD_m_1960_1990=(WD_m-lag(WD_m))/30,
         delta_P_W_m_1960_1990=(P_W_m-lag(P_W_m))/30
  ) %>%
  filter(Year==1990) %>%
  select(pays,delta_WDc_f_1960_1990,delta_WD_f_1960_1990,delta_P_W_f_1960_1990,delta_WD_naif_f_1960_1990,
         delta_WDc_m_1960_1990,delta_WD_m_1960_1990,delta_P_W_m_1960_1990)

# jointure avec la base originelle restreite à l'année 1960:
base_conv_1960_1990<- base_WD_international_1960_2018 %>%
  filter(Year==1960 & !(pays=="Islande")) %>%
  select(pays,WDc_f,WD_f,P_W_f,WD_naif_f,
         WDc_m,WD_m,P_W_m) %>%
  rename(WDc_f_1960=WDc_f,WD_f_1960=WD_f,P_W_f_1960=P_W_f,WD_naif_f_1960=WD_naif_f,
         WDc_m_1960=WDc_m,WD_m_1960=WD_m,P_W_m_1960=P_W_m    
  ) %>%
  left_join(base_delta_1960_1990,by="pays")
base_conv_1960_1990<-base_conv_1960_1990 %>%
  column_to_rownames("pays")

# 1990-2018
# calcul de la variation moyenne entre 1990 et 2018 des différents indicateurs de veuvage.
base_delta_1990_2018<-base_WD_international_1960_2018 %>%
  filter(Year %in% c(1990,2018) & !(pays=="Islande")) %>%
  group_by(pays) %>%
  mutate(delta_WDc_f_1990_2018=(WDc_f-lag(WDc_f))/28,
         delta_WD_f_1990_2018=(WD_f-lag(WD_f))/28,
         delta_P_W_f_1990_2018=(P_W_f-lag(P_W_f))/28,
         delta_WD_naif_f_1990_2018=(WD_naif_f-lag(WD_naif_f))/28,
         delta_WDc_m_1990_2018=(WDc_m-lag(WDc_m))/28,
         delta_WD_m_1990_2018=(WD_m-lag(WD_m))/28,
         delta_P_W_m_1990_2018=(P_W_m-lag(P_W_m))/28
  ) %>%
  filter(Year==2018) %>%
  select(pays,delta_WDc_f_1990_2018,delta_WD_f_1990_2018,delta_P_W_f_1990_2018,delta_WD_naif_f_1990_2018,
         delta_WDc_m_1990_2018,delta_WD_m_1990_2018,delta_P_W_m_1990_2018)

# jointure avec la base originelle restreite à l'année 1990:
base_conv_1990_2018<- base_WD_international_1960_2018 %>%
  filter(Year==1990 & !(pays=="Islande")) %>%
  select(pays,WDc_f,WD_f,P_W_f,WD_naif_f,
         WDc_m,WD_m,P_W_m) %>%
  rename(WDc_f_1990=WDc_f,WD_f_1990=WD_f,P_W_f_1990=P_W_f,WD_naif_f_1990=WD_naif_f,
         WDc_m_1990=WDc_m,WD_m_1990=WD_m,P_W_m_1990=P_W_m
  ) %>%
  left_join(base_delta_1990_2018,by="pays")
base_conv_1990_2018<-base_conv_1990_2018 %>%
  column_to_rownames("pays")

# On découpe la période 1960-2018 en décennies et on mesure la bêta-convergence sur chaque décennie:

# 1960-1970
# calcul de la variation moyenne entre 1960 et 1970 des différents indicateurs de veuvage.
base_delta_1960_1970<-base_WD_international_1960_2018 %>%
  filter(Year %in% c(1960,1970) & !(pays=="Islande")) %>%
  group_by(pays) %>%
  mutate(delta_WDc_f_1960_1970=(WDc_f-lag(WDc_f))/10,
         delta_WD_f_1960_1970=(WD_f-lag(WD_f))/10,
         delta_P_W_f_1960_1970=(P_W_f-lag(P_W_f))/10,
         delta_WD_naif_f_1960_1970=(WD_naif_f-lag(WD_naif_f))/10,
         delta_WDc_m_1960_1970=(WDc_m-lag(WDc_m))/10,
         delta_WD_m_1960_1970=(WD_m-lag(WD_m))/10,
         delta_P_W_m_1960_1970=(P_W_m-lag(P_W_m))/10
  ) %>%
  filter(Year==1970) %>%
  select(pays,delta_WDc_f_1960_1970,delta_WD_f_1960_1970,delta_P_W_f_1960_1970,delta_WD_naif_f_1960_1970,
         delta_WDc_m_1960_1970,delta_WD_m_1960_1970,delta_P_W_m_1960_1970)

# jointure avec la base originelle restreite à l'année 1960:
base_conv_1960_1970<- base_WD_international_1960_2018 %>%
  filter(Year==1960 & !(pays=="Islande")) %>%
  select(pays,WDc_f,WD_f,P_W_f,WD_naif_f,
         WDc_m,WD_m,P_W_m) %>%
  rename(WDc_f_1960=WDc_f,WD_f_1960=WD_f,P_W_f_1960=P_W_f,WD_naif_f_1960=WD_naif_f,
         WDc_m_1960=WDc_m,WD_m_1960=WD_m,P_W_m_1960=P_W_m    
  ) %>%
  left_join(base_delta_1960_1970,by="pays")
base_conv_1960_1970<-base_conv_1960_1970 %>%
  column_to_rownames("pays")

# 1970-1980
# calcul de la variation moyenne entre 1970 et 1980 des différents indicateurs de veuvage.
base_delta_1970_1980<-base_WD_international_1960_2018 %>%
  filter(Year %in% c(1970,1980) & !(pays=="Islande")) %>%
  group_by(pays) %>%
  mutate(delta_WDc_f_1970_1980=(WDc_f-lag(WDc_f))/10,
         delta_WD_f_1970_1980=(WD_f-lag(WD_f))/10,
         delta_P_W_f_1970_1980=(P_W_f-lag(P_W_f))/10,
         delta_WD_naif_f_1970_1980=(WD_naif_f-lag(WD_naif_f))/10,
         delta_WDc_m_1970_1980=(WDc_m-lag(WDc_m))/10,
         delta_WD_m_1970_1980=(WD_m-lag(WD_m))/10,
         delta_P_W_m_1970_1980=(P_W_m-lag(P_W_m))/10
  ) %>%
  filter(Year==1980) %>%
  select(pays,delta_WDc_f_1970_1980,delta_WD_f_1970_1980,delta_P_W_f_1970_1980,delta_WD_naif_f_1970_1980,
         delta_WDc_m_1970_1980,delta_WD_m_1970_1980,delta_P_W_m_1970_1980)

# jointure avec la base originelle restreite à l'année 1970:
base_conv_1970_1980<- base_WD_international_1960_2018 %>%
  filter(Year==1970 & !(pays=="Islande")) %>%
  select(pays,WDc_f,WD_f,P_W_f,WD_naif_f,
         WDc_m,WD_m,P_W_m) %>%
  rename(WDc_f_1970=WDc_f,WD_f_1970=WD_f,P_W_f_1970=P_W_f,WD_naif_f_1970=WD_naif_f,
         WDc_m_1970=WDc_m,WD_m_1970=WD_m,P_W_m_1970=P_W_m    
  ) %>%
  left_join(base_delta_1970_1980,by="pays")
base_conv_1970_1980<-base_conv_1970_1980 %>%
  column_to_rownames("pays")

# 1980-1990
# calcul de la variation moyenne entre 1980 et 1990 des différents indicateurs de veuvage.
base_delta_1980_1990<-base_WD_international_1960_2018 %>%
  filter(Year %in% c(1980,1990) & !(pays=="Islande")) %>%
  group_by(pays) %>%
  mutate(delta_WDc_f_1980_1990=(WDc_f-lag(WDc_f))/10,
         delta_WD_f_1980_1990=(WD_f-lag(WD_f))/10,
         delta_P_W_f_1980_1990=(P_W_f-lag(P_W_f))/10,
         delta_WD_naif_f_1980_1990=(WD_naif_f-lag(WD_naif_f))/10,
         delta_WDc_m_1980_1990=(WDc_m-lag(WDc_m))/10,
         delta_WD_m_1980_1990=(WD_m-lag(WD_m))/10,
         delta_P_W_m_1980_1990=(P_W_m-lag(P_W_m))/10
  ) %>%
  filter(Year==1990) %>%
  select(pays,delta_WDc_f_1980_1990,delta_WD_f_1980_1990,delta_P_W_f_1980_1990,delta_WD_naif_f_1980_1990,
         delta_WDc_m_1980_1990,delta_WD_m_1980_1990,delta_P_W_m_1980_1990)

# jointure avec la base originelle restreite à l'année 1980:
base_conv_1980_1990<- base_WD_international_1960_2018 %>%
  filter(Year==1980 & !(pays=="Islande")) %>%
  select(pays,WDc_f,WD_f,P_W_f,WD_naif_f,
         WDc_m,WD_m,P_W_m) %>%
  rename(WDc_f_1980=WDc_f,WD_f_1980=WD_f,P_W_f_1980=P_W_f,WD_naif_f_1980=WD_naif_f,
         WDc_m_1980=WDc_m,WD_m_1980=WD_m,P_W_m_1980=P_W_m    
  ) %>%
  left_join(base_delta_1980_1990,by="pays")
base_conv_1980_1990<-base_conv_1980_1990 %>%
  column_to_rownames("pays")

# 1990-2000
# calcul de la variation moyenne entre 1990 et 2000 des différents indicateurs de veuvage.
base_delta_1990_2000<-base_WD_international_1960_2018 %>%
  filter(Year %in% c(1990,2000) & !(pays=="Islande")) %>%
  group_by(pays) %>%
  mutate(delta_WDc_f_1990_2000=(WDc_f-lag(WDc_f))/10,
         delta_WD_f_1990_2000=(WD_f-lag(WD_f))/10,
         delta_P_W_f_1990_2000=(P_W_f-lag(P_W_f))/10,
         delta_WD_naif_f_1990_2000=(WD_naif_f-lag(WD_naif_f))/10,
         delta_WDc_m_1990_2000=(WDc_m-lag(WDc_m))/10,
         delta_WD_m_1990_2000=(WD_m-lag(WD_m))/10,
         delta_P_W_m_1990_2000=(P_W_m-lag(P_W_m))/10
  ) %>%
  filter(Year==2000) %>%
  select(pays,delta_WDc_f_1990_2000,delta_WD_f_1990_2000,delta_P_W_f_1990_2000,delta_WD_naif_f_1990_2000,
         delta_WDc_m_1990_2000,delta_WD_m_1990_2000,delta_P_W_m_1990_2000)

# jointure avec la base originelle restreite à l'année 1990:
base_conv_1990_2000<- base_WD_international_1960_2018 %>%
  filter(Year==1990 & !(pays=="Islande")) %>%
  select(pays,WDc_f,WD_f,P_W_f,WD_naif_f,
         WDc_m,WD_m,P_W_m) %>%
  rename(WDc_f_1990=WDc_f,WD_f_1990=WD_f,P_W_f_1990=P_W_f,WD_naif_f_1990=WD_naif_f,
         WDc_m_1990=WDc_m,WD_m_1990=WD_m,P_W_m_1990=P_W_m    
  ) %>%
  left_join(base_delta_1990_2000,by="pays")
base_conv_1990_2000<-base_conv_1990_2000 %>%
  column_to_rownames("pays")

# 2000-2010
# calcul de la variation moyenne entre 2000 et 2010 des différents indicateurs de veuvage.
base_delta_2000_2010<-base_WD_international_1960_2018 %>%
  filter(Year %in% c(2000,2010) & !(pays=="Islande")) %>%
  group_by(pays) %>%
  mutate(delta_WDc_f_2000_2010=(WDc_f-lag(WDc_f))/10,
         delta_WD_f_2000_2010=(WD_f-lag(WD_f))/10,
         delta_P_W_f_2000_2010=(P_W_f-lag(P_W_f))/10,
         delta_WD_naif_f_2000_2010=(WD_naif_f-lag(WD_naif_f))/10,
         delta_WDc_m_2000_2010=(WDc_m-lag(WDc_m))/10,
         delta_WD_m_2000_2010=(WD_m-lag(WD_m))/10,
         delta_P_W_m_2000_2010=(P_W_m-lag(P_W_m))/10
  ) %>%
  filter(Year==2010) %>%
  select(pays,delta_WDc_f_2000_2010,delta_WD_f_2000_2010,delta_P_W_f_2000_2010,delta_WD_naif_f_2000_2010,
         delta_WDc_m_2000_2010,delta_WD_m_2000_2010,delta_P_W_m_2000_2010)

# jointure avec la base originelle restreite à l'année 2000:
base_conv_2000_2010<- base_WD_international_1960_2018 %>%
  filter(Year==2000 & !(pays=="Islande")) %>%
  select(pays,WDc_f,WD_f,P_W_f,WD_naif_f,
         WDc_m,WD_m,P_W_m) %>%
  rename(WDc_f_2000=WDc_f,WD_f_2000=WD_f,P_W_f_2000=P_W_f,WD_naif_f_2000=WD_naif_f,
         WDc_m_2000=WDc_m,WD_m_2000=WD_m,P_W_m_2000=P_W_m    
  ) %>%
  left_join(base_delta_2000_2010,by="pays")
base_conv_2000_2010<-base_conv_2000_2010 %>%
  column_to_rownames("pays")

# 2010-2018
# calcul de la variation moyenne entre 2010 et 2018 des différents indicateurs de veuvage.
base_delta_2010_2018<-base_WD_international_1960_2018 %>%
  filter(Year %in% c(2010,2018) & !(pays=="Islande")) %>%
  group_by(pays) %>%
  mutate(delta_WDc_f_2010_2018=(WDc_f-lag(WDc_f))/8,
         delta_WD_f_2010_2018=(WD_f-lag(WD_f))/8,
         delta_P_W_f_2010_2018=(P_W_f-lag(P_W_f))/8,
         delta_WD_naif_f_2010_2018=(WD_naif_f-lag(WD_naif_f))/8,
         delta_WDc_m_2010_2018=(WDc_m-lag(WDc_m))/8,
         delta_WD_m_2010_2018=(WD_m-lag(WD_m))/8,
         delta_P_W_m_2010_2018=(P_W_m-lag(P_W_m))/8
  ) %>%
  filter(Year==2018) %>%
  select(pays,delta_WDc_f_2010_2018,delta_WD_f_2010_2018,delta_P_W_f_2010_2018,delta_WD_naif_f_2010_2018,
         delta_WDc_m_2010_2018,delta_WD_m_2010_2018,delta_P_W_m_2010_2018)

# jointure avec la base originelle restreite à l'année 2010:
base_conv_2010_2018<- base_WD_international_1960_2018 %>%
  filter(Year==2010 & !(pays=="Islande")) %>%
  select(pays,WDc_f,WD_f,P_W_f,WD_naif_f,
         WDc_m,WD_m,P_W_m) %>%
  rename(WDc_f_2010=WDc_f,WD_f_2010=WD_f,P_W_f_2010=P_W_f,WD_naif_f_2010=WD_naif_f,
         WDc_m_2010=WDc_m,WD_m_2010=WD_m,P_W_m_2010=P_W_m    
  ) %>%
  left_join(base_delta_2010_2018,by="pays")
base_conv_2010_2018<-base_conv_2010_2018 %>%
  column_to_rownames("pays")


# 2) Estimations économétriques (MCO):

# a) Proba de veuvage:

# 1960-2018:
reg_OLS_PWf_1960_2018 <- lm(delta_P_W_f_1960_2018~P_W_f_1960, data=base_conv_1960_2018) 
summary(reg_OLS_PWf_1960_2018)

# 1960-1990:
reg_OLS_PWf_1960_1990 <- lm(delta_P_W_f_1960_1990~P_W_f_1960, data=base_conv_1960_1990) 
summary(reg_OLS_PWf_1960_1990)

# 1990-2018:
reg_OLS_PWf_1990_2018 <- lm(delta_P_W_f_1990_2018~P_W_f_1990, data=base_conv_1990_2018) 
summary(reg_OLS_PWf_1990_2018)

# On découpe la période 1960-2018 en décennies et on mesure la bêta-convergence sur chaque décennie:

# 1960-1970:
reg_OLS_PWf_1960_1970 <- lm(delta_P_W_f_1960_1970~P_W_f_1960, data=base_conv_1960_1970) 
summary(reg_OLS_PWf_1960_1970)

# 1970-1980:
reg_OLS_PWf_1970_1980 <- lm(delta_P_W_f_1970_1980~P_W_f_1970, data=base_conv_1970_1980) 
summary(reg_OLS_PWf_1970_1980)

# 1980-1990:
reg_OLS_PWf_1980_1990 <- lm(delta_P_W_f_1980_1990~P_W_f_1980, data=base_conv_1980_1990) 
summary(reg_OLS_PWf_1980_1990)

# 1990-2000:
reg_OLS_PWf_1990_2000 <- lm(delta_P_W_f_1990_2000~P_W_f_1990, data=base_conv_1990_2000) 
summary(reg_OLS_PWf_1990_2000)

# 2000-2010:
reg_OLS_PWf_2000_2010 <- lm(delta_P_W_f_2000_2010~P_W_f_2000, data=base_conv_2000_2010) 
summary(reg_OLS_PWf_2000_2010)

# 2010-2018:
reg_OLS_PWf_2010_2018 <- lm(delta_P_W_f_2010_2018~P_W_f_2010, data=base_conv_2010_2018) 
summary(reg_OLS_PWf_2010_2018)

# b) Durée de veuvage:

# 1960-2018:
reg_OLS_WDf_1960_2018 <- lm(delta_WDc_f_1960_2018~WDc_f_1960, data=base_conv_1960_2018) 
summary(reg_OLS_WDf_1960_2018)

# 1960-1990:
reg_OLS_WDf_1960_1990 <- lm(delta_WDc_f_1960_1990~WDc_f_1960, data=base_conv_1960_1990) 
summary(reg_OLS_WDf_1960_1990)

# 1990-2018:
reg_OLS_WDf_1990_2018 <- lm(delta_WDc_f_1990_2018~WDc_f_1990, data=base_conv_1990_2018) 
summary(reg_OLS_WDf_1990_2018)

# On découpe la période 1960-2018 en décennies et on mesure la bêta-convergence sur chaque décennie:

# 1960-1970:
reg_OLS_WDf_1960_1970 <- lm(delta_WDc_f_1960_1970~WDc_f_1960, data=base_conv_1960_1970) 
summary(reg_OLS_WDf_1960_1970)

# 1970-1980:
reg_OLS_WDf_1970_1980 <- lm(delta_WDc_f_1970_1980~WDc_f_1970, data=base_conv_1970_1980) 
summary(reg_OLS_WDf_1970_1980)

# 1980-1990:
reg_OLS_WDf_1980_1990 <- lm(delta_WDc_f_1980_1990~WDc_f_1980, data=base_conv_1980_1990) 
summary(reg_OLS_WDf_1980_1990)

# 1990-2000:
reg_OLS_WDf_1990_2000 <- lm(delta_WDc_f_1990_2000~WDc_f_1990, data=base_conv_1990_2000) 
summary(reg_OLS_WDf_1990_2000)

# 2000-2010:
reg_OLS_WDf_2000_2010 <- lm(delta_WDc_f_2000_2010~WDc_f_2000, data=base_conv_2000_2010) 
summary(reg_OLS_WDf_2000_2010)

# 2010-2018:
reg_OLS_WDf_2010_2018 <- lm(delta_WDc_f_2010_2018~WDc_f_2010, data=base_conv_2010_2018) 
summary(reg_OLS_WDf_2010_2018)


# Tableau 2: estimation en "panel" de la bêta-convergence absolue de la proba/durée de veuvage
# Estimation "pooled OLS" et "within" (avec effet fixe pays) et distinguer différentes sous-périodes temporelles (une colonne du tableau
# correspond à une sous-période); 

# 1) Préparation des données:

base_WD_international_1960_2018_reg<-base_WD_international_1960_2018 %>%
  filter(!(pays=="Islande")) %>%
  select(pays,Year,P_W_f,WDc_f,P_W_m,WDc_m) %>%
  mutate(Year=as.factor(Year),
         pays=as.factor(pays)) %>%
  #mutate_if(is.numeric,log) %>%
  group_by(pays) %>%
  mutate(delta_P_W_f=P_W_f-lag(P_W_f),
         delta_log_WD_f=log(WDc_f)-lag(log(WDc_f)),
         delta_P_W_m=P_W_f-lag(P_W_m),
         delta_log_WD_m=log(WDc_m)-lag(log(WDc_m)),
         lag_P_W_f=lag(P_W_f),
         lag_P_W_m=lag(P_W_m),
         lag_log_WD_f=lag(log(WDc_f)),
         lag_log_WD_m=lag(log(WDc_m))
         ) %>%
  drop_na()

# 2) Estimations économétriques:

# a) Proba de veuvage:

# Estimation LSDV (effet fixe "pays")
# -> Least Squares Dummy Variable Model (LSDV).
# Estimateur largement utilisé, surtout pour des macro-panels: peu d'individus et beaucoup de périodes temporelles. 

#reg_LSDV_PWf_1960_2018 <- lm(delta_P_W_f ~ lag_P_W_f + factor(Year)-1,data =base_WD_international_1960_2018_reg)
#summary(reg_LSDV_PWf_1960_2018)

# Pour comparer avec d'autres estimateurs, on teste:
library(plm)

# Estimateur "within" (effet fixe "pays"):
reg_WITHIN_PWf_1960_2018 <-plm(delta_P_W_f ~  lag_P_W_f, data =base_WD_international_1960_2018_reg, model = "within")
summary(reg_WITHIN_PWf_1960_2018)
# Estimateur "pooled" OLS (sans effet fixe "pays"):
reg_POOLED_PWf_1960_2018 <-plm(delta_P_W_f ~  lag_P_W_f, data =base_WD_international_1960_2018_reg, model = "pooling")
summary(reg_POOLED_PWf_1960_2018)

# Choix entre "POOLED" et "FE":
# Test: HO: "Pooled OLS model is consistent" contre H1: "Fixed effect model is consistent".
pFtest(reg_WITHIN_PWf_1960_2018,reg_POOLED_PWf_1960_2018)
# The P-value=0.000 -> we reject H0 "the pooled OLS model is consistent"!
# Il semble donc pertinent de retenir dans le modèle un effet fixe "pays".

# Estimation "Random effect":
reg_RE_PWf_1960_2018 <-plm(delta_P_W_f ~  lag_P_W_f, data =base_WD_international_1960_2018_reg, model = "random")
summary(reg_RE_PWf_1960_2018)

# Choic entre "FE" et "RE":
# Test de Hausman:
# We test here HO: "Random Effect is consistent" vs H1: "Fixed Effect is consistent";
phtest(reg_WITHIN_PWf_1960_2018,reg_RE_PWf_1960_2018)
# p-value=0.000<<0.05 -> we can reject H0. Therefore the fixed effect model is an appropriate estimator (random effects
# are probably correlated with X_it).
# In fine, il faut mieux retenir le modèle "Fixed effects".

# On estime la bêta-convergence (en panel avec un effet fixe "pays") sur les différentes sous-périodes suivantes:
# cela permet de repérer les variations de la vitesse de convergence au cours du temps (voire les périodes de
# non-convergence):

# 1960-1990:
reg_WITHIN_PWf_1960_1990 <-plm(delta_P_W_f ~  lag_P_W_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))<1991), model = "within")
summary(reg_WITHIN_PWf_1960_1990)

# 1990-2018:
reg_WITHIN_PWf_1990_2018 <-plm(delta_P_W_f ~  lag_P_W_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))>1989), model = "within")
summary(reg_WITHIN_PWf_1990_2018)

# Découpage de la période d'étude en décennies:

# 1960-1970:
reg_WITHIN_PWf_1960_1970 <-plm(delta_P_W_f ~  lag_P_W_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))<1971), model = "within")
summary(reg_WITHIN_PWf_1960_1970)

# 1970-1980:
reg_WITHIN_PWf_1970_1980 <-plm(delta_P_W_f ~  lag_P_W_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))>1969 & as.numeric(as.character(Year))<1981), model = "within")
summary(reg_WITHIN_PWf_1970_1980)

# 1980-1990:
reg_WITHIN_PWf_1980_1990 <-plm(delta_P_W_f ~  lag_P_W_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))>1979 & as.numeric(as.character(Year))<1991), model = "within")
summary(reg_WITHIN_PWf_1980_1990)

# 1990-2000:
reg_WITHIN_PWf_1990_2000 <-plm(delta_P_W_f ~  lag_P_W_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))>1989 & as.numeric(as.character(Year))<2001), model = "within")
summary(reg_WITHIN_PWf_1990_2000)

# 2000-2010:
reg_WITHIN_PWf_2000_2010 <-plm(delta_P_W_f ~  lag_P_W_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))>1999 & as.numeric(as.character(Year))<2011), model = "within")
summary(reg_WITHIN_PWf_2000_2010)

# 2010-2018:
reg_WITHIN_PWf_2010_2018 <-plm(delta_P_W_f ~  lag_P_W_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))>2009 & as.numeric(as.character(Year))<2019), model = "within")
summary(reg_WITHIN_PWf_2010_2018)


# b) Durée de veuvage:

# Estimateur "within" (effet fixe "pays"):
reg_WITHIN_WDf_1960_2018 <-plm(delta_log_WD_f ~  lag_log_WD_f, data =base_WD_international_1960_2018_reg, model = "within")
summary(reg_WITHIN_WDf_1960_2018)
# Estimateur "pooled" OLS (sans effet fixe "pays"):
reg_POOLED_WDf_1960_2018 <-plm(delta_log_WD_f ~  lag_log_WD_f, data =base_WD_international_1960_2018_reg, model = "pooling")
summary(reg_POOLED_WDf_1960_2018)

# Choix entre "POOLED" et "FE":
# Test: HO: "Pooled OLS model is consistent" contre H1: "Fixed effect model is consistent".
pFtest(reg_WITHIN_WDf_1960_2018,reg_POOLED_WDf_1960_2018)
# The P-value=0.334 -> we don't reject H0 "the pooled OLS model is consistent"!
# Il semble donc pertinent de retenir ici  le modèle sans effet fixe "pays".

# On estime la bêta-convergence (en panel sans effet fixe "pays") sur les différentes sous-périodes suivantes:
# cela permet de repérer les variations de la vitesse de convergence au cours du temps (voire les périodes de
# non-convergence):

# 1960-1990:
reg_POOLED_WDf_1960_1990 <-plm(delta_log_WD_f ~  lag_log_WD_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))<1991), model = "pooling")
summary(reg_POOLED_WDf_1960_1990)

# 1990-2018:
reg_POOLED_WDf_1990_2018 <-plm(delta_log_WD_f ~  lag_log_WD_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))>1989), model = "pooling")
summary(reg_POOLED_WDf_1990_2018)

# Découpage de la période d'étude en décennies:

# 1960-1970:
reg_POOLED_WDf_1960_1970 <-plm(delta_log_WD_f ~  lag_log_WD_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))<1971), model = "pooling")
summary(reg_POOLED_WDf_1960_1970)

# 1970-1980:
reg_POOLED_WDf_1970_1980 <-plm(delta_log_WD_f ~  lag_log_WD_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))>1969 & as.numeric(as.character(Year))<1981), model = "pooling")
summary(reg_POOLED_WDf_1970_1980)

# 1980-1990:
reg_POOLED_PWf_1980_1990 <-plm(delta_log_WD_f ~  lag_log_WD_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))>1979 & as.numeric(as.character(Year))<1991), model = "pooling")
summary(reg_POOLED_PWf_1980_1990)

# 1990-2000:
reg_POOLED_WDf_1990_2000 <-plm(delta_log_WD_f ~  lag_log_WD_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))>1989 & as.numeric(as.character(Year))<2001), model = "pooling")
summary(reg_POOLED_WDf_1990_2000)

# 2000-2010:
reg_POOLED_WDf_2000_2010 <-plm(delta_log_WD_f ~  lag_log_WD_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))>1999 & as.numeric(as.character(Year))<2011), model = "pooling")
summary(reg_POOLED_WDf_2000_2010)

# 2010-2018:
reg_POOLED_WDf_2010_2018 <-plm(delta_log_WD_f ~  lag_log_WD_f, data =base_WD_international_1960_2018_reg %>%
                                 filter(as.numeric(as.character(Year))>2009 & as.numeric(as.character(Year))<2019), model = "pooling")
summary(reg_POOLED_WDf_2010_2018)








