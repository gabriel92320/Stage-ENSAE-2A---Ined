# Fonction préliminaire qui seta utilisée dans la fonction principale "estimation_param_Gompertz":
# -> cette fonction permet de calculer l'espérance de vie à chaque âge à partir des proba de survie à chaque âge.
compute_life_table = function(data){
  
  data$T = 0
  
  for (a in unique(data$Age)){
    val= as.numeric(a)
    temp = data %>% filter(Age>val)
    Ti = sum(temp$y, na.rm=T)
    data[which(data$Age==a),]$T = Ti
  }
  
  data = data %>% 
    mutate(
      e = T/y + 0.5
    )
  
  return(data)
}