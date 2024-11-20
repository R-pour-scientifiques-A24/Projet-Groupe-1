#Fonction de prédiction

prediction_vei <- function(donnee,region=unique(donnee$region), event=unique(donnee$event_type), type=unique(donnee$primary_volcano_type), annee, alti){
  region <- match.arg(region)
  event <- match.arg(event)
  type <- match.arg(type)
  if(!(annee %in% -11345:2020)){
    stop("L'année entrée n'est pas dans l'intervalle ciblé de notre jeu de donnée")
  }
  if(!(alti %in% -2500:6879)){
    stop("L'altitude entrée n'est pas dans l'intervalle ciblé de notre jeu de donnée")
  }
  mod_final<-lm(vei~region+start_year+event_type+primary_volcano_type+elevation, data=donnee)
  nouv_donnee <- data.frame("region"=region, "start_year"=annee, "event_type"=event, "primary_volcano_type"=type, "elevation"=alti )
  prediction<-round(predict(mod_final, nouv_donnee))
  ifelse(prediction>7, 7, ifelse(prediction<0,0,prediction))
  class(prediction) <- "volcano"
}

print.volcano <- function(x){
  print("Avec les arguments fournis, on prévoit un VEI de", x)
  
}




