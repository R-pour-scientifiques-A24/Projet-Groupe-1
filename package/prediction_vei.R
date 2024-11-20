
#Fonction qui prédit l'indice d'explosivité volcanique
# donnee : un jeu de données de type data frame

# region : région dans laquelle se trouve le volcan. Il doit être dans les valeurs ci-dessous 
#         "Africa and Red Sea", "Alaska", "Antarctica", "Atlantic Ocean", "Canada and Western USA", "Hawaii and Pacific Ocean"      
#         "Iceland and Arctic Ocean", "Indonesia", "Japan, Taiwan, Marianas", "Kamchatka and Mainland Asia", "Kuril Islands", "Mediterranean and Western Asia"
#         "Melanesia and Australia", "México and Central America", "Middle East and Indian Ocean", "New Zealand to Fiji", "Philippines and SE Asia", "South America", "West Indies"

# event : le type d'événement où l'on veut prédire l'indice. Il doit être dans les valeurs ci-dessous 
#         "Caldera", "Caldera(s)", "Complex", "Complex(es)", "Compound", "Crater rows", "Fissure vent(s)", "Lava cone", "Lava dome", "Lava dome(s)"       
#         "Maar(s)", "Pyroclastic cone", "Pyroclastic cone(s)", "Pyroclastic shield", "Shield", "Shield(s)", "Stratovolcano", "Stratovolcano(es)", "Stratovolcano?", "Subglacial", "Submarine", "Tuff cone(s)", "Volcanic field" 

# type : le type du volcan. Il doit être dans les valeurs ci-dessous 
#       "Ash", "Ash Plume", "Ashfall", "Blocks","Bombs", "Caldera formation", "Cinder cone formation", "Crater formation", "Debris avalanches"                  
#       "Deformation (deflation)", "Deformation (inflation)", "Deformation (undefined)", "Degassing", "Directed explosion", "Earthquake (tectonic)"              
#       "Earthquakes (undefined)", "Edifice destroyed", "Eruption cloud", "Evacuations", "Explosion", "Fatalities", "Fauna kill", "Fissure formation", "Flames"                             
#       "Fumarolic or solfataric", "Glow", "Incandescent ejecta", "Island formation", "Jokulhaup", "Lahar or mudflow"                   
#       "Lapilli", "Lava dome formation", "Lava flow(s)", "Lava fountains", "Lava lake", "Lightning", "Liquid sulfur", "Loud audible noises", "Mud"                                
#       "Observation", "Partial collapse at end of eruption", "Phreatic activity", "Phreatomagmatic eruption", "Property damage", "Pumice", "Pyroclastic flow", "Scoria", "Seismicity (volcanic)"              
#       "Spine formation", "Tephra", "Thermal anomaly", "Tsunami", "VEI (Explosivity Index)", "Volcanic smoke", "Volcanic tremor"

# annee : l'année où l'événement s'est produit.
# alti : l'altitude du volcan.

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
  return(prediction)
}


print.volcano <- function(x){
  cat("Avec les arguments fournis, on prévoit un VEI de",x)
  
}



#Tests qui fonctionnent
qui_donne_3 <- prediction_vei(volcan,"Alaska", "Tephra", "Maar(s)", -1000, 2200)
print(qui_donne_3)
qui_donne_1 <- prediction_vei(volcan, "Antarctica", "Ash", "Shield", 2020, 6879)
print(qui_donne_1)
#On regarde si la classe est correcte
class(qui_donne_1)

#Tests qui ne fonctionnent pas
#Mauvaise région
prediction_vei(volcan,"hahahah", "Tephra", "Maar(s)", -1000, 2200)
#Mauvais événement
prediction_vei(volcan,"Alaska", "jakjdhdakjhg", "Maar(s)", -1000, 2200)
#Mauvais type de volcan
prediction_vei(volcan,"Alaska", "Tephra", "kkkkkk", -1000, 2200)
#Mauvaise année
prediction_vei(volcan,"Alaska", "Tephra", "Maar(s)", -100000000, 2200)
#Mauvaise altitude
prediction_vei(volcan,"Alaska", "Tephra", "Maar(s)", -1000, 2200000)
#Mauvais type pour région
Antarctica <- c(1,2,3,4)
prediction_vei(volcan, Antarctica, "Ash", "Shield", 2020, 6879)
#Mauvais type pour événement
prediction_vei(volcan, "Antarctica", 2, "Shield", 2020, 6879)
#Mauvais type pour type de volcan
prediction_vei(volcan, "Antarctica", "Ash", 9, 2020, 6879)
#Mauvais type pour année
prediction_vei(volcan, "Antarctica", "Ash", "Shield", "2020", 6879)
#Mauvais type pour altitude
prediction_vei(volcan, "Antarctica", "Ash", "Shield", 2020, "6879")
#Manque le jeu de donnée
prediction_vei("Alaska", "Tephra", "Maar(s)", -1000, 2200)
#Manque un argument (peu importe)
prediction_vei(volcan, "Antarctica", "Ash", "Shield", 2020)


