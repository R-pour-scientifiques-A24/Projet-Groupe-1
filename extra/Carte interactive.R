install.packages("leaflet")
library(leaflet)

#Création d'un tableau contenant les infos que l'on veut afficher
tab_point <- data.frame(nom=volcan$volcano_name,long=volcan$longitude,lati=volcan$latitude, pays=volcan$country, derniere_erup=volcan$last_eruption_year, altitude=volcan$elevation)

#Retrait des doublons
tab_point <- unique(tab_point)

#Création de l'icône que l'on souhaite afficher
icone <- icons(iconUrl = "https://raw.githubusercontent.com/R-CoderDotCom/chinchet/main/inst/red.png",
               iconWidth = 50, iconHeight = 50)

#Création de la carte                    
map <- leaflet() %>% addTiles() %>% 
  addMarkers(data = data.frame(lng = tab_point$long, lat = tab_point$lati),
             popup = paste0("Nom du volcan: ",tab_point$nom, "<br>", "Pays: ",tab_point$pays, "<br>",
                            "Altitude: ",tab_point$altitude,"<br>","Année de la dernière activité volcanique: ",tab_point$derniere_erup),
             icon = icone)
map

