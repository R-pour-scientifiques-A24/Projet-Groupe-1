library(readr)
eruptions <- read_csv("data/eruptions.csv")
events <- read_csv("data/events.csv")
volcano <- read_csv("data/volcano.csv")
 
volcan1<-merge(eruptions,events, by="eruption_number")
View(volcan1)
volcan<-merge(volcan1,volcano, by.x="volcano_number.x", by.y="volcano_number")
View(volcan)

sum(duplicated(volcan$event_number)) #C'est bon! On a une ligne par no. d'événement

volcan<-volcan[, -c(8,9,12,13,16,17,18,21,23,24,25,31,32,37:40,42:45)]
View(volcan)

#volcan<-volcan[, c(8,9,12,13,16,17,18,21,23,24,25,31,32,37:40,42:45)]
#On peut vérifier qu'on a retiré les bonnes colonnes!

colnames(volcan)[1] <- "volcano_number"
colnames(volcan)[3] <- "volcano_name"
colnames(volcan)[10] <- "latitude"
colnames(volcan)[11] <- "longitude"
