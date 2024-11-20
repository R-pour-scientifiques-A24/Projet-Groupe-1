library(readr)

#Chargement des tableaux
eruptions <- read_csv("data/eruptions.csv")
events <- read_csv("data/events.csv")
volcano <- read_csv("data/volcano.csv")
#Création du jeu de donnée
volcan1<-merge(eruptions,events, by="eruption_number") #Jeu intermédiaire pour 1er merge
volcan<-merge(volcan1,volcano, by.x="volcano_number.x", by.y="volcano_number")
volcan<-volcan[, -c(8,9,12,13,16,17,18,21,23,24,25,31,32,37:40,42:45)]
colnames(volcan)[1] <- "volcano_number"
colnames(volcan)[3] <- "volcano_name"
colnames(volcan)[10] <- "latitude"
colnames(volcan)[11] <- "longitude"
volcan$last_eruption_year<-ifelse(volcan$last_eruption_year == "Unknown", NA,volcan$last_eruption_year)
volcan$last_eruption_year<-as.numeric(volcan$last_eruption_year)
volcan$minor_rock_1<-ifelse(volcan$minor_rock_1 == unique(volcan$minor_rock_1)[3],NA,volcan$minor_rock_1)
remove(volcan1)#retrait du jeu intermédiaire

volcan$volcano_number<-as.character(volcan$volcano_number)
volcan$eruption_number<-as.character(volcan$eruption_number)
volcan$event_number<-as.character(volcan$event_number)

volcan$eruption_category<-as.factor(volcan$eruption_category)
volcan$evidence_method_dating<-as.factor(volcan$evidence_method_dating)
volcan$event_type<-as.factor(volcan$event_type)
volcan$primary_volcano_type<-as.factor(volcan$primary_volcano_type)
volcan$region<-as.factor(volcan$region)
volcan$tectonic_settings<-as.factor(volcan$tectonic_settings)
volcan$evidence_category<-as.factor(volcan$evidence_category)
volcan$major_rock_1<-as.factor(volcan$major_rock_1)
volcan$minor_rock_1<-as.factor(volcan$minor_rock_1)


write.csv(volcan, "volcan.csv")
