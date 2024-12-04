library(readr)
volcan <- read.csv("package/data/volcan.csv")

#Retrait variable random
volcan <- volcan[,-1]

#ajuster le bon type
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