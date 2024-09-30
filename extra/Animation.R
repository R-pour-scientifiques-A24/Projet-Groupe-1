#Carte animée.

library(ggplot2)
library(gganimate)
library(ggmap)

geoanim<-data.frame(nom=volcan$volcano_name,
                    long=volcan$longitude,
                    lati=volcan$latitude, 
                    pays=volcan$country,
                    vei=volcan$vei,
                    pop=volcan$population_within_5_km,
                    annee=as.character(volcan$start_year))

geoanim<-unique(geoanim)

geoanim$siecle<-ifelse(nchar(geoanim$annee)>2,substr(geoanim$annee,1,nchar(geoanim$annee)-2), geoanim$annee)
geoanim$siecle<-ifelse(substr(geoanim$siecle,1,1) !="-" & nchar(geoanim$siecle)>2, 1, geoanim$siecle)
geoanim$siecle<-ifelse(geoanim$siecle=="-","-1",geoanim$siecle)
geoanim$siecle<-as.numeric(geoanim$siecle)
geoanim$annee<-as.numeric(geoanim$annee)

freqsiecle<-as.data.frame(table(geoanim$siecle, geoanim$nom))
colnames(freqsiecle)<-c("Siecle","Volcan", "Freq_erup")
sommevei<-as.data.frame(aggregate(vei~nom+siecle, data=geoanim, FUN=sum))
colnames(sommevei)<-c("Volcan", "Siecle", "Somme_vei")


data_anim<-merge(freqsiecle, sommevei)
data_anim<-merge(data_anim, geoanim[,1:3], by.y = "nom", by.x="Volcan")
data_anim$Freq_erup<-as.factor(data_anim$Freq_erup)
data_anim$Siecle<-as.integer(data_anim$Siecle)
data_anim<-unique(data_anim)
data_anim<-data_anim[order(data_anim$Siecle),]
View(data_anim)

mapWorld <- borders("world", colour="gray50", fill="gray50")
mp <- ggplot(data_anim, aes(x=long, y=lati, colour = (Somme_vei), size= Freq_erup)) + 
  mapWorld + geom_point() + scale_color_gradient(low="blue", high="red")+transition_time(Siecle)  
mp

#Beaucoup de choses encore à peaufiner, mais ça marche!
#animate(mp, fps = 20, duration = 25, end_pause = 95)
