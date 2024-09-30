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

quantile(geoanim$pop)
geoanim$popquant<-ifelse(geoanim$pop<=6,25,
                         ifelse(geoanim$pop>=7&geoanim$pop<=328, 50,
                                ifelse(geoanim$pop>=329&geoanim$pop<=3006,75,100)))
geoanim$popquant<-as.factor(geoanim$popquant)
geoanim$popquant<-ordered(geoanim$popquant, c("25","50","75","100"))


freqsiecle<-as.data.frame(table(geoanim$siecle, geoanim$nom))
colnames(freqsiecle)<-c("Siecle","Volcan", "Freq_erup")
sommevei<-as.data.frame(aggregate(vei~nom+siecle, data=geoanim, FUN=sum))
colnames(sommevei)<-c("Volcan", "Siecle", "Somme_vei")


data_anim<-merge(freqsiecle, sommevei)
data_anim<-merge(data_anim, geoanim[,c(1:3)], by.y = "nom", by.x="Volcan")
data_anim$Freq_erup<-as.factor(data_anim$Freq_erup)
data_anim$Siecle<-as.integer(data_anim$Siecle)
data_anim<-unique(data_anim)
data_anim<-data_anim[order(data_anim$Siecle),]
View(data_anim)
#table avec aggrégations par siècle




mapWorld <- borders("world", colour="limegreen", fill="limegreen")
#mp <- ggplot(data_anim, aes(x=long, y=lati, colour = (Somme_vei), size= Freq_erup)) + 
#  mapWorld + geom_point() + scale_color_gradient(low="blue", high="red")+transition_time(Siecle)  
#mp
#test par siècle - size = fréquence d'éruption dans le siècle

mp2 <- ggplot(geoanim, aes(x=long, y=lati, colour = vei, size = popquant)) + theme(panel.background = element_rect(fill = 'skyblue', colour = 'gray90'))+
  mapWorld + geom_point() + scale_color_gradient(low="yellow", high="red")+transition_time(annee)  
mp2
#test par année - size = population à 5km
#On pourrait ajouter une shape aux points, autre catégorie pour disons les types de volcans, ou si c'est confirmé

length(unique(data_anim$annee)) #1470 frames

#Beaucoup de choses encore à peaufiner, mais ça marche!
#animate(mp, fps = 20, duration = 25, end_pause = 95)