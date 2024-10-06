#Carte animée.

library(ggplot2)
library(gganimate)
library(ggmap)
library(gapminder)

geoanim<-data.frame(nom=volcan$volcano_name,
                    long=volcan$longitude,
                    lati=volcan$latitude, 
                    pays=volcan$country,
                    vei=volcan$vei,
                    pop=volcan$population_within_5_km,
                    annee=as.character(volcan$start_year))

geoanim<-unique(geoanim)

geoanim$siecle<-ifelse(nchar(geoanim$annee)>2,substr(geoanim$annee,1,nchar(geoanim$annee)-2), geoanim$annee)
geoanim$siecle<-ifelse(substr(geoanim$siecle,1,1) !="-" & nchar(geoanim$siecle)>2, "1", geoanim$siecle)
geoanim$siecle<-ifelse(geoanim$siecle=="-","-1",geoanim$siecle)
geoanim$siecle<-as.integer(geoanim$siecle)
geoanim$annee<-as.integer(geoanim$annee)


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
#table avec aggrégations par siècle


mapWorld <- borders("world", colour="palegreen3", fill="palegreen3")

#Plot pas animé:
mp1 <- ggplot(geoanim[geoanim$annee==2016,], aes(x=long, y=lati, colour=as.factor(vei), size=popquant, fill=vei),alpha=0.7) + 
  theme(panel.background = element_rect(fill = 'lightskyblue1', colour = 'gray90'))+
  mapWorld + 
  geom_point()+
  scale_colour_manual(name="vei", values=colsvei)+
  theme(aspect.ratio=3/4)
  #facet_wrap(~vei)+
  #labs(title = 'Année: {frame_time}') + 
  #transition_time(annee)
print(mp1)

#Erreur dans animation + facet

#Pas d'erreur dans facet


#Pas d'erreur dans anim sans facet

quantile(geoanim$annee)

#nframes détermine le nombre de frames. Par défaut, 100, équidistants.
#On fait 2 gif de 100 frames sur quartiles 0-50 et sur 50-100
#Ça fait une bonne représentation des données. Coller les 2 gif ensemble ensuite.

cols<-colorRampPalette(c("yellow", "red"))
colsvei<-cols(8)

mp4 <- ggplot(geoanim[geoanim$annee<=1830,], aes(x=long, y=lati, colour=as.factor(vei), size=popquant, fill=vei),alpha=0.7) + 
  theme(panel.background = element_rect(fill = 'lightskyblue1', colour = 'gray90')) +
  mapWorld + geom_point() + 
  scale_colour_manual(name="vei", values=colsvei)+
  #facet_wrap(~vei)+
  labs(title = 'Année: {frame_time}') + 
  transition_time(annee)
gif1<-animate(mp4,fps=3, width=900, height=500, nframes=100) #100 frames pour 1ère moitié du jeu de données
anim_save(filename="min-1830.gif", gif1)

mp5 <- ggplot(geoanim[geoanim$annee>1830,], aes(x=long, y=lati, colour=as.factor(vei), size=popquant),alpha=0.7) + 
  theme(panel.background = element_rect(fill = 'lightskyblue1', colour = 'gray90')) +
  mapWorld + geom_point() + 
  scale_colour_manual(name="vei", values=colsvei)+
  #facet_wrap(~vei)+
  labs(title = 'Année: {frame_time}') + 
  transition_time(annee)

gif2<-animate(mp5,fps=3, width=900, height=500, nframes=100) #100 frames pour 2e moitié du jeu de données
anim_save(filename="1830-2020.gif", gif2)


#nframes=length(unique(geoanim$annee)) : 1540 frames pour avoir toutes les années
#valeur par défaut de nframes = 100