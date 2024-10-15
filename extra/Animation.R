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
geoanim$Population_5km<-ifelse(geoanim$pop<100,"<100",
                          ifelse(geoanim$pop>=100&geoanim$pop<=500, "100-500",
                            ifelse(geoanim$pop>500&geoanim$pop<=1000, "500-1000",
                              ifelse(geoanim$pop>1000&geoanim$pop<=5000, "1000-5000",
                                ifelse(geoanim$pop>5000&geoanim$pop<=10000, "5000-10 000",
                                  ifelse(geoanim$pop>10000&geoanim$pop<=50000, "10 000-50 000",
                                    ifelse(geoanim$pop>50000&geoanim$pop<=100000, "50 000-100 000",
                                      ifelse(geoanim$pop>100000&geoanim$pop<=500000, "100 000-500 000",
                                        ifelse(geoanim$pop>500000&geoanim$pop<=1000000, "500 000-1 000 000",
                                          ">1M")))))))))

geoanim$Population_5km<-as.factor(geoanim$Population_5km)
geoanim$Population_5km<-ordered(geoanim$Population_5km, c("<100","100-500","500-1000","1000-5000","5000-10 000","10 000-50 000","50 000-100 000","100 000-500 000","500 000-1 000 000", ">1M"))


cols<-colorRampPalette(c("yellow", "red"))
colsvei<-cols(8)

mapWorld <- borders("world", colour="palegreen3", fill="palegreen3")

#Plot pas animé, essais sur les styles:
mp1 <- ggplot(geoanim[geoanim$annee==2004,], aes(x=long, y=lati, colour=as.factor(vei), size=Population_5km)) + 
  theme(panel.background = element_rect(fill = 'lightskyblue1', colour = 'gray90'))+
  mapWorld + 
  geom_point(alpha=0.7)+
  scale_colour_manual(name="vei", values=colsvei)+
  theme(aspect.ratio=3/4)+
  labs(title="Éruptions volcaniques de 2004", x="longitude", y="latitude" )
mp1
ggsave("plot.tiff")

#Avec facet
mp2 <- ggplot(geoanim[geoanim$annee==1812,], aes(x=long, y=lati, colour=as.factor(vei), size=Population_5km)) + 
  theme(panel.background = element_rect(fill = 'lightskyblue1', colour = 'gray90'))+
  mapWorld + 
  geom_point(alpha=0.7)+
  scale_colour_manual(name="vei", values=colsvei)+
  theme(aspect.ratio=3/4)+
  facet_wrap(~vei)+
  labs(title="Éruptions volcaniques de 1812", x="longitude", y="latitude" )
mp2
ggsave("plot.tiff")

mp3 <- ggplot(geoanim[geoanim$annee>2010,], aes(x=long, y=lati, colour=as.factor(vei), size=popquant)) + 
  theme(panel.background = element_rect(fill = 'lightskyblue1', colour = 'gray90'))+
  mapWorld + 
  geom_point(alpha=0.7)+
  scale_colour_manual(name="vei", values=colsvei)+
  theme(aspect.ratio=3/4)+
  facet_wrap(~popquant)
#labs(title = 'Année: {frame_time}') + 
#transition_time(annee)
mp3

#Animation

quantile(geoanim$annee)

#nframes détermine le nombre de frames. Par défaut: 100 équidistants.
#On fait 2 gif de 100 frames sur quartiles 0-50 et sur 50-100
#Ça fait une bonne représentation des données. Coller les 2 gif ensemble ensuite.


mp4 <- ggplot(geoanim[geoanim$annee<=1830,], aes(x=long, y=lati, colour=as.factor(vei), size=popquant)) + 
  theme(panel.background = element_rect(fill = 'lightskyblue1', colour = 'gray90')) +
  mapWorld + geom_point(alpha=0.7) + 
  scale_colour_manual(name="vei", values=colsvei)+
  #facet_wrap(~vei)+
  labs(title = 'Année: {frame_time}') + 
  transition_time(annee)
gif1<-animate(mp4,fps=3, width=900, height=600, nframes=100) #100 frames pour 1ère moitié du jeu de données
anim_save(filename="min-1830.gif", gif1)

mp5 <- ggplot(geoanim[geoanim$annee>1830,], aes(x=long, y=lati, colour=as.factor(vei), size=popquant)) + 
  theme(panel.background = element_rect(fill = 'lightskyblue1', colour = 'gray90')) +
  mapWorld + geom_point(alpha=0.7) + 
  scale_colour_manual(name="vei", values=colsvei)+
  #facet_wrap(~vei)+
  labs(title = 'Année: {frame_time}') + 
  transition_time(annee)
gif2<-animate(mp5,fps=3, width=900, height=600, nframes=100) #100 frames pour 2e moitié du jeu de données
anim_save(filename="1830-2020.gif", gif2)

#nframes=length(unique(geoanim$annee)) : 1540 frames pour avoir toutes les années
#valeur par défaut de nframes = 100


#Éruptions fortes avec population proche:

geoanim2<-geoanim[!is.na(geoanim$vei),]
geoanim2<-geoanim2[geoanim2$vei>=5 & geoanim2$popquant=="100",]

mp7 <- ggplot(geoanim2, aes(x=long, y=lati, colour=as.factor(vei), size=famous)) + 
  theme(panel.background = element_rect(fill = 'lightskyblue1', colour = 'gray90')) +
  mapWorld + geom_point(alpha=0.7) + 
  scale_colour_manual(name="vei", values=colsvei[c(4,6,8)])+
  labs(title = 'Année: {frame_time}') + 
  transition_time(annee)

gif4<-animate(mp7,fps=2, width=900, height=600) 
anim_save(filename="DangerZone.gif", gif4)
print(mp7)


  
#Animation + facet

mp6 <- ggplot(geoanim[geoanim$annee>1900,], aes(x=long, y=lati, colour=as.factor(vei), size=popquant)) + 
  theme(panel.background = element_rect(fill = 'lightskyblue1', colour = 'gray90')) +
  mapWorld + geom_point(alpha=0.7) + 
  scale_colour_manual(name="vei", values=colsvei)+
  facet_wrap(~popquant)+
  labs(title = 'Année: {frame_time}') + 
  transition_time(annee)
gif3<-animate(mp6,fps=3, width=900, height=600, nframes=20)
anim_save(filename="2000-2020_vei.gif", gif3)
