freqvei<-table(volcan$vei)
freqvei
barplot(freqvei)
boxplot(volcan$vei)

min(volcan$start_year)
max(volcan$start_year)
hist(volcan$start_year, xlim=c(-11345,2020), breaks=50)
hist(volcan$start_year, xlim=c(1500,2020), breaks=1000)



#Statistiques bivariées à partir des variables du modèle prédictif
mod_final<-lm(vei~region+start_year+event_type+primary_volcano_type+elevation, 
              data=volcan)

# BOXPLOT par VEI en fonction de l'ÉLÉVATION
library(ggplot2)
ggplot(
  volcan, mapping=aes(x=vei, y=elevation)
) +
  geom_boxplot( aes(group=vei), fill="grey"
  ) +
  theme_classic() +
  labs(
    title="Élévation d'un volcan par indice de force de ses éruptions",
    x="Indice de force", y="Élévation"
  ) +
  theme(
    plot.title=element_text(hjust=0.5)
  )


boxplot(
  elevation ~ vei, data = volcan, 
  lwd = 1.5,
  main = "Élévation d'un volcan selon sa valeur d'intensité de force",
  xlab = "Indice de force",
  ylab = "Élévation"
)


# VEI par REGION
ggplot(
  volcan, mapping=aes(x=vei, y=region)
) +
  geom_count(
    aes(color=after_stat(prop), size=after_stat(prop))
  ) +
  guides(color="legend") +
  scale_size_area(max_size = 10) +
  theme_classic() +
  labs(
    title="Proportion de volcans par région selon l'indice de force de ses éruptions",
    x="Indice de force", y="Région"
  ) +
  theme(
    plot.title=element_text(hjust=0.5)
  )


# VEI par ANNÉE DE DÉBUT 
ggplot(
  volcan, mapping=aes(x=vei, y=start_year)
) +
  geom_boxplot( aes(group=vei), fill="grey"
  ) +
  theme_classic() +
  labs(
    title="Année où un événement à débuté par indice de force de ses éruptions",
    x="Indice de force", y="Année"
  ) +
  theme(
    plot.title=element_text(hjust=0.5)
  )

boxplot(
  start_year ~ vei, data = volcan, 
  lwd = 1.5,
  main = "Année où un événement à débuté par indice de force de ses éruptions",
  xlab = "Indice de force",
  ylab = "Année"
)


# VEI par TYPE D'ÉVÉNEMENT
ggplot(
  volcan, mapping=aes(x=vei, y=event_type)
) +
  geom_count(
    aes(color=after_stat(prop), size=after_stat(prop))
  ) +
  guides(color="legend") +
  theme_classic() +
  labs(
    title="Proportion de volcans par type d'événement selon l'indice de force de ses éruptions",
    x="Indice de force", y="Type d'événement"
  ) +
  theme(
    plot.title=element_text(hjust=0.5)
  )


#VEI par TYPE DE VOLCAN
ggplot(
  volcan, mapping=aes(x=vei, y=primary_volcano_type)
) +
  geom_count(
    aes(color=after_stat(prop), size=after_stat(prop))
  ) +
  guides(color="legend") +
  theme_classic() +
  labs(
    title="Proportion de volcans par type de volcan selon l'indice de force de ses éruptions",
    x="Indice de force", y="Type de volcan"
  ) +
  theme(
    plot.title=element_text(hjust=0.5)
  )
