mod1<-lm(vei~., data=volcan)
summary(mod1)

shapiro.test(mod1$residuals)
shapiro.test(rstandard(mod1))

mod2<-lm(sqrt(vei)~., data=volcan)
summary(mod2)
shapiro.test(mod2$residuals)

mod3<-lm((vei)^2~., data=volcan)
shapiro.test(mod3$residuals)

hist(mod2$residuals, breaks=100)
hist(mod1$residuals, breaks=100)
hist(mod3$residuals, breaks=100)


mod4<-lm(vei~country+start_year+eruption_category+evidence_method_dating+end_year
         +event_type+primary_volcano_type+last_eruption_year+elevation+evidence_category+major_rock_1+minor_rock_1, data=volcan)
summary(mod4)
anova(mod4)
plot(mod4)
hist(mod4$residuals, breaks=100)
shapiro.test(mod4$residuals)

step(mod4)

library(MASS)
mod5<-lm(vei~country, data=volcan)
stepAIC(mod5, direction="forward", scope=~.)



mod4<-lm(vei~country+start_year+eruption_category+evidence_method_dating+end_year
         +event_type+primary_volcano_type+last_eruption_year+elevation+evidence_category+major_rock_1+minor_rock_1, data=volcan)

mod41<-lm(vei~country+start_year+eruption_category+evidence_method_dating+end_year+end_year*start_year+major_rock_1*minor_rock_1
         +event_type+primary_volcano_type+last_eruption_year+elevation+evidence_category+major_rock_1+minor_rock_1 
         +end_year*last_eruption_year+evidence_method_dating*end_year+evidence_method_dating*evidence_category , data=volcan)

mod42<-lm(vei~country+start_year+eruption_category+evidence_method_dating+end_year+end_year*start_year
          +event_type+primary_volcano_type+last_eruption_year+elevation+evidence_category
          +end_year*last_eruption_year+evidence_method_dating*end_year+evidence_method_dating*evidence_category , data=volcan)

mod43<-lm(vei~eruption_category+evidence_method_dating
          +event_type+primary_volcano_type+last_eruption_year+elevation+evidence_category
          +evidence_method_dating*evidence_category+major_rock_1+minor_rock_1 , data=volcan)

mod44<-lm(vei~eruption_category+event_type+primary_volcano_type+last_eruption_year+elevation
          +major_rock_1+minor_rock_1 , data=volcan)



mod6<-lm(vei~country+start_year+eruption_category+evidence_method_dating+end_year+latitude+longitude+population_within_5_km
               +event_type+primary_volcano_type+last_eruption_year+elevation+evidence_category+major_rock_1+minor_rock_1, data=volcan)

mod61<-lm(vei~country+start_year+eruption_category+evidence_method_dating+end_year+latitude+longitude
         +event_type+primary_volcano_type+last_eruption_year+elevation+evidence_category+major_rock_1+minor_rock_1, data=volcan)


mod7<-lm(vei~country+start_year+eruption_category+evidence_method_dating+end_year+major_rock_1*minor_rock_1
         +event_type+primary_volcano_type+last_eruption_year+elevation+evidence_category+major_rock_1+minor_rock_1, data=volcan)

mod8<-lm(vei~country+start_year+eruption_category+evidence_method_dating+end_year+end_year*start_year
          +event_type+primary_volcano_type+last_eruption_year+elevation+evidence_category+major_rock_1+minor_rock_1 
          +end_year*last_eruption_year+evidence_method_dating*end_year+evidence_method_dating*evidence_category , data=volcan)


#Calcul des critères pour les différents modèles
round( c(R2=summary(mod1)$r.square, R2ajust=summary(mod1)$adj.r.squared, AIC=AIC(mod1), BIC=BIC(mod1)) , 2)
round( c(R2=summary(mod2)$r.square, R2ajust=summary(mod2)$adj.r.squared, AIC=AIC(mod2), BIC=BIC(mod2)) , 2)
round( c(R2=summary(mod3)$r.square, R2ajust=summary(mod3)$adj.r.squared, AIC=AIC(mod3), BIC=BIC(mod3)) , 2)
round( c(R2=summary(mod4)$r.square, R2ajust=summary(mod4)$adj.r.squared, AIC=AIC(mod4), BIC=BIC(mod4)) , 2)
round( c(R2=summary(mod41)$r.square, R2ajust=summary(mod41)$adj.r.squared, AIC=AIC(mod41), BIC=BIC(mod41)) , 2)
round( c(R2=summary(mod42)$r.square, R2ajust=summary(mod42)$adj.r.squared, AIC=AIC(mod42), BIC=BIC(mod42)) , 2)
round( c(R2=summary(mod43)$r.square, R2ajust=summary(mod43)$adj.r.squared, AIC=AIC(mod43), BIC=BIC(mod43)) , 2)
round( c(R2=summary(mod44)$r.square, R2ajust=summary(mod44)$adj.r.squared, AIC=AIC(mod44), BIC=BIC(mod44)) , 2)
round( c(R2=summary(mod5)$r.square, R2ajust=summary(mod5)$adj.r.squared, AIC=AIC(mod5), BIC=BIC(mod5)) , 2)
round( c(R2=summary(mod6)$r.square, R2ajust=summary(mod6)$adj.r.squared, AIC=AIC(mod6), BIC=BIC(mod6)) , 2)
round( c(R2=summary(mod61)$r.square, R2ajust=summary(mod61)$adj.r.squared, AIC=AIC(mod61), BIC=BIC(mod61)) , 2)
round( c(R2=summary(mod7)$r.square, R2ajust=summary(mod7)$adj.r.squared, AIC=AIC(mod7), BIC=BIC(mod7)) , 2)
round( c(R2=summary(mod8)$r.square, R2ajust=summary(mod8)$adj.r.squared, AIC=AIC(mod8), BIC=BIC(mod8)) , 2)

#mod41 semble être le meilleur modèle !



#Modèle final :
mod41<-lm(vei~country+start_year+eruption_category+evidence_method_dating+end_year+end_year*start_year+major_rock_1*minor_rock_1
          +event_type+primary_volcano_type+last_eruption_year+elevation+evidence_category+major_rock_1+minor_rock_1 
          +end_year*last_eruption_year+evidence_method_dating*end_year+evidence_method_dating*evidence_category , data=volcan)

#REGION plutôt que country
mod41<-lm(vei~region+start_year+eruption_category+evidence_method_dating+end_year+end_year*start_year+major_rock_1*minor_rock_1
          +event_type+primary_volcano_type+last_eruption_year+elevation+evidence_category+major_rock_1+minor_rock_1 
          +end_year*last_eruption_year+evidence_method_dating*end_year+evidence_method_dating*evidence_category , data=volcan)

#enlever minor_rock_1
mod41<-lm(vei~region+start_year+eruption_category+evidence_method_dating+end_year+end_year*start_year
          +event_type+primary_volcano_type+last_eruption_year+elevation+evidence_category+major_rock_1 
          +end_year*last_eruption_year+evidence_method_dating*end_year+evidence_method_dating*evidence_category , data=volcan)
#NON, ON LA GARDE!!!

#Enlever end_year
mod41<-lm(vei~region+start_year+eruption_category+evidence_method_dating+major_rock_1*minor_rock_1
          +event_type+primary_volcano_type+last_eruption_year+elevation+evidence_category+major_rock_1+minor_rock_1 
          +evidence_method_dating*evidence_category , data=volcan)

#Enlever evidence_method_dating
mod41<-lm(vei~region+start_year+eruption_category+major_rock_1*minor_rock_1
          +event_type+primary_volcano_type+last_eruption_year+elevation+evidence_category+major_rock_1+minor_rock_1 
           , data=volcan)

#Enlever evidence_category
mod41<-lm(vei~region+start_year+eruption_category +major_rock_1+minor_rock_1
          +event_type+primary_volcano_type+last_eruption_year+elevation+major_rock_1+minor_rock_1, data=volcan)


round( c(R2=summary(mod41)$r.square, R2ajust=summary(mod41)$adj.r.squared, AIC=AIC(mod41), BIC=BIC(mod41)) , 2)




#Modèle final :
mod_final<-lm(vei~country+start_year+event_type+elevation+country*start_year+country*event_type+country*elevation
              +start_year*event_type+start_year*elevation+event_type*elevation, data=volcan)
round( c(R2=summary(mod_final)$r.square, R2ajust=summary(mod_final)$adj.r.squared, AIC=AIC(mod_final), BIC=BIC(mod_final)) , 2)
anova(mod_final)



#Observation de notre modèle
qqnorm(rstandard(mod41))
qqline(rstandard(mod41))
hist(rstandard(mod41))
boxplot(rstandard(mod41))
plot(x=fitted.values(mod41), y=rstandard(mod41))

#Données de notre modèle
summary(mod41)
anova(mod41)

#Petits tests AMG: evidence_category n'est pas significatif
modAM<-lm(vei~country+start_year+eruption_category+evidence_method_dating+end_year+end_year*start_year+major_rock_1*minor_rock_1
          +event_type+primary_volcano_type+last_eruption_year+elevation+major_rock_1+minor_rock_1 
          +end_year*last_eruption_year+evidence_method_dating*end_year+evidence_method_dating, data=volcan)
summary(modAM)
anova(modAM)

install.packages("AICcmodavg")
install.packages('pbapply')
library(AICcmodavg)

models<-list(modAM,mod41)
modelnames<-c("modAM", "mod41")

rbind("modAM"= round(c(R2=summary(modAM)$r.square, R2ajust=summary(modAM)$adj.r.squared, AIC=AIC(modAM), BIC=BIC(modAM)) , 2),
      "mod41"= round(c(R2=summary(mod41)$r.square, R2ajust=summary(mod41)$adj.r.squared, AIC=AIC(mod41), BIC=BIC(mod41)) , 2))

aictab(cand.set = models, modnames = modelnames)

#mod41 est le meilleur modèle!


mod41<-lm(vei~region+start_year+event_type+primary_volcano_type+elevation, data=volcan)

round( c(R2=summary(mod41)$r.square, R2ajust=summary(mod41)$adj.r.squared, AIC=AIC(mod41), BIC=BIC(mod41)) , 2)



#test de prédiction à 2 variables:
newdata = data.frame(region=c("Antarctica","Alaska","West Indies"),
                     elevation=c(600, 1100, 1600),
                     start_year=c(1980, -1000, 1870),
                     eruption_category=c("Confirmed Eruption", "Uncertain Eruption", "Confirmed Eruption"),
                     evidence_method_dating=c("Historical Observations", "Historical Observations", "Historical Observations"),
                     end_year=c(1982, -998, 1874),
                     event_type=c("Spine formation", "Thermal anomaly", "Volcanic smoke"),
                     primary_volcano_type=c("Compound", "Complex", "Compound"),
                     last_eruption_year=c(2010, 1910, 1910),
                     evidence_category=c("Eruption Dated" , "Eruption Observed", "Eruption Observed"),
                     major_rock_1=c("Foidite", "Trachyte / Trachydacite", "Rhyolite"),
                     minor_rock_1 =c("Trachybasalt / Tephrite Basanite","Trachybasalt / Tephrite Basanite","Trachybasalt / Tephrite Basanite")
)
predict(mod_final, newdata)

#test de prédiction à 2 variables:
newdata = data.frame(region=c("Antarctica","Alaska","West Indies"),
                     elevation=c(600, 1100, 1600),
                     start_year=c(1980, -1000, 1870),
                     eruption_category=c("Confirmed Eruption", "Uncertain Eruption", "Confirmed Eruption"),
                     event_type=c("Spine formation", "Thermal anomaly", "Volcanic smoke"),
                     primary_volcano_type=c("Compound", "Complex", "Compound"),
                     last_eruption_year=c(2010, 1910, 1910),
                     major_rock_1=c("Foidite", "Trachyte / Trachydacite", "Rhyolite"),
                     minor_rock_1 =c("Trachybasalt / Tephrite Basanite","Trachybasalt / Tephrite Basanite","Trachybasalt / Tephrite Basanite")
)
predict(mod_final, newdata)



#On a un problème: Retourne des valeurs de VEI impossibles
#Ne reconnaît pas beaucoup des facteurs que j'avais initialement mis dans new data
#Les dates sont des facteurs
#Certains NA sont des " ", ou "Unknown"
#Si je mets des NA il ne fait pas de prédiction (ca prend des valeurs pour toutes
#les variables du modèle - faire un tout petit modèle?)
#faire un glm qui retourne un facteur (0:7) plutôt qu'une var. numérique continue?
unique(volcan$vei)



#test de prédiction à 2 variables:
newdata = data.frame(region=c("Antarctica","Alaska","West Indies"),
                     elevation=c(600, 1100, 1600),
                     start_year=c(1980, -1000, 1870),
                     eruption_category=c("Confirmed Eruption", "Uncertain Eruption", "Confirmed Eruption"),
                     event_type=c("Spine formation", "Thermal anomaly", "Volcanic smoke"),
                     primary_volcano_type=c("Compound", "Complex", "Compound"),
                     last_eruption_year=c(2010, 1910, 1910),
                     major_rock_1=c("Foidite", "Trachyte / Trachydacite", "Rhyolite"),
                     minor_rock_1 =c("Trachybasalt / Tephrite Basanite","Trachybasalt / Tephrite Basanite","Trachybasalt / Tephrite Basanite")
)
prediction<-predict(mod_final, newdata)

#test de prédiction à 2 variables:
newdata2 = data.frame(region=c("Hawaii and Pacific Ocean","Kuril Islands","West Indies"),
                      elevation=c(550, 752, -107),
                      start_year=c(1568, -3420, 870),
                      eruption_category=c("Confirmed Eruption", "Uncertain Eruption", "Confirmed Eruption"),
                      event_type=c("Lava lake", "Property damage", "Lapilli"),
                      primary_volcano_type=c(unique(volcan$primary_volcano_type)[3],"Compound","Compound"), 
                      last_eruption_year=c(1944, 1911, -6550),                                         
                      major_rock_1=c("Basalt / Picro-Basalt", "Phonolite", "Andesite / Basaltic Andesite"),
                      minor_rock_1 =c("Dacite","Rhyolite","Phonolite"))
                      
prediction<-predict(mod_final, newdata2)


#troisième test de prédiction à 2 variables:
newdata3 = data.frame(region=c("Hawaii and Pacific Ocean","Kuril Islands","West Indies"),
                     elevation=c(550, 752, -107),
                     start_year=c(1568, -3420, 870),
                     eruption_category=c("Confirmed Eruption", "Uncertain Eruption", "Confirmed Eruption"),
                     event_type=c("Lava lake", "Property damage", "Lapilli"),
                     primary_volcano_type=as.character(c(unique(volcan$primary_volcano_type)[3],unique(volcan$primary_volcano_type)[15],unique(volcan$primary_volcano_type)[7])), 
                     last_eruption_year=c(1944, 1911, -6550),                                         
                     major_rock_1=c("Basalt / Picro-Basalt", "Phonolite", "Andesite / Basaltic Andesite"),
                     minor_rock_1 =c("Dacite","Rhyolite","Phonolite")
)
prediction<-predict(mod_final, newdata3)

# Question pour Aurélien: unique(volcan$primary_volcano_type)[15]=="Tuff cone(s)", Pourquoi on reçoit une erreur lorsqu'on veut le mettre dans le newdata?
# Erreur dans model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
 #  le facteur primary_volcano_type a des nouveaux niveaux Tuff cone(s)




#Conditions à imposer pour que la prédiction nous donne des valeurs de vei comprises parmi : 0,1,2,3,4,5,6,7.
result_predict <- ifelse(prediction<0.5, 0, 
                         ifelse(prediction>=0.5 & prediction<1.5, 1, 
                                ifelse(prediction>=1.5 & prediction<2.5, 2,
                                       ifelse(prediction>=2.5 & prediction<3.5, 3,
                                              ifelse(prediction>=3.5 & prediction<4.5, 4,
                                                     ifelse(prediction>=4.5 & prediction<5.5, 5, 
                                                            ifelse(prediction>=5.5 & prediction<6.5, 6, 7)))))) )
as.numeric(result_predict)


mod41<-lm(vei~region+start_year+eruption_category+major_rock_1*minor_rock_1
          +event_type+primary_volcano_type+last_eruption_year+elevation+major_rock_1+minor_rock_1 , data=volcan)
summary(mod41)

#Option avec polr:
library(MASS)
?polr

freq_vei<-table(volcan$vei)
vei_factor<-as.factor(volcan$vei)

data_polr<-polr(vei_factor~region+start_year+eruption_category+event_type+primary_volcano_type+last_eruption_year
                +elevation+major_rock_1+minor_rock_1 +major_rock_1*minor_rock_1, data=volcan ,
                contrasts = NULL, Hess = TRUE, model = TRUE, method = c("logistic"))

#Impossible de trouver la fonction "polr.predict" ...?
polr.predict (mod41, values=0:7, sim.count=1000, conf.int=0.95, sigma=NULL, set.seed=NULL) 
# the values of the case as vector in the order how they appear in the summary (model) estimate OPTIONAL numbers 
# of simulations to be done by the function. default: 1000 OPTIONAL the confidence interval used by the function. default: 0.95

prediction<-predict(mod41, data_polr)


plot(volcan$vei~volcan$start_year+volcan$elevation)

#Boxplot des vei en fonction de l'élévation
boxplot(volcan$elevation~volcan$vei)

#Boxplot des vei en fonction de start_years
boxplot(volcan$start_year~volcan$vei)



newdata = data.frame(region=input$var1,
                     elevation=input$var2,
                     start_year=input$var3,
                     event_type=input$var4,
                     primary_volcano_type=input$var5)

############################################################################################

# VRAI MODELE : 
mod_final<-lm(vei~region+start_year+event_type+primary_volcano_type+elevation, data=volcan)

#test de prédiction à 2 variables:
newdata = data.frame(region=c("Antarctica","Alaska","West Indies"),
                     elevation=c(600, 1100, 1600),
                     start_year=c(1980, -1000, 1870),
                     event_type=c("Spine formation", "Thermal anomaly", "Volcanic smoke"),
                     primary_volcano_type=c("Compound", "Complex", "Compound"))
prediction<-predict(mod_final, newdata)

#Conditions à imposer pour que la prédiction nous donne des valeurs de vei comprises parmi : 0,1,2,3,4,5,6,7.
result_predict <- ifelse(prediction<0.5, 0, 
                         ifelse(prediction>=0.5 & prediction<1.5, 1, 
                                ifelse(prediction>=1.5 & prediction<2.5, 2,
                                       ifelse(prediction>=2.5 & prediction<3.5, 3,
                                              ifelse(prediction>=3.5 & prediction<4.5, 4,
                                                     ifelse(prediction>=4.5 & prediction<5.5, 5, 
                                                            ifelse(prediction>=5.5 & prediction<6.5, 6, 7)))))) )
as.numeric(result_predict)
