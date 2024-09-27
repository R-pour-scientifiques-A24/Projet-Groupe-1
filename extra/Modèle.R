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


#Observation de notre modèle
qqnorm(rstandard(mod41))
qqline(rstandard(mod41))
hist(rstandard(mod41))
boxplot(rstandard(mod41))
plot(x=fitted.values(mod41), y=rstandard(mod41))

#Données de notre modèle
summary(mod41)
anova(mod41)
