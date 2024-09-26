mod1<-lm(vei~, data=volcan)
summary(mod1)

shapiro.test(mod1$residuals)

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

plot(mod4)
hist(mod4$residuals, breaks=100)
shapiro.test(mod4$residuals)

step(mod4)

library(MASS)
mod5<-lm(vei~country, data=volcan)
stepAIC(mod5, direction="forward", scope=~.)
