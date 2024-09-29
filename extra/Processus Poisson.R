randexp<-rexp(100, 1)
hist(randexp, prob=TRUE, breaks = 20, main = "Distribution des temps écoulés entre 2 événements")
x<-seq(from=0, to=6, length=1000)
lines(x,dexp(x,1), col = "red", lwd = 2)


even<-c(0,1:100)
temps<-c(0,cumsum(randexp))
plot(even~temps, type = "s", main = "Dénombrement des événements en fonction du temps écoulé")

#Si T~Exp(a), alors le nombre d'événement en t N(t)~Poisson(at)
#Probabilité que le nombre d'événements entre les temps 100 et 150 > 60 = ?
#N(t)-N(s)~Poisson(a(t-s)) -> N(150)-N(100)~Poisson(50)
#P((Poisson(50))>60) = 0.07216018
ppois(60, lambda=50, lower = FALSE)

#Test d'ajustement de modèle
ks.test(randexp,"pexp") #on ne rejette pas!
qqplot(x=qexp(ppoints(100)), y=randexp)
qqline(randexp, distribution=qexp)
