
fct_catego <- function(x){
  if (!(is.factor(x))){
    stop(" L'argument fourni n'est pas de type Facteur.")
  } else {
    liste_statcateg<-list(nombre_observations=list(length(x), sum(is.na(x))), 
                    frequences=data.frame(Modalite=levels(x), Frequence=table(x), Proportion=prop.table(table(x))),
                    mode=which.max(table(x)), observations=x)
    class(liste_statcateg) <- "statcateg"
    return(liste_statcateg)
  }
}

categ<-fct_catego(volcan$eruption_category)
str(categ)

