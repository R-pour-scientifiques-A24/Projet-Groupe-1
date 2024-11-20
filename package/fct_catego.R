
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


# x : Un objet de type statnum 
print.statcateg<-function(x){
  if (class(x)!="statcateg"){
    stop("L'argument fourni n'est pas de classe statcateg.")
  }else{
    ligne<-c(x[[2]], x[[3]], x[[4]])
    names(ligne)<-c("0%", "10%", "25%", "50%", "75%", "90%", "100%", "Moyenne", "E.T.")
    print(ligne)
  }
}
print(categ)
