fct_catego <- function(x){
  if (!(is.factor(x))){
    stop(" L'argument fourni n'est pas de type Facteur.")
  } else {
    
    freq<-as.matrix(rbind(names(table(volcan$eruption_category)), table(volcan$eruption_category), prop.table(table(volcan$eruption_category))))
    freq<-as.data.frame(t(freq))
    colnames(freq)<-c("Modalités","Fréquences", "Proportion")
    rownames(freq) <- NULL
    
    liste_statcateg<-list(nombre_observations=list(length(x), sum(is.na(x))), 
                    frequences=freq,
                    mode=which.max(table(x)), observations=x)
    class(liste_statcateg) <- "statcateg"
    return(liste_statcateg)
  }
}

categ<-fct_catego(volcan$eruption_category)
str(categ)


# x : Un objet de type statcateg
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

# x : Un objet de type statcateg
summary.statcateg<-function(x){
  if (class(x)!="statcateg"){
    stop("L'argument fourni n'est pas de classe statcateg.")
  }else{
    cat("Variable catégoriquee\n\nNombre d'observations:\n")
    cat("Total:", as.character(x[[1]][1]), "\nManquantes(NA):", as.character(x[[1]][2]))
    cat("\n\nFréquences:")
    as.data.frame(x[[2]])
  }
}
summary(categ)

