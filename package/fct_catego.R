fct_catego <- function(x){
  if (!(is.factor(x))){
    stop(" L'argument fourni n'est pas de type facteur.")
  } else {
    
    freq<-cbind(names(table(x)), table(x), prop.table(table(x)))
    freq<-as.data.frame(freq)
    freq[,2]<-as.integer(freq[,2])
    freq[,3]<-as.numeric(freq[,3])
    colnames(freq)<-c("Modalités","Fréquences", "Proportion")
    rownames(freq) <- NULL
    liste_statcateg<-list(nombre_observations=c("Total :"=length(x),  "Manquantes (NA) :"=sum(is.na(x))), 
                    frequences=freq,
                    mode=which.max(table(x)), observations=x)
    class(liste_statcateg) <- "statcateg"
    return(liste_statcateg)
  }
}

categ<-fct_catego(volcan$eruption_category)
str(categ)


<<<<<<< HEAD
# x : Un objet de type statcateg 
=======
# x : Un objet de type statcateg
>>>>>>> 1a5d94d3cfaa4ff20901e7da4a937c2afb3463f6
print.statcateg<-function(x){
  if (class(x)!="statcateg"){
    stop("L'argument fourni n'est pas de classe statcateg.")
  }else{
<<<<<<< HEAD
    infos <- data.frame(c(levels(x), table(x), prop.table(table(x))))
=======
    print(as.data.frame(x[[2]]), row.names=FALSE)
>>>>>>> 1a5d94d3cfaa4ff20901e7da4a937c2afb3463f6
  }
}
print(categ)

<<<<<<< HEAD
=======
# x : Un objet de type statcateg
summary.statcateg<-function(x){
  if (class(x)!="statcateg"){
    stop("L'argument fourni n'est pas de classe statcateg.")
  }else{
    cat("Variable catégorique\n\nNombre d'observations:\n")
    cat("Total:", as.character(x[[1]][1]), "\nManquantes(NA):", as.character(x[[1]][2]))
    cat("\n\nFréquences:\n")
    print(as.data.frame(x[[2]]), row.names=FALSE)
  }
}
summary(categ)

>>>>>>> 1a5d94d3cfaa4ff20901e7da4a937c2afb3463f6
