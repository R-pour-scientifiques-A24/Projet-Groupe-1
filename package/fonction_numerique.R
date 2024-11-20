# x : Une colonne de type numérique data$num, ou un vecteur numérique
fct_numerique <- function(x){
  if (!(is.numeric(x) & is.vector(x))){
    stop("L'argument fourni n'est pas de type numerique.")
  } else {
    liste_statnum<-list(list(length(x), sum(is.na(x))), quantile(x, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1) , na.rm=TRUE),
              mean(x, na.rm=TRUE), sd(x, na.rm=TRUE), x)
    class(liste_statnum) <- "statnum"
    return(liste_statnum)
  }
}

year<-fct_numerique(volcan$start_year)
str(year)

# x : Un objet de type statnum 
print.statnum<-function(x){
  if (class(x)!="statnum"){
    stop("L'argument fourni n'est pas de classe statnum.")
  }else{
    statsnums<-c(x[[2]], x[[3]], x[[4]])
    names(statsnums)<-c("0%", "10%", "25%", "50%", "75%", "90%", "100%", "Moyenne", "E.T.")
    print(statsnums)
  }
}
print(year)

# x : Un objet de type statnum
summary.statnum<-function(x){
  if (class(x)!="statnum"){
    stop("L'argument fourni n'est pas de classe statnum.")
  }else{
    cat("Variable numérique\n\nNombre d'observations:\n")
    cat("Total:", as.character(x[[1]][1]), "\nManquantes(NA):", as.character(x[[1]][2]))
    cat("\n\nQuantiles:")
    qt<-as.data.frame(x[[2]])
    colnames(qt)<-""
    qt
  }
}
summary(year)
