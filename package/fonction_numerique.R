# x : Une colonne de type numérique data$num, ou un vecteur numérique
fct_numerique <- function(x){
  if (!(is.numeric(x) & is.vector(x))){
    stop("L'argument fourni n'est pas de type numerique.")
  } else {
    liste_sd<-list(list(length(x), sum(is.na(x))), quantile(x, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1) , na.rm=TRUE),
              mean(x, na.rm=TRUE), sd(x, na.rm=TRUE), x)
     
    class(liste_sd) <- "sd"
    
    return(liste_sd)
  }
}

year<-fct_numerique(volcan$start_year)
str(year)
print(year)

# x : Un objet de type sd 
print.sd<-function(x){
  ligne<-c(x[[2]], x[[3]], x[[4]])
  names(ligne)<-c("0%", "10%", "25%", "50%", "75%", "90%", "100%", "Moyenne", "E.T.")
  print(ligne)
}

print(year)

# x : Un objet de type sd 
summary.sd<-function(x){
  cat("Variable numérique\n\nNombre d'observations:\n")
  cat("Total:", as.character(x[[1]][1]), "\nManquantes(NA):", as.character(x[[1]][2]))
  cat("\n\nQuantiles:")
  qt<-as.data.frame(x[[2]])
  colnames(qt)<-""
  qt
  }
summary(year)
