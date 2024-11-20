
fct_numerique <- function(x){
  if (!(is.numeric(x) & is.vector(x))){
    stop("L'argument fourni n'est pas de type Numerique.")
  } else {
     liste_sd <- list(nrow(x), length(is.na(x)), quantile(x, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1) , na.rm=TRUE),
                       mean(x, na.rm=TRUE), sd(x, na.rm=TRUE), return(x))
     class(liste_sd) <- "sd"
     return(liste_sd)
  }
}

fct_numerique(volcan$start_year)
