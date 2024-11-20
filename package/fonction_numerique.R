fct_numerique <- function(x){
  if (!(is.numeric(x) & is.vector(x))){
    stop("L'argument fourni n'est pas de type numerique.")
  } else {
    liste_sd<-list(list(length(x), sum(is.na(x))), quantile(x, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1) , na.rm=TRUE),
         mean(x, na.rm=TRUE), sd(x, na.rm=TRUE), x)
     class(liste_sd) <- "sd"
  }
}

year<-fct_numerique(volcan$start_year)
str(year)
print(year)

print.sd