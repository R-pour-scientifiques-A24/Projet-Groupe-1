fct_principale <- function(x){
  if (!is.data.frame(x)){
    stop("L'argument fourni n'est pas un data frame.")
  } else{
    for (colonne in 1:ncol(x)){
      if (is.numeric(colonne) & is.vector(colonne)){
        liste <- fct_numerique(colonne)
      }
      if (is.factor(colonne)){
        fct_catego(colonne)
      } else {
        class(colonne) <- "Autre"
      }
    }
  }
}
jeu_random <- data.frame(num=c(1,2,3,4,5), varfac=as.factor(c("a","b","a","a","b")))
str(fonction_principale(jeu_random))

str(fct_numerique(jeu_random$num))
fct_numerique(c(2,3,3,3,3))[1]
fct_numerique(c(2,3,3,3,3))[[1]]


