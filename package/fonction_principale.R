fonction_principale <- function(x){
  if (!is.data.frame(x)){
    stop("L'argument fourni n'est pas un data frame.")
  } else{
    for (colonne in 1:length(x)){
      if (is.numeric(colonne) & is.vector(colonne)){
        num <- lapply(colonne, FUN=fct_numerique)
      }
      if (is.factor(colonne)){
        categ <- lapply(colonne, FUN=fct_catego)
      } else {
        autre <- "autre"
      }
    }
  }
}

test <- fonction_principale(volcan)
str(test)


