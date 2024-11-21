fonction_principale <- function(x){
  if (!is.data.frame(x)){
    stop("L'argument fourni n'est pas un data frame.")
  } else{
    for (colonne in 1:length(x)){
      if (is.numeric(colonne) & is.vector(colonne)){
        num <- lapply(x, FUN=fct_numerique(colonne))
      }
      if (is.factor(colonne)){
        categ <- lapply(x, FUN=fct_catego(colonne))
      } else {
        autre <- "autre"
      }
    }
  }
}

test <- fonction_principale(volcan)
str(test)


