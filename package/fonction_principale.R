fct_principale <- function(x){
  if (!is.data.frame(x)){
    stop("L'argument fourni n'est pas un data frame.")
  } else{
    for (colonne in 1:ncol(x)){
      if (is.numeric(colonne) & is.vector(colonne)){
        fct_numerique(colonne)
      }
      if (is.factor(colonne)){
        fct_catego(colonne)
      } 
    }
  }
}

test <- fct_principale(volcan)
str(test)


fct_principale <- function(x){
  if (!is.data.frame(x)){
    stop("L'argument fourni n'est pas un data frame.")
  } else{
    liste <- list()
    for (colonne in 1:ncol(x)){
      if (is.numeric(colonne) & is.vector(colonne)){
        num <- fct_numerique(colonne)
        nombre_observations <- fct_numerique(colonne)[1]
        type_variable <- "Numerique"
        stat_num <- num
      }
      if (is.factor(colonne)){
        catego <- fct_catego(colonne)
        nombre_observations <- fct_catego(colonne)[1]
        type_variable <- "Facteur"
        stat_fact <- list(catego[2], catego[3])
      } else {
        class(colonne) <- "autre"
        nombre_observations <- list(nombre_observations=list(length(colonne), 
                                                             sum(is.na(colonne))))
        type_variable <- "Autre"
        var_autre <- names(colonne)
      }
    }
  }
}

