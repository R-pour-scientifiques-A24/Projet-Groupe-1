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
    matrice_nb_obs <- matrix(NA, nrow=2, ncol=ncol(x))
    rownames(matrice_nb_obs) <- c( "Total :", "Manquantes (NA) :")
    matrice_type_variable <- matrix(NA,nrow=1,ncol=col(x))
    matrice_stat_num <- NULL
    for (colonne in 1:ncol(x)){
      if (is.numeric(x[,colonne]) & is.vector(x[,colonne])){
        matrice_nb_obs[,colonne] <- as.matrix(fct_numerique(x[,colonne])[[1]])
        matrice_type_variable[,colonne] <- "Numerique"
        matrice_stat_num <- cbind(matrice_stat_num, fct_numerique(x[,colonne])[c(2,3,4)])
      }
      if (is.factor(x[,colonne])){
        catego <- fct_catego(colonne)
        matrice_nb_obs[,colonne] <- as.matrix(fct_catego(x[,colonne])[[1]])
        matrice_type_variable[,colonne] <- "Facteur"
        stat_fact <- list(catego[2], catego[3])
      } else {
        class(colonne) <- "autre"
        matrice_nb_obs[,colonne] <- c("Total"=length(x[,colonne]), "Manquantes (NA)"=sum(is.na(x[,colonne])))
        matrice_type_variable[,colonne] <- "Autre"
      }
    }
  }
}

