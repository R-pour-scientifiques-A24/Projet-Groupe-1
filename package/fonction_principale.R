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


fct_principale<- function(x){
  if (!is.data.frame(x)){
    stop("L'argument fourni n'est pas un data frame.")
  } else{
    liste <- list()
    matrice_nb_obs <- as.data.frame(matrix(NA, nrow=2, ncol=ncol(x)))
    rownames(matrice_nb_obs) <- c( "Total :", "Manquantes (NA) :")
    matrice_type_variable <- matrix(NA,nrow=1,ncol=ncol(x))
    matrice_stat_num <- NULL
    liste_stat_fact <- list()
    vecteur_var_autre <- c()
    for (colonne in 1:ncol(x)){
      if (is.numeric(x[,colonne]) & is.vector(x[,colonne])){
        matrice_nb_obs[,colonne] <- as.matrix(fct_numerique(x[,colonne])[[1]])
        colnames(matrice_nb_obs)[colonne] <- colnames(x)[colonne]
        matrice_type_variable[,colonne] <- "Numerique"
        stat_num <- matrix(c(fct_numerique(x[,colonne])[[2]],fct_numerique(x[,colonne])[[3]],fct_numerique(x[,colonne])[[4]]),9,1)
        colnames(stat_num) = colnames(x)[colonne]
        matrice_stat_num <- cbind(matrice_stat_num,stat_num)
      }
      if (is.factor(x[,colonne])){
        catego <- fct_catego(x[,colonne])
        matrice_nb_obs[,colonne] <- as.matrix(fct_catego(x[,colonne])[[1]])
        colnames(matrice_nb_obs)[colonne] <- colnames(x)[colonne]
        matrice_type_variable[,colonne] <- "Facteur"
       # liste_stat_fact <- c(liste_stat_fact,catego[[2]],catego[[3]])
       # liste_stat_fact <- list("Fréquences" = as.data.frame(liste_stat_fact,catego[[2]]), "Mode" = catego[[3]])
       # liste_stat_fact <- list(as.data.frame(liste_stat_fact,catego[[2]]), catego[[3]])
        
      } else {
        class(x[,colonne]) <- "autre"
        matrice_nb_obs[,colonne] <- c("Total"=length(x[,colonne]), "Manquantes (NA)"=sum(is.na(x[,colonne])))
        colnames(matrice_nb_obs)[colonne] <- colnames(x)[colonne]
        matrice_type_variable[,colonne] <- "Autre"
        vecteur_var_autre <- c(vecteur_var_autre, names(x)[colonne])
      }
    }
    row.names(matrice_stat_num) = c("0%", "10%", "25%", "50%", "75%", "90%", "100%", "Moyenne", "E.T.")
    liste_finale <- list(nombre_observations=as.matrix(matrice_nb_obs), type_variable=matrice_type_variable, stat_num=as.matrix(matrice_stat_num), stat_fact=liste_stat_fact, var_autre=vecteur_var_autre)
    return(liste_finale)
    class(liste_finale) <- "principale"
  }
}

result <- fct_principale(volcan)
str(result)

# Méthode summary pour un objet retourné par la fct_principale
summary.principale <- function(x){
  if (class(x)!="principale"){
    stop("L'argument fourni n'est pas de classe principale")
  } else{
    
  }
}
summary(result)

