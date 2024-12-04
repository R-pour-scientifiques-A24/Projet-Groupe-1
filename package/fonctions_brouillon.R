#######################################################################
fct_principale <- function(x){
  if (!is.data.frame(x)){
    stop("L'argument fourni n'est pas un data frame.")
  } else{
    liste <- list()
    matrice_nbobs<- matrix(NA, nrow=2, ncol=ncol(x))
    rownames(matrice_nbobs)<- c("Total", "Manquantes (NA)")
    matrice_type <- matrix(NA, nrow=1, ncol=ncol(x))
    matrice_statnum <- NULL
    liste_statfreq <- list()
    
    
    for (colonne in 1:ncol(x)){
      if (is.numeric(x[,colonne]) & is.vector(x[,colonne])){
        matrice_nbobs[,colonne]<- fct_numerique(x[,colonne])[[1]]
        matrice_type[,colonne]<- "Numérique"
        statnum <- c(fct_numerique(x[,colonne])[[2]],fct_numerique(x[,colonne])[[3]],fct_numerique(x[,colonne])[[4]])
        statnum <- t(statnum)
        matrice_statnum <- cbind(matrice_statnum,statnum)
        
      }
      if (is.factor(x[,colonne])){
        matrice_nbobs[,colonne]<- fct_catego(x[,colonne])[[1]]
        matrice_type[,colonne]<- "Catégorique"
        liste_stat_fact <- c(liste_stat_fact, fct_catego(x[,colonne])[2], fct_catego(x[,colonne])[3])
        
      } else {
        class(colonne) <- "Autre"
        matrice_nbobs[,colonne]<-c("Total"=length(x[,colonne]), "Manquantes (NA)"=sum(is.na(x[,colonne])))
        matrice_type[,colonne]<- "Autre"
        
      }
    }
  }
}





jeu_random <- data.frame(num=c(1,2,3,4,5), varfac=as.factor(c("a","b","a","a","b")))
str(fct_numerique(jeu_random$num))
names(jeu_random)[2]
colnames(jeu_random[,2])
def <- fct_numerique(jeu_random$num)
abc <- fct_numerique(c(2,3,NA,3,3))
fct_numerique(c(2,3,3,3,3))[[1]]
matrice <- NULL
#matrice <- as.matrix(abc[[1]])
matrice <- cbind(matrice,def[[1]])
matrice[,2] <- abc[[1]]
fff <- c(fct_numerique(jeu_random$num)[[2]],fct_numerique(jeu_random$num)[[3]],fct_numerique(jeu_random$num)[[4]])
fff <- t(fff)
matrice <- cbind(matrice,fff)
################################################################################




fct_principale<- function(x){
  if (!is.data.frame(x)){
    stop("L'argument fourni n'est pas un data frame.")
  } else{
    liste <- list()
    matrice_nb_obs <- as.data.frame(matrix(NA, nrow=2, ncol=ncol(x)))
    rownames(matrice_nb_obs) <- c( "Total :", "Manquantes (NA) :")
    matrice_type_variable <- as.data.frame(matrix(NA,nrow=1,ncol=ncol(x)))
    matrice_stat_num <- NULL
    liste_stat_fact <- list()
    vecteur_var_autre <- c()
    for (colonne in 1:ncol(x)){
      if (is.numeric(x[,colonne]) & is.vector(x[,colonne])){
        matrice_nb_obs[,colonne] <- as.matrix(fct_numerique(x[,colonne])[[1]])
        colnames(matrice_nb_obs)[colonne] <- colnames(x)[colonne]
        matrice_type_variable[,colonne] <- "Numerique"
        colnames(matrice_type_variable)[colonne] <- colnames(x)[colonne]
        stat_num <- c(fct_numerique(x[,colonne])[[2]],fct_numerique(x[,colonne])[[3]],fct_numerique(x[,colonne])[[4]])
        stat_num <- t(stat_num)
        matrice_stat_num <- cbind(matrice_stat_num,stat_num)
      }
      if (is.factor(x[,colonne])){
        catego <- fct_catego(x[,colonne])
        matrice_nb_obs[,colonne] <- as.matrix(fct_catego(x[,colonne])[[1]])
        colnames(matrice_nb_obs)[colonne] <- colnames(x)[colonne]
        matrice_type_variable[,colonne] <- "Facteur"
        colnames(matrice_type_variable)[colonne] <- colnames(x)[colonne]
        liste_stat_fact <- c(liste_stat_fact, catego[[2]], catego[[3]])
      } else {
        class(x[,colonne]) <- "autre"
        matrice_nb_obs[,colonne] <- c("Total"=length(x[,colonne]), "Manquantes (NA)"=sum(is.na(x[,colonne])))
        colnames(matrice_nb_obs)[colonne] <- colnames(x)[colonne]
        matrice_type_variable[,colonne] <- "Autre"
        colnames(matrice_type_variable)[colonne] <- colnames(x)[colonne]
        vecteur_var_autre <- c(vecteur_var_autre, names(x)[colonne])
      }
    }
    liste_finale <- list(nombre_observations=as.matrix(matrice_nb_obs), type_variable=as.matrix(matrice_type_variable), stat_num=matrice_stat_num, stat_fact=liste_stat_fact, var_autre=vecteur_var_autre)
    return(liste_finale)
    class(liste_finale) <- "principale"
  }
}

result <- fct_principale(volcan)

str(result)
