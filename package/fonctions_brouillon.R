#######################################################################
# Version 1 à ignorer
fct_principale<- function(x){
  if (!is.data.frame(x)){
    stop("L'argument fourni n'est pas un data frame.")
  } else{
    liste <- list()
    matrice_nb_obs <- as.data.frame(matrix(NA, nrow=2, ncol=ncol(x)))
    rownames(matrice_nb_obs) <- c( "Total :", "Manquantes (NA) :")
    matrice_type_variable <- as.data.frame(matrix(NA,nrow=1,ncol=ncol(x)))
    nb_num <- sum(sapply(x, is.numeric))
    matrice_stat_num <- as.data.frame(matrix(NA,nrow=9,ncol=nb_num))
    nom_col <- c()
    liste_stat_fact <- list()
    vecteur_var_autre <- c()
    for (colonne in 1:ncol(x)){
      if (is.numeric(x[,colonne]) & is.vector(x[,colonne])){
        matrice_nb_obs[,colonne] <- as.matrix(fct_numerique(x[,colonne])[[1]])
        colnames(matrice_nb_obs)[colonne] <- colnames(x)[colonne]
        matrice_type_variable[,colonne] <- "Numerique"
        colnames(matrice_type_variable)[colonne] <- colnames(x)[colonne]
        stat_num <- c(fct_numerique(x[,colonne])[[2]],fct_numerique(x[,colonne])[[3]],fct_numerique(x[,colonne])[[4]])
        matrice_stat_num <- cbind(matrice_stat_num, stat_num)
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

#Version 2 à ignorer
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
    class(liste_finale) <- "principale"
    return(liste_finale)
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
################################################################################



#Version qui marche
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
    freq <- list()
    mode <- list()
    nom_col_facteur <- c()
    vecteur_var_autre <- c()
    for (colonne in 1:ncol(x)){
      if (is.numeric(x[,colonne]) & is.vector(x[,colonne])){
        matrice_nb_obs[,colonne] <- as.matrix(fct_numerique(x[,colonne])[[1]])
        colnames(matrice_nb_obs)[colonne] <- colnames(x)[colonne]
        matrice_type_variable[,colonne] <- "Numerique"
        colnames(matrice_type_variable)[colonne] <- colnames(x)[colonne]
        stat_num <- matrix(c(fct_numerique(x[,colonne])[[2]],fct_numerique(x[,colonne])[[3]],fct_numerique(x[,colonne])[[4]]),9,1)
        colnames(stat_num) = colnames(x)[colonne]
        matrice_stat_num <- cbind(matrice_stat_num,stat_num)
        #colnames(matrice_stat_num)[colonne] <- colnames(x)[colonne]
        #  print(matrice_stat_num)
      }
      if (is.factor(x[,colonne])){
        nom_col_facteur<-c(nom_col_facteur, colnames(x)[colonne])
        catego <- fct_catego(x[,colonne])
        matrice_nb_obs[,colonne] <- as.matrix(fct_catego(x[,colonne])[[1]])
        matrice_type_variable[,colonne] <- "Facteur"
        colnames(matrice_type_variable)[colonne] <- colnames(x)[colonne]
        tab_freq <- data.frame(catego[[2]])
        freq <- c(freq,list(tab_freq))
        tab_mode <- data.frame(catego[[3]])
        mode <- c(mode,list(tab_mode))
        colnames(matrice_nb_obs)[colonne] <- colnames(x)[colonne]
      } else {
        class(x[,colonne]) <- "autre"
        matrice_nb_obs[,colonne] <- c("Total"=length(x[,colonne]), "Manquantes (NA)"=sum(is.na(x[,colonne])))
        colnames(matrice_nb_obs)[colonne] <- colnames(x)[colonne]
        matrice_type_variable[,colonne] <- "Autre"
        colnames(matrice_type_variable)[colonne] <- colnames(x)[colonne]
        vecteur_var_autre <- c(vecteur_var_autre, names(x)[colonne])
      }
    }
    row.names(matrice_stat_num) = c("0%", "10%", "25%", "50%", "75%", "90%", "100%", "Moyenne", "E.T.")
    names(freq) <- nom_col_facteur
    names(mode) <- nom_col_facteur
    liste_stat_fact <- c(liste_stat_fact, "Fréquences"=list(freq), "Mode"=list(mode))
    liste_finale <- list(nombre_observations=as.matrix(matrice_nb_obs), type_variable=as.matrix(matrice_type_variable), stat_num=as.matrix(matrice_stat_num), stat_fact=liste_stat_fact, var_autre=vecteur_var_autre)
    class(liste_finale) <- "principale"
    return(liste_finale)
  }
}

summary.principale <- function(x){
  if (class(x)!="principale"){
    stop("L'argument fourni n'est pas de classe principale.")
  }else{
    cat("Type de variable :\n\n")
    print(t(x[[2]]))
    cat("\n\n")
    cat("Nombre d'observations par variable :\n\n")
    print(t(x[[1]]))
    cat("\n\n")
    cat("*********************** \n\n")
    cat("Statistiques descriptives des variables numeriques:\n\n")
    cat("Quantiles par variable numerique:\n\n")
    print(t(x[[3]])[,1:7])
    cat("\n\n")
    cat("Moyenne et variance par variable numerique:\n\n")
    print(t(x[[3]])[,c(8,9)])
    cat("\n\n")
    cat("*********************** \n\n")
    cat("Statistiques descriptives des facteurs: \n\n")
    for(i in 1:length(x[[4]][[1]])){
      cat(names(x[[4]][[1]][i]), ": Fréquences des catégories: \n\n")
      print(x[[4]][[1]][[i]])
      cat("\n\n")
      mode_fac <- as.numeric(x[[4]][[2]][[i]])
      cat(names(x[[4]][[2]][i]),": Le mode est", mode_fac, "\n\n\n")
    }
    
  }
}

result <- fct_principale(volcan)
str(result)
names(result[[4]][[2]][1])
result[2]
summary(result)
length(as.character(result[[4]][[2]][[1]]))
as.numeric(categ[[3]])
class(result)
x= volcan
liste_stat_fact <- list()
freq <- list(1,2,3,4)
mode <- list(5,6,7,8)
catego <- fct_catego(x)
tab_freq <- data.frame(catego[2])
freq <- c(freq,tab_freq)
tab_mode <- data.frame(catego[[3]])
mode <- c(mode,list(tab_mode))
liste_stat_fact <- c(liste_stat_fact, freq, mode)
str(liste_stat_fact)

liste_random <- list(freq,mode)
names(liste_random)<- c("a","b")
liste_random
# trouver comment changer le nom data frame et summary
liste_statcateg
