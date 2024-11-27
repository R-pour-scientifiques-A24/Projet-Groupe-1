fct_principale <- function(x){
  if (!is.data.frame(x)){
    stop("L'argument fourni n'est pas un data frame.")
  } else{
    liste <- list()
    matrice_nbobs<- matrix(NA, nrow=2, ncol=ncol(x))
    rownames(matrice_nbobs)<- c("Total", "Manquantes (NA)")
    matrice_type <- matrix(NA, nrow=1, ncol=ncol(x))
    matrice_statnum <- NULL
    
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

