
fct_numerique <- function(x){
  if (!(is.numeric(x) & is.vector(x))){
    stop("L'argument fourni n'est pas de type numerique.")
  } else {
<<<<<<< HEAD
     liste_sd <- list(length(x), length(is.na(x)), quantile(x, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1) , na.rm=TRUE),
                       mean(x, na.rm=TRUE), sd(x, na.rm=TRUE), return(x))
=======
    liste_sd<-list(list(length(x), sum(is.na(x))), quantile(x, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1) , na.rm=TRUE),
         mean(x, na.rm=TRUE), sd(x, na.rm=TRUE), x)
>>>>>>> f08c9f950985b167d9d0649bd140279bf1a7ca16
     class(liste_sd) <- "sd"
  }
}

<<<<<<< HEAD
fct_numerique(volcan$start_year)
=======
year<-fct_numerique(volcan$start_year)
str(year)
>>>>>>> f08c9f950985b167d9d0649bd140279bf1a7ca16
