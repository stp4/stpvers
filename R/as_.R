#' @name as_irgenwas2
#' @rdname as_irgenwas2
#' @title as_Familie
#' @description Prueft ob objekt bestimmte Eigenschaften aufweist.
#' Fuer Dataframe gibt es \code{is_all_identical2()}
#' @param x zu pruefendes Objekt
#' @return Output ist ein Vector bzq dataframe mit gleicgher Laenge wie der Input.
#' @examples
#' data<- data.frame(a=1:10,
#'   b=as.character(1:10),
#' c=gl(2, 5, labels = c("Control", "Treat")),
#' d=gl(2, 5, labels = c("1", "3"))
#'
#' )
#' as_numeric(data$a)+1
#' as_numeric(data$b)+1
#' as_numeric(data$c)+1
#' as_numeric(data$d)+1
#' as_numeric(data)
#'
NULL


#' @rdname as_irgenwas2
#' @description  as_numeric fuer factor und string-objekte
#' @export
#' @examples 
#' x<- c("0", "-3",  " 5", " 5.6", "   3,5  ", "super")
#' character_to_numeric(factor(x))
#' 
#' x<- c("0", "-3",  " 5", " 5.6", "   3,5  ")
#' character_to_numeric(x)

as_numeric <- function(x) {
    #-- library(Hmisc )
    if (is.factor(x) | is.character(x) )
      character_to_numeric(x)
    else if (is.numeric(x) | is.integer(x)) as.numeric(x)
    else if (is.data.frame(x)) stp25aggregate::dapply2(x, character_to_numeric)
    else    rep(NA,  length.out = length(x))
}


character_to_numeric<-
  function (x)
  {
    clean_space<- function(x){
      x <- sub("[[:space:]]+$", "", x)
      x<-sub("^[[:space:]]+", "", x)
      sub(",", ".", x)
    }
    if(is.character(x))  x <- as.factor(clean_space(x))
    else levels(x) <- clean_space(levels(x))
    
    nx<- nlevels(x)
    lx<-levels(x)
    lx<-gsub("[^0-9.-]+", NA, lx)
    lx<- unique(lx)
    
    if (nx==length(lx)) as.numeric(as.character(x))
    else as.numeric(x)
  }


# String und Text ---------------------------------------------------------











# Manupulate Dataframes ---------------------------------------------------



# rechentest <- data.frame(pauli=c(20,37,83,43,31,32,
#                                  15,30,42,24,07,19,35,28)/10,
#                          g= factor(c(rep(1,6), rep(2,8)), 1:2,
#                                    c("psychogen", "somatogen")),
#                                    y=NA
#                          )
#
#
# check_data(rechentest , c("pauli", "x") )
# check_data(rechentest , c("pauli", "g") )
# check_data(rechentest , c("pauli", "y") )










#' Clean_Umlaute
#' @description Clean_Umlaute2(): Funktion entfernt stoerende Umlaute,
#' @param x string
#' @export

Clean_Umlaute2 <- function(x)
{
  ## ----------------------------------------------------------------------
  ## Funktion entfernt stoerende Umlaute, unten stehende Liste ggf. erweitern
  ## ----------------------------------------------------------------------
  #  sprintf("%X", as.integer(charToRaw("Ae")))
  
  x <- gsub("\u00e4","ae", x)
  x <- gsub("\u00fc","ue", x)
  x <- gsub("\u00f6","oe", x)
  x <- gsub("\u00dc","Ue", x)
  x <- gsub("\u00c4","Ae", x)
  x <- gsub("\u00d6","Oe", x)
  x <- gsub("\u00df","ss", x)
  x <- gsub(" ", "_", x)
  x
}