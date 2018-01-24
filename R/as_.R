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
#' @export
as_numeric <- function(x) {
    #-- library(Hmisc )
    if (is.factor(x)) {# wenn levels Zahlen
        if (Hmisc::all.is.numeric(x))
            Hmisc::all.is.numeric(x, 'vector')
        else
            as.numeric(x)
    } else if (is.character(x) &
               Hmisc::all.is.numeric(x)) {
        Hmisc::all.is.numeric(x, 'vector')
    } else if (is.numeric(x) | is.integer(x)) {
        x
    } else if (is.data.frame(x)) stp25aggregate::dapply2(x, as_numeric)

    else
        rep(NA,  length.out = length(x))
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




