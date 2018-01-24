#' @rdname test_output
#' @title test_output
#' @name test_output
#' @description Testet ob ein Objekt auch die Attribute hat
#' @param x Objekt
#' @return logic-value
#' @export
test_output <- function(x){
  test_res<-TRUE
  if(is.data.frame(x)) {
    test_res <- test_is_data_frame(x)
  }
  else if(is.list(x)){
    for(i in 1:length(x)){
     test_res<-c(test_res, test_is_data_frame(x[[i]]) )
    }
  }
  else if(is.character(x)){
    test_res<-TRUE
  }
  else {test_res<-FALSE}
  return(all(test_res))
}


test_is_data_frame<- function(x){

  if(is.data.frame(x)) {
    if(length(x)==0) {
      warning("Keine Ergebnisse in data.frame")
      return(FALSE)
    }
    else test_attributs(x)
  }
}
test_attributs<- function(x){

  res<- list()

  if(!is.data.frame(x)) res["class"] <- class(x)[1]

  if(is.null(attr(x, "caption")))  res["caption"] <- FALSE
  if(is.null(attr(x, "note")))     res["note"] <- FALSE
  if(is.null(attr(x, "N")))        res["N"] <- FALSE
  if(is.null(attr(x, "labels")))   res["labels"] <- FALSE

  if(length(res)==0) TRUE
  else{
    res<- unlist(res)
    warning(paste(paste(names(res), " = ", res), collapse=", "))
    FALSE
  }
}

