#' cleansing_umlaute cleansing
#' 
#' Data cleansing 
#' 
#' cleansing_umlaute(): Funktion entfernt stoerende Umlaute,
#' Funktion entfernt stoerende Umlaute, unten stehende Liste ggf. erweitern 
#' sprintf("%X", as.integer(charToRaw("Ae")))
#' @param x string
#' 
#' @noRd
cleansing_umlaute <- function(x){
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

#' clean_space
#' 
#' Leerzeichen entfernen
#'
#' @param x string
#'
#' @noRd
clean_space <- function(x) {
  x <- sub("[[:space:]]+$", "", x)
  x <- sub("^[[:space:]]+", "", x)
  sub(",", ".", x)
}