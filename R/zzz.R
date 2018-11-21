.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()

  stp25_attach()
}



#' @rdname is_irgendwas
#' @export
#' @examples
#' is_attached("stpvers")
is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
