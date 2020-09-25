bullets <- function(...) {
  message(paste0(" * ", ..., collapse = "\n"))
}


startup_message <- function(...) {
  packageStartupMessage(...)
}


rule <- function(..., pad = "-", startup = FALSE) {
  if (nargs() == 0) {
    title <- ""
  } else {
    title <- paste0(...)
  }
  width <- min(getOption("width") - nchar(title) - 1, 68)
  text <- paste0(title, " ", paste(rep(pad, width), collapse = ""))

  if (startup) {
    startup_message(text)
  } else {
    message(text)
  }
}


invert <- function(x) {
  if (length(x) == 0) return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}
