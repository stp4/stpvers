core <- c("stp25output",
          "stp25stat",
          "stp25aggregate",
          "stp25plot",
          "stp25data")

stp25_attach <- function() {
  versions <-
    vapply(core, function(x)
      as.character(utils::packageVersion(x)), character(1))

  suppressPackageStartupMessages(lapply(
    core,
    library,
    character.only = TRUE,
    warn.conflicts = FALSE
  ))

  invisible()
}


#' @importFrom rstudioapi isAvailable getVersion
platform_info <- function() {
  if (rstudioapi::isAvailable()) {
    ver <- rstudioapi::getVersion()
    ui <- paste0("RStudio ", ver, "")
  } else {
    ui <- .Platform$GUI
  }

  ver <- R.version

  c(
    Date = format(Sys.Date()),
    R = paste0(ver$major, ".", ver$minor),
    OS = os(),
    GUI = ui,
    Locale = Sys.getlocale("LC_COLLATE"),
    TZ = Sys.timezone()
  )
}

os <- function() {
  x <- utils::sessionInfo()$running
  x <- gsub("Service Pack", "SP", x)
  x <- gsub(" [(]build \\d+[)]", "", x)
  x
}
