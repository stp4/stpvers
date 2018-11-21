#' Neues Projekt erstellen
#'
#' @description  Erstellt ein neues Projekt mit der Ordnerstrucktur
#'  und den R-Auswertungs-Files.
#' @param project Name des projektes
#' @param datum Datum
#' @param comment Beschreibung
#' @param path Pfad default Dropbox/1_Projekte
#' @export
#' @examples
#' ## Not run:
#'
#' #'
#' #================================
#' Name      = "Hans Dampf (P)"  # M Medizin Z Zahnmedizin
#' # Dr Artzt #v Veterinär
#' # B BWL  P Psychologie PH Pädagogische Hochschule OÖ
#' # U Umit/FH-gesund  X Alles ander
#' # F Firma
#' Email     = "hansi@gmail.com>"
#' Tel       = "Tel 0650 8550 525"
#' Adresse	  = " "
#' Aufwand	  = "2-5 Stunden"
#' Thema	    = "Vergleich bzgl Entwicklungswissens"
#' 
#' Kommentar = " "
#' #================================
#' #  75  Student
#' #  80  Doktorand berufsbegleitend
#' #  90  Klinik
#' # 125  Firmen
#' Stundensatz = 75
#' Datum =  format(Sys.time(), "%d.%m.%Y")
#' Zeit =format(Sys.time(), "%H:%M")
#' Folder  = "C:/Users/wpete/Dropbox/1_Projekte"
#' KNr       = NA
#' 
#' 
#' 
#' 
#' 
#' 
#' Name<- gsub("[^A-Za-z0-9 ()]", "", Name)
#' FunktionsTest<-FALSE
#' ##  devtools::install_github("jennybc/googlesheets")
#' ##  install.packages("XML")
#' ##  devtools::install_github("hadley/xml2")
#' library(stpvers)
#' library(googlesheets)
#' library(dplyr)
#' 
#' user_session_info <- gs_user()
#' 
#' Projekt <- gs_title("ProjektVorlageKunde") %>% gs_gs()
#' Kunde <-   Projekt %>% gs_read(ws = "Stammdaten", range = cell_cols("A:K"))
#' 
#' n<-nrow(Kunde)
#' last_KNr <-Kunde[n, 1]
#' n_row<- n+2
#' KNr<- as.numeric(as.character(last_KNr))+1
#' Kunden_Daten<-c(KNr,
#'                 Datum,Zeit,	Name,
#'                 Email,Tel,Adresse,Aufwand,
#'                 Thema,Kommentar,Stundensatz)
#' neuer_Kunde <- paste(KNr, Name)
#' 
#' #-- neuen Kunde anlegen ---------------------------------
#' myCopy <-  Projekt %>%
#'   gs_copy(to = neuer_Kunde)
#' 
#' cat("\n Erstelle neues Googel-Dokument:", myCopy$sheet_title, "\n\n")
#' 
#' try(Projekt %>% gs_edit_cells(ws = "Stammdaten",
#'                               input = Kunden_Daten,
#'                               anchor = paste0("A", n_row),
#'                               trim = TRUE,
#'                               byrow = TRUE
#' ))
#' cat("\nKopiere neuen Kunden in die Projektliste\n" )
#' 
#' try(myCopy %>%  gs_edit_cells(ws = "Stammdaten",
#'                               input = Kunden_Daten,
#'                               anchor = paste0("A", 2),
#'                               trim = TRUE,
#'                               byrow = TRUE
#' ))
#' 
#' if(!FunktionsTest){
#'   #-- Create Project ---------------------------------------------
#'   setwd(Folder)
#'   CreateProjekt( project = neuer_Kunde,
#'                  datum = Datum,
#'                  comment = Kommentar,
#'                  path = Folder)
#' }
#' 
#' 


CreateProjekt <-function(project = "000 Dummy",
                         datum = date(),
                         comment = "Test Dummy",
                         path = "C:/Users/wpete/Dropbox/1_Projekte"
                         ){
  WD <- getwd()
  on.exit(setwd(WD))
  day_time<- Sys.time()
  day1<- format(day_time, "%d.%m")
  t1 <- format(day_time, "%H:%M")
  t2<-  format(day_time+17*60, "%H:%M")
  
  Rdata<- paste0(gsub("[^[:alpha:]]", "", cleansing_umlaute(project)), ".Rdata")

  if(file.exists(paste0(path, "/", project))){
    cat(paste0("\"", paste0(path, "/", project), "\" already exists:\nDo you want to overwrite?\n\n"))
    ans <- menu(c("Yes", "No"))
    if (ans == "2") {stop("new_project aborted")
    }else {file.remove(paste0(path, "/", project))}
  }
  x <- suppressWarnings(invisible(
           folder(folder.name = paste0(path,"/", project))))
  setwd(x)
  "Processed data" <- "Raw scripts" <- "Raw data"  <-  Results <- R <-Docs <- Fig <- NULL
  invisible(folder("Processed data", "Raw data", Results, R, Docs, Fig))
  myswd<- paste0("setwd(\"", x,"\")")
 #---------------------------------------------------------------------------------


  cat( "#-- Eigene Funktionen", file = "R/miscFun.r")
  
  cat(paste(project, datum, path, comment, sep="\n"), file = "README.txt")
 
  
  cat("",
       file = paste0(project, "(1).docx"))


  cat(
    paste0(
'
require(stpvers)
require(tidyverse)

# set_my_options(prozent=list(digits=c(1,0), style=2))
  graphics.off()
  MySet()
'
,myswd,
'
Projekt("", "',project,'", "'      ,datum,'")

Arbeitszeit("
Datum  Start   Ende   Task
',day1,'  ', t1,'   ', t2,'  Einarbeiten.ins.Thema
',day1,'  ', t1,'   ', t2,'  Aufbereiten.der.Daten
',day1,'  ', t1,'   ', t2,'  Auswerten
',day1,'  ', t1,'   ', t2,'  Tel/Skype

 ")

Methode()
  Materials("Data laden und transformieren")
  Research_Design("Beschreibung des Studiendesigns (Experiment, Kohortenstudie, ...)")
  Measures("Fragebogen und Skalen (Reliabilitaetsanalyse)")
    # load("Processed data/', Rdata,'")
    # N <- nrow(DF)
Results()
  Demographic_Variables()
  Statistic("H1 Korrelation", file="(4) Analyse.R")
  Statistic("H2 Regressionsanalyse", file="(5) Analyse.R")
  Statistic("H3 Korrelation", file="(6) Analyse.R")
  Statistic("H4 Regressionsanalyse", file="(7) Analyse.R")
  Statistic("Weitere Befunde", file="(8) Analyse.R")

  #Anhang()
End()

'), file = "(0) Run All.R")

 cat(
      paste0(
'
#require(stpvers)
#require(tidyverse)

# source("R/miscFun.r", echo=F)
# -- Load Data ---------------------------------------------
# data <- unzip("Raw data/auswertung.zip", exdir= Folder[1] )
# car::some(DF <- GetData("Raw data/File.R"))
# save(DF, file="Raw data/', Rdata,'")
# -- Tidy Data ---------------------------------------------
# DF %>% Drop_NA(key) %>%
#        mutate(jahr = factor(jahr)) %>%
#        Label(sex=Geschlecht)

#  save(DF, file="Processed data/', Rdata,'")

'), file = "(1) Get Data.R")


 cat(paste0(
   '
# load("Raw data/', Rdata,'")
# -- Tidy Data ---------------------------------------------
# DF %>% Drop_NA(key) %>%
   #        mutate(jahr = factor(jahr)) %>%
   #        Label(sex=Geschlecht)

# fit1<- Principal(DF[c( )], 4, cut=.35, sort=FALSE)
# fit1$Loadings %>% Output()
#
# DF$x1 <- Reliability2(DF[ ])$index
# DF$c2 <- Reliability2(DF ])$index
#   save(DF, file="Processed data/', Rdata,'")
   '), file = "(2) Measures.R")

 cat(paste0(
   '
# load("Processed data/', Rdata,'")
# APA2(~ sex + age, DF, caption="Beschreibung der Untersuchungsgruppe")
#
# DF %>% Tabelle2(sex, caption="Skalen")
'), file = "(3) Demographic.R")


cat(paste0(
'
# load("Processed data/', Rdata,'")
'), file = "(4) Analyse.R")

cat(paste0(
  '
# load("Processed data/', Rdata,'")
  '), file = "(5) Analyse.R")
cat(paste0(
  '
# load("Processed data/', Rdata,'")
  '), file = "(6) Analyse.R")
cat(paste0(
  '
# load("Processed data/', Rdata,'")
  '), file = "(7) Analyse.R")
cat(paste0(
  '
# load("Processed data/', Rdata,'")
  '), file = "(8) Analyse.R")


  cat("\nOk\n\n")
}


#-- Functions stolen from library(report)
scrubber <- function(text.var, rm.quote = TRUE, fix.comma = TRUE, ...){
        x <- reducer(Trim(clean(text.var)))
        if (rm.quote) {
            x  <- gsub('\"', "", x)
        }
        if (fix.comma) {
            x <- gsub(" ,", ",", x)
        }
        ncx <- nchar(x)
        x <- paste0(Trim(substring(x, 1, ncx - 1)), substring(x, ncx))
        x[is.na(text.var)] <- NA
        x
    }
#internal not exported
reducer <- function(x) gsub("\\s+", " ", x)
#internal not exported
Trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#internal not exported
unblanker <- function(x) subset(x, nchar(x)>0)
#internal not exported
clean <- function(text.var) sub("\\s+", " ", gsub("\r|\n|\t", " ", text.var))
mgsub <- function(pattern, replacement = NULL, text.var, fixed = TRUE, ...){
        key <- data.frame(pat=pattern, rep=replacement,
                          stringsAsFactors = FALSE)
        msubs <-function(K, x, ...){
            sapply(seq_len(nrow(K)), function(i){
                x <<- gsub(K[i, 1], K[i, 2], x, fixed = fixed, ...)
            }
            )
            return(gsub(" +", " ", x))
        }
        x <- Trim(msubs(K=key, x=text.var, ...))
        return(x)
    }

#' Create Folder
#'
#' \code{folder} - Create a folder/directory.
#' @param ... Namen der Folder
#' @param folder.name weis nicht  = NULL
#' @return \code{folder} creates a folder/directory.
#' @rdname file_handling
#' @export
folder <- function(..., folder.name = NULL) {
    if (!is.null(folder.name)) {
        x <- strsplit(folder.name, split = ", ")
    } else {
        x <- substitute(...())
    }
    if (!is.null(x)) {
        x <- unblanker(scrubber(unlist(lapply(x, function(y) {
            as.character(y)}))))
    }
    hfolder <- function(folder.name = NULL) {
        if (is.null(folder.name)) {
            FN <- mgsub(c(":", " "), c(".", "_"),
                        substr(Sys.time(), 1, 19))
        } else {
            FN <-folder.name
        }
        if (length(unlist(strsplit(FN, "/"))) == 1) {
            x <- paste(getwd(), "/", FN, sep = "")
        } else {
            x <- FN
        }
        dir.create(x)
        return(x)
    }
    if (is.null(x)) {
        hfolder()
    } else {
        if (length(x) == 1) {
            hfolder(x)
        } else {
            lapply(x, function(z) {
                hfolder(z)
            })
        }
    }
}



