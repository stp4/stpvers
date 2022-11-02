
<!-- README.md is generated from README.Rmd. Please edit that file -->



This is a purely experimental collection of functions and will no longer be updated!


# stpvers

Die stpvers ist eine Kopie von tidyverse und ladet die library stpAPA2,
stpOutput, stpAggregate und stpData.

## Overview

library(stpvers)

  - `is_irgendwas` Prüft eine Bedingung.
  - `as_irgenwas` Transformiert einen Vektor zu irgendwas.
  - `test_output` Testet den Output aus stpAPA2 ob er mit stpOutput
    verarbeitet werden kann.
  - `CreateProjekt` Projekt Erstellung mit Kunden-Datenblatt sowie die
    der Folder-Strucktur mit den R-Files.

## Installation

### Requirements

To use `stpvers` you need to make sure the following software is
installed on your computer:

  - [R](http://www.r-project.org/) (3.4.3 or later)
  - [RStudio](http://www.rstudio.com/) is optional

### Install stpvers

Once all that is taken care of, install `stpvers` from GitHub:

``` {r}
devtools::install_github("stp4/stpvers")
# devtools::install_github("stp4/stp25")

devtools::install_github("stp4/stp25APA2")
devtools::install_github("stp4/stp25output")
devtools::install_github("stp4/stp25data")
devtools::install_github("stp4/stp25aggregate")
devtools::install_github("stp4/stp25plot")
 
```

### Usage

``` r


library(stpvers)
#> 
#> Attaching package: 'stpvers'
#> The following object is masked _by_ 'package:stp25output':
#> 
#>     Clean_Umlaute2
getwd()
#> [1] "C:/Users/wpete/Dropbox/3_Forschung/R-Project/stpvers"

isFALSE(TRUE)
#> [1] FALSE
```

Please note that this project is released only as an experimental
project.
