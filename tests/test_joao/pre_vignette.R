
# checking stuff----
rm(list=ls())
devtools::load_all()
devtools::document()
devtools::load_all()

devtools::check(manual = TRUE
                ,vignettes = FALSE
                ,cran = TRUE)
devtools::install()

# libraries----
library(data.table)
library(magrittr)
library(gtfs2gps)
library(devtools)
library(mapview)

# r1----