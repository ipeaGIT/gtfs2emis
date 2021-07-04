library(sf)
library(data.table)
library(magrittr)
library(roxygen2)
library(devtools)
library(usethis)
library(profvis)
library(dplyr)
library(mapview)
library(covr)
library(testthat)
library(ggplot2)
library(checkmate)
library(geobr)
library(gtfs2gps)
library(tictoc)
library(mapview)
mapviewOptions(platform = 'leafgl')


library(gtfs2emis)



##### INPUT  ------------------------





##### Coverage ------------------------

 Sys.setenv(NOT_CRAN = "true")


# each function separately
covr::function_coverage(fun=pkg::fun, test_file("tests/testthat/test-fun.R"))


# nocov start

# nocov end

# the whole package
Sys.setenv(NOT_CRAN = "true")
cov <- covr::package_coverage(path = ".", type = "tests")
cov

x <- as.data.frame(cov)
covr::codecov( coverage = cov, token ='xx' )







### CMD Check ----------------
# Check package errors

# LOCAL
Sys.setenv(NOT_CRAN = "true")
devtools::check(pkg = ".",  cran = FALSE, env_vars = c(NOT_CRAN = "true"))

# CRAN
Sys.setenv(NOT_CRAN = "false")
devtools::check(pkg = ".",  cran = TRUE, env_vars = c(NOT_CRAN = "false"))

devtools::check_win_release(pkg = ".")

# devtools::check_win_oldrelease()
# devtools::check_win_devel()


beepr::beep()



tictoc::tic()
devtools::check(pkg = ".",  cran = TRUE, env_vars = c(NOT_CRAN = "false"))
tictoc::toc()





# build binary -----------------
system("R CMD build . --resave-data") # build tar.gz




### pkgdown: update website ----------------

# Run to build the website
pkgdown::build_site()
