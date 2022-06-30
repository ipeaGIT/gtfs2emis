library(gtfs2emis)
library(gtfstools)
library(data.table)
library(sf)
library(ggplot2)
library(progressr)



##### test ------------------------





# run transport model
test <- function(gtfs_file){
  
gtfs <- gtfstools::read_gtfs(gtfs_file) 
tp_model <- transport_model(gtfs_data = gtfs,
                           min_speed = 2,
                           max_speed = 80,
                           new_speed = 20,
                           spatial_resolution = 100,
                           parallel = FALSE)
}

cur <- system.file("extdata/bra_cur/bra_cur_gtfs.zip", package = "gtfs2emis")
det <- system.file("extdata/usa_det/usa_det_gtfs.zip", package = "gtfs2emis")
dub <- system.file("extdata/irl_dub/irl_dub_gtfs.zip", package = "gtfs2emis")

a <- system.time( x <- test(gtfs_file = cur ) )
b <- system.time( y <- test(gtfs_file = det ) )
c <- system.time( z <- test(gtfs_file = dub ) )





##### Coverage ------------------------
library(covr)
library(testthat)
Sys.setenv(NOT_CRAN = "true")


# each function separately
a <- covr::function_coverage(fun=gtfs2emis::emission_model, test_file("tests/testthat/test_emission_model.R"))
a

# nocov start

# nocov end

# the whole package
Sys.setenv(NOT_CRAN = "true")
cov <- covr::package_coverage(path = ".", type = "tests")
cov

x <- as.data.frame(cov)
covr::codecov( coverage = cov, token ='xx' )





devtools::document()

### CMD Check ----------------
# Check package errors

# LOCAL
tictoc::tic()
Sys.setenv(NOT_CRAN = "true")
devtools::check(pkg = ".",  cran = FALSE, env_vars = c(NOT_CRAN = "true"))
tictoc::toc()



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
