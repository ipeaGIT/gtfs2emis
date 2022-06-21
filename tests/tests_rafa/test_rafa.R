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
library(tictoc)
library(mapview)
mapviewOptions(platform = 'leafgl')


library(gtfs2emis)
library(gtfs2gps)
library(gtfstools)



##### CUT GTFS curitiba ------------------------

data_path <- system.file("extdata/bra_cur/bra_cur_gtfs.zip", package = "gtfs2emis")
data_path <- "R:/Dropbox/bases_de_dados/GTFS/Curitiba/gtfs_curitiba_muni_20191002.zip"

# read GTFS
gtfs <- gtfstools::read_gtfs(data_path)

# filter time of the day
gtfs_cut <- gtfstools::filter_by_time_of_day(gtfs,from = '07:00:00', '19:00:00')

object.size(gtfs)
object.size(gtfs_cut)


shapes <- gtfstools::convert_shapes_to_sf(gtfs)
stops <- gtfstools::convert_stops_to_sf(gtfs)
mapview(stops)

center <- subset(stops, stop_id == 26337)

buff <- sf::st_buffer(x = center, dist = set_units(3.5, km) )





# mapview(center) + buff

ggplot() +
  geom_sf(data=buff, fill='gray', alpha=.3) +
  geom_sf(data=shapes, color='green') + 
  geom_sf(data=center, color='red') 


sf_use_s2(FALSE)
gtfs_cut <- gtfstools::filter_by_sf(gtfs = gtfs,
                                   geom = buff,
                                   spatial_operation = sf::st_crop )

shapes_cut <- gtfstools::convert_shapes_to_sf(gtfs_cut)

ggplot() +
  geom_sf(data=buff, fill='gray', alpha=.3) +
  geom_sf(data=shapes_cut, color='green') + 
  geom_sf(data=center, color='red') 

gtfstools::write_gtfs(gtfs_cut, 'gtfs_cut.zip')




##### Coverage ------------------------
library(covr)
library(testthat)
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





devtools::document()

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
