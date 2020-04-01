# readme
usethis::use_readme_rmd() # create template
# edit template manually
rmarkdown::render("README.Rmd") ## or use "Knit HTML" in RStudio

devtools::check() # make sure
devtools::install()
# -----
# path to gps2linestring
usethis::use_package("sf")
usethis::use_package("data.table")
usethis::use_package("stringr")
usethis::use_package("units")
usethis::use_package("magrittr")
usethis::use_package("readr")
usethis::use_package("gtfs2gps")
# create a script called utils.R
usethis::use_testthat()
usethis::use_test("gps_to_linestring") #
devtools::load_all()
devtools::document() # it creates a man, updates namespace
devtools::check()
devtools::install()
