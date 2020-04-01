usethis::create_package()
usethis::use_git()
# create function manually
devtools::load_all()
devtools::check()
# edit description manually
usethis::use_mit_license("Joao Bazzo")
devtools::document() # it creates a man, updates namespace
devtools::check() # again, with no warnings


usethis::use_testthat()
usethis::use_test("fbind") # creates with
# edit file of tests manually on /tests/test-fbind.R
devtools::test()
# -----------------------------------------
# adding dependencies
usethis::use_package("forcats") # Adding 'forcats' to Imports field in DESCRIPTION
# creat function 2 manually
devtools::load_all()
devtools::document() # it creates a man, updates namespace
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
# create a script called utils.R
usethis::use_testthat()
usethis::use_test("gps_to_linestring") #
devtools::load_all()
devtools::document() # it creates a man, updates namespace
devtools::check()
devtools::install()
