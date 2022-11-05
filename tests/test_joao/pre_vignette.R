
rm(list = ls())
devtools::load_all(".")
#devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))
devtools::document()
devtools::load_all(".")

# examples ----
devtools::run_examples(pkg = ".",start = "transport_model")
devtools::run_examples(pkg = ".",start = "emission_model")

covr::package_coverage(type = c("examples"))
list.files(tempdir(),recursive = TRUE)
# test vigntte----------
covr::package_coverage(path = "."
                       ,type = c("vignettes")
                       ,combine_types = TRUE # Default
                       ,quiet = FALSE
)
covr::package_coverage(path = "."
                       ,type = c("tests")
                       ,combine_types = TRUE # Default
                       ,quiet = FALSE
)

devtools::test(pkg = ".",filter = "emission_model")



covr::package_coverage(
   path = "."
  ,type = c("examples")
  ,combine_types = TRUE # Default
  ,quiet = FALSE
)

covr::package_coverage(path = "."
                       ,type = c("all")
                       ,combine_types = TRUE # Default
                       ,quiet = FALSE
)

# test---------
devtools::test_coverage(pkg = ".",show_report = TRUE)
devtools::test(pkg = ".")
devtools::test(pkg = ".",filter = "ef_scaled_euro")
devtools::test(pkg = ".",filter = "ef_usa")


# check---------
tictoc::tic()
Sys.setenv(NOT_CRAN = "false" )
devtools::check(pkg = "."
                ,  cran = TRUE
                , env_vars = c(NOT_CRAN = "false")
                , vignettes = TRUE
)
tictoc::toc()

tictoc::toc()
devtools::check(pkg = "."
                ,  cran = TRUE
                , env_vars = c(NOT_CRAN = "FALSE")
                , vignettes = FALSE
)
tictoc::toc()

# install----------
tictoc::toc()
devtools::install(pkg = "."
                  ,reload = TRUE
                  ,quick = TRUE
                  ,build = FALSE
                  ,args = "devtools.install.args"
                  ,quiet = FALSE
                  ,build_vignettes = FALSE
                  )
tictoc::toc()

# release-----------
devtools::spell_check(pkg = ".",vignettes = TRUE)
urlchecker::url_check()
devtools::check(remote = TRUE, manual = TRUE)
devtools::release(pkg = ".",check = TRUE)

devtools::check_win_oldrelease()
devtools::check_win_release()
devtools::check_win_devel()
devtools::check_mac_release()
devtools::check_rhub(pkg = "."
                     ,email = "joao.bazzo@gmail.com")
rhub::check_for_cran(show_status = TRUE)
