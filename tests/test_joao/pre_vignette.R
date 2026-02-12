rm(list = ls())


easypackages::packages('devtools','rhubv2'
                       ,'tictoc','pak'
                       ,'urlchecker','purrr'
                       ,'checkmate')
# 1. Document package  ------------

devtools::document()


# 2. Load package  ------------

devtools::load_all(".")


# 3. Run tests  ------------

devtools::test()

# Optional: test coverage
devtools::test_coverage(pkg = ".", show_report = TRUE)


# 4. Coverage checks  ------------

covr::package_coverage(type = "all", show_report = TRUE)


# 5. Local package check  ------------

devtools::check()


# 6. Remote CRAN-like check  ------------

rhub::rhub_platforms()
devtools::build()
rhub::rhub_check(platforms = 'linux')


# 7. Install locally ------------

tictoc::tic()
devtools::install(
  pkg = ".",
  reload = TRUE,
  quick = TRUE,
  build = FALSE,
  build_vignettes = FALSE
)
tictoc::toc()


# 8. Pre-release checks  ------------

pak::local_install_dev_deps(upgrade = TRUE)

devtools::build_readme()

devtools::spell_check(vignettes = TRUE)

urlchecker::url_check()

# Final CRAN-like check  ------------
Sys.setenv(NOT_CRAN = "false")
devtools::check(remote = TRUE, manual = TRUE)


# 9. Release  ------------

devtools::release(pkg = ".", check = TRUE)
