
devtools::load_all(".")
devtools::document(roclets = c('rd', 'collate', 'namespace', 'vignette'))
devtools::document()
devtools::load_all(".")

devtools::check(vignettes = TRUE)
devtools::check(vignettes = FALSE)
devtools::install()
devtools::check(vignettes = TRUE)

# test vigntte
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

covr::package_coverage(path = "."
                       ,type = c("examples")
                       ,combine_types = TRUE # Default
                       ,quiet = FALSE
)

covr::package_coverage(path = "."
                       ,type = c("all")
                       ,combine_types = TRUE # Default
                       ,quiet = FALSE
)

# test
devtools::test_coverage(pkg = ".",show_report = TRUE)
devtools::test(pkg = ".",filter = "ef_scaled_euro")
devtools::test(pkg = ".",filter = "ef_usa")
devtools::test(pkg = ".")

# check
tictoc::tic()
Sys.setenv(NOT_CRAN = "true" )
devtools::check(pkg = "."
                ,  cran = FALSE
                , env_vars = c(NOT_CRAN = "true")
                , vignettes = FALSE
)
tictoc::toc()