
test_that("ef_usa_emfac", {
  # GTFS2gps filter
  library(data.table)
  library(gtfs2gps)
  library(magrittr)
  library(gtfstools)
  fort <- gtfstools::read_gtfs(system.file("extdata/fortaleza.zip"
                                          , package = "gtfs2gps"))  %>%
    gtfs2gps::filter_single_trip() %>% 
    gtfstools::filter_by_shape_id(c("shape804-I"))
  
  fort_gps <- gtfs2gps::gtfs2gps(fort, parallel = TRUE) %>% 
    gtfs2gps::adjust_speed()
  
  fort_gpslines <- gtfs2gps::gps_as_sflinestring(fort_gps)
  
  fort_gpslines$dist <- units::set_units(fort_gpslines$dist, "km")
  
  # fleet
  total_fleet <- data.table::data.table(year = c(2005,2010,2011,2012
                                                 ,2014,2015,2017,2018,2019),
                                        bus = c(1,61,50,1,45,18,62,27,31))
  
  total_fleet[,fleet_composition := bus/sum(bus)]
  
  # function
  EF_usa <- ef_usa_emfac(pollutant = c("CO","PM10"),
                              reference_year = "2019",
                              model_year = total_fleet$year,
                              speed = fort_gpslines$speed,
                              fuel = "D")

  # Expect equal----
  expect_equal(names(EF_usa),c("pollutant","model_year","fuel","EF" ))
  expect_equal(as.numeric(sum(EF_usa$EF,na.rm = TRUE)), 19.45502, 0.01)

  # Expect error----
  expect_error(ef_usa_emfac(pollutant = c("CO","PM10"),
                            reference_year = "2019",
                      model_year = total_fleet$year,
                      speed = as.numeric(fort_gpslines$speed),
                      fuel = "D"),
               "speed neeeds to has class 'units' in 'km/h'. Please, check package 'units'")
})
