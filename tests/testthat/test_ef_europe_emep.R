
test_that("ef_europe_emep", {
  # GTFS2gps filter
  library(data.table)
  library(gtfs2gps)
  library(magrittr)
  library(gtfstools)
  fort <- gtfstools::read_gtfs(system.file("extdata/fortaleza.zip"
                                          , package = "gtfs2gps"))  %>%
    gtfs2gps::filter_single_trip() %>% 
    gtfstools::filter_by_shape_id(c("shape804-I", "shape806-I"))
  
  fort_gps <- gtfs2gps::gtfs2gps(fort, parallel = TRUE) %>% 
    gtfs2gps::adjust_speed()
  
  fort_gpslines <- gtfs2gps::gps_as_sflinestring(fort_gps)
  
  fort_gpslines$dist <- units::set_units(fort_gpslines$dist, "km")
  
  # Fleet
  total_fleet <- data.table::data.table(year = c(2005,2010,2011,2012,2014,2015,2017,2018,2019),
                                        bus = c(1,61,50,1,45,18,62,27,31),
                                        veh_type_euro = "Ubus Std 15 - 18 t",
                                        euro_stage = c("II", "IV", "IV", "V", "V", "V", "V", "V","V"))
  
  total_fleet[,fleet_composition := bus/sum(bus)]
  
  det_fleet <- data.table::data.table(shape_id = unique(fort$shapes$shape_id),
                                      bus_age = c("2010", "2011", "2012", "2013"),
                                      bus_fuel = "D")
  # gtfs2emis
  EF_europe <- ef_europe_emep(pollutant = c("CO", "PM10"),
                         speed = fort_gpslines$speed,
                         veh_type = total_fleet$veh_type_euro,
                         tech = "SCR",
                         euro = total_fleet$euro_stage)
  
  # Expect equal -----
  expect_equal(length(EF_europe), 8)
  expect_equal(length(EF_europe$pollutant), 18)
  expect_equal(length(EF_europe$veh_type), 18)
  expect_equal(length(EF_europe$euro), 18)
  expect_equal(units::deparse_unit(EF_europe$EF$CO_Euro_II), "g km-1")
  expect_equal(sum(EF_europe$EF,na.rm = TRUE), 810.4236,0.1)
  

  # Expect error
  expect_error(ef_europe_emep(pollutant = c("CO", "PM"),
                         speed = fort_gpslines$speed,
                         veh_type = total_fleet$veh_type_euro,
                         tech = "SCR",
                         euro = total_fleet$euro_stage))
  expect_error(ef_emep_europe(pollutant = c("CO", "PM"),
                         speed = 1:100,
                         veh_type = total_fleet$veh_type_euro,
                         tech = "SCR",
                         euro = total_fleet$euro_stage))
  
})
