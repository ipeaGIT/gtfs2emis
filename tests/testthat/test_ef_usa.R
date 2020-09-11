
test_that("emis", {
  spo <- gtfs2gps::read_gtfs(system.file("extdata/saopaulo.zip", package = "gtfs2gps"))  %>%
    gtfs2gps::filter_by_shape_id(53000:53020)
  
  spo_gps <- gtfs2gps::gtfs2gps(spo, parallel = TRUE)

  spo_gpslines <- gtfs2gps::gps_as_sflinestring(spo_gps) %>% 
    dplyr::select(trip_id,speed, dist, departure_time)
  
  spo_gpslines$dist <- units::set_units(spo_gpslines$dist, "km")
  
  total_fleet <- data.table::data.table(year = c(2005,2010,2011,2012,2014,2015,2017,2018,2019),
                                        bus = c(1,61,50,1,45,18,62,27,31),
                                        veh_type_euro = "Urban Buses Standard 15 - 18 t",
                                        euro_stage = c("II", "IV", "IV", "V", "V", "V", "V", "V","V"))
  
  total_fleet[,fleet_composition := bus/sum(bus)]

  det_fleet <- data.table::data.table(shape_id = unique(spo$shapes$shape_id),
                                      bus_age = c("2010", "2011", "2012", "2013"),
                                      bus_fuel = "Diesel")
  
  EF_usa <- ef_usa(pollutant = c("CO","PM10"),
                              calendar_year = "2019",
                              model_year = total_fleet$year,
                              speed = spo_gpslines$speed,
                              fuel = "Diesel")

  expect_equal(sum(EF_usa), 32090.02, 0.1)

  spo_gpslines$speed <- as.numeric(spo_gpslines$speed)
  
  expect_error(ef_usa(pollutant = c("CO","PM10"),
                      calendar_year = "2019",
                      model_year = total_fleet$year,
                      speed = spo_gpslines$speed,
                      fuel = "Diesel"),
               "speed neeeds to has class 'units' in 'km/h'. Please, check package 'units'")
})
