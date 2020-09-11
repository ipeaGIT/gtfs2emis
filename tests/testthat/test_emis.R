
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

  EF_usa <- ef_usa(pollutant = c("CO","PM10"),
                              calendar_year = "2019",
                              model_year = total_fleet$year,
                              speed = spo_gpslines$speed,
                              fuel = "Diesel")

  emi_usa <- emis(fleet_composition = total_fleet$fleet_composition,
                             dist = spo_gpslines$dist,
                             ef = EF_usa, 
                             prefix = "USA")

  
  expect_equal(sum(emi_usa$USA_CO_total), 1012.183, 0.05)
  
  spo_gpslines$dist <- units::set_units(spo_gpslines$dist, "m")

  expect_error(emis(fleet_composition = total_fleet$fleet_composition,
                    dist = spo_gpslines$dist,
                    ef = EF_usa, 
                    prefix = "USA"), "dist need to has 'units' in 'km'.")

  spo_gpslines$dist <- as.numeric(spo_gpslines$dist)
  
  expect_error(emis(fleet_composition = total_fleet$fleet_composition,
                  dist = spo_gpslines$dist,
                  ef = EF_usa, 
                  prefix = "USA"), "dist neeeds to has class 'units' in 'km'. Please, check package 'units'")
})
