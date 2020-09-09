
test_that("emis", {
  spo <- gtfs2gps::read_gtfs(system.file("extdata/saopaulo.zip", package = "gtfs2gps"))  %>%
    gtfs2gps::filter_by_shape_id(53000:53020)
  
  spo_gps <- gtfs2gps::gtfs2gps(spo, parallel = TRUE)

  spo_gpslines <- gtfs2gps::gps_as_sflinestring(spo_gps) %>% 
    dplyr::select(trip_id,speed, dist, departure_time)
  
  spo_gpslines$speed <- units::set_units(spo_gpslines$speed, "km/h")
  spo_gpslines$dist <- units::set_units(spo_gpslines$dist, "m") %>% 
    units::set_units("km")
  
  total_fleet <- data.table::data.table(year = c(2005,2010,2011,2012,2014,2015,2017,2018,2019),
                                        bus = c(1,61,50,1,45,18,62,27,31),
                                        veh_type_euro = "Urban Buses Standard 15 - 18 t",
                                        euro_stage = c("II", "IV", "IV", "V", "V", "V", "V", "V","V"))
  
  total_fleet[,fleet_composition := bus/sum(bus)]

  det_fleet <- data.table::data.table(shape_id = unique(spo$shapes$shape_id),
                                      bus_age = c("2010", "2011", "2012", "2013"),
                                      bus_fuel = "Diesel")
  
  EF_europe <- ef_europe(pollutant = c("CO", "PM"),
                                    speed = spo_gpslines$speed,
                                    veh_type = total_fleet$veh_type_euro,
                                    tech = "SCR",
                                    show.equation = TRUE,
                                    euro = total_fleet$euro_stage)

  expect_equal(sum(EF_europe), 591312.8, 0.1)
  
  spo_gpslines$speed <- as.numeric(spo_gpslines$speed)
  
  expect_error(ef_europe(pollutant = c("CO", "PM"),
                         speed = spo_gpslines$speed,
                         veh_type = total_fleet$veh_type_euro,
                         tech = "SCR",
                         show.equation = TRUE,
                         euro = total_fleet$euro_stage),
               "speed neeeds to has class 'units' in 'km/h'. Please, check package 'units'.")
})
