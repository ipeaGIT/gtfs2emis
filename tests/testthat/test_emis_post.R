
test_that("emis_post", {
  spo <- gtfs2gps::read_gtfs(system.file("extdata/saopaulo.zip", package = "gtfs2gps"))  %>%
    gtfs2gps::filter_by_shape_id(53000:53010)
  
  spo_gps <- gtfs2gps::gtfs2gps(spo, parallel = TRUE)
  
  spo_gpslines <- gtfs2gps::gps_as_sflinestring(spo_gps) %>% dplyr::select(trip_id,speed, dist, departure_time)
  
  spo_gpslines$dist <- units::set_units(spo_gpslines$dist, "km")
  
  total_fleet <- data.table::data.table(year = c(2005,2010,2011,2012,2014,2015,2017,2018,2019),
                                        bus = c(1,61,50,1,45,18,62,27,31),
                                        veh_type_euro = "Ubus Std 15 - 18 t",
                                        euro_stage = c("II", "IV", "IV", "V", "V", "V", "V", "V","V"))
  
  total_fleet[,fleet_composition := bus/sum(bus)]
  
  det_fleet <- data.table::data.table(shape_id = unique(spo$shapes$shape_id),
                                      bus_age = c("2010", "2011", "2012", "2013"),
                                      bus_fuel = "D")
  
  EF_europe <- ef_europe(pollutant = c("CO", "PM"),
                                    speed = spo_gpslines$speed,
                                    veh_type = total_fleet$veh_type_euro,
                                    tech = "SCR",
                                    euro = total_fleet$euro_stage)
  
  EF_usa <- ef_usa(pollutant = c("CO","PM10"),
                              calendar_year = "2019",
                              model_year = total_fleet$year,
                              speed = spo_gpslines$speed,
                              fuel = "D")

  EF_brazil <- ef_brazil(pollutant = c("CO","CO2"),
                         veh_type = "BUS_URBAN_D",
                         years = total_fleet$year) # fleet_composition
  
  emi_usa <- emis(fleet_composition = total_fleet$fleet_composition,
                             dist = spo_gpslines$dist,
                             ef = EF_usa, 
                             prefix = "USA")
  
  emi_europe <- emis(fleet_composition = total_fleet$fleet_composition,
                                dist = spo_gpslines$dist,
                                ef = EF_europe,
                                prefix = "EU")
  
  emi_brazil <- emis(fleet_composition = total_fleet$fleet_composition,
                                dist = spo_gpslines$dist,
                                ef = EF_brazil,
                                prefix = "BR")
  
  spo_emis <- cbind(spo_gpslines, emi_usa)
  spo_emis <- cbind(spo_emis, emi_europe)
  spo_emis <- cbind(spo_emis, emi_brazil)
  
  expect_error(emis_post(data = spo_emis, time_column = "abc"), "'abc' is missing in the 'data'")
  
  hour_emissions <- emis_post(data = spo_emis,
                                         emi = c('BR_CO_total','EU_CO_total'),
                                         time_class = "hour",
                                         time_column = 'departure_time')

  
  expect_equal(sum(hour_emissions), 9101.791, 0.1)
})
