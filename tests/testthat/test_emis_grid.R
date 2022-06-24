
test_that("emis_grid", {
  
  EF_europe <- ef_europe_emep(pollutant = c("CO", "PM10"),
                              speed = tp_model_irl$speed,
                              veh_type = fleet_data_europe$veh_type,
                              tech = "SCR",
                              euro = fleet_data_europe$euro)
  emi_europe <- emis(fleet_composition = fleet_data_europe$fleet_composition,
                     dist = tp_model_irl$dist,
                     ef = EF_europe)
  # Grid
  grid_gps <- sf::st_make_grid(tp_model_irl
                               , cellsize = 0.25 / 102.47
                               , square = FALSE
                               , crs = 4326
  ) 
  
  grid_gps <- sf::st_sf(id = 1:length(grid_gps)
                        , geometry = grid_gps)
  #
  emi_sf <- cbind(tp_model_irl,emi_europe$emi)
  
  # library(mapview)
  # mapview(grid_gps) + mapview(tp_model_irl$geometry)
  
  pol_gps <- emis_grid(data = emi_sf,
                       emi = c("CO_total","PM10_total"),
                       grid = grid_gps,
                       time_class = 'all periods')
  
  pol_gps_hour <- emis_grid(data = emi_sf,
                            emi = c("CO_total","PM10_total"),
                            grid = grid_gps,
                            time_class = 'hour',
                            time_column = 'timestamp')
  
  pol_gps_hour_minute <- emis_grid(data = emi_sf,  
                                   emi = c("CO_total","PM10_total"),
                                   grid = grid_gps,
                                   time_class = 'hour-minute',
                                   time_column = 'timestamp')
  
  # Expect equal -----
  expect_equal(nrow(pol_gps), 113)
  expect_equal(nrow(pol_gps_hour), 113)
  expect_equal(nrow(pol_gps_hour_minute), 702)
  expect_equal(ncol(pol_gps), 4)
  expect_equal(ncol(pol_gps_hour), 5)
  expect_equal(ncol(pol_gps_hour_minute), 5)
  expect_equal(as.numeric(sum(pol_gps$CO_total)), 544.01,0.01)
  expect_equal(as.numeric(sum(pol_gps_hour$CO_total)), 544.01,0.01)
  expect_equal(as.numeric(sum(pol_gps_hour_minute$CO_total)), 544.01,0.01)
  
  
})
