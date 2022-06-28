test_that("ef_europe_emep", {
  # GTFS2gps filter
  
  # gtfs2emis
  EF_europe <- ef_europe_emep(pollutant = c("CO", "PM10"),
                              speed = tp_model_irl$speed,
                              veh_type = fleet_data_europe$veh_type,
                              tech = "SCR",
                              euro = fleet_data_europe$euro)
  
  # Expect equal -----
  expect_equal(length(EF_europe), 8)
  expect_equal(length(EF_europe$pollutant), 6)
  expect_equal(length(EF_europe$veh_type), 6)
  expect_equal(length(EF_europe$euro), 6)
  expect_equal(units::deparse_unit(EF_europe$EF$CO_Euro_II), "g km-1")
  expect_equal(sum(EF_europe$EF,na.rm = TRUE), 1188.842,0.1)
  
  
  # Expect error
  expect_error(
    ef_europe_emep(pollutant = c("CO", "PM"),
                   speed = tp_model_irl$speed,
                   veh_type = fleet_data_europe$veh_type,
                   tech = "SCR",
                   euro = fleet_data_europe$euro)
  )
  expect_error(
    ef_europe_emep(pollutant = c("CO", "PM"),
                   speed = 1:100,
                   veh_type = fleet_data_europe$veh_type,
                   tech = fleet_data_europe$tech,
                   euro = fleet_data_europe$euro)
  )
  
})
