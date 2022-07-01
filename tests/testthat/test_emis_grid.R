
test_that("emis_grid", {
  
  
  my_emis <- emission_model(tp_model_irl
                            ,"ef_europe_emep"
                            ,fleet_data_europe
                            ,pollutant = c("CO","PM10","NOx"))
  grid <- sf::st_make_grid(tp_model_irl
                               , cellsize = 0.25 / 102.47
                               , square = FALSE
                               , crs = 4326
  ) 
  
  pol_gps        <- emis_grid(my_emis, grid)
  pol_gps_hour   <- emis_grid(my_emis, grid,time_resolution = "hour")
  pol_gps_minute <- emis_grid(my_emis, grid,time_resolution = "minute")
  pol_gps_agr     <- emis_grid(my_emis, grid,aggregate = TRUE)
  
  dim(pol_gps        )
  dim(pol_gps_hour   )
  dim(pol_gps_minute )
  dim(pol_gps_agr    )
  # Expect equal -----
  expect_equal(dim(pol_gps), c(37,10))
  expect_equal(dim(pol_gps_hour   ), c(144,11))
  expect_equal(dim(pol_gps_minute ), c(294,11))
  expect_equal(dim(pol_gps_agr    ), c(37,4))
  #expect_equal(as.numeric(sum(pol_gps$CO_total)), 544.01,0.01)
  #expect_equal(as.numeric(sum(pol_gps_hour$CO_total)), 544.01,0.01)
  #expect_equal(as.numeric(sum(pol_gps_hour_minute$CO_total)), 544.01,0.01)
  
  
})
