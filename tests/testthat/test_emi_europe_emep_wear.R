test_that("emi_europe_emep_wear", {
  
  emi_dt <- emi_europe_emep_wear(dist = units::set_units(c(1,1),"km"),
                               speed =  units::set_units(c(30,30),"km/h"),
                               pollutant = c("PM10","TSP","PM2.5"),
                               veh_type = c("Ubus Std 15 - 18 t","Ubus Artic >18 t"),
                               fleet_composition = c(0.5,0.5),
                               load = 0.5,
                               process = c("brake","tyre","road"),
                               as_list = FALSE)
  
  emi_list <- emi_europe_emep_wear(dist = units::set_units(c(1,1),"km"),
                                 speed =  units::set_units(c(30,30),"km/h"),
                                 pollutant = c("PM10","TSP","PM2.5"),
                                 veh_type = c("Ubus Std 15 - 18 t","Ubus Artic >18 t"),
                                 fleet_composition = c(0.5,0.5),
                                 load = 0.5,
                                 process = c("brake","tyre","road"),
                                 as_list = TRUE)
  
  expect_equivalent(dim(emi_dt), c(2,18))
  
  tp_model_slope_true <- slope_class_europe_emep(tp_model,raster_cur,keep = TRUE)
  expect_equivalent(length(emi_list), 7)
  expect_equivalent(length(emi_list$pollutant), 3)
  expect_equivalent(length(emi_list$veh_type), 2)
  expect_equivalent(length(emi_list$process), 3)
})