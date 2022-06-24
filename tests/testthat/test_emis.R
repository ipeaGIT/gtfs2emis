
test_that("emis", {
  
  # EF -----------
  ef_emfac <- ef_usa_emfac(pollutant = c("CO","PM10"),
                           reference_year = 2019,
                           model_year = fleet_data_cetesb$model_year,
                           speed = tp_model_usa$speed,
                           fuel = "D")
  emi_emfac <- emis(fleet_composition = fleet_data_cetesb$fleet_composition,
                    dist = tp_model_usa$dist,
                    ef = ef_emfac)
  
  # expect_equal -----
  expect_equal(length(emi_emfac),5)
  expect_equal(
    as.numeric(sum(emi_emfac$emi$CO_total,na.rm = TRUE))
    , 86.26028, 0.05)
  expect_equal(
    as.numeric(sum(emi_emfac$emi$PM10_total,na.rm = TRUE))
    , 2.533859, 0.005)
  
  
  # expect_error -----
  expect_error(emis(fleet_composition = total_fleet$fleet_composition,
                    dist = units::set_units(tp_model_usa$dist, "m"),
                    ef = ef_emfac), "dist need to has 'units' in 'km'.")
  
  
  expect_error(emis(fleet_composition = total_fleet$fleet_composition,
                    dist = as.numeric(tp_model_usa$dist),
                    ef = ef_emfac)
               , "dist neeeds to has class 'units' in 'km'. Please, check package 'units'")
})
