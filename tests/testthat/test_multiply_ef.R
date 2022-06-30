
test_that("emis", {
  
  # EF -----------
  ef_emfac <- ef_usa_emfac(pollutant = c("CO","PM10"),
                           reference_year = 2019,
                           model_year = fleet_data_cetesb$model_year,
                           speed = tp_model_irl$speed,
                           fuel = "D")
  emi_emfac <- multiply_ef(fleet_composition = fleet_data_cetesb$fleet_composition,
                    dist = tp_model_irl$dist,
                    ef = ef_emfac)
  
  # # expect_equal -----
  # expect_equal(length(emi_emfac),5)
  # expect_equal(
  #   as.numeric(sum(emi_emfac$emi$CO_total,na.rm = TRUE))
  #   , 30.86623, 0.05)
  # expect_equal(
  #   as.numeric(sum(emi_emfac$emi$PM10_total,na.rm = TRUE))
  #   , 0.8944941, 0.005)
  # 
  
  # expect_error -----
  # dist wrong units
  expect_error(
    multiply_ef(fleet_composition = total_fleet$fleet_composition,
         dist = units::set_units(tp_model_irl$dist, "m"),
         ef = ef_emfac)
  )
  # dist as.numeric
  expect_error(
    multiply_ef(fleet_composition = total_fleet$fleet_composition,
         dist = as.numeric(tp_model_irl$dist),
         ef = ef_emfac)
  )
  # EF as.numeric
  expect_error(
    multiply_ef(fleet_composition = total_fleet$fleet_composition,
         dist = tp_model_irl$dist,
         ef = as.numeric(ef_emfac$EF$CO_2010_D))
  )
  # EF wrong units
  expect_error(
    multiply_ef(
      fleet_composition = total_fleet$fleet_composition,
      dist = tp_model_irl$dist,
      ef = units::set_units(as.numeric(ef_emfac$EF$CO_2010_D),"km/h")
    )
  )
})
