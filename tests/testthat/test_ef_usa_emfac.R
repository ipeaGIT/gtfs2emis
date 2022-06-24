
test_that("ef_usa_emfac", {
  
  EF_usa <- ef_usa_emfac(pollutant = c("CO","PM10"),
                         reference_year = "2019",
                         model_year = fleet_data_usa$model_year,
                         speed = tp_model_usa$speed,
                         fuel = "D")
  
  # Expect equal----
  expect_equal(names(EF_usa),c("pollutant","model_year","fuel","EF" ))
  expect_equal(as.numeric(sum(EF_usa$EF,na.rm = TRUE)), 2032.034, 0.01)
  
  # Expect error----
  expect_error(
    
    object =  ef_usa_emfac(pollutant = c("CO","PM10"),
                            reference_year = "2019",
                            model_year = fleet_data_usa$model_year,
                            speed = as.numeric(fleet_data_usa$speed),
                            fuel = "D")
    ,regexp = "speed neeeds to has class 'units' in 'km/h'. Please, check package 'units'"
    )

  })
