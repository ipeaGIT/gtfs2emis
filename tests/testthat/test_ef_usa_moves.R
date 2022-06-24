
test_that("ef_usa_moves", {
  
  EF_usa <- ef_usa_moves(pollutant = c("CO","PM10"),
                         reference_year = "2019",
                         model_year = fleet_data_usa$model_year,
                         speed = tp_model_usa$speed,
                         fuel = "D")
  
  # Expect equal----
  expect_equal(names(EF_usa),c("pollutant" ,"model_year","fuel","EF"))
  expect_equal(as.numeric(sum(EF_usa$EF,na.rm = TRUE)), 510909.7, 0.01)
  
  # Expect error----
  expect_error(
    object = ef_usa_moves(pollutant = c("CO","PM10"),
                          model_year = fleet_data_usa$year,
                          speed = as.numeric(tp_model_usa$speed),
                          fuel = "D")
    , regexp = "speed neeeds to has class 'units' in 'km/h'. Please, check package 'units'"
  )
  
  expect_error(
    object = ef_usa_moves(pollutant = c("CO","PM10"),
                          reference_year = 2020,
                          model_year = fleet_data_usa$year,
                          speed = as.numeric(tp_model_usa$speed),
                          fuel = "D")
    , regexp = "speed neeeds to has class 'units' in 'km/h'. Please, check package 'units'"
  )
})
