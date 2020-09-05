
test_that("fuel_cor", {
  expect_error(result <- ef_euro_scaled(
    dfcol = units::set_units(0.5,g/km),
    speed = units::set_units(30,km/h),
    euro = "Euro V", 
    SDC = units::set_units(34.12,km/h),
    veh = "Urban Buses Standard 15 - 18 t",
    pollutant = "CO"),
    NULL)#"No available emission factor. Please check `data(europe)` for available data.")

  
#  expect_equal(sum(result), 36.14574, 0.001)
})
