
test_that("ef_scaled_euro", {
  result <- ef_scaled_euro(
    ef_local = units::set_units(0.5,g/km),
    speed = units::set_units(30,km/h),
    euro = "V", 
    SDC = units::set_units(19,km/h),
    veh = "Ubus Std 15 - 18 t",
    pollutant = "CO")
  
  expect_equal(names(result)
               ,c("pollutant", "veh_type", "euro", "fuel", "tech", "slope", "load", "EF"))
  expect_equal(as.numeric(result$EF),0.3370477,tolerance = 0.001)

  
#  expect_equal(sum(result), 36.14574, 0.001)
})
