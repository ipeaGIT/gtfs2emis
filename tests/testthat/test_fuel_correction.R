
test_that("fuel_correction", {
  euro_stage <- c("PRE", "PRE", "I", "I", "II", "III", "IV", "V", "VI")
  pollutant <- c("CO","PM10","VOC","NOx")
  
  result <- fuel_correction(pollutant, euro_stage)
  
  expect_equal(sum(result), 36.14574, 0.001)
})
