
test_that("ef_brazil_cetesb", {
  
  
  ef_brazil <- ef_brazil_cetesb(
    pollutant = c("PM10","CO2","NOx","N2O")
    ,veh_type = c("BUS_URBAN_D","BUS_MICRO_D","BUS_ARTIC_D")
    ,model_year = c(2015,2015,2015)
  )
  
  # Expect equal tests -------
  expect_equal(length(ef_brazil), 4)
  expect_equal(length(ef_brazil$pollutant), 12)
  expect_equal(length(ef_brazil$veh_type), 12)
  expect_equal(length(ef_brazil$model_year), 12)
  expect_equal(units::deparse_unit(ef_brazil$EF), "g km-1")
  expect_true(is(ef_brazil$EF, "units"))
  #expect_equal(as.numeric(sum(ef_brazil$EF,na.rm = TRUE)), 3837.902,0.1)
  
  
  # Expect error tests -------
  
  expect_error( 
    ef_brazil_cetesb(pollutant = c("CO","CO2"),
                     veh_type = "BUS_MICRO_D",
                     model_year = "2010")
 )
  
  expect_error( 
    ef_brazil_cetesb(pollutant = c("CO","CO2"),
                     veh_type = "BUS_MICRO_Daa",
                     model_year = "2010")
)
  
  expect_error( 
    ef_brazil_cetesb(pollutant = c("CO","CO21"),
                                 veh_type = "BUS_MICRO_D",
                                 model_year = 2010)
 )
  
  expect_error( 
    ef_brazil_cetesb(pollutant = c("CO","CO2"),
                                 veh_type = rep("BUS_MICRO_D",3),
                                 model_year = rep(2010,2))
)
  
  
  expect_error( 
    ef_brazil_cetesb(pollutant = c("CO","CO2"),
                                 veh_type = "BUS_MICRO_D",
                                 model_year = 2050)
 )
  
})
