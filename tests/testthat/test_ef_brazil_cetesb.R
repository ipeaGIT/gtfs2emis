
test_that("ef_brazil_cetesb", {
  
  
  library(data.table)
  library(magrittr)
 
  # fleet
  total_fleet <- data.table::data.table(year = c(2005,2010,2011,2012
                                                 ,2014,2015,2017,2018,2019),
                                        bus = c(1,61,50,1,45,18,62,27,31),
                                        veh_type_euro = "Ubus Std 15 - 18 t",
                                        euro_stage = c("II", "IV", "IV", "V"
                                                       , "V", "V", "V", "V","V"))
  
  total_fleet[,fleet_composition := bus/sum(bus)]
  
  # function
  
  EF_brazil <- ef_brazil_cetesb(pollutant = c("CO","CO2"),
                                veh_type = "BUS_URBAN_D",
                                model_year = total_fleet$year) 
  

  
  # Expect equal tests -------
  expect_equal(length(EF_brazil), 4)
  expect_equal(length(EF_brazil$pollutant), 18)
  expect_equal(length(EF_brazil$veh_type), 18)
  expect_equal(length(EF_brazil$model_year), 18)
  expect_equal(units::deparse_unit(EF_brazil$EF), "g km-1")
  expect_equal(class(EF_brazil$EF), "units")
  
  expect_equal(as.numeric(sum(EF_brazil$EF,na.rm = TRUE)), 12042.3,0.1)
  
  
  # Expect error tests -------
  
  expect_error( ef_brazil_cetesb(pollutant = c("CO","CO2"),
                                      veh_type = "BUS_MICRO_D",
                                      model_year = "2010")
               ,regexp = "'model_year' argument should be a numeric value."
               ,fixed = TRUE)
  expect_error( ef_brazil_cetesb(pollutant = c("CO","CO2"),
                                 veh_type = "BUS_MICRO_Daa",
                                 model_year = "2010")
                ,regexp = paste0("Vehicle type 'BUS_MICRO_Daa' not found in CETESB Emission factor database:\n",
                       "Please check available data in `data(ef_brazil_cetesb_db)`.")
                ,fixed = TRUE)
  expect_error( ef_brazil_cetesb(pollutant = c("CO","CO21"),
                                 veh_type = "BUS_MICRO_D",
                                 model_year = 2010)
                ,regexp = paste0("Pollutant 'CO21' not found in CETESB Emission factor database:\n",
                       "Please check available data in `data(ef_brazil_cetesb_db)`.")
                ,fixed = TRUE)
  expect_error( ef_brazil_cetesb(pollutant = c("CO","CO2"),
                                 veh_type = rep("BUS_MICRO_D",3),
                                 model_year = rep(2010,2))
                ,regexp = paste0("Arguments 'model_year' and 'veh_type' should have the same length.")
                ,fixed = TRUE)
  expect_error( ef_brazil_cetesb(pollutant = c("CO","CO2"),
                                 veh_type = "BUS_MICRO_D",
                                 model_year = 2050)
                ,regexp = paste0("'model_year' argument should be between 1960 - 2020:\n"
                        ,"Please check available data in `data(ef_brazil_cetesb_db)`.")
                ,fixed = TRUE)
  
  })
