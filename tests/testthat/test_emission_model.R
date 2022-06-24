context("emission_model")

# function tester -----------------------------------------------------

default_tester <- function(tp_model = tp_model_bra,
                           ef_model = "ef_usa_moves",
                           fleet_data = fleet_data_usa,
                           pollutant = c("CO","PM10","CO2"),
                           reference_year = 2019,
                           output_path = NULL,
                           parallel = TRUE) {

  results <- gtfs2emis::emission_model(tp_model = tp_model,
                                       ef_model = ef_model,
                                       fleet_data = fleet_data,
                                       pollutant = pollutant,
                                       reference_year = reference_year,
                                        output_path = output_path,
                                       parallel = parallel
                                       )
  return(results)
}



# errors and warnings -----------------------------------------------------


test_that("adequately raises errors", {

  # invalid tp_model
  expect_error(default_tester(tp_model = 'banana'))
  expect_error(default_tester(tp_model = NULL))
  
  # invalid ef_model
  expect_error(default_tester(ef_model = 'banana'))
  expect_error(default_tester(ef_model = NULL))
  
  # invalid fleet_data
  expect_error(default_tester(fleet_data = 'banana'))
  expect_error(default_tester(fleet_data = NULL))

  temp_fleet <- fleet_data_usa
  temp_fleet$veh_type <- NULL
  expect_error(default_tester(ef_model = "ef_usa_moves",
                              fleet_data=temp_fleet))
  
  temp_fleet <- fleet_data_cetesb
  temp_fleet$veh_type <- NULL
  expect_error(default_tester(ef_model = "ef_brazil_cetesb",
                              fleet_data = temp_fleet))
  

  # invalid pollutant
  expect_error(default_tester(pollutant = 'banana'))
  expect_error(default_tester(pollutant = NULL))
  
  
  # invalid parallel
  expect_error(default_tester(parallel = 'banana'))
  expect_error(default_tester(parallel = 123))
  expect_error(default_tester(parallel = NULL))
  
  # invalid reference_year
  expect_error(default_tester(reference_year = 'banana'))
  expect_error(default_tester(reference_year = 123))
  
  # invalid output_path
  expect_error(default_tester(output_path = 'banana'))
  expect_error(default_tester(output_path = 123))
  
  
})


# adequate behavior ------------------------------------------------------


test_that("output is correct", {

  # Europe EMEP
  expect_is( default_tester(tp_model = tp_model_irl, 
                            ef_model = "ef_europe_emep", 
                            fleet_data = fleet_data_europe), 'list')
  
  
  # default_tester(tp_model = tp_model_bra, 
  #                ef_model = "ef_brazil_cetesb", 
  #                fleet_data = fleet_data_ef_cetesb)
  
  # USA MOVES
  expect_is( default_tester(tp_model = tp_model_usa, 
                            ef_model = "ef_usa_moves", 
                            fleet_data = fleet_data_usa), 'list')
  
  # USA EMFAC
  expect_is( default_tester(tp_model = tp_model_usa, 
                            ef_model = "ef_usa_emfac", 
                            fleet_data = fleet_data_usa), 'list')
  
  })

  
