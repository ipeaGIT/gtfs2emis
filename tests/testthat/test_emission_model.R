context("emission_model")

# function tester -----------------------------------------------------

default_tester <- function(tp_model = tp_model_bra,
                           ef_model = "ef_usa_moves",
                           fleet_data = fleet_data_ef_moves,
                           pollutant = c("CO","PM10","CO2"),
                           reference_year = 2019,
                           parallel = TRUE,
                           output_path = NULL) {

  results <- gtfs2emis::emission_model(tp_model = tp_model,
                                       ef_model = ef_model,
                                       fleet_data = fleet_data,
                                       pollutant = pollutant,
                                       reference_year = reference_year,
                                       parallel = parallel,
                                       output_path = output_path
                                       )
  return(results)
}



# errors and warnings -----------------------------------------------------


test_that("adequately raises errors", {

expect_error()
}


# adequate behavior ------------------------------------------------------


test_that("output is correct", {

  # Europe EMEP
  expect_is( default_tester(tp_model = tp_model_irl, 
                            ef_model = "ef_europe_emep", 
                            fleet_data = fleet_data_ef_europe), 'list')
  
  
  # default_tester(tp_model = tp_model_bra, 
  #                ef_model = "ef_brazil_cetesb", 
  #                fleet_data = fleet_data_ef_cetesb)
  
  # USA MOVES
  expect_is( default_tester(tp_model = tp_model_usa, 
                            ef_model = "ef_usa_moves", 
                            fleet_data = fleet_data_ef_moves), 'list')
  
  # USA EMFAC
  expect_is( default_tester(tp_model = tp_model_usa, 
                            ef_model = "ef_usa_emfac", 
                            fleet_data = fleet_data_ef_emfac), 'list')
  
  })

  
