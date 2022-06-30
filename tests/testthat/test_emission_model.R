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
  
  # tp_model 
  expect_is( default_tester(tp_model = tp_model_irl, 
                            ef_model = "ef_europe_emep", 
                            fleet_data = fleet_data_europe), 'list')
  

  # tp_model detailed---------------
  
  # tp_model
  irl_shapeids <- c("60-42-b12-1.250.I", "60-42-d12-1.249.O", "60-43-d12-1.252.I")
  gtfs_irl2 <- gtfstools::filter_by_shape_id(gtfs_irl_raw, 
                                             shape_id = irl_shapeids)
  
  tp_model_irl2 <- transport_model(gtfs = gtfs_irl2, parallel = TRUE)
  
  # fleet
  fleet_data_europe2 <- fleet_data_europe 
  fleet_data_europe2$shape_id <- irl_shapeids
  
  # loop
  tmp_emis_detailed <- lapply(irl_shapeids,function(i){ # i = irl_shapeids[2]
    emi_list <- default_tester(
      tp_model = tp_model_irl2[tp_model_irl2$shape_id == i,]
      ,ef_model = "ef_europe_emep"
      ,fleet_data = fleet_data_europe2[fleet_data_europe2$shape_id == i,]
    )
  })
  
  # checks
  expect_equal(length(tmp_emis_detailed),3)
  for(i in 1:3) expect_is(tmp_emis_detailed[[i]] , 'list') 
  for(i in 1:3) expect_equal(length(tmp_emis_detailed[[i]]),11)

})


