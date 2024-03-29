test_that("emis_summary", {
  
  EF_europe <- ef_europe_emep(pollutant = c("CO", "PM10"),
                              speed = tp_model_irl$speed,
                              veh_type = fleet_data_europe$veh_type,
                              tech = "SCR",
                              euro = fleet_data_europe$euro)
  emi_europe <- multiply_ef(fleet_composition = fleet_data_europe$fleet_composition,
                     dist = tp_model_irl$dist,
                     ef = EF_europe
                     ,aggregate = FALSE)
  emi_europe$tp_model <- tp_model_irl
  #emi_list <- emi_europe
  
  # Expect equal -----
  summary_time <- emis_summary(emi_list = emi_europe
                               ,by = 'time')
  
  summary_veh <- emis_summary(emi_europe
                              ,by = 'vehicle')
  
  summary_pol <- emis_summary(emi_europe)
  
  # Expect equal-----
  expect_equal(dim(summary_time),c(8,4)) # ncol
  expect_equal(dim(summary_veh),c(6,4))  # ncol
  expect_equal(dim(summary_pol),c(2,3))  # ncol
  #expect_equal(as.numeric(sum(summary_time$emi)),571.9754,0.01) # sum(emi)
  #expect_equal(as.numeric(sum(summary_veh$emi)),571.9754,0.01)  # sum(emi)
  #expect_equal(as.numeric(sum(summary_pol$emi)),571.9754,0.01)  # sum(emi)
  
})

  