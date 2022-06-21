
test_that("emis_summary", {

  library(data.table)
  library(magrittr)
  library(gtfstools)
  gtfs_file <- system.file("extdata/irl_dub/irl_dub_gtfs.zip", package = "gtfs2emis")
  gtfs <- gtfstools::read_gtfs(gtfs_file) %>% 
    gtfstools::filter_by_trip_id(., trip_id = c('619.3.60-40-d12-1.224.O'))
  
  # Transport model
  tranp_model <- transport_model(gtfs_data = gtfs,
                              spatial_resolution = 100,
                              parallel = TRUE)
  
  # fleet -----------
  total_fleet <- data.table::data.table(year = c(2005,2010,2011,2012
                                                 ,2014,2015,2017,2018,2019),
                                        bus = c(1,61,50,1,45,18,62,27,31),
                                        veh_type = "Ubus Std 15 - 18 t",
                                        euro = c("II", "IV", "IV", "V"
                                                       , "V", "V", "V", "V","V")
                                        , tech = c("-",rep("SCR",8))
                                        ,fuel = "D")
  
  total_fleet[,fleet_composition := bus/sum(bus)]
  
  emi_list <- emission_model(tp_model = tranp_model
                             , ef_model = "ef_europe_emep"
                             , fleet_data = total_fleet
                             , pollutant = c("CO2","PM10"))
  # Expect equal -----
  summary_time <- emis_summary(emi_list
                               ,by = 'time'
                               ,segment_vars = "tp_model"
                               ,time_column = "timestamp")
  summary_veh <- emis_summary(emi_list
                              ,by = 'veh_type')
  summary_pol <- emis_summary(emi_list)
  
  # Expect equal-----
  expect_equal(ncol(summary_time),3) # ncol
  expect_equal(ncol(summary_veh),3) # ncol
  expect_equal(ncol(summary_pol),2) # ncol
  expect_equal(nrow(summary_time),4) # nrow
  expect_equal(nrow(summary_veh),2) # nrow
  expect_equal(nrow(summary_pol),2) # nrow
  expect_equal(as.numeric(sum(summary_time$emi)),30883.18,0.01) # sum(emi)
  expect_equal(as.numeric(sum(summary_veh$emi)),30883.18,0.01) # sum(emi)
  expect_equal(as.numeric(sum(summary_pol$emi)),30883.18,0.01) # sum(emi)
  
})

  