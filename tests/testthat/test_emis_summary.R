
test_that("emis_summary", {
  # GTFS2gps filter-----
  library(data.table)
  fort <- gtfs2gps::read_gtfs(system.file("extdata/fortaleza.zip"
                                          , package = "gtfs2gps"))  %>%
    gtfs2gps::filter_single_trip() %>% 
    gtfs2gps::filter_by_shape_id(c("shape804-I", "shape806-I"))
  
  fort_gps <- gtfs2gps::gtfs2gps(fort, parallel = TRUE)
  
  fort_gpslines <- gtfs2gps::gps_as_sflinestring(fort_gps)
  
  fort_gpslines$dist <- units::set_units(fort_gpslines$dist, "km")
  
  # fleet -----------
  total_fleet <- data.table::data.table(year = c(2005,2010,2011,2012
                                                 ,2014,2015,2017,2018,2019),
                                        bus = c(1,61,50,1,45,18,62,27,31),
                                        veh_type_euro = "Ubus Std 15 - 18 t",
                                        euro_stage = c("II", "IV", "IV", "V"
                                                       , "V", "V", "V", "V","V"))
  
  total_fleet[,fleet_composition := bus/sum(bus)]
  
  
  # Ef ----
  set.seed(123)
  EF_europe <- ef_europe(pollutant = c("CO", "PM10"),
                         speed = fort_gpslines$speed,
                         veh_type = total_fleet$veh_type_euro,
                         tech = "SCR",
                         euro = total_fleet$euro_stage,
                         fcorr = rnorm(9, 0.5, 0.1))
  # EUROPE
  emi_europe <- emis(fleet_composition = total_fleet$fleet_composition,
                     dist = fort_gpslines$dist,
                     ef = EF_europe,
                     prefix = "EU"
                     ,aggregate = FALSE)
  
  # Expect equal -----
  summary_time <- emis_summary(emi = emi_europe
                              ,emi_vars = 'emi'
                              ,veh_vars = 'veh_type'
                              ,pol_vars = 'pollutant'
                              ,by = 'time'
                              ,time_column = fort_gpslines$timestamp)
  summary_veh <- emis_summary(emi = emi_europe
                              ,emi_vars = 'emi'
                              ,veh_vars = 'veh_type'
                              ,pol_vars = 'pollutant'
                              ,by = 'veh_type')
  summary_pol <- emis_summary(emi = emi_europe
                              ,emi_vars = 'emi'
                              ,veh_vars = 'veh_type'
                              ,pol_vars = 'pollutant'
                              ,by = 'pollutant')
  
  # Expect equal-----
  expect_equal(ncol(summary_time),3) # ncol
  expect_equal(ncol(summary_veh),4) # ncol
  expect_equal(ncol(summary_pol),3) # ncol
  expect_equal(nrow(summary_time),2) # nrow
  expect_equal(nrow(summary_veh),62) # nrow
  expect_equal(nrow(summary_pol),62) # nrow
  expect_equal(sum(summary_time),11.39935,0.01) # sum(emi)
  expect_equal(as.numeric(sum(summary_veh$emi,na.rm = TRUE)),19.78512,0.01) # sum(emi)
  expect_equal(as.numeric(sum(summary_pol$emi,na.rm = TRUE)),19.78512,0.01) # sum(emi)
  
})
