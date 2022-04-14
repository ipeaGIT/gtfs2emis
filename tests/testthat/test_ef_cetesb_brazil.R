
test_that("ef_cetesb_brazil", {
  
  
  # GTFS2gps filter-----
  library(data.table)
  fort <- gtfs2gps::read_gtfs(system.file("extdata/fortaleza.zip"
                                          , package = "gtfs2gps"))  %>%
    gtfs2gps::filter_single_trip() %>% 
    gtfs2gps::filter_by_shape_id(c("shape804-I", "shape806-I"))
  
  fort_gps <- gtfs2gps::gtfs2gps(fort, parallel = TRUE)
  
  fort_gpslines <- gtfs2gps::gps_as_sflinestring(fort_gps)
  
  fort_gpslines$dist <- units::set_units(fort_gpslines$dist, "km")
  
  # fleet
  total_fleet <- data.table::data.table(year = c(2005,2010,2011,2012
                                                 ,2014,2015,2017,2018,2019),
                                        bus = c(1,61,50,1,45,18,62,27,31),
                                        veh_type_euro = "Ubus Std 15 - 18 t",
                                        euro_stage = c("II", "IV", "IV", "V"
                                                       , "V", "V", "V", "V","V"))
  
  total_fleet[,fleet_composition := bus/sum(bus)]
  
  # function
  
  EF_brazil <- ef_cetesb_brazil(pollutant = c("CO","CO2"),
                         veh_type = "BUS_URBAN_D",
                         model_year = total_fleet$year) 
  
  EF_brazil1 <- ef_cetesb_brazil(pollutant = c("CO","CO2"),
            veh_type = c("BUS_MICRO_D","BUS_URBAN_D"),
            model_year = c(2005))
  
  # Expect equal tests -------
  expect_equal(length(EF_brazil), 4)
  expect_equal(length(EF_brazil$pollutant), 18)
  expect_equal(length(EF_brazil$veh_type), 18)
  expect_equal(length(EF_brazil$years), 18)
  expect_equal(units::deparse_unit(EF_brazil$EF), "g km-1")
  expect_equal(class(EF_brazil$EF), "units")
  
  expect_equal(as.numeric(sum(EF_brazil$EF,na.rm = TRUE)), 12042.3,0.1)
  expect_equal(length(EF_brazil1$veh_type), 4)
  # Expect error tests -------
  
  expect_error(EF_brazil <- ef_brazil(pollutant = c("CO","CO2","P1"),
                                      veh_type = "BUS_MICRO_D",
                                      model_year = total_fleet$year),NULL)
  
  
  
  })
