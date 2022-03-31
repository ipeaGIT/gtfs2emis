
test_that("emis", {
  
  # GTFS2gps filter-----
  library(data.table)
  fort <- gtfs2gps::read_gtfs(system.file("extdata/fortaleza.zip"
                                          , package = "gtfs2gps"))  %>%
    gtfs2gps::filter_single_trip() %>% 
    gtfs2gps::filter_by_shape_id(c("shape804-I", "shape806-I"))
  
  fort_gps <- gtfs2gps::gtfs2gps(fort, parallel = TRUE)
  
  fort_gpslines <- gtfs2gps::gps_as_sflinestring(fort_gps)
  
  fort_gpslines$dist <- units::set_units(fort_gpslines$dist, "km")
  
  # fleet-----------
  total_fleet <- data.table::data.table(year = c(2005,2010,2011,2012
                                                 ,2014,2015,2017,2018,2019),
                                        bus = c(1,61,50,1,45,18,62,27,31),
                                        veh_type_euro = "Ubus Std 15 - 18 t",
                                        euro_stage = c("II", "IV", "IV", "V"
                                                       , "V", "V", "V", "V","V"))
  
  total_fleet[,fleet_composition := bus/sum(bus)]
  
  # EF -----------
  ef_emfac <- ef_usa_emfac(pollutant = c("CO","PM10"),
                           calendar_year = "2019",
                           model_year = total_fleet$year,
                           speed = fort_gpslines$speed,
                           fuel = "D")
  ef_moves <- ef_usa_moves(pollutant = c("CO","PM10"),
                           calendar_year = "2019",
                           model_year = total_fleet$year,
                           speed = fort_gpslines$speed,
                           fuel = "D")
  # emis ----
  emi_emfac <- emis(fleet_composition = total_fleet$fleet_composition,
                    dist = fort_gpslines$dist,
                    ef = ef_emfac, 
                    prefix = "EMFAC")
  emi_moves <- emis(fleet_composition = total_fleet$fleet_composition,
                    dist = fort_gpslines$dist,
                    ef = ef_moves, 
                    prefix = "MOVES")
  
  # expect_equal -----
  expect_equal(
    as.numeric(sum(emi_emfac$emi$EMFAC_CO_total,na.rm = TRUE))
    , 2.061608, 0.05)
  expect_equal(
    as.numeric(sum(emi_moves$emi$MOVES_PM10_total,na.rm = TRUE))
    , 0.2622532, 0.005)
  
  
  # expect_error -----
  expect_error(emis(fleet_composition = total_fleet$fleet_composition,
                    dist = units::set_units(fort_gpslines$dist, "m"),
                    ef = ef_moves, 
                    prefix = "MOVES"), "dist need to has 'units' in 'km'.")
  
  
  expect_error(emis(fleet_composition = total_fleet$fleet_composition,
                    dist = as.numeric(fort_gpslines$dist),
                    ef = ef_moves, 
                    prefix = "EMFAC"), "dist neeeds to has class 'units' in 'km'. Please, check package 'units'")
})
