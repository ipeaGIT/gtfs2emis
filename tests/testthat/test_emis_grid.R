
test_that("emis_grid", {
  
  # GTFS2gps filter-----
  library(data.table)
  library(gtfs2gps)
  library(gtfstools)
  library(magrittr)
  
  gtfs_file <- system.file("extdata/fortaleza.zip", package = "gtfs2gps")
  
  fort <- gtfstools::read_gtfs(gtfs_file)  %>%
    gtfs2gps::filter_single_trip() %>% 
    gtfstools::filter_by_shape_id(c("shape804-I", "shape806-I"))
  
  fort_gps <- gtfs2gps::gtfs2gps(fort, parallel = TRUE) %>% 
    gtfs2gps::adjust_speed()
  
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
  set.seed(1234)
  EF_europe <- ef_europe_emep(pollutant = c("CO", "PM10"),
                         speed = fort_gpslines$speed,
                         veh_type = total_fleet$veh_type_euro,
                         tech = "SCR",
                         euro = total_fleet$euro_stage,
                         fcorr = rnorm(9, 0.5, 0.1))
  
  EF_usa_emfac <- ef_usa_emfac(pollutant = c("CO", "PM10"),
                               reference_year = "2019",
                               model_year = total_fleet$year,
                               speed = fort_gpslines$speed,
                               fuel = "D")
  
  EF_usa_moves <- ef_usa_moves(pollutant = c("CO", "PM10"),
                               reference_year = "2019",
                               model_year = total_fleet$year,
                               speed = fort_gpslines$speed,
                               fuel = "D")
  
  EF_brazil <- ef_brazil_cetesb(pollutant = c("CO", "CO2"),
                         veh_type = "BUS_URBAN_D",
                         model_year = total_fleet$year) # fleet_composition
  
  #emi ----
  
  # USA
  emi_usa_emfac <- emis(fleet_composition = total_fleet$fleet_composition,
                        dist = fort_gpslines$dist,
                        ef = EF_usa_emfac, 
                        prefix = "Emfac")
  emi_usa_moves <- emis(fleet_composition = total_fleet$fleet_composition,
                        dist = fort_gpslines$dist,
                        ef = EF_usa_moves, 
                        prefix = "Moves")
  
  # EUROPE
  emi_europe <- emis(fleet_composition = total_fleet$fleet_composition,
                     dist = fort_gpslines$dist,
                     ef = EF_europe,
                     prefix = "EU")
  
  # BRAZIL (not speed dependent emission factor)
  emi_brazil <- emis(fleet_composition = total_fleet$fleet_composition,
                     dist = fort_gpslines$dist,
                     ef = EF_brazil,
                     prefix = "BR")
  
  # CBIND 
  for_emis <- cbind(fort_gpslines, emi_usa_emfac$emi)
  for_emis <- cbind(for_emis, emi_usa_moves$emi)
  for_emis <- cbind(for_emis, emi_europe$emi)
  for_emis <- cbind(for_emis, emi_brazil$emi)
  
  
  # Grid
  grid_gps <- sf::st_make_grid(for_emis
                               , cellsize = 0.25 / 102.47
                               , square = FALSE
                               ) 
  grid_gps <- sf::st_sf(id = 1:length(grid_gps)
                        , geometry = grid_gps)
  for_sf <- sf::st_as_sf(for_emis)
  
  pol_gps <- emis_grid(data = for_sf,
                       emi = c("Emfac_CO_total","Moves_CO_total","EU_CO_total", "BR_CO_total"),
                       grid = grid_gps,
                       time_class = 'all periods')
  
  pol_gps_hour <- emis_grid(data = for_sf,
                            emi = c("Emfac_CO_total","Moves_CO_total","EU_CO_total", "BR_CO_total"),
                            grid = grid_gps,
                            time_class = 'hour',
                            time_column = 'timestamp')
  
  pol_gps_hour_minute <- emis_grid(data = for_sf,
                                   emi = c("Emfac_CO_total","Moves_CO_total","EU_CO_total","BR_CO_total"),
                                   grid = grid_gps,
                                   time_class = 'hour-minute',
                                   time_column = 'timestamp')
  # Expect equal -----
  expect_equal(nrow(pol_gps), 58)
  expect_equal(nrow(pol_gps_hour), 60)
  expect_equal(nrow(pol_gps_hour_minute), 96)
  expect_equal(ncol(pol_gps), 6)
  expect_equal(ncol(pol_gps_hour), 7)
  expect_equal(ncol(pol_gps_hour_minute), 7)
  expect_equal(as.numeric(sum(pol_gps$Moves_CO_total)), 436.9472,0.01)
  expect_equal(as.numeric(sum(pol_gps_hour$Moves_CO_total)), 436.9472,0.01)
  expect_equal(as.numeric(sum(pol_gps_hour_minute$Moves_CO_total)), 436.9472,0.01)
  
  
})
