
test_that("emi_to_dt", {
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
  
  EF_usa_emfac <- ef_usa_emfac(pollutant = c("CO", "PM10"),
                               calendar_year = "2019",
                               model_year = total_fleet$year,
                               speed = fort_gpslines$speed,
                               fuel = "D")
  
  EF_usa_moves <- ef_usa_moves(pollutant = c("CO", "PM10"),
                               calendar_year = "2019",
                               model_year = total_fleet$year,
                               speed = fort_gpslines$speed,
                               fuel = "D")
  
  EF_brazil <- ef_brazil(pollutant = c("CO", "CO2"),
                         veh_type = "BUS_URBAN_D",
                         model_year = total_fleet$year) # fleet_composition
  
  #emi ----
  
  # USA
  emi_usa_emfac <- emis(fleet_composition = total_fleet$fleet_composition,
                        dist = fort_gpslines$dist,
                        ef = EF_usa_emfac, 
                        prefix = "Emfac"
                        ,aggregate = FALSE)
  emi_usa_moves <- emis(fleet_composition = total_fleet$fleet_composition,
                        dist = fort_gpslines$dist,
                        ef = EF_usa_moves, 
                        prefix = "Moves"
                        ,aggregate = FALSE)
  
  # EUROPE
  emi_europe <- emis(fleet_composition = total_fleet$fleet_composition,
                     dist = fort_gpslines$dist,
                     ef = EF_europe,
                     prefix = "EU"
                     ,aggregate = FALSE)
  
  # BRAZIL (not speed dependent emission factor)
  emi_brazil <- emis(fleet_composition = total_fleet$fleet_composition,
                     dist = fort_gpslines$dist,
                     ef = EF_brazil,
                     prefix = "BR"
                     ,aggregate = FALSE)
  
  # Expect equal -----
  emi_dt_europe <- emi_to_dt(emi_list = emi_europe
                            ,emi_vars = 'emi'
                            ,veh_vars = c("veh_type","euro","fuel","tech")
                            ,pol_vars = 'pollutant'
                            ,segment_vars = c("slope","load")) 
  emi_dt_brazil <- emi_to_dt(emi_list = emi_brazil
                            ,emi_vars = 'emi'
                            ,veh_vars = c("years","veh_type")
                            ,pol_vars = 'pollutant') 
  emi_dt_usa_emfac <- emi_to_dt(emi_list = emi_usa_emfac
                            ,emi_vars = 'emi'
                            ,veh_vars = c("model_year","fuel")
                            ,pol_vars = 'pollutant') 
  emi_dt_usa_moves <- emi_to_dt(emi_list = emi_usa_moves
                            ,emi_vars = 'emi'
                            ,veh_vars = c("model_year","fuel")
                            ,pol_vars = 'pollutant') 

# Expect equal-----
expect_equal(ncol(emi_dt_europe),9) # ncol
expect_equal(ncol(emi_dt_brazil),5) # ncol
expect_equal(ncol(emi_dt_usa_emfac),5) # ncol
expect_equal(ncol(emi_dt_usa_moves),5) # ncol
expect_equal(nrow(emi_dt_europe),558) # nrow
expect_equal(nrow(emi_dt_brazil),558) # nrow
expect_equal(nrow(emi_dt_usa_emfac),558) # nrow
expect_equal(nrow(emi_dt_usa_moves),558) # nrow
expect_equal(as.numeric(sum(emi_dt_europe$emi,na.rm = TRUE)),19.78512,0.01) # sum(emi)
expect_equal(as.numeric(sum(emi_dt_brazil$emi,na.rm = TRUE)),19598.96,0.01) # sum(emi)
expect_equal(as.numeric(sum(emi_dt_usa_emfac$emi,na.rm = TRUE)),2.116568,0.01) # sum(emi)
expect_equal(as.numeric(sum(emi_dt_usa_moves$emi,na.rm = TRUE)),26.5729,0.01) # sum(emi)

})
