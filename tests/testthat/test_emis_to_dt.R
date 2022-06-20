
test_that("emis_to_dt", {
  # GTFS2gps filter-----
  set.seed(232)
  # fleet -----------
  total_fleet <- data.table::data.table(year = c(2005,2010,2011,2012
                                                 ,2014,2015,2017,2018,2019),
                                        bus = c(1,61,50,1,45,18,62,27,31),
                                        veh_type_euro = "Ubus Std 15 - 18 t",
                                        euro_stage = c("II", "IV", "IV", "V"
                                                       , "V", "V", "V", "V","V"))
  
  total_fleet[,fleet_composition := bus/sum(bus)]
  
  
  # Ef ----
  EF_europe <- ef_europe_emep(pollutant = c("CO", "PM10"),
                         speed = units::set_units(seq(1,100,10),"km/h"),
                         veh_type = total_fleet$veh_type_euro,
                         tech = "SCR",
                         euro = total_fleet$euro_stage,
                         fcorr = rnorm(9, 0.5, 0.1))
  
  EF_usa_emfac <- ef_usa_emfac(pollutant = c("CO", "PM10"),
                               reference_year = 2019,
                               model_year = total_fleet$year,
                               speed = units::set_units(seq(1,100,10),"km/h"),
                               fuel = "D")
  
  EF_usa_moves <- ef_usa_moves(pollutant = c("CO", "PM10"),
                               reference_year = "2019",
                               model_year = total_fleet$year,
                               speed = units::set_units(seq(1,100,10),"km/h"),
                               fuel = "D")
  
  EF_brazil <- ef_brazil_cetesb(pollutant = c("CO", "CO2"),
                         veh_type = "BUS_URBAN_D",
                         model_year = total_fleet$year) # fleet_composition
  
  #emi ----
  
  # USA
  emi_usa_emfac <- emis(fleet_composition = total_fleet$fleet_composition,
                        dist = units::set_units(rnorm(10,20,1),"km"),
                        ef = EF_usa_emfac, 
                        prefix = "Emfac"
                        ,aggregate = FALSE)
  emi_usa_moves <- emis(fleet_composition = total_fleet$fleet_composition,
                        dist = units::set_units(rnorm(10,20,1),"km"),
                        ef = EF_usa_moves, 
                        prefix = "Moves"
                        ,aggregate = FALSE)
  
  # EUROPE
  emi_europe <- emis(fleet_composition = total_fleet$fleet_composition,
                     dist = units::set_units(rnorm(10,20,1),"km/h"),
                     ef = EF_europe,
                     prefix = "EU"
                     ,aggregate = FALSE)
  
  # BRAZIL (not speed dependent emission factor)
  emi_brazil <- emis(fleet_composition = total_fleet$fleet_composition,
                     dist = units::set_units(rnorm(10,20,1),"km/h"),
                     ef = EF_brazil,
                     prefix = "BR"
                     ,aggregate = FALSE)
  
  # Expect equal -----
  emi_dt_europe <- emis_to_dt(emi_list = emi_europe
                            ,emi_vars = 'emi'
                            ,veh_vars = c("veh_type","euro","fuel","tech")
                            ,pol_vars = 'pollutant'
                            ,segment_vars = c("slope","load")) 
  emi_dt_brazil <- emis_to_dt(emi_list = emi_brazil
                            ,emi_vars = 'emi'
                            ,veh_vars = c("model_year","veh_type")
                            ,pol_vars = 'pollutant') 
  emi_dt_usa_emfac <- emis_to_dt(emi_list = emi_usa_emfac
                            ,emi_vars = 'emi'
                            ,veh_vars = c("model_year","fuel")
                            ,pol_vars = 'pollutant') 
  emi_dt_usa_moves <- emis_to_dt(emi_list = emi_usa_moves
                            ,emi_vars = 'emi'
                            ,veh_vars = c("model_year","fuel")
                            ,pol_vars = 'pollutant') 

# Expect equal-----
expect_equal(ncol(emi_dt_europe),9) # ncol
expect_equal(ncol(emi_dt_brazil),5) # ncol
expect_equal(ncol(emi_dt_usa_emfac),5) # ncol
expect_equal(ncol(emi_dt_usa_moves),5) # ncol
expect_equal(nrow(emi_dt_europe),180) # nrow
expect_equal(nrow(emi_dt_brazil),180) # nrow
expect_equal(nrow(emi_dt_usa_emfac),180) # nrow
expect_equal(nrow(emi_dt_usa_moves),180) # nrow
expect_equal(as.numeric(sum(emi_dt_europe$emi,na.rm = TRUE)),217.147,0.01) # sum(emi)
expect_equal(as.numeric(sum(emi_dt_brazil$emi,na.rm = TRUE)),272501.2,0.01) # sum(emi)
expect_equal(as.numeric(sum(emi_dt_usa_emfac$emi,na.rm = TRUE)),19.82793,0.01) # sum(emi)
expect_equal(as.numeric(sum(emi_dt_usa_moves$emi,na.rm = TRUE)),5469.831,0.01) # sum(emi)

})
