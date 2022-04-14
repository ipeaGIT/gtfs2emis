
# checking stuff----
devtools::load_all()
devtools::document()
devtools::load_all()
devtools::check(manual = FALSE,vignettes = FALSE)
devtools::install()


library(data.table)
library(magrittr)
library(gtfs2gps)
gtfs <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps")) %>%
  filter_week_days() %>%
  filter_by_shape_id(.,"176-1") %>% 
  filter_single_trip()

sf_line <- transport_model(gtfs = gtfs,parallel = TRUE)

fleet_data_ef_europe <- data.table::data.table("veh_type" = c("Ubus Midi <=15 t"
                                                              ,"Ubus Std 15 - 18 t"
                                                              ,"Ubus Artic >18 t")
                                               ,"euro" = c("III","IV","V")
                                               ,"fuel" = rep("D",3)
                                               ,"tech" = c("-","SCR","SCR")
                                               ,"fleet_composition" = c(0.4,0.5,0.1))
fleet_data_ef_emfac <- data.table::data.table("veh_type" = "BUS_URBAN_D"
                                              ,"model_year" = 2011:2015
                                              ,"fuel" = "D"
                                              ,"calendar_year" = 2019
                                              ,"fleet_composition" = rep(0.2,5))
fleet_data_ef_moves <- data.table::data.table("veh_type" = "BUS_URBAN_D"
                                              ,"model_year" = 2011:2015
                                              ,"fuel" = "D"
                                              ,"calendar_year" = 2016
                                              ,"fleet_composition" = rep(0.2,5))
fleet_data_ef_cetesb <- data.table::data.table("veh_type" = c("BUS_MICRO_D"
                                                              ,"BUS_URBAN_D"
                                                              ,"BUS_ARTIC_D")
                                               ,"model_year" = rep(2010,3)
                                               ,"fuel" = rep("D",3)
                                               ,"fleet_composition" = c(0.4,0.4,0.2))

sf_emis <- emission_model(gps = sf_line
                          ,ef_data_base = "emep_europe"
                          ,fleet_data = fleet_data_ef_europe
                          ,pollutant = c("CO","PM10","CO2","CH4","NOx"))
names(sf_emis)
sf_emis$time_column <- sf_emis$gps$timestamp

# emiss summary
my_emis_summary <- emis_summary(emi_list = sf_emis,
                    emi_var = "emi", 
                    by = "time", 
                    time_column = "time_column",
                    veh_var = "veh_type", 
                    pol_var = "pollutant") 
my_emis_summary
my_emis_summary <- emis_summary(emi_list = sf_emis,
                                 emi_var = "emi", 
                                 by = "veh_type", 
                                 time_column = "time_column",
                                 veh_var = "veh_type", 
                                 pol_var = "pollutant") 
my_emis_summary
my_emis_summary <- emis_summary(emi_list = sf_emis,
                                emi_var = "emi", 
                                by = "veh_type", 
                                time_column = "time_column",
                                veh_var = c("veh_type","euro","fuel"),
                                pol_var = "pollutant") 
my_emis_summary
my_emis_summary <- emis_summary(emi_list = sf_emis,
                                 emi_var = "emi", 
                                 by = "pollutant", 
                                 time_column = "time_column",
                                 veh_var = "veh_type", 
                                 pol_var = "pollutant") 

# emi_to_dt

sf_emis$time_column <- sf_emis$gps$timestamp
my_emis_dt <- emi_to_dt(emi_list = sf_emis
                    ,emi_vars = "emi"
                    ,veh_vars = c("veh_type","euro","fuel")
                    ,pol_vars =  "pollutant"
                    ,segment_vars = "time_column")
my_emis_dt

# emis grid

grid_gps <- vein::make_grid(spobj = sf_emis$gps, width =  0.25 / 102.47) # 500 meters
grid_gps <- sf::st_as_sf(grid_gps)


names(sf_emis$emi)
# we add info of emissions into gps file
sf_emis$gps <- cbind(sf_emis$gps,sf_emis$emi)

pol_grid <- emis_grid(data = sf_emis$gps,
                     emi = c("CO_Euro_IV"),
                     grid = grid_gps,
                     time_class = 'all periods')
plot(pol_grid["CO_Euro_IV"])
mapview::mapview(pol_grid["CO_Euro_IV"])
