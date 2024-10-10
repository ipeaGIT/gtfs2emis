rm(list=ls())
library(devtools)
devtools::document()
devtools::load_all()
devtools::test()


# tyre -----
emi_list_wear <- emi_europe_emep_wear(dist = units::set_units(rep(1),"km"),
                                      speed =  units::set_units(30,"km/h"),
                                      pollutant = c("PM10","TSP","PM2.5"),
                                      veh_type = c("Ubus Std 15 - 18 t","Ubus Artic >18 t"),
                                      fleet_composition = c(0.5,0.5),
                                      load = 0.5,
                                      process = c("brake","tyre","road"),
                                      as_list = TRUE)
emi_list_wear
emi_list <- emi_list_wear
#emi_list <- emi_list_wear$emi
source("R/emis_to_dt.R")
names(emi_list_wear)
wear_dt <- emis_to_dt(emi_list = emi_list_wear
                      ,segment_vars = c("speed","dist")
                      ,veh_vars = c("veh_type")
                      ,process_vars = "process")
wear_dt
wear_dt[,sum(as.numeric(emi)),by = .(pollutant,veh_type,process)]
source("R/emis_summary.R")
emi_sum_wear <- emis_summary(emi_list = emi_list_wear
                             ,by = "pollutant"
                             ,veh_vars =  c("veh_type","fleet_composition"))
emi_sum_wear
emi_sum_wear <- emis_summary(emi_list = emi_list_wear
                             ,by = "vehicle"
                             ,veh_vars =  c("veh_type"))
emi_sum_wear

# hot_exaust------------
source("R/ef_europe_emep.R")
emi_he_list <- ef_europe_emep(euro = c("V","V"),
                              speed = units::set_units(1:100,"km/h"),
                              pollutant = c("PM10"),
                              veh_type = c("Ubus Std 15 - 18 t","Ubus Artic >18 t"),
                              load = 0.5,
                              as_list = TRUE)
names(emi_he_list)
head(emi_he_list$EF,1)
source("R/emis_summary.R")
emi_sum <- emis_summary(emi_list = emi_he_list
                        ,by = "pollutant"
                        ,veh_vars =  c("veh_type","fleet_composition")
                        ,process_vars = "process")
emi_sum_wear
#emi_list = emi_he_list
#segment_vars = c("speed")
#veh_vars = c("veh_type","euro")
#emi_vars = "EF"
#pol_vars = "pollutant"
#process_vars = "process" 
source("R/emis_to_dt.R")
hex_dt <- emis_to_dt(emi_list = emi_he_list
                     ,segment_vars = c("speed")
                     ,veh_vars = c("veh_type","euro")
                     ,emi_vars = "EF"
                     ,pol_vars = "pollutant"
                     ,process_vars = "process" 
)
hex_dt
# merge emissions-----


lcf_bw <- 1.00 + 0.79 * load
# emission factor
ef_tsp_pc_bw <- units::set_units(0.0075,'g/km')
ef_tsp_bus_bw <- 3.13 * lcf_bw * ef_tsp_pc_bw
#ef_tsp_bus_bw <- units::set_units(ef_tsp_bus_bw,'g/km')

# speed corrections
speed_cor_bw <- speed_cor_bw_f(speed)
# emission
a <- dist * speed_cor_bw
a
b <- a * ef_tsp_bus_bw
b * mass_bw
a <- (dist * as.matrix(speed_cor_bw) )
b <- ef_tsp_bus_bw * mass_bw
for(i in 1:ncol(b)) b[]
dim(a)
dim(b)
a %*% b
a %*% t(b)
b %*% t(a)
b %*% a
tmp_emi <- (ef_tsp_bus_bw %*% * mass_bw) 