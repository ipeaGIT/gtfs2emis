## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")

## ---- eval = FALSE, message = FALSE-------------------------------------------
#  # From CRAN
#  install.packages("gtfs2emis")
#  
#  # or use the development version with latest features
#  utils::remove.packages('gtfs2emis')
#  devtools::install_github("ipeaGIT/gtfs2emis")
#  

## ---- message = FALSE---------------------------------------------------------
library(gtfs2emis)
library(gtfstools)
library(gtfs2gps)
library(data.table)
library(sf)
library(units)
library(magrittr)
library(ggplot2)

## ---- message = FALSE---------------------------------------------------------
gtfs <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps")) %>%
  filter_week_days() %>%
  filter_by_shape_id(., "176-1") %>% 
  filter_single_trip()

## ---- message = FALSE---------------------------------------------------------
sf_line <- transport_model(gtfs = gtfs,parallel = TRUE)

## ---- message = FALSE, fig.width=3,fig.height=3-------------------------------
head(sf_line$geometry)
sf_line <- sf::st_as_sf(sf_line)
plot(sf_line["trip_number"])

## ---- message = FALSE---------------------------------------------------------
fleet_data_ef_europe <- data.table::data.table("veh_type" = c("Ubus Midi <=15 t"
                                                              ,"Ubus Std 15 - 18 t"
                                                              ,"Ubus Artic >18 t")
                                               ,"euro" = c("III","IV","V")
                                               ,"fuel" = rep("D",3)
                                               ,"tech" = c("-","SCR","SCR")
                                               ,"fleet_composition" = c(0.4,0.5,0.1))
fleet_data_ef_europe

## ---- message = FALSE---------------------------------------------------------
fleet_data_ef_emfac <- data.table::data.table("veh_type" = "BUS_URBAN_D"
                                              ,"model_year" = 2011:2015
                                              ,"fuel" = "D"
                                              ,"calendar_year" = 2019
                                              ,"fleet_composition" = rep(0.2,5))
fleet_data_ef_emfac

## ---- message = FALSE---------------------------------------------------------
fleet_data_ef_moves <- data.table::data.table("veh_type" = "BUS_URBAN_D"
                                              ,"model_year" = 2011:2015
                                              ,"fuel" = "D"
                                              ,"calendar_year" = 2016
                                              ,"fleet_composition" = rep(0.2,5))
fleet_data_ef_moves

## ---- message = FALSE---------------------------------------------------------
fleet_data_ef_cetesb <- data.table::data.table("veh_type" = c("BUS_MICRO_D"
                                                              ,"BUS_URBAN_D"
                                                              ,"BUS_ARTIC_D")
                                               ,"model_year" = rep(2010, 3)
                                               ,"fuel" = rep("D", 3)
                                               ,"fleet_composition" = c(0.4, 0.4, 0.2))
fleet_data_ef_cetesb

## ---- message = FALSE---------------------------------------------------------
sf_emis <- emission_model(gps = sf_line
                          ,ef_data_base = "emep_europe"
                          ,fleet_data = fleet_data_ef_europe
                          ,pollutant = c("CO","PM10","CO2","CH4","NOx"))
class(sf_emis)
names(sf_emis)

## ---- message = FALSE---------------------------------------------------------
sf_emis$time_column <- sf_emis$gps$timestamp

names(sf_emis)
my_emis_summary <- emis_summary(emi_list = sf_emis,
                    emi_var = "emi", 
                    by = "time", 
                    time_column = "time_column",
                    veh_var = "veh_type", 
                    pol_var = "pollutant") 
my_emis_summary

## ---- message = FALSE---------------------------------------------------------
my_emis_summary <- emis_summary(emi_list = sf_emis,
                                 emi_var = "emi", 
                                 by = "veh_type", 
                                 time_column = "time_column",
                                 veh_var = "veh_type", 
                                 pol_var = "pollutant") 
my_emis_summary

## ---- message = FALSE---------------------------------------------------------
my_emis_summary <- emis_summary(emi_list = sf_emis,
                                emi_var = "emi", 
                                by = "veh_type", 
                                time_column = "time_column",
                                veh_var = c("veh_type", "euro", "fuel"),
                                pol_var = "pollutant") 
my_emis_summary

## ---- message = FALSE---------------------------------------------------------
my_emis_summary <- emis_summary(emi_list = sf_emis,
                                 emi_var = "emi", 
                                 by = "pollutant", 
                                 time_column = "time_column",
                                 veh_var = "veh_type", 
                                 pol_var = "pollutant") 
my_emis_summary

## ---- message = FALSE---------------------------------------------------------
sf_emis$time_column <- sf_emis$gps$timestamp
my_emis_dt <- emi_to_dt(emi_list = sf_emis
                    ,emi_vars = "emi"
                    ,veh_vars = c("veh_type", "euro", "fuel")
                    ,pol_vars = "pollutant"
                    ,segment_vars = "time_column")
head(my_emis_dt, 5)

## ---- message = FALSE---------------------------------------------------------
grid_gps <- vein::make_grid(spobj = sf_emis$gps, width =  0.25 / 102.47) # 500 meters
grid_gps <- sf::st_as_sf(grid_gps)

## ---- message = FALSE, fig.width=4,fig.height=4-------------------------------
names(sf_emis$emi)
# we add info of emissions into gps file
sf_emis$gps <- cbind(sf_emis$gps,sf_emis$emi)

pol_grid <- emis_grid(data = sf_emis$gps,
                     emi = "CO_Euro_IV",
                     grid = grid_gps,
                     time_class = 'all periods')
plot(pol_grid["CO_Euro_IV"])

## ----message=FALSE------------------------------------------------------------
gtfs <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps")) %>%
  filter_week_days() %>%
  filter_single_trip()

gtfs$stop_times[,arrival_time := data.table::as.ITime(arrival_time)]
gtfs$stop_times[,departure_time := data.table::as.ITime(departure_time)]

gps_poa <- gtfs2gps(gtfs)
data.table::setDT(gps_poa)

## -----------------------------------------------------------------------------
gps_poa_fix <- adjust_speed(gps_data = gps_poa
                            ,min_speed = 5
                            ,max_speed = 75
                            ,new_speed = 34
                            ,clone = TRUE)

## -----------------------------------------------------------------------------
linestring_poa <- gps_as_sflinestring(gps_poa_fix)

## ---- fig.width=4,fig.height=6------------------------------------------------
data.table::setDT(linestring_poa)
# speeds
linestring_poa[,weighted.mean(speed,dist),by=shape_id]
linestring_poa[,min(dist),by=shape_id]
linestring_poa[,mean(dist),by=shape_id]
linestring_poa[,max(dist),by=shape_id]
# plot
plot(sf::st_as_sf(linestring_poa)["shape_id"],lwd=3)

## -----------------------------------------------------------------------------
fleet_data_ef_europe <- data.table::data.table("veh_type" = rep(c("Ubus Midi <=15 t"
                                                                  ,"Ubus Std 15 - 18 t"
                                                                  ,"Ubus Artic >18 t"),2)
                                               ,"euro" = rep(c("III","IV","V"),2)
                                               ,"fuel" = rep("D",6)
                                               ,"tech" = rep(c("-","SCR","SCR"),2)
                                               ,"fleet_composition" = c(rep(1,3),0.4,0.5,0.1)
                                               ,"shape_id" = c("T2-1","A141-1",
                                                               "176-1",rep("R10-2",3)))
fleet_data_ef_europe

## -----------------------------------------------------------------------------
fleet_data_ef_europe[,sum(fleet_composition),by = shape_id]

## ----message = FALSE----------------------------------------------------------
ef_europe <- ef_emep_europe(speed = units::set_units(10:100,"km/h")
                            ,veh_type = c("Ubus Midi <=15 t"
                                          ,"Ubus Std 15 - 18 t"
                                          ,"Ubus Artic >18 t")
                            ,euro = c("III", "IV", "V")
                            ,pollutant = c("CO2", "NOx")
                            ,fuel = rep("D", 3)
                            ,tech = c("-", "SCR", "SCR")
                            ,as_list = TRUE)

## -----------------------------------------------------------------------------
ef_europe$speed <- units::set_units(10:100,"km/h")
ef_europe_dt <- emi_to_dt(emi_list = ef_europe
                          ,emi_vars = "EF"
                          ,veh_vars = c("veh_type", "fuel", "euro", "tech")
                          ,pol_vars = "pollutant"
                          ,segment_vars = "speed")

## ---- fig.width=7,fig.height=3------------------------------------------------
setDT(ef_europe_dt)
ef_europe_dt[,name_fleet := paste0(veh_type, " / Euro ", euro)]

# plot
library(ggplot2)
ggplot(ef_europe_dt) + 
  geom_line(aes(x = speed,y = EF,color = name_fleet))+
  labs(color = "Category / EURO")+
  facet_wrap(~pollutant,scales = "free")

## -----------------------------------------------------------------------------
linestring_poa$shape_id %>% unique()

## ---- message = FALSE---------------------------------------------------------
emi_data <- lapply(unique(gps_poa$shape_id),function(i){
  tmp_ef_euro <- ef_emep_europe(speed = linestring_poa[shape_id == i]$speed,
                                ,veh_type = fleet_data_ef_europe[shape_id == i]$veh_type
                                ,euro =  fleet_data_ef_europe[shape_id == i]$euro
                                ,pollutant = c("CO2","NOx")
                                ,fuel =  fleet_data_ef_europe[shape_id == i]$fuel
                                ,tech =  fleet_data_ef_europe[shape_id == i]$tech
                                ,as_list = TRUE)
  tmp_emis <- emis(fleet_composition = fleet_data_ef_europe[shape_id == i]$fleet_composition
                   ,dist = units::set_units(linestring_poa[shape_id == i]$dist,"km")
                   ,ef = tmp_ef_euro
                   ,aggregate = FALSE
                   ,as_list = TRUE)
  return(tmp_emis)
})

## -----------------------------------------------------------------------------
# Create Grid
grid_gps <- vein::make_grid(spobj = sf::st_as_sf(linestring_poa$geometry)
                            , width =  0.25 / 102.47) # 500 meters
grid_gps <- sf::st_as_sf(grid_gps)

tmp_emis_dt <- lapply(1:length(emi_data),function(i){ # i = 3
  tmp_emi_dt <- emi_to_dt(emi_list = emi_data[[i]]
                          ,emi_vars = "emi"
                          ,veh_vars = "veh_type"
                          ,pol_vars = "pollutant")
  tmp_gps <- cbind(linestring_poa[shape_id == unique(gps_poa$shape_id)[i]]
                   ,tmp_emi_dt[pollutant == "CO2"])
  tmp_gps <- sf::st_as_sf(tmp_gps)
  tmp_emi_grid <- emis_grid(data = tmp_gps
                            ,emi = "emi"
                            ,grid = grid_gps)
  return(tmp_emi_grid)
}) %>% data.table::rbindlist()

## ---- fig.width=4,fig.height=4------------------------------------------------
tmp_emis_dt <- tmp_emis_dt[,total_emi := sum(emi), by = id][, .SD[1], by = id]
tmp_emis_dt2 <- sf::st_as_sf(tmp_emis_dt)

plot(tmp_emis_dt2["emi"])

