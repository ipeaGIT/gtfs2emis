
# checking stuff----
devtools::load_all()
devtools::document()
devtools::load_all()
devtools::check(manual = FALSE,vignettes = FALSE)
devtools::install()



# libraries----
library(data.table)
library(magrittr)
library(gtfs2gps)

# make grid
gtfs <- gtfs2gps::read_gtfs(system.file("extdata/fortaleza.zip", package = "gtfs2gps")) %>%
  gtfs2gps::filter_single_trip() %>% 
  gtfs2gps::gtfs2gps() %>% 
  gtfs2gps::adjust_speed() %>% 
  gtfs2gps::gps_as_sflinestring()
nrow(gtfs)
gtfs_tp <- gtfs2gps::read_gtfs(system.file("extdata/fortaleza.zip", package = "gtfs2gps")) %>%
  gtfs2gps::filter_single_trip() %>% 
  transport_model()
nrow(gtfs_tp)

setDT(gtfs)
setDT(gtfs_tp)
gtfs[shape_id == "R10-2"   & trip_number == 2,]
gtfs_tp[shape_id == "R10-2"& trip_number == 2,]

gtfs[,.N,by=.(shape_id)]
gtfs_tp[,.N,by=.(shape_id)]
gtfs[shape_id == "R10-2"   & trip_number == 2,]
gtfs_tp[shape_id == "R10-2"& trip_number == 2,]
identical(gtfs,gtfs_tp)
identical(unique(gtfs$shape_id),unique(gtfs_tp$shape_id))
identical(unique(gtfs$trip_id),unique(gtfs_tp$trip_id))
identical(nrow(gtfs),nrow(gtfs_tp))

gtfs
gtfs_tp
grid <- sf::st_make_grid(gtfs, cellsize = 0.25 / 102.47, square = FALSE)
grid1 <- sf::st_make_grid(tp_model$geometry, cellsize = 0.25 / 102.47, square = FALSE)

#mapview::mapview(grid1) + mapview::mapview(emi_list$tp_model$geometry)
mapview::mapview(grid) + mapview::mapview(gtfs$geometry)

class(gtfs$geometry)
class(emi_list$tp_model$geometry)
class(grid)
class(grid1)

## Read data----
gtfs <- gtfs2gps::read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps")) %>%
   gtfstools::filter_by_weekday(weekday ='monday') %>%
  gtfs2gps::filter_week_days() %>% 
  gtfstools::filter_by_shape_id(shape_id = '176-1') 


gps <- gtfs2gps::gtfs2gps(gtfs)
gps_fix <- gtfs2gps::adjust_speed(gps)
#gps_fix[stop_sequence==2]
gps_line <- gtfs2gps::gps_as_sflinestring(gps_fix)

gps_line
## transport model----

sf_line <- transport_model(gtfs = gtfs,parallel = TRUE)

## fleet ----
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

## emission model ----
sf_emis <- emission_model(gps = sf_line
                          ,ef_data_base = "emep_europe"
                          ,fleet_data = fleet_data_ef_europe
                          ,pollutant = c("CO","PM10","CO2","CH4","NOx"))
names(sf_emis)
sf_emis$time_column <- sf_emis$gps$timestamp

## summary ----
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
## emi_to_dt ----

sf_emis$time_column <- sf_emis$gps$timestamp
my_emis_dt <- emi_to_dt(emi_list = sf_emis
                        ,emi_vars = "emi"
                        ,veh_vars = c("veh_type","euro","fuel")
                        ,pol_vars =  "pollutant"
                        ,segment_vars = "time_column")
my_emis_dt

## grid----

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


# Detailed Usage ----
## Read data----

gtfs <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps")) %>%
  filter_week_days() %>%
  filter_single_trip()

unique(gtfs$shapes$shape_id)

gps_poa <- gtfs2gps(gtfs)

# Here we can define diferent ranges of minimum and maximum allowed speeds
# as well as average speed. In this case we considered the average speed 
# of 34 km/h.

gps_poa_fix <- adjust_speed(gps_data = gps_poa
                            ,min_speed = 5
                            ,max_speed = 75
                            ,new_speed = 34
                            ,clone = TRUE)


linestring_poa <- gps_as_sflinestring(gps_poa_fix)

# checking some basic statistics of the transport model

data.table::setDT(linestring_poa)
# average speed
linestring_poa[,weighted.mean(speed,dist),by=shape_id]
# average speed
linestring_poa[,min(dist),by=shape_id]
linestring_poa[,mean(dist),by=shape_id]
linestring_poa[,max(dist),by=shape_id]
# plot
plot(sf::st_as_sf(linestring_poa)["shape_id"],lwd=3)

# Fleet -----

# Users can provide a vehicle specific data for each shape_id
# The example below shows a situation where the 
# vehicle characteristics is known for three shape_ids ("T2-1","A141-1", "176-1")
# but is unknown for "R10-2"
# 
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
# the sum of fleet composition per shape_id can be checked
fleet_data_ef_europe[,sum(fleet_composition),by = shape_id]

# Visualizing the Emission factors -----

# We use the european database of EF. We can plot the EF to check how it behaves
# according to speed, veh_type

ef_europe <- ef_emep_europe(speed = units::set_units(10:100,"km/h")
                            ,veh_type = c("Ubus Midi <=15 t"
                                          ,"Ubus Std 15 - 18 t"
                                          ,"Ubus Artic >18 t")
                            ,euro = c("III","IV","V")
                            ,pollutant = c("CO2","NOx")
                            ,fuel = rep("D",3)
                            ,tech = c("-","SCR","SCR")
                            ,as_list = TRUE)

# We can use the emi_to_dt function to process EF
ef_europe$speed <- units::set_units(10:100,"km/h")
ef_europe_dt <- emi_to_dt(emi_list = ef_europe
                          ,emi_vars = "EF"
                          ,veh_vars = c("veh_type","fuel","euro","tech")
                          ,pol_vars = "pollutant"
                          ,segment_vars = "speed")

# lets use a fleet name as veh_type / euro_type
setDT(ef_europe_dt)
ef_europe_dt[,name_fleet := paste0(veh_type," / Euro ",euro)]

# plot
library(ggplot2)
ggplot(ef_europe_dt) + 
  geom_line(aes(x = speed,y = EF,color = name_fleet))+
  labs(color = "Category / EURO")+
  facet_wrap(~pollutant,scales = "free")

# Emissions estimates

# As the emissions varies with shape_id and fleet, we compute emissions separately, in a loop.


linestring_poa$shape_id %>% unique()
# [1] "176-1"  "A141-1" "R10-2"  "T2-1"

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

## grid----

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

tmp_emis_dt <- tmp_emis_dt[,total_emi := sum(emi),by = id][,.SD[1],by=id]
tmp_emis_dt2 <- sf::st_as_sf(tmp_emis_dt)

plot(tmp_emis_dt2["emi"])
  
