
# checking stuff----
rm(list=ls())
devtools::load_all()
devtools::document()
devtools::load_all()
#devtools::check(manual = FALSE,vignettes = FALSE)
#devtools::install()

# libraries----
library(data.table)
library(magrittr)
library(gtfs2gps)
library(devtools)
library(mapview)

# r1----
gtfs <- gtfs2gps::read_gtfs(system.file("extdata/fortaleza.zip", package = "gtfs2gps")) %>%
  gtfs2gps::filter_single_trip()

# # r2----
# gps_for <- gtfs2gps::gtfs2gps(gtfs_for) 
# gps_line <- gps_for %>% gtfs2gps::adjust_speed() %>% gtfs2gps::gps_as_sflinestring1()

# r3----
sf_line <- transport_model(gtfs = gtfs,parallel = TRUE)

# r4----
head(sf_line$geometry)
plot(sf_line["trip_number"])

# r5----
fleet_data_ef_europe <- data.frame("veh_type" = c("Ubus Midi <=15 t"
                                                  ,"Ubus Std 15 - 18 t"
                                                  ,"Ubus Artic >18 t")
                                   ,"euro" = c("III","IV","V")
                                   ,"fuel" = rep("D",3)
                                   ,"tech" = c("-","SCR","SCR")
                                   ,"fleet_composition" = c(0.4,0.5,0.1))
fleet_data_ef_europe

# r6-----
sf_emis <- emission_model(tp_model = sf_line
                          ,ef_model =  "ef_europe_emep"
                          ,fleet_data = fleet_data_ef_europe
                          ,pollutant = c("CO","PM10","CO2","CH4","NOx"))
class(sf_emis)
names(sf_emis)

# r7-----

sf_emis$time_column <- sf_emis$tp_model$timestamp

names(sf_emis)
my_emis_summary <- emis_summary(emi_list = sf_emis,
                                emi_var = "emi", 
                                by = "time", 
                                time_column = "time_column",
                                veh_var = "veh_type", 
                                pol_var = "pollutant") 
my_emis_summary


# r8----
my_emis_summary <- emis_summary(emi_list = sf_emis,
                                emi_var = "emi", 
                                by = "veh_type", 
                                veh_var = "veh_type", 
                                pol_var = "pollutant") 
my_emis_summary

# r9----
my_emis_summary <- emis_summary(emi_list = sf_emis,
                                emi_var = "emi", 
                                by = "veh_type", 
                                veh_var = c("veh_type", "euro", "fuel"),
                                pol_var = "pollutant") 
my_emis_summary

# r10----
my_emis_summary <- emis_summary(emi_list = sf_emis,
                                emi_var = "emi", 
                                by = "pollutant", 
                                veh_var = "veh_type", 
                                pol_var = "pollutant") 
my_emis_summary

# r11----
my_emis_dt <- emis_to_dt(emi_list = sf_emis
                         ,emi_vars = "emi"
                         ,veh_vars = c("veh_type", "euro", "fuel")
                         ,pol_vars = "pollutant"
                         ,segment_vars = "time_column")
head(my_emis_dt, 5)

# r12----
# we add info of emissions into gps file
c


grid_gps <- sf::st_make_grid(
  x = sf::st_bbox(sf_emis$tp_model[1:nrow(sf_emis$tp_model),]$geometry)
  , cellsize = 0.25 / 102.47
  , crs= 4329
  , what = "polygons"
  , square = FALSE)
grid_gps <- sf::st_sf(id = 1:length(grid_gps),geometry = grid_gps)

pol_grid <- emis_grid(data = sf::st_as_sf(sf_emis$tp_model),
                      emi = "CO_Euro_IV",
                      grid = grid_gps,
                      time_class = 'all periods')

mapview(pol_grid["CO_Euro_IV"])+
  mapview(sf_emis$tp_model1$geometry)

## Detailed ------


#r1----
gtfs <- read_gtfs(system.file("extdata/fortaleza.zip", package="gtfs2gps")) %>%
  filter_single_trip()

gtfs$stop_times[,arrival_time := data.table::as.ITime(arrival_time)]
gtfs$stop_times[,departure_time := data.table::as.ITime(departure_time)]

gps_for <- gtfs2gps(gtfs)
data.table::setDT(gps_for)

# r2 ----
gps_for_fix <- adjust_speed(gps_data = gps_for
                            ,min_speed = 5
                            ,max_speed = 75
                            ,new_speed = 34
                            ,clone = TRUE)

# r3----
linestring_for <- gps_as_sflinestring(gps_for_fix)

# r4----
data.table::setDT(linestring_for)

# speeds
linestring_for[,min(dist),by=shape_id]
linestring_for[,mean(dist),by=shape_id]
linestring_for[,max(dist),by=shape_id]
# plot
plot(sf::st_as_sf(linestring_for)["shape_id"],lwd=3)

# r5-----
my_shp <- unique(gps_for$shape_id)
fleet_data_ef_europe <- data.frame("veh_type" = c(rep(c("Ubus Midi <=15 t"
                                                      ,"Ubus Std 15 - 18 t"
                                                      ,"Ubus Artic >18 t"),12),
                                                  rep("Ubus Artic >18 t",2))
                                   ,"euro" = c(rep(c("III","IV","V"),12),
                                               rep("V",2))
                                   ,"fuel" = rep("D",38)
                                   ,"tech" = c(rep(c("-","SCR","SCR"),12)
                                               ,rep("SCR",2))
                                   ,"fleet_composition" = c(rep(c(0.4,0.5,0.1),12)
                                                            ,c(1,1))
                                   ,"shape_id" = c(rep(my_shp[1:12],each = 3)
                                                   ,my_shp[13:14]))
fleet_data_ef_europe

setDT(fleet_data_ef_europe)[,sum(fleet_composition),by = shape_id]


# r6 -----
ef_europe <- ef_europe_emep(speed = units::set_units(10:100,"km/h"),
                              ,veh_type = fleet_data_ef_europe$veh_type
                              ,euro =  fleet_data_ef_europe$euro
                              ,pollutant = c("CO2","NOx")
                              ,fuel =  fleet_data_ef_europe$fuel
                              ,tech =  fleet_data_ef_europe$tech
                              ,as_list = TRUE)
ef_europe$speed <- units::set_units(10:100,"km/h")
ef_europe_dt <- emis_to_dt(emi_list = ef_europe
                           ,emi_vars = "EF"
                           ,veh_vars = c("veh_type", "fuel", "euro", "tech")
                           ,pol_vars = "pollutant"
                           ,segment_vars = "speed")

# r7 -----

setDT(ef_europe_dt)
ef_europe_dt[,name_fleet := paste0(veh_type, " / Euro ", euro)]

# plot
library(ggplot2)
ggplot(ef_europe_dt) + 
  geom_line(aes(x = speed,y = EF,color = name_fleet))+
  labs(color = "Category / EURO")+
  facet_wrap(~pollutant,scales = "free")

# r8----
linestring_for$shape_id %>% unique()

# r9----

emi_data <- lapply(unique(gps_for$shape_id),function(i){
  tmp_ef_euro <- ef_europe_emep(speed = linestring_for[shape_id == i]$speed,
                                ,veh_type = fleet_data_ef_europe[shape_id == i]$veh_type
                                ,euro =  fleet_data_ef_europe[shape_id == i]$euro
                                ,pollutant = c("CO2","NOx")
                                ,fuel =  fleet_data_ef_europe[shape_id == i]$fuel
                                ,tech =  fleet_data_ef_europe[shape_id == i]$tech
                                ,as_list = TRUE)

  tmp_emis <- emis(fleet_composition = fleet_data_ef_europe[shape_id == i]$fleet_composition
                   ,dist = units::set_units(linestring_for[shape_id == i]$dist,"km")
                   ,ef = tmp_ef_euro
                   ,aggregate = FALSE
                   ,as_list = TRUE)
  return(tmp_emis)
})

# r10----
grid_gps <- sf::st_make_grid(
  x = sf::st_bbox(linestring_for[1:nrow(linestring_for),]$geometry)
  , cellsize = 0.25 / 102.47
  , crs= 4329
  , what = "polygons"
  , square = FALSE)
grid_gps <- sf::st_sf(id = 1:length(grid_gps),geometry = grid_gps)

tmp_emis_dt <- lapply(1:length(emi_data),function(i){ # i = 3
  tmp_emi_dt <- emis_to_dt(emi_list = emi_data[[i]]
                          ,emi_vars = "emi"
                          ,veh_vars = "veh_type"
                          ,pol_vars = "pollutant")
  tmp_gps <- cbind(linestring_for[shape_id == unique(gps_for$shape_id)[i]]
                   ,tmp_emi_dt[pollutant == "CO2"])
  tmp_gps <- sf::st_as_sf(tmp_gps)
  tmp_emi_grid <- emis_grid(data = tmp_gps
                            ,emi = "emi"
                            ,grid = grid_gps)
  return(tmp_emi_grid)
}) %>% data.table::rbindlist()

# r11----
tmp_emis_dt <- tmp_emis_dt[,total_emi := sum(emi), by = id][, .SD[1], by = id]
tmp_emis_dt2 <- sf::st_as_sf(tmp_emis_dt)

mapview(tmp_emis_dt2["emi"])
