# 0) Load libraries ----

rm(list=ls())
gc(reset = TRUE)
library(data.table)
library(magrittr)
library(ggplot2)
library(sfheaders)
library(sf)
library(gtfs2gps)
library(gtfstools)
library(mapview)
library(gtfs2emis)

# 1) GTFS2GPS ---------

# read gtfs
spo_gtfs <- gtfstools::read_gtfs(system.file("extdata/saopaulo.zip", package = "gtfs2gps"))

# convert frequency to stop_times
spo_gtfs <- gtfstools::frequencies_to_stop_times(gtfs = spo_gtfs)

spo_gtfs$stop_times[,arrival_time := data.table::as.ITime(arrival_time)]
spo_gtfs$stop_times[,departure_time := data.table::as.ITime(departure_time)]

# filter by bus route
temp_routeid <- spo_gtfs$routes[route_type == 3,route_id]
temp_shapeids <- spo_gtfs$trips[route_id %in% unique(temp_routeid),shape_id]
spo_gtfs <- gtfs2gps::filter_by_shape_id(gtfs_data = spo_gtfs,
                                         shape_ids = unique(temp_shapeids))
spo_gtfs1 <- gtfs2gps::filter_by_day(gtfs_data = spo_gtfs
                                     ,days = "monday")

# generate gps
dir.create("article/data/gps_spo/")
gtfs2gps::gtfs2gps(gtfs_data = spo_gtfs
                   ,snap_method = "nearest2"
                   ,spatial_resolution = 50
                   ,parallel = TRUE
                   ,filepath = "article/data/gps_spo"
                   ,continue = FALSE
                   ,compress = TRUE)

# 2) Adjust gps speed---------
dir.create("article/data/gps_spo_adjusted/")
files_gps <- list.files("article/data/gps_spo/",full.names = TRUE)
files_gps_names <- list.files("article/data/gps_spo/",full.names = FALSE)

gps_speed_fix <- lapply(seq_along(files_gps),function(i){ # i =1 
  
  message(paste0("adjust gps speed of file '",files_gps_names[i],"'"))
  
  tmp_gps <- readr::read_rds(files_gps[i])
  tmp_gps[, dist := units::set_units(dist,"m")]
  tmp_gps[, cumdist := units::set_units(cumdist,"m")]
  tmp_gps[, cumtime := units::set_units(cumtime,"m")]
  tmp_gps_fix <- gtfs2gps::adjust_speed(gps_data = tmp_gps)
  readr::write_rds(x = tmp_gps_fix
                   ,file = paste0("article/data/gps_spo_adjusted/",files_gps_names[i]))
  return(NULL)
})

# 3) Gps points to Linestring ------
dir.create("article/data/gps_spo_linestring/")
files_gps <- list.files("article/data/gps_spo_adjusted/",full.names = TRUE)
files_gps_names <- list.files("article/data/gps_spo_adjusted/",full.names = FALSE)

gpsLine_speed_fix <- lapply(seq_along(files_gps),function(i){ # i = 1
  
  message(paste0("Gps points to Linestring file '",files_gps_names[i],"'"))
  
  
  tmp_gps <- readr::read_rds(files_gps[i])
  tmp_gps_fix <- gtfs2gps::gps_as_sflinestring(gps = tmp_gps)
  
  readr::write_rds(x = tmp_gps_fix
                   ,file = paste0("article/data/gps_spo_linestring/",files_gps_names[i]))
  return(NULL)
})

# 3.1) Generall statistics ----

files_gps <- list.files("article/data/gps_spo_linestring/",full.names = TRUE)
files_gps_names <- list.files("article/data/gps_spo_linestring/",full.names = FALSE)

gen_stats <- lapply(seq_along(files_gps),function(i){ # i = 1
  
  message(paste0("Stats of Linestring file '",files_gps_names[i],"'"))
  
  tmp_gps <- readr::read_rds(files_gps[i])
  data.table::setDT(tmp_gps)
  
  stats_dt <- data.table::data.table(
    "shape_id" = gsub(".rds","",files_gps_names[i])
    ,"number_trips" = data.table::uniqueN(tmp_gps$trip_id)
    ,"number_stop_id" = data.table::uniqueN(tmp_gps$from_stop_id)
    ,"number_stop_id_per_trip" = data.table::uniqueN(tmp_gps$from_stop_id) / data.table::uniqueN(tmp_gps$trip_id)
    ,"Q25" = quantile(tmp_gps$speed,.25,na.rm = TRUE)
    ,"Q50" = quantile(tmp_gps$speed,.50,na.rm = TRUE)
    ,"Q75" = quantile(tmp_gps$speed,.75,na.rm = TRUE)
    ,"VTK" = units::set_units(sum(tmp_gps$dist),"km")
    ,"VTK_per_trip" = units::set_units(sum(tmp_gps$dist) / data.table::uniqueN(tmp_gps$trip_id),"km")
    ,"total_time" = units::set_units(sum(tmp_gps$dist/tmp_gps$speed,na.rm=TRUE),"h")
  )
  
  
  return(stats_dt)
}) %>% data.table::rbindlist()

gen_stats[,total_time := as.numeric(total_time) ]
gen_stats[,lapply(.SD,weighted.mean,VTK)
          ,.SDcols = c("Q25","Q50","Q75","VTK_per_trip","VTK")]
gen_stats[,lapply(.SD,weighted.mean,as.numeric(VTK))
          ,.SDcols = c("number_trips","total_time","number_stop_id")]
gen_stats[,lapply(.SD,sum)
          ,.SDcols = c("number_trips","total_time","VTK")]
gen_stats$number_stop_id %>% sum()

# 3.2) Plot GTFS trips -----


# read Tiles & Boundaries
my_tile <- readr::read_rds("article/data/bra_spo_mapbox.rds")
my_bound <- readr::read_rds("article/data/bra_spo_boundary.rds")

# bbox tile
xtile <- max(my_tile$x) - min(my_tile$x)
ytile <- max(my_tile$y) - min(my_tile$y)
ratio_tile <- ytile/xtile

# list-files
gps_lines_sf <- gtfstools::convert_shapes_to_sf(gtfs = spo_gtfs)
stops_sf <- gtfstools::convert_stops_to_sf(gtfs = spo_gtfs)

# aggregate by sum
stops_sf <- sf::st_transform(stops_sf,3857)
gps_lines_sf <- sf::st_transform(gps_lines_sf,3857)
gps_lines_sf$label <- "SPTRAN's shape_ids"

# plot
map_scale <- as.numeric(sf::st_bbox(my_bound)[3]) -  
  as.numeric(sf::st_bbox(my_bound)[1])
plot_scale <- 10
xlim_coord <- c( min(my_tile$x), max(my_tile$x))
ylim_coord <- c( min(my_tile$y), max(my_tile$y))

# plot spatial
ggplot() + 
  # add raster
  geom_raster(data = my_tile, aes(x, y, fill = hex), alpha = 1) +
  coord_cartesian(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  coord_equal() +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # add gps_lines_sf
  ggnewscale::new_scale_color() +
  geom_sf(data = gps_lines_sf,aes(color = label)
          ,alpha = 0.75, size = 0.55,fill = NA) +
  scale_color_manual(values = "red",name = NULL,labels = "SPTRAN's shape_ids")+
  # add boundary
   ggnewscale::new_scale_color() +
   geom_sf(data = my_bound,aes(color = city_name)
           ,linetype = "dashed",alpha = 0.5, size = 0.35,fill = NA) +
   scale_color_manual(values = "black",name = NULL
                      ,labels = "City \nboundary")+
  coord_sf(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  # labels and theme
  labs(title = NULL
       , color = NULL
       , x = NULL
       , y = NULL) +
  theme(legend.position = c(0.9,0.1)) + 
  theme_minimal() + 
  theme_void() +
  # map itens
  ggsn::north(data = my_bound,
              location = "topright",symbol = 12) +
  ggsn::scalebar(x.min = sf::st_bbox(sf::st_transform(my_bound,3857))[1] %>% as.numeric(),
                 x.max = sf::st_bbox(sf::st_transform(my_bound,3857))[3] %>% as.numeric(),
                 y.min = sf::st_bbox(sf::st_transform(my_bound,3857))[2] %>% as.numeric(),
                 y.max = sf::st_bbox(sf::st_transform(my_bound,3857))[4] %>% as.numeric(),
                 dist = plot_scale,
                 dist_unit = "km",st.size = 3,
                 st.bottom = FALSE, st.color = "black",
                 transform = FALSE, model = "WGS84") 
# save
ggplot2::ggsave(filename = "article/data/plots/basic_sptrans.png",
                scale = 1.0,width = 18,
                bg = "white",
                height = 20,units = "cm",dpi = 300)



# 4) Read fleet -----
fleet_spo <- readr::read_rds("article/data/bra_spo_fleet.rds")

# adjust technology
fleet_spo <- fleet_spo[fuel %in% "D",] # remove electric
fleet_spo[euro %in% "V", Technology := "SCR"]
fleet_spo[euro %in% "III",Technology := "-"]
fleet_spo[,fleet_composition := N/sum(N)]


# 5) Emission factor & Emissions -----

dir.create("article/data/emissions/")

# local EF
temp_ef_br <- gtfs2emis::ef_brazil(pollutant = c("CO2","NOx","PM10","CH4"),
                                   veh_type = fleet_spo$type_name_br,
                                   model_year = as.numeric(fleet_spo$year),
                                   as_list = TRUE)

# list-files
files_gps <- list.files(path = 'article/data/gps_spo_linestring/',full.names = TRUE)
files_gps_names <- list.files(path = 'article/data/gps_spo_linestring/',full.names = FALSE)


# Emissions estimates
emission_estimate <- lapply(seq_along(files_gps),function(i){ # i = 1
  
  message(paste0("Emissions estimates of file '",files_gps_names[i],"'"))
  
  
  tmp_gps <- readr::read_rds(files_gps[i])
  # EF scaled
  temp_ef <- base::suppressMessages(gtfs2emis::ef_euro_scaled(ef_local = temp_ef_br$EF,
                                                              speed = tmp_gps$speed, 
                                                              veh_type = fleet_spo$type_name_eu,
                                                              euro = fleet_spo$euro,
                                                              fuel = fleet_spo$fuel,
                                                              tech = fleet_spo$Technology,
                                                              pollutant = c("CO2","NOx","PM10","CH4")) )
  # EMISSION estimates 
  temp_emis <- gtfs2emis::emis(fleet_composition = fleet_spo$fleet_composition
                               , dist = units::set_units(tmp_gps$dist,"km")
                               , ef = temp_ef
                               , aggregate = FALSE
                               , as_list = TRUE)
  
  # remove EF from list
  temp_emis$EF = temp_ef$EF
  
  # cbind geometry and emisions
  temp_emis$gps <- tmp_gps
  
  readr::write_rds(x = temp_emis
                   ,file = paste0("article/data/emissions/",files_gps_names[i])
                   ,compress = "gz")
  return(NULL)
})

# 6) Time processing -----

dir.create("article/data/emi_time/")

# list-files
files_gps <- list.files(path = 'article/data/emissions/',full.names = TRUE)
files_gps_names <- list.files(path = 'article/data/emissions/',full.names = FALSE)


time_processing <- lapply(seq_along(files_gps),function(i){ # i = 1
  
  
  message(paste0("Emi time of file '",files_gps_names[i],"'"))
  
  
  temp_emi <- readr::read_rds(files_gps[i])
  emi_post <- emis_summary(emi = temp_emi,
                           emi_vars = "emi",
                           by = "time",
                           time_column = temp_emi$gps$timestamp,
                           pol_vars = "pollutant")
  
  readr::write_rds(x = emi_post
                   ,file = paste0("article/data/emi_time/",files_gps_names[i]),compress = 'gz')
  return(NULL)
})

# 7) Spatial post-processing-----

dir.create("article/data/emi_grid/")

# list-files
files_gps <- list.files(path = 'article/data/emissions/',full.names = TRUE)
files_gps_names <- list.files(path = 'article/data/emissions/',full.names = FALSE)


temp_grid <- readr::read_rds("article/data/bra_spo_grid09.rds")

spatial_processing <- lapply(seq_along(files_gps),function(i){ # i = 1
  
  gc(reset = TRUE, full = TRUE)
  message(paste0("Emi grid of file '",files_gps_names[i],"'"))
  
  # read
  temp_emi <- readr::read_rds(files_gps[i])
  # emi_to_dt
  temp_emi_dt <-  gtfs2emis::emi_to_dt(emi_list = temp_emi,
                                       emi_vars = "emi",
                                       veh_vars = c("veh_type","fuel"),
                                       pol_vars = "pollutant")
  namePol <- unique(temp_emi_dt$pollutant)
  # sum emissions
  temp_emi_dt <- temp_emi_dt[,lapply(.SD,sum, na.rm = TRUE),
                             by = .(pollutant,segment_id), .SDcols = ("emi")]
  # reshape
  temp_emi_dt <- data.table::dcast(temp_emi_dt,segment_id ~ pollutant,value.var = "emi")
  
  # rename segment_id
  temp_emi$gps$segment_id <- 1:nrow(temp_emi$gps)
  # 
  temp_emi2 <- data.table::copy(temp_emi$gps)
  data.table::setDT(temp_emi2)
  temp_emi2 <- temp_emi2[temp_emi_dt,on = "segment_id"]
  
  # dt to sf to transform
  temp_emi2 <- sf::st_as_sf(temp_emi2) %>% 
    sf::st_transform("+proj=utm +zone=23 +ellps=WGS84 +south +units=m")
  
  # grid
  emi_grid <- base::suppressMessages(
    gtfs2emis::emis_grid(data = temp_emi2,
                         emi = namePol,
                         grid = temp_grid)
  )
  
  # write
  readr::write_rds(x = emi_grid
                   ,file = paste0("article/data/emi_grid/",files_gps_names[i]),compress = 'gz')
  return(NULL)
})


# 8) Plot temporal emissions ---------

dir.create("article/data/plots/")

# list-files
files_gps <- list.files(path = 'article/data/emi_time/',full.names = TRUE)
files_gps_names <- list.files(path = 'article/data/emi_time/',full.names = FALSE)

#  my function to_compartible_units-
to_compartible_units <- function(i){  #i = my_time$CO2
  pol_digits <- floor(log10(as.numeric(i))) + 1
  if(median(pol_digits,na.rm = TRUE) <= 2){return(units::set_units(i,"g"))}
  if(median(pol_digits,na.rm = TRUE) <= 5){return(units::set_units(i,"kg"))}
  if(median(pol_digits,na.rm = TRUE) >= 6){return(units::set_units(i,"t"))}
}

tmp_my_time <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist(fill = TRUE)

# aggregate by summing
colPol <- names(tmp_my_time)[!(names(tmp_my_time) %in% "time")]
my_time <- tmp_my_time[,lapply(.SD,sum),by = time,.SDcols = colPol]
my_time <- my_time[,(colPol) := lapply(.SD,to_compartible_units),.SDcols = colPol]
my_time <- my_time[order(time),]

# plot time-
tmpTime1 <- data.table::copy(my_time)[,.SD,.SDcols = c("time","PM10")]
names(tmpTime1) <- c("time","pol")
getUnit_graphic <- units::deparse_unit(tmpTime1$pol)

ggplot(data = tmpTime1) + 
  geom_bar(aes(x = time, y = as.numeric(pol),fill = as.numeric(pol)),
           stat = "identity")+
  labs(title = NULL,
       x = "Hour",
       y = paste0("PM10 (",getUnit_graphic,")")) + 
  viridis::scale_fill_viridis(option = "D",direction = -1)+
  theme_minimal()+
  theme(legend.position = "none")

ggplot2::ggsave(filename = "article/data/plots/temporal_PM10.png",
                scale = 1.0,bg = "white",
                width = 18,height = 10,units = "cm",dpi = 300)


# 9) Plot spatial emissions -----

# read Tiles & Boundaries
my_tile <- readr::read_rds("article/data/bra_spo_mapbox.rds")
my_bound <- readr::read_rds("article/data/bra_spo_boundary.rds")

# bbox tile
xtile <- max(my_tile$x) - min(my_tile$x)
ytile <- max(my_tile$y) - min(my_tile$y)
ratio_tile <- ytile/xtile

# list-files
files_gps <- list.files(path = 'article/data/emi_grid/',full.names = TRUE)
files_gps_names <- list.files(path = 'article/data/emi_grid/',full.names = FALSE)

#  my function to_compartible_units-
to_compartible_units <- function(i){  #i = my_time$CO2
  pol_digits <- floor(log10(as.numeric(i))) + 1
  if(median(pol_digits,na.rm = TRUE) <= 2){return(units::set_units(i,"g"))}
  if(median(pol_digits,na.rm = TRUE) <= 5){return(units::set_units(i,"kg"))}
  if(median(pol_digits,na.rm = TRUE) >= 6){return(units::set_units(i,"t"))}
}

# read files
tmp_my_grid <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist()

# aggregate by sum
colPol <- names(tmp_my_grid)[!(names(tmp_my_grid) %in% "id" | names(tmp_my_grid) %in% "geometry")]
my_grid <- tmp_my_grid[,(colPol) := lapply(.SD,sum),by = id,.SDcols = colPol]
my_grid <- my_grid[,.SD[1],by = id]
my_grid <- my_grid[,(colPol) := lapply(.SD,to_compartible_units),.SDcols = colPol]
my_grid <- sf::st_as_sf(my_grid) %>% sf::st_transform(3857)


# plot
j = 3 # pol = "PM10"
getUnit_map <- my_grid[[colPol[j]]][1] %>% units::deparse_unit()
map_scale <- as.numeric(sf::st_bbox(my_grid)[3]) -  
  as.numeric(sf::st_bbox(my_grid)[1])
plot_scale <- 10
xlim_coord <- c( min(my_tile$x), max(my_tile$x))
ylim_coord <- c( min(my_tile$y), max(my_tile$y))

# plot spatial
ggplot() + 
  # add raster
  geom_raster(data = my_tile, aes(x, y, fill = hex), alpha = 1) +
  coord_cartesian(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  coord_equal() +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # add emissions
  ggnewscale::new_scale_fill() +
  geom_sf(data = my_grid
          , aes(fill = as.numeric(get(colPol[j])))
          , colour = "transparent")  +
  viridis::scale_fill_viridis(option = "D"
                              ,direction = -1) +
  # add boundary
  ggnewscale::new_scale_color() +
  geom_sf(data = my_bound,aes(color = city_name)
          ,linetype = "dashed",alpha = 0.5, size = 0.35,fill = NA) +
  scale_color_manual(values = "black"
                     ,labels = "City \nboundary",)+
  coord_sf(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  # labels and theme
  labs(title = NULL
       , subtitle = paste0(colPol[j]," emissions")
       , fill = paste0(colPol[j]," (",getUnit_map,")")
       , color = NULL
       , x = NULL
       , y = NULL) +
  theme(legend.position = c(0.9,0.1)) + 
  theme_minimal() + 
  theme_void() +
  # map itens
  ggsn::north(data = my_grid,
              location = "topright",symbol = 12) +
  ggsn::scalebar(x.min = sf::st_bbox(sf::st_transform(my_bound,3857))[1] %>% as.numeric(),
                 x.max = sf::st_bbox(sf::st_transform(my_bound,3857))[3] %>% as.numeric(),
                 y.min = sf::st_bbox(sf::st_transform(my_bound,3857))[2] %>% as.numeric(),
                 y.max = sf::st_bbox(sf::st_transform(my_bound,3857))[4] %>% as.numeric(),
                 dist = plot_scale,
                 dist_unit = "km",st.size = 3,
                 st.bottom = FALSE, st.color = "black",
                 transform = FALSE, model = "WGS84") 
# save
ggplot2::ggsave(filename = sprintf("article/data/plots/spatial_%s.png",colPol[j]),
                scale = 1.0,width = 18,
                bg = "white",
                height = 20,units = "cm",dpi = 300)


# 10) Plot EF (@ speed = 34.12 kph) -----


# CETESB
my_ef_br_df <- gtfs2emis::ef_brazil(pollutant = c("PM10","CO2")
                                    ,veh_type = "BUS_URBAN_D"
                                    ,model_year = 2011
                                    ,as_list = FALSE)
my_ef_br_df <- as.data.frame(my_ef_br_df)
data.table::setDT(my_ef_br_df)[,source := "CETESB"]

# EMEP/EEA
my_ef_eu_df <- gtfs2emis::ef_europe(speed =  units::set_units(34.12,"km/h")
                                    ,veh_type = "Ubus Std 15 - 18 t"
                                    ,euro = "V"
                                    ,pollutant = c("PM10","CO2")
                                    ,fuel = "D"
                                    ,tech = "SCR"
                                    ,as_list = FALSE)
my_ef_eu_df[,source := "EEA"]

# EPA / MOVES
my_ef_moves_df <- gtfs2emis::ef_usa_moves(pollutant = c("PM10","CO2")
                                          ,model_year = 2011
                                          ,calendar_year = 2019
                                          ,speed = units::set_units(34.12,"km/h")
                                          ,fuel_type = "D"
                                          ,as_list = FALSE)
my_ef_moves_df[,source := "MOVES"]

# EMFAC
my_ef_emfac_df <- gtfs2emis::ef_usa_emfac(pollutant =  c("PM10","CO2")
                                          ,calendar_year = 2019
                                          ,fuel = "D"
                                          ,model_year = 2011
                                          ,speed = units::set_units(34.12,"km/h")
                                          ,as_list = FALSE)
my_ef_emfac_df[,source := "EMFAC"]

# rbind & process
my_ef_bind <- list(my_ef_br_df,my_ef_eu_df,my_ef_emfac_df,my_ef_moves_df) %>% 
  data.table::rbindlist(use.names = FALSE)

my_ef_plot <- data.table::melt.data.table(data = my_ef_bind
                                          ,id.vars = "source"
                                          ,measure.vars = c("PM10_2011","CO2_2011"))
my_ef_plot[, variable := gsub("_2011","",variable)]
my_ef_plot[, value := as.numeric(value)]
my_ef_plot[, label := "data source"]

# plot
ggplot(data = my_ef_plot) + 
  geom_bar(aes(y= value,x = source,fill = source),stat = "identity",position = "dodge")+
  facet_wrap(~variable,scales = "free")+
  labs(fill = NULL,x = NULL,y = "EF (g/km)")+
  viridis::scale_fill_viridis(discrete = TRUE,option = "H",alpha = 0.75)+
  theme_light()+
  guides(fill = "none")+
  scale_y_continuous(expand = expansion(mult = 0.05)) +
  theme(axis.text.x = element_text(size = 8)
        ,strip.text = element_text(colour = 'black'))

# save
ggsave(filename = "article/article_files/ef_plots.png"
       ,width = 36,height = 20,dpi = 300,units = "cm",scale = 0.5)

# 11) Plot EF @ different speeds for NOx------


# CETESB
my_ef_br_df <- gtfs2emis::ef_brazil(pollutant = c("PM10","CO2")
                                    ,veh_type = "BUS_URBAN_D"
                                    ,model_year = 2011
                                    ,as_list = FALSE)
my_ef_br_df <- gtfs2emis::ef_euro_scaled(ef_local = my_ef_br_df,
                                         speed =  units::set_units(seq(10,100,10),"km/h")
                                         ,veh_type = "Ubus Std 15 - 18 t"
                                         ,euro = "V"
                                         ,pollutant = c("PM10","CO2")
                                         ,fuel = "D"
                                         ,tech = "SCR")
my_ef_br_df <- my_ef_br_df$EF
my_ef_br_df[,source := "CETESB"]

# EMEP/EEA
my_ef_eu_df <- gtfs2emis::ef_europe(speed =  units::set_units(seq(10,100,10),"km/h")
                                    ,veh_type = "Ubus Std 15 - 18 t"
                                    ,euro = "V"
                                    ,pollutant = c("PM10","CO2")
                                    ,fuel = "D"
                                    ,tech = "SCR"
                                    ,as_list = FALSE)
my_ef_eu_df[,source := "EEA"]

# EPA / MOVES
my_ef_moves_df <- gtfs2emis::ef_usa_moves(pollutant = c("PM10","CO2")
                                          ,model_year = 2011
                                          ,calendar_year = 2019
                                          ,speed = units::set_units(seq(10,100,10),"km/h")
                                          ,fuel_type = "D"
                                          ,as_list = FALSE)
my_ef_moves_df[,source := "MOVES"]

# EMFAC
my_ef_emfac_df <- gtfs2emis::ef_usa_emfac(pollutant =  c("PM10","CO2")
                                          ,calendar_year = 2019
                                          ,fuel = "D"
                                          ,model_year = 2011
                                          ,speed = units::set_units(seq(10,100,10),"km/h")
                                          ,as_list = FALSE)
my_ef_emfac_df[,source := "EMFAC"]

# rbind & process
my_ef_bind <- list(my_ef_br_df,my_ef_eu_df,my_ef_emfac_df,my_ef_moves_df) %>% 
  data.table::rbindlist(use.names = FALSE)

my_ef_plot <- data.table::melt.data.table(data = my_ef_bind
                                          ,id.vars = "source"
                                          ,measure.vars = c("PM10_Euro_V","CO2_Euro_V"))
my_ef_plot[, variable := gsub("_Euro_V","",variable)]
my_ef_plot[, value := as.numeric(value)]
my_ef_plot[, label := "data source"]
my_ef_plot[, speed := rep(seq(10,100,10),8)]

# plot
ggplot(data = my_ef_plot) + 
  geom_line(aes(y= value,x = speed,color = source),lwd = 0.8)+
  geom_point(aes(y= value,x = speed,color = source))+
  facet_wrap(~variable,scales = "free",ncol = 2)+
  labs(color = "EF source",x = "Speed (km/h)",y = "EF (g/km)")+
  viridis::scale_color_viridis(discrete = TRUE,option = "H",alpha = 1)+
  theme_light()+
  scale_y_continuous(expand = expansion(mult = 0.05)) +
  theme(axis.text.x = element_text(size = 8)
        ,strip.text = element_text(colour = 'black'))

# save
ggsave(filename = "article/data/plots/ef_speed.png",scale = 1.3,
       width = 18,height = 8,units = "cm",dpi = 300)

# 12) Plot Fleet of São Paulo ----

# read
fleet_spo <- readr::read_rds("article/data/bra_spo_fleet.rds")

# adjust technology
fleet_spo[fuel %in% "Elétrico",fuel := "Electric"]
fleet_spo[fuel %in% "D",fuel := "Diesel"]
fleet_spo <- fleet_spo[,lapply(.SD,sum),by = .(year,type_name_eu,fuel),.SDcols = "N"]
fleet_spo[,type_name_eu_f := factor(x = type_name_eu
                                    ,levels = c("Ubus Midi <=15 t",
                                                "Ubus Std 15 - 18 t",
                                                "Ubus Artic >18 t")
                                    ,labels = c("Midi",
                                                "Standard",
                                                "Articulated"))]


# plot
ggplot(data = fleet_spo[fuel == "Diesel"]) + 
  geom_bar(aes(y= N,x = type_name_eu_f,fill = as.factor(year)),stat = "identity"
           ,position = "dodge",width = 0.75)+
  #facet_wrap(~fuel,scales = "free",ncol = 1)+
  labs(fill = "Year",x = "Urban bus category",y = "Total number")+
  viridis::scale_fill_viridis(discrete = TRUE,option = "H",alpha = 0.85
                              ,direction = -1)+
  theme_light()+
  #guides(fill = "none")+
  #scale_y_continuous(expand = expansion(mult = 0.05)) +
  theme(axis.text.x = element_text(size = 8)
        ,strip.text = element_text(colour = 'black'))
# plot1
ggplot(data = fleet_spo[fuel == "Diesel"]) + 
  geom_bar(aes(y= N,x = as.factor(year),fill = type_name_eu_f),stat = "identity"
           ,position = "dodge",width = 0.75)+
  #facet_wrap(~fuel,scales = "free",ncol = 1)+
  labs(x = "Year",fill = "Urban bus \ncategory",y = "Total number")+
  scale_fill_manual(values = viridis::turbo(n = 9,alpha = 0.85)[c(1,3,9)])+
  #viridis::scale_fill_viridis(discrete = TRUE,option = "H",alpha = 0.85
  #                            ,direction = -1)+
  #viridis::scale_fill_viridis(discrete = TRUE,option = "G",alpha = 0.85
  #                            ,direction = +1)+
  theme_light()+
  #guides(fill = "none")+
  #scale_y_continuous(expand = expansion(mult = 0.05)) +
  theme(axis.text.x = element_text(size = 8)
        ,strip.text = element_text(colour = 'black'))
# save
ggsave(filename = "article/data/plots/fleet_sp.png",scale = 1.3,
       width = 18,height = 8,units = "cm",dpi = 300)
