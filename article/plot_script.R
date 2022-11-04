# 0) Load libraries ----

rm(list=ls())
gc(reset = TRUE)

library(devtools)

#devtools::load_all()
#devtools::document()
#devtools::load_all()
#devtools::install()
#library(gtfs2emis)

library(aopdata) # devtools::install_github("ipeaGIT/aopdata",subdir = "r-package")
library(data.table)
library(magrittr)
library(ggplot2)
library(sfheaders)
library(furrr)
library(sf)
library(clipr)
library(gtfs2gps) #devtools::install_github("ipeaGIT/gtfs2gps")
library(gtfstools)  #devtools::install_github("ipeaGIT/gtfstools")
#library(gtfs2emis) # devtools::install(build_vignettes = FALSE,build = FALSE)
devtools::load_all()

# install font
# remotes::install_version("Rttf2pt1", version = "1.3.8")
library(extrafont) 
library(extrafontdb) 
library(fontcm)
library(extrafont) 
extrafont::font_import()
extrafont::loadfonts(device = "win")
extrafont::fonts()
#extrafont::font_install('fontcm')


# 1)  Prep GTFS ----

# read gtfs
# spo_gtfs <- gtfstools::read_gtfs("../../Pessoal/IPEA/gtfs2gps/data-raw/gtfs/gtfs_spo_sptrans_2019-10.zip")
spo_gtfs <- gtfstools::read_gtfs("../../../data-raw/gtfs/bra_spo/gtfs_spo_sptrans_2019-06.zip")

# filter by bus route
spo_gtfs <- gtfstools::filter_by_route_type(spo_gtfs,route_type = 3)

#' so restrictive
object.size(spo_gtfs)
# set dirs

getwd()
dir.create("article")
dir.create("article/data/")

# save gtfs
gtfstools::write_gtfs(spo_gtfs,"article/data/gtfs_spo_sptrans_prep.zip")

# b) Transport model ----
rm(list=ls())
gc(reset = TRUE)

# read gtfs
spo_gtfs <- gtfstools::read_gtfs("article/data/gtfs_spo_sptrans_prep.zip")

spo_gtfs$trips$trip_id %>% uniqueN()      # 2271
spo_gtfs$trips$shape_id %>% uniqueN()     # 2271
spo_gtfs$trips$trip_id  %>% uniqueN()     # 2271
spo_gtfs$shapes$shape_id  %>% uniqueN()   # 2271
spo_gtfs$stop_times$trip_id %>% uniqueN() # 2271
spo_gtfs$trips$shape_id  %>% uniqueN()    # 2271
spo_gtfs$trips %>% nrow() # 2271

# generate gps

devtools::load_all()
transport_model(gtfs_data = spo_gtfs
                ,min_speed = 2
                ,max_speed = 80
                ,new_speed = NULL
                ,parallel = TRUE
                ,ncores = 37
                ,spatial_resolution = 100
                ,output_path = "article/data"
                ,continue = TRUE)


## general statistics ----
rm(list=ls())
gc(reset = TRUE)
spo_gtfs <- gtfstools::read_gtfs("article/data/gtfs_spo_sptrans_prep.zip")

#' number of routes

uniqueN(spo_gtfs$routes$route_id)
#' trips per day


files_gps <- list.files("article/data/gps_line/",full.names = TRUE)
files_gps_names <- list.files("article/data/gps_line/",full.names = FALSE)

future::plan(strategy =  "multisession",workers = 35)

gen_stats <- furrr::future_map(seq_along(files_gps)
                               ,
                               function(i){ # i = 1
                                 
                                 #message(paste0("Stats of Linestring file '",files_gps_names[i],"'"))
                                 
                                 tmp_gps <- readr::read_rds(files_gps[i])
                                 data.table::setDT(tmp_gps)
                                 
                                 #
                                 return(tmp_gps)
                                 #  stats_dt <- data.table::data.table(
                                 #    "shape_id" = gsub(".rds","",files_gps_names[i])
                                 #    ,"number_trips" = data.table::uniqueN(tmp_gps$trip_id)
                                 #    ,"number_stop_id" = data.table::uniqueN(tmp_gps$from_stop_id)
                                 #    ,"number_stop_id_per_trip" = data.table::uniqueN(tmp_gps$from_stop_id) / data.table::uniqueN(tmp_gps$trip_id)
                                 #    ,"Q25" = quantile(tmp_gps$speed,.25,na.rm = TRUE)
                                 #    ,"Q50" = quantile(tmp_gps$speed,.50,na.rm = TRUE)
                                 #    ,"Q75" = quantile(tmp_gps$speed,.75,na.rm = TRUE)
                                 #    ,"dist" = units::set_units(sum(tmp_gps$dist),"km")
                                 #    ,"VTK_per_trip" = units::set_units(sum(tmp_gps$dist) / data.table::uniqueN(tmp_gps$trip_id),"km")
                                 #    ,"total_time" = units::set_units(sum(tmp_gps$dist/tmp_gps$speed,na.rm=TRUE),"h")
                                 #  )
                                 # 
                                 # 
                                 # 
                                 # return(stats_dt)
                               }
) %>% data.table::rbindlist(use.names = TRUE)

gen_stats$dist %>% sum()

gen_stats[spo_gtfs$trips, on = "shape_id",route_id := i.route_id]
stats_dt <- data.table::data.table(
  #"shape_id" = gsub(".rds","",files_gps_names[i])
  "number_trips" = data.table::uniqueN(gen_stats$trip_id)
  ,"number_stop_id" = data.table::uniqueN(gen_stats$from_stop_id)
  ,"number_stop_id_per_trip" = data.table::uniqueN(gen_stats$from_stop_id) /
    data.table::uniqueN(gen_stats$trip_id)
  ,"Q25" = Hmisc::wtd.quantile(x = gen_stats$speed,weights = as.numeric(gen_stats$dist),probs = .25,na.rm = TRUE)
  ,"Q50" = Hmisc::wtd.quantile(x = gen_stats$speed,weights = as.numeric(gen_stats$dist),probs = .50,na.rm = TRUE)
  ,"Q75" = Hmisc::wtd.quantile(x = gen_stats$speed,weights = as.numeric(gen_stats$dist),probs = .75,na.rm = TRUE)
  ,"VTK" = units::set_units(sum(gen_stats$dist),"km")
  ,"VTK_per_trip" = units::set_units(sum(gen_stats$dist) /
                                       data.table::uniqueN(gen_stats$trip_id),"km")
  ,"total_time" = units::set_units(sum(gen_stats$dist/gen_stats$speed,na.rm=TRUE),"h")
)

stats_dt
gen_stats[,VTK := as.numeric(VTK)]

gen_stats[,{
  list(
    "number_trips_per_route" = data.table::uniqueN(trip_id) / 
      data.table::uniqueN(route_id),
    "number_routes" = data.table::uniqueN(route_id),
    "number_stop_id" = data.table::uniqueN(from_stop_id),
    "number_stop_id_per_route" = data.table::uniqueN(from_stop_id) / 
      data.table::uniqueN(route_id),
    
    "Q25" = weighted.mean(speed,as.numeric(dist)),
    "Q50" = weighted.mean(speed,as.numeric(dist)),
    "Q75" = weighted.mean(speed,as.numeric(dist)),
    "VTK" = sum(dist),
    
    "VTK_per_route" = sum(dist) /data.table::uniqueN(route_id),
    
    "total_time" = sum(units::set_units(sum(
      dist/speed
      ,na.rm=TRUE),"h"))
  )}]

gen_stats
#readr::write_rds(gen_stats,"article/data/general_stats.rds")
gen_stats[,total_time := as.numeric(total_time) ]
gen_stats[,lapply(.SD,weighted.mean,as.numeric(VTK))
          ,.SDcols = c("Q25","Q50","Q75")]
gen_stats[,lapply(.SD,sum)
          ,.SDcols = c("number_trips","total_time","VTK")]
gen_stats$number_stop_id %>% sum()


sum(gen_stats$number_stop_id)/nrow(gen_stats)


## f) Plot GTFS trips -----
# Needs to Run Code item b) to reproduce this section

# read Tiles & Boundaries
my_tile <- readr::read_rds("../../../data-raw/maptiles/mapbox/bra_spo.rds")
my_bound <- readr::read_rds("../../../data-raw/boundaries/bra_spo/bra_spo.rds")

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
my_bound <- sf::st_transform(my_bound,3857)
gps_lines_sf$label <- "Bus routes"

# plot
map_scale <- as.numeric(sf::st_bbox(my_bound)[3]) -  
  as.numeric(sf::st_bbox(my_bound)[1])
plot_scale <- 10
xlim_coord <- c( min(my_tile$x), max(my_tile$x))
ylim_coord <- c( min(my_tile$y), max(my_tile$y))

# plot spatial
p <- ggplot() + 
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
          ,alpha = 1, size = 0.25,fill = NA) +
  scale_color_manual(values = "red",name = NULL)+
  # add boundary
  ggnewscale::new_scale_color() +
  geom_sf(data = my_bound,aes(color = city_name)
          ,linetype = "solid",alpha = 0.75, size = 0.25,fill = NA) +
  scale_color_manual(values = "black",name = NULL
                     ,labels = "City boundary")+
  coord_sf(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  # labels and theme
  labs(title = NULL
       , color = NULL
       , x = NULL
       , y = NULL) +
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
                 #text = element_text(family = "LM Roman 10"),
                 family = "LM Roman 10",
                 fontface = "plain",
                 dist_unit = "km",st.size = 3,
                 st.bottom = FALSE, st.color = "black",
                 transform = FALSE, model = "WGS84") +
  theme(legend.position = c(0.75,0.2),
        text = element_text(family = "LM Roman 10"),
        legend.box.background = element_rect(fill = "white",color = "white"),
        legend.box.margin = margin(0,0,0,0, "pt")) 
#p
# save
ggplot2::ggsave(plot = p,
                filename = "article/data/plots/basic_sptrans.png",
                scale = 0.6,width = 14,
                bg = "white",
                height = 20,units = "cm",dpi = 300)



# 2) Emissions -----
# obs: this needs to read fleet first
#
rm(list=ls())
gc(reset=TRUE)
#devtools::load_all()
#devtools::document()
#devtools::load_all()

# Read fleet 
fleet_spo <- readr::read_rds("../../../data/fleet/bra_spo/bra_spo.rds")
# adjust technology
fleet_spo <- fleet_spo[fuel %in% "D",] # remove electric
fleet_spo[euro %in% "V", Technology := "SCR"]
fleet_spo[euro %in% "III",Technology := "-"]
fleet_spo <- fleet_spo[,list(N = sum(N))
                       ,by = .(year,fuel,euro,type_name_eu
                               ,Technology)]
fleet_spo[,fleet_composition := N/sum(N)]

setnames(fleet_spo,"Technology","tech")
setnames(fleet_spo,"year","model_year")
setnames(fleet_spo,"type_name_eu","veh_type")
dir.create("article/data/emissions/")

tp <- readr::read_rds("article/data/gps_line/41432.rds")
emission_model(tp_model = tp
               ,ef_model = "ef_europe_emep"
               ,pollutant = c("CO2","NOx","PM10","CH4")
               ,fleet_data = fleet_spo
               ,parallel = FALSE
               #,parallel = TRUE
               #,ncores = 35
               ,output_path = "article/data/emissions"
               ,continue = FALSE
               ,quiet = TRUE)


future::plan(strategy =  "multisession",workers = 35)

# 4) Processing -----
## a) Time processing -----
rm(list=ls())
gc(reset=TRUE)
dir.create("article/data/emi_time/")
devtools::load_all()

# list-files
files_gps <- list.files(path = 'article/data/emissions/',full.names = TRUE)
files_gps_names <- list.files(path = 'article/data/emissions/',full.names = FALSE)

oplan <- future::plan(strategy =  "multisession",workers = 35)
requiredPackages = c('data.table', 'sf', 'units')

time_processing <- furrr::future_map(seq_along(files_gps),function(j){ # j = 1
  
  
  # message(paste0("Emi time of file '",files_gps_names[i],"'"))
  
  #message(i)
  temp_emi <- readr::read_rds(files_gps[j])
  emi_post <- emis_summary(emi_list = temp_emi
                           ,by = "time")
  #emi_post <- units::drop_units(emi_post)
  readr::write_rds(x = emi_post
                   ,file = paste0("article/data/emi_time/",files_gps_names[j])
                   ,compress = 'gz')
  return(NULL)
},.options = furrr::furrr_options(seed=TRUE
                                  ,packages = requiredPackages))

## b) Spatial post-processing-----
rm(list=ls())
gc(reset = TRUE)
devtools::load_all()

dir.create("article/data/emi_grid/")

# list-files
files_gps <- list.files(path = 'article/data/emissions/',full.names = TRUE)
files_gps_names <- list.files(path = 'article/data/emissions/',full.names = FALSE)
temp_grid <- readr::read_rds("../../../data/grid/res9/bra_spo.rds")
#temp_grid <- aopdata::read_grid(city = "spo")
temp_grid <-  sf::st_transform(temp_grid,"+proj=utm +zone=23 +ellps=WGS84 +south +units=m")

spatial_f <- function(i){ # i = 2
  #message(paste0("Emi grid of file '",files_gps_names[i],"'"))
  
  temp_emi <- readr::read_rds(files_gps[i])
  
  #  transform
  temp_emi$tp_model <- temp_emi$tp_model %>% 
    sf::st_transform("+proj=utm +zone=23 +ellps=WGS84 +south +units=m")
  
  # grid
  output_grid <-  emis_grid(emi_list =temp_emi
                            ,grid = temp_grid
                            ,quiet = TRUE
                            ,aggregate = TRUE)
  # write
  readr::write_rds(x = output_grid
                   ,file = paste0("article/data/emi_grid/",files_gps_names[i])
                   ,compress = 'gz')
  return(NULL) 
}


oplan <- future::plan(strategy =  "multisession",workers = 35)
requiredPackages = c('data.table', 'sf', 'units')

spatial_processing <- furrr::future_map(seq_along(files_gps)
                                        ,spatial_f 
                                        ,.options = furrr::furrr_options(
                                          seed=TRUE
                                          ,packages = requiredPackages))

## b) Spatial / TIME post-processing-----
rm(list=ls())
gc(reset = TRUE)
devtools::load_all()

dir.create("article/data/emi_grid_time/")

# list-files
files_gps <- list.files(path = 'article/data/emissions/',full.names = TRUE)
files_gps_names <- list.files(path = 'article/data/emissions/',full.names = FALSE)
temp_grid <- readr::read_rds("../../../data/grid/res9/bra_spo.rds")

spatial_f <- function(i){ # i = 2
  #message(paste0("Emi grid of file '",files_gps_names[i],"'"))
  
  temp_emi <- readr::read_rds(files_gps[i])
  
  #  transform
  temp_emi$tp_model <- temp_emi$tp_model %>% 
    sf::st_transform("+proj=utm +zone=23 +ellps=WGS84 +south +units=m")
  
  # grid
  output_grid <-  emis_grid(emi_list =temp_emi
                            ,grid = temp_grid
                            ,quiet = TRUE
                            ,aggregate = TRUE
                            ,time_resolution = "hour")
  # write
  readr::write_rds(x = output_grid
                   ,file = paste0("article/data/emi_grid_time/",files_gps_names[i])
                   ,compress = 'gz')
  return(NULL) 
}


oplan <- future::plan(strategy =  "multisession",workers = 35)
requiredPackages = c('data.table', 'sf', 'units')

spatial_processing <- furrr::future_map(seq_along(files_gps)
                                        ,spatial_f 
                                        ,.options = furrr::furrr_options(
                                          seed=TRUE
                                          ,packages = requiredPackages))

## c) Vehicle-age post-processing-----
rm(list=ls())
gc(reset = TRUE)
devtools::load_all()

dir.create("article/data/emi_age/")

# list-files
files_gps <- list.files(path = 'article/data/emissions/',full.names = TRUE)
files_gps_names <- list.files(path = 'article/data/emissions/',full.names = FALSE)

f_veh_agr <- function(i){ # i = 1
  # read
  temp_emi <- readr::read_rds(files_gps[i])
  # process
  temp_emi_dt <- emis_summary(emi_list = temp_emi
                              ,by = "vehicle"
                              ,veh_vars = c("veh_type","euro"
                                            ,"fuel","tech","fleet_composition"
                              ))
  temp_emi_dt$VTK <- sum(units::set_units(temp_emi$tp_model$dist,"km"))
  # write
  readr::write_rds(x = temp_emi_dt
                   ,file = paste0("article/data/emi_age/",files_gps_names[i])
                   ,compress = 'gz')
  return(NULL)
}

future::plan(strategy =  "multisession",workers = 35)
requiredPackages = c('data.table', 'sf', 'units')
furrr::future_map(seq_along(files_gps)
                  ,f_veh_agr 
                  ,.options = furrr::furrr_options(
                    seed=TRUE
                    ,packages = requiredPackages))


# 5) Plot ------
## a) Temporal emissions ---------

# import fonts
# extrafont::font_import(paths = "C://Users//B138750230//Downloads//Latin-Modern-Roman-fontfacekit//web fonts/")

# create folder
rm(list=ls())
gc(reset = TRUE)
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


# total 
tmp_my_time[,sum(emi),by = .(pollutant)]
# aggregate by summing
my_time <- tmp_my_time[,sum(emi),by = .(timestamp_hour,pollutant)]
my_time <- my_time[pollutant == "PM10"]
data.table::setkeyv(my_time,cols = c("pollutant","timestamp_hour"))
my_time[,V2 := to_compartible_units(V1)]


# plot time-

ggplot(data = my_time) + 
  geom_bar(aes(x = timestamp_hour, y = as.numeric(V2), fill = as.numeric(V2)),
           stat = "identity")+
  labs(title = NULL,
       x = "Hour",
       y = expression(PM[10][] (kg)))+
  #y = paste0("PM10 (",getUnit_graphic,")")) + 
  viridis::scale_fill_viridis(option = "D",direction = -1)+
  theme_minimal()+
  theme(legend.position = "none",
        text = element_text(family = "LM Roman 10"))

ggplot2::ggsave(filename = "article/data/plots/temporal_PM10.png",
                scale = .55,bg = "white",
                width = 18,height = 10,units = "cm",dpi = 300)

## b) Plot EF | MEF by age----------------
rm(list=ls())
gc(reset = TRUE)

# list-files
files_gps <- list.files(path = 'article/data/emi_age/',full.names = TRUE)
files_gps_names <- list.files(path = 'article/data/emi_age/',full.names = FALSE)

# read files
tmp_my_age <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist()

#tmp_my_age[,VTK := units::drop_units(VTK)]
tmp_my_age[,fleet_composition := as.numeric(as.character(fleet_composition))]
tmp_my_age[,single_VTK := VTK * fleet_composition]

# aggregate by sum
tmp_my_age[,list(
  emi = sum(emi)
  ,single_VTK = sum(single_VTK)
  ,avg_ef = sum(emi)/sum(single_VTK)
),by = .(pollutant)]

# aggregate by sum
tmp_my_age <- tmp_my_age[,list(
  emi = sum(emi)
  ,single_VTK = sum(single_VTK)
  ,avg_ef = sum(emi)/sum(single_VTK)
),by = .(veh_type,euro ,pollutant)]

# factors
tmp_my_age[,model_year_f := factor(x = euro ,levels = c("III","V"))]
tmp_my_age[, pollutant_f := dplyr::recode_factor(pollutant
                                                 , `CO2` = "CO[2]"
                                                 , `PM10` = "PM[10]"
                                                 , `CH4` = "CH[4]"
                                                 , `NOx` = "NO[X]")]
## c) emissions_age ----

ggplot(data = tmp_my_age[pollutant != "CH4"]) + 
  geom_bar(aes(y= as.numeric(avg_ef)
               ,x = model_year_f
               ,fill = veh_type)
           ,stat = "identity"
           ,position = "dodge")+
  facet_wrap(~pollutant_f
             ,scales = "free"
             , label = "label_parsed") +
  labs(fill = "Vehicle \ncategory",x = "Euro"
       ,y = "Marginal \nEmission Factor (g/km)")+
  scale_fill_manual(values = viridis::rocket(4))+
  #scale_x_discrete(breaks = c(seq(2008,2019,3),2019),
  #                 labels = c(seq(2008,2019,3),2019))+
  theme_light()+
  scale_y_continuous(expand = expansion(mult = 0.05)) +
  theme(axis.text.x = element_text(size = 8)
        ,text = element_text(family = "LM Roman 10")
        ,strip.text = element_text(colour = 'black')
        ,legend.position = "bottom"
  )

ggsave(filename = "article/data/plots/emissions_age.png"
       ,width = 36,height = 17.5,dpi = 300,units = "cm",scale = 0.5)

###
ggplot(data = tmp_my_age[pollutant  != "CH4"]) + 
  geom_line(aes(y= as.numeric(avg_ef),x = model_year_f,color = veh_type
                ,group = veh_type))+
  geom_point(aes(y= as.numeric(avg_ef),x = model_year_f,color = veh_type))+
  facet_wrap(~pollutant_f
             ,scales = "free"
             , label = "label_parsed") +
  labs(color = "Vehicle \ncategory",x = "Year of fleet",y = "Marginal \nEmission Factor (g/km)")+
  scale_color_manual(values = viridis::rocket(4))+
  scale_x_discrete(breaks = c(seq(2008,2019,3),2019),
                   labels = c(seq(2008,2019,3),2019))+
  theme_light()+
  scale_y_continuous(expand = expansion(mult = 0.05)) +
  theme(axis.text.x = element_text(size = 8)
        ,text = element_text(family = "LM Roman 10")
        ,strip.text = element_text(colour = 'black')
        ,legend.position = "bottom")

ggsave(filename = "article/data/plots/emissions_age_v1.png"
       ,width = 40,height = 19,dpi = 300,units = "cm",scale = 0.405)


## d) ef_age ----

ggplot(data = tmp_my_age) + 
  geom_bar(aes(y= as.numeric(mef),x = model_year_f ,fill = veh_type)
           ,stat = "identity",position = "dodge")+
  facet_wrap(~pollutant_meff
             ,scales = "free"
             , label = "label_parsed") +
  labs(fill = NULL,x = "Year of fleet",y = "Marginal Emission Factors (g/km)")+
  scale_fill_manual(values = viridis::cividis(4))+
  scale_x_discrete(breaks = c(seq(2008,2019,3),2019),
                   labels = c(seq(2008,2019,3),2019))+
  theme_light()+
  scale_y_continuous(expand = expansion(mult = 0.05)) +
  theme(axis.text.x = element_text(size = 8)
        ,text = element_text(family = "LM Roman 10")
        ,strip.text = element_text(colour = 'black')
        ,legend.position = c(0.875,0.9))

# save
ggsave(filename = "article/data/plots/MEF_age.png"
       ,width = 36,height = 30,dpi = 300,units = "cm",scale = 0.5)

## e1) zoom emissions ----



rm(list=ls())
library(magrittr)
gc(reset = TRUE)

# read Tiles & Boundaries
my_tile <- readr::read_rds("../../../data-raw/maptiles/mapbox/bra_spo.rds")
my_bound <- readr::read_rds("../../../data-raw/boundaries/bra_spo/bra_spo.rds")


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
colPol <- names(tmp_my_grid)[!(names(tmp_my_grid) %in% c("id_hex","h3_resolution","geometry"))]

my_grid <- tmp_my_grid[,(colPol) := lapply(.SD,sum),by = id_hex,.SDcols = colPol]
my_grid <- my_grid[,.SD[1],by = id_hex]
my_grid <- my_grid[,(colPol) := lapply(.SD,to_compartible_units),.SDcols = colPol]
my_grid <- sf::st_as_sf(my_grid) %>% sf::st_transform(3857)


# plot
getUnit_map <- c("t","g") # CO2, PM10

map_scale <- as.numeric(sf::st_bbox(my_grid)[3]) -  
  as.numeric(sf::st_bbox(my_grid)[1])
plot_scale <- 10
xlim_coord <- c( min(my_tile$x), max(my_tile$x))
ylim_coord <- c( min(my_tile$y), max(my_tile$y))

# organize data to plot
bind_grid <- data.table::melt(data = my_grid
                              , variable.name = "pollutant"
                              , id.vars = c("id_hex","geometry","h3_resolution"))
bind_grid <- sf::st_as_sf(bind_grid)
bind_grid <- sf::st_set_crs(x = bind_grid,value = sf::st_crs(my_grid))


get_bbox <- c(-46.63319,-23.54904) %>% 
  sf::st_point(.,dim = "XY") %>% 
  sf::st_sfc() %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_transform(3857) %>% 
  sf::st_buffer(4000) %>% 
  sf::st_coordinates() %>% 
  as.data.frame() %>% 
  data.table::setDT() %>% 
  .[,list(X =  c(min(X),max(X),max(X),min(X))
          ,Y = c(min(Y) + 1000,min(Y) + 1000,max(Y) - 1000,max(Y) - 1000))] %>% 
  sfheaders::sf_polygon(.,x = "X",y = "Y") %>% 
  sf::st_set_crs(x = .,3857)

intersect_bound <- sf::st_intersection(x = bind_grid
                                       ,y = get_bbox)

get_bbox_scale <- c(-46.63319,-23.54904) %>% 
  sf::st_point(.,dim = "XY") %>% 
  sf::st_sfc() %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_transform(3857) %>% 
  sf::st_buffer(3500) %>% 
  sf::st_coordinates() %>% 
  as.data.frame() %>% 
  data.table::setDT() %>% 
  .[,list(X =  c(min(X),max(X),max(X),min(X))
          ,Y = c(min(Y) + 1000,min(Y) + 1000,max(Y) - 1000,max(Y) - 1000))] %>% 
  sfheaders::sf_polygon(.,x = "X",y = "Y") %>% 
  sf::st_set_crs(x = .,3857)


lim_coord_zoom <- get_bbox %>%
  sf::st_coordinates() %>% 
  as.data.frame() %>% 
  data.table::setDT() %>% 
  .[,list(X =  c(min(X),max(X))
          ,Y = c(min(Y),max(Y)))]

lim_coord_zoom

lim_fill_pm10 <- bind_grid[bind_grid$pollutant == "PM10",] %>% 
  data.table::setDT() %>% 
  .[,list(min(value),max(value))] %>% 
  as.numeric() %>% as.vector()

lim_fill_pm10

lim_fill_co2 <- bind_grid[bind_grid$pollutant == "CO2",] %>% 
  data.table::setDT() %>% 
  .[,list(min(value),max(value))] %>% 
  as.numeric() %>% as.vector()

# gtfs
spo_gtfs <- gtfstools::read_gtfs("article/data/gtfs_spo_sptrans_prep.zip")
gps_lines_sf <- gtfstools::convert_shapes_to_sf(gtfs = spo_gtfs)
stops_sf <- gtfstools::convert_stops_to_sf(gtfs = spo_gtfs)


intersect_lines_sf <- sf::st_intersection(x = sf::st_transform(gps_lines_sf,3857)
                                          ,y = get_bbox)
intersect_stops_sf <- sf::st_intersection(x = sf::st_transform(stops_sf,3857)
                                          ,y = get_bbox)
rm(gps_lines_sf)
rm(stops_sf)
rm(spo_gtfs)

##### ggplot PM10 ----
pm10_plot <-  ggplot(bind_grid[bind_grid$pollutant == "PM10",]) + 
  # add raster
  geom_raster(data = my_tile, aes(x, y, fill = hex), alpha = 1) +
  coord_cartesian(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  coord_equal() +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # add emissions
  ggnewscale::new_scale_fill() +
  geom_sf(aes(fill = as.numeric(value))
          , colour = "transparent")  +
  viridis::scale_fill_viridis(option = "D"
                              ,direction = -1)+
  #viridis::scale_fill_viridis(option = "D"
  #                            ,direction = -1
  #                            , breaks = c(0,10,100,200)
  #                            ,labels = c(0,10,100,200)
  #                            ,trans = "pseudo_log"
  #) +
  ## add boundary
  ggnewscale::new_scale_color() +
  geom_sf(data = my_bound,color = "black"
          ,linetype = "solid",alpha = 0.5, size = 0.15,fill = NA) +
  # facet_wrap(~pollutant)+
  scale_color_manual(values = "black"
                     ,labels = "City \nboundary",)+
  coord_sf(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  # labels and theme
  labs(title = NULL
       #, subtitle =  expression(PM[10][] emissions)
       , subtitle =  expression(PM[10][])
       , fill =  expression(PM[10][] (g))
       , color = NULL
       , x = NULL
       , y = NULL) +
  theme_minimal() + 
  theme_void() +
  # map itens
  ggsn::north(data = my_grid,
              location = "topright",symbol = 12) +
  ggsn::scalebar(x.min = sf::st_bbox(sf::st_transform(my_bound,3857))[1] %>% as.numeric()
                 , x.max = sf::st_bbox(sf::st_transform(my_bound,3857))[3] %>% as.numeric()
                 , y.min = sf::st_bbox(sf::st_transform(my_bound,3857))[2] %>% as.numeric()
                 , y.max = sf::st_bbox(sf::st_transform(my_bound,3857))[4] %>% as.numeric()
                 , dist = plot_scale/2
                 , family = "LM Roman 10"
                 , fontface = "plain"
                 , dist_unit = "km"
                 , st.size = 2.5
                 , location = "bottomright"
                 , st.dist = 0.035
                 , st.bottom = FALSE
                 , st.color = "black"
                 , transform = FALSE
                 , model = "WGS84") +
  theme(legend.position = c(0.8,0.325),
        text = element_text(family = "LM Roman 10"),
        legend.box.background = element_rect(fill = "white",color = "white"),
        legend.key.size = ggplot2::unit(0.03250,"npc"),
        legend.box.margin = margin(3,3,3,3, "pt")) 

pm10_plot
#### ggplot CO2-------

co2_plot <-  ggplot(bind_grid[bind_grid$pollutant == "CO2",]) + 
  # add raster
  geom_raster(data = my_tile, aes(x, y, fill = hex), alpha = 1) +
  coord_cartesian(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  coord_equal() +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # add emissions
  ggnewscale::new_scale_fill() +
  geom_sf(aes(fill = as.numeric(value))
          , colour = "transparent")  +
  viridis::scale_fill_viridis(option = "E"
                              ,direction = -1)+
  #viridis::scale_fill_viridis(option = "E"
  #                            ,direction = -1
  #                            , breaks = 1:5
  #                            ,labels = 1:5
  #                            ,trans = "pseudo_log"
  #) +
  
  ## add boundary
  ggnewscale::new_scale_color() +
  geom_sf(data = my_bound,color = "black"
          ,linetype = "solid",alpha = 0.5, size = 0.15,fill = NA) +
  # facet_wrap(~pollutant)+
  scale_color_manual(values = "black"
                     ,labels = "City \nboundary",)+
  coord_sf(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  # labels and theme
  labs(#title = "Total emissions"
    subtitle = expression(CO[2][])
    , fill =  expression(CO[2][] (t))
    , color = NULL
    , x = NULL
    , y = NULL) +
  theme(legend.position = c(0.9,0.1)) + 
  theme_minimal() + 
  theme_void() +
  theme(legend.position = c(0.8,0.3),
        text = element_text(family = "LM Roman 10"),
        legend.box.background = element_rect(fill = "white",color = "white"),
        legend.key.size = ggplot2::unit(0.035,"npc"),
        legend.box.margin = margin(3,3,3,3, "pt")) 

co2_plot
##### ggplot zoom pm10 -----

pm10_zoom <- ggplot(intersect_bound[intersect_bound$pollutant == "PM10",]) + 
  # data
  geom_raster(data = my_tile, aes(x, y, fill = hex), alpha = 1) +
  scale_fill_identity() +
  # add emissions
  ggnewscale::new_scale_fill() +
  geom_sf(aes(fill = as.numeric(value))
          , colour = "transparent")  +
  viridis::scale_fill_viridis(option = "D"
                              ,direction = -1
                              , limits = lim_fill_pm10) +
  #viridis::scale_fill_viridis(option = "D"
  #                            ,direction = -1
  #                            ,labels = scales::number_format()
  #                            ,trans = "pseudo_log"
  #                            , limits = lim_fill_pm10
  #) +
  geom_sf(data = intersect_lines_sf,alpha = 0.15,size = 0.25,color = "black")+
  # limits
  coord_sf(xlim = lim_coord_zoom$X, ylim = lim_coord_zoom$Y,expand = FALSE) +
  # labels 
  labs(title = NULL
       , color = NULL
       , x = NULL
       , y = NULL) +
  # theme
  theme(axis.ticks.length = unit(0, "pt"), axis.ticks.length.x = NULL, 
        axis.ticks.length.x.top = NULL, axis.ticks.length.x.bottom = NULL, 
        axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL, 
        axis.ticks.length.y.right = NULL,
        axis.text = element_blank(), axis.title = element_blank(),
        legend.position = "none")+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 0.75))+
  ggsn::north(data = get_bbox_scale,
              location = "topright",symbol = 12) +
  ggsn::scalebar(x.min = lim_coord_zoom$X[1],
                 x.max = lim_coord_zoom$X[2]-350,
                 y.min = lim_coord_zoom$Y[1]+350,
                 y.max = lim_coord_zoom$Y[2],
                 st.dist = 0.075,
                 dist = 1,
                 family = "LM Roman 10",
                 fontface = "plain",
                 dist_unit = "km"
                 ,st.size = 2.5,
                 location = "bottomright",
                 st.bottom = FALSE, st.color = "black",
                 transform = FALSE, model = "WGS84")

pm10_zoom

##### ggplot zoom co2 -----

co2_zoom <- ggplot(intersect_bound[intersect_bound$pollutant == "CO2",]) + 
  # data
  geom_raster(data = my_tile[my_tile$x > min(lim_coord_zoom$X) & 
                               my_tile$x < max(lim_coord_zoom$X) & 
                               my_tile$y > min(lim_coord_zoom$Y) & 
                               my_tile$y < max(lim_coord_zoom$Y),]
              , aes(x, y, fill = hex), alpha = 1) +
  scale_fill_identity() +
  # add emissions
  ggnewscale::new_scale_fill() +
  geom_sf(aes(fill = as.numeric(value))
          , colour = "transparent")  +
  viridis::scale_fill_viridis(option = "E"
                              ,direction = -1
                              , limits = lim_fill_co2) +
  #viridis::scale_fill_viridis(option = "E"
  #                            ,direction = -1
  #                            ,labels = scales::number_format()
  #                            ,trans = "pseudo_log"
  #                            , limits = lim_fill_co2) +
  geom_sf(data = intersect_lines_sf,alpha = 0.15,size = 0.25,color = "black")+
  # limits
  coord_sf(xlim = lim_coord_zoom$X, ylim = lim_coord_zoom$Y,expand = FALSE) +
  # labels 
  labs(title = NULL
       , color = NULL
       , x = NULL
       , y = NULL) +
  # theme
  theme(axis.ticks.length = unit(0, "pt"), axis.ticks.length.x = NULL, 
        axis.ticks.length.x.top = NULL, axis.ticks.length.x.bottom = NULL, 
        axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL, 
        axis.ticks.length.y.right = NULL,
        axis.text = element_blank(), axis.title = element_blank(),
        legend.position = "none")+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 0.75))

#### patchwork ----
library(patchwork)

pm10_plot_squared <- pm10_plot  +
  ggnewscale::new_scale_color()+
  geom_sf(data = get_bbox,color = "red",fill = NA)

pm10_final <- pm10_plot_squared / pm10_zoom

co2_plot_squared <- co2_plot  +
  ggnewscale::new_scale_color()+
  geom_sf(data = get_bbox,color = "red",fill = NA)

co2_final <- co2_plot_squared / co2_zoom


final <- co2_final | pm10_final

#final

ggplot2::ggsave(plot = final
                , filename = "article/data/plots/spatial_co2_pm10_v1_linear.png"
                , scale = 0.7
                , width = 17
                , bg = "white"
                , height = 19.5
                , units = "cm"
                , dpi = 300)

## e2) Spatial HOUR emissions -----
#### ggplot v1-----
rm(list=ls())
library(magrittr)
gc(reset = TRUE)

to_compartible_units <- function(i){  #i = my_time$CO2
  pol_digits <- floor(log10(as.numeric(i))) + 1
  if(median(pol_digits,na.rm = TRUE) <= 2){return(units::set_units(i,"g"))}
  if(median(pol_digits,na.rm = TRUE) <= 5){return(units::set_units(i,"kg"))}
  if(median(pol_digits,na.rm = TRUE) >= 6){return(units::set_units(i,"t"))}
}
# read Tiles & Boundaries
my_tile <- readr::read_rds("../../../data-raw/maptiles/mapbox/bra_spo.rds")
my_bound <- readr::read_rds("../../../data-raw/boundaries/bra_spo/bra_spo.rds")


# bbox tile
xtile <- max(my_tile$x) - min(my_tile$x)
ytile <- max(my_tile$y) - min(my_tile$y)
ratio_tile <- ytile/xtile

# list-files
files_gps <- list.files(path = 'article/data/emi_grid_time/',full.names = TRUE)

# read files
tmp_my_grid <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist()


# aggregate by sum
colPol <- names(tmp_my_grid)[!(names(tmp_my_grid) %in% c("timestamp"
                                                         ,"id_hex"
                                                         ,"h3_resolution"
                                                         ,"geometry"))]
tmp_my_grid[timestamp %in% 0:2   ,horario := "0:00 am - 2:59 am"] #
tmp_my_grid[timestamp %in% 3:5   ,horario := "3:00 am - 5:59 am"]
tmp_my_grid[timestamp %in% 6:8   ,horario := "6:00 am - 8:59 am"] #
tmp_my_grid[timestamp %in% 9:11  ,horario := "9:00 am - 11:59 am"]
tmp_my_grid[timestamp %in% 12:14 ,horario := "12:00 am - 2:59 pm"]
tmp_my_grid[timestamp %in% 15:17 ,horario := "3:00 pm - 5:59 pm"]
tmp_my_grid[timestamp %in% 18:20 ,horario := "6:00 pm - 8:59 pm"]
tmp_my_grid[timestamp %in% 21:23 ,horario := "9:00 pm - 11:59 pm"] #

#tmp_my_grid[timestamp %in% 6:7,horario_status := "Peak hour"]
#tmp_my_grid[timestamp %in% 14:15,horario_status := "Off-Peak hours"]

# remove other hours
tmp_my_grid <- tmp_my_grid[timestamp %in% c(0:2,6:8,21:23),]
my_grid <- tmp_my_grid[,(colPol) := lapply(.SD,sum)
                       ,by = .(id_hex,horario)
                       ,.SDcols = colPol]
my_grid <- my_grid[,.SD[1],by = .(id_hex,horario)]
my_grid <- sf::st_as_sf(my_grid) %>% sf::st_transform(3857)



# plot
getUnit_map <- c("t","g") # CO2, PM10

map_scale <- as.numeric(sf::st_bbox(my_grid)[3]) -  
  as.numeric(sf::st_bbox(my_grid)[1])
plot_scale <- 10
xlim_coord <- c( min(my_tile$x), max(my_tile$x))
ylim_coord <- c( min(my_tile$y), max(my_tile$y))

# organize data to plot
bind_grid <- data.table::melt(data = my_grid
                              , variable.name = "pollutant"
                              , id.vars = c("id_hex","horario"
                                            ,"geometry"
                                            ,"h3_resolution","timestamp"))

pol_limit <- bind_grid
pol_limit <- data.table::setDT(pol_limit) %>% 
  .[pollutant == "PM10",] %>% 
  .[,list(min(value),max(value))] %>% 
  as.numeric() 
pol_limit 


bind_grid <- sf::st_as_sf(bind_grid)
bind_grid <- sf::st_set_crs(x = bind_grid,value = sf::st_crs(my_grid))

bind_grid$horario %>% unique() %>% sort() %>% paste0(.,collapse = "', '")
bind_grid$horario_f <- factor(
  x = bind_grid$horario
  ,levels = c(
    '0:00 am - 2:59 am'
    , '6:00 am - 8:59 am'
    , '9:00 pm - 11:59 pm')
  ,labels = c(
    '0:00 am - 2:59 am\n'
    , '6:00 am - 8:59 am\n'
    , '9:00 pm - 11:59 pm\n')
)




plot_hour_pm10 <- ggplot(bind_grid[bind_grid$pollutant == "PM10",]) + 
  # add raster
  geom_raster(data = my_tile, aes(x, y, fill = hex), alpha = 1) +
  coord_cartesian(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  coord_equal() +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  #add emissions
  ggnewscale::new_scale_fill() +
  geom_sf(aes(fill = as.numeric(value))
          , colour = "transparent")  +
  viridis::scale_fill_viridis(option = "D"
                              ,direction = -1
                              ,limits = pol_limit)+
  # add boundary
  ggnewscale::new_scale_color() +
  geom_sf(data = my_bound,color = "black"
          ,linetype = "solid",alpha = 0.5, size = 0.15,fill = NA) +
  facet_wrap(~horario_f ,nrow = 1)+
  scale_color_manual(values = "black"
                     ,labels = "City \nboundary",)+
  coord_sf(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  # labels and theme
  labs(title = NULL
       #, subtitle =  expression(PM[10][] emissions)
       #, subtitle =  expression(PM[10][])
       , fill =  expression(PM[10][] (g))
       , color = NULL
       , x = NULL
       , y = NULL) +
  theme_minimal() + 
  theme_void() +
  theme(legend.position = "bottom",
        text = element_text(family = "LM Roman 10"),
        legend.box.background = element_rect(fill = "white",color = "white"),
        legend.box.margin = margin(3,3,3,3, "pt")
        ,plot.margin = unit(c(0,0,0.35,0),"cm")) 

plot_hour_pm10

ggplot2::ggsave(plot = plot_hour_pm10
                , filename = "article/data/plots/spatial_hour_pm10_linear_3periods.png"
                , scale = 0.7
                , width = 21
                , bg = "white"
                , height = 14
                , units = "cm"
                , dpi = 300)

#### ggplot v2------

rm(list=ls())
library(magrittr)
gc(reset = TRUE)

to_compartible_units <- function(i){  #i = my_time$CO2
  pol_digits <- floor(log10(as.numeric(i))) + 1
  if(median(pol_digits,na.rm = TRUE) <= 2){return(units::set_units(i,"g"))}
  if(median(pol_digits,na.rm = TRUE) <= 5){return(units::set_units(i,"kg"))}
  if(median(pol_digits,na.rm = TRUE) >= 6){return(units::set_units(i,"t"))}
}
# read Tiles & Boundaries
my_tile <- readr::read_rds("../../../data-raw/maptiles/mapbox/bra_spo.rds")
my_bound <- readr::read_rds("../../../data-raw/boundaries/bra_spo/bra_spo.rds")


# bbox tile
xtile <- max(my_tile$x) - min(my_tile$x)
ytile <- max(my_tile$y) - min(my_tile$y)
ratio_tile <- ytile/xtile

# list-files
files_gps <- list.files(path = 'article/data/emi_grid_time/',full.names = TRUE)

# read files
tmp_my_grid <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist()
tmp_my_grid[,NOx := NULL]
tmp_my_grid[,CO2 := NULL]
tmp_my_grid[,CH4 := NULL]

# aggregate by sum
colPol <- names(tmp_my_grid)[!(names(tmp_my_grid) %in% c("timestamp"
                                                         ,"id_hex"
                                                         ,"h3_resolution"
                                                         ,"geometry"))]
tmp_my_grid[timestamp %in% 0:5   ,":="(horario = "0:00 - 5:59 am"  , n = length(0:5))]
tmp_my_grid[timestamp %in% 6:8   ,":="(horario = "6:00 - 08:59 am" , n = length(6:8))]
tmp_my_grid[timestamp %in% 9:11  ,":="(horario = "9:00 - 11:59 am" , n = length(9:11))]
tmp_my_grid[timestamp %in% 12:15 ,":="(horario = "12:00 - 15:59 am", n = length(12:15))]
tmp_my_grid[timestamp %in% 16:18 ,":="(horario = "16:00 - 17:59 pm", n = length(16:18))]
tmp_my_grid[timestamp %in% 19:23 ,":="(horario = "21:00 - 23:59 pm", n = length(19:23))]

my_grid <- copy(tmp_my_grid)[,(colPol) := lapply(.SD,sum)
                             ,by = .(id_hex,horario)
                             ,.SDcols = colPol]
my_grid <- my_grid[,.SD[1],by = .(id_hex,horario)]

my_grid <- my_grid[,PM10 := PM10 / n,by = .(id_hex,horario)]

my_grid <- sf::st_as_sf(my_grid) %>% sf::st_transform(3857)

my_grid$horario %>% unique() %>% sort() %>% paste0(.,collapse = "', '")

my_grid$horario_f <- factor(
  x = my_grid$horario
  ,levels = c('0:00 - 5:59 am', '6:00 - 08:59 am', '9:00 - 11:59 am'
              , '12:00 - 15:59 am', '16:00 - 17:59 pm', '21:00 - 23:59 pm')
)


# plot
getUnit_map <- c("t","g") # CO2, PM10

map_scale <- as.numeric(sf::st_bbox(my_grid)[3]) -  
  as.numeric(sf::st_bbox(my_grid)[1])
plot_scale <- 10
xlim_coord <- c( min(my_tile$x), max(my_tile$x))
ylim_coord <- c( min(my_tile$y), max(my_tile$y))

pol_limit <- c(min(my_grid$PM10),max(my_grid$PM10))
pol_limit <- as.numeric(pol_limit)

#### ggplot v1-----
plot_hour_pm10 <- ggplot(my_grid) + 
  # add raster
  geom_raster(data = my_tile, aes(x, y, fill = hex), alpha = 1) +
  coord_cartesian(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  coord_equal() +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  #add emissions
  ggnewscale::new_scale_fill() +
  geom_sf(aes(fill = as.numeric(PM10))
          , colour = "transparent")  +
  viridis::scale_fill_viridis(option = "D"
                              ,direction = -1
                              ,limits = round(pol_limit,0)
                              , breaks = c(0,1,5,10)
                              ,labels = c(0,1,5,10)
                              ,trans = "pseudo_log"
  )+
  # add boundary
  ggnewscale::new_scale_color() +
  geom_sf(data = my_bound,color = "black"
          ,linetype = "dashed",alpha = 0.5, size = 0.35,fill = NA) +
  facet_wrap(~horario_f ,nrow = 2)+
  scale_color_manual(values = "black"
                     ,labels = "City \nboundary",)+
  coord_sf(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  # labels and theme
  labs(title = NULL
       #, subtitle =  expression(PM[10][] emissions)
       , subtitle =  expression("Mean hourly PM"[10][])
       , fill =  expression(PM[10][] (g))
       , color = NULL
       , x = NULL
       , y = NULL) +
  theme_minimal() + 
  theme_void() +
  theme(legend.position = "bottom",
        text = element_text(family = "LM Roman 10"),
        legend.box.background = element_rect(fill = "white",color = "white"),
        legend.box.margin = margin(3,3,3,3, "pt")
        ,plot.margin = unit(c(0,0,0.5,0),"cm")) 



ggplot2::ggsave(plot = plot_hour_pm10
                , filename = "article/data/plots/spatial_hour_pm10_pseudolog_mean.png"
                , scale = 0.7
                , width = 14
                , bg = "white"
                , height = 17
                , units = "cm"
                , dpi = 300)


## f) EF (@ speed = 19 kph) -----
rm(list=ls())
gc(reset = TRUE)
devtools::load_all()

# CETESB
my_ef_br_df <- ef_brazil_cetesb(pollutant = c("PM10","CO2","NOx")
                                ,veh_type = "BUS_URBAN_D"
                                ,model_year = 2011
                                ,as_list = FALSE)
my_ef_br_df <- as.data.frame(my_ef_br_df)
data.table::setDT(my_ef_br_df)[,source := "CETESB"]

# EMEP/EEA
my_ef_eu_df <- ef_europe_emep(speed =  units::set_units(19,"km/h")
                              ,veh_type = "Ubus Std 15 - 18 t"
                              ,euro = "V"
                              ,pollutant = c("PM10","CO2","NOx")
                              ,fuel = "D"
                              ,tech = "SCR"
                              ,as_list = FALSE)
my_ef_eu_df[,source := "EEA"]

# EPA / MOVES
my_ef_moves_df <- ef_usa_moves(pollutant = c("PM10","CO2","NOx")
                               ,model_year = 2011
                               ,reference_year = 2019
                               ,speed = units::set_units(19,"km/h")
                               ,fuel = "D"
                               ,as_list = FALSE)
my_ef_moves_df[,source := "MOVES"]

# EMFAC
my_ef_emfac_df <- ef_usa_emfac(pollutant =  c("PM10","CO2","NOx")
                               ,reference_year = 2019
                               ,fuel = "D"
                               ,model_year = 2011
                               ,speed = units::set_units(19,"km/h")
                               ,as_list = FALSE)
my_ef_emfac_df[,source := "EMFAC"]

# rbind & process
my_ef_bind <- list(my_ef_br_df,my_ef_eu_df
                   ,my_ef_emfac_df,my_ef_moves_df) %>% 
  data.table::rbindlist(use.names = FALSE)


my_ef_plot <- data.table::melt.data.table(
  data = my_ef_bind
  ,id.vars = "source"
  ,measure.vars = c("PM10_2011","CO2_2011","NOx_2011"))
my_ef_plot[, variable := gsub("_2011","",variable)]
my_ef_plot[, variable_f := dplyr::recode_factor(variable
                                                , `CO2` = "CO[2]"
                                                , `PM10` = "PM[10]"
                                                , `NOx` = "NO[X]")]
my_ef_plot[, value := as.numeric(value)]
my_ef_plot[, label := "data source"]

#labeller_status <- c(CO2 = expression(CO[2][]),
#                     PM10 = expression(PM[10][]))
# plot
ggplot(data = my_ef_plot) + 
  geom_bar(aes(y= value,x = source,fill = source)
           ,stat = "identity",position = "dodge")+
  facet_wrap(~variable_f
             ,scales = "free"
             , label = "label_parsed") +
  labs(fill = NULL,x = NULL,y = "EF (g/km)")+
  viridis::scale_fill_viridis(discrete = TRUE
                              ,option = "H",alpha = 0.75) +
  theme_light()+
  guides(fill = "none")+
  scale_y_continuous(expand = expansion(mult = 0.05)) +
  theme(axis.text.x = element_text(size = 8)
        ,text = element_text(family = "LM Roman 10")
        ,strip.text = element_text(colour = 'black'))

# save
ggsave(filename = "article/data/plots/ef_plots.png"
       ,width = 36,height = 20,dpi = 300,units = "cm",scale = 0.5)

## g) EF @ different speeds for NOx------


# CETESB
my_ef_br_df <- ef_brazil_cetesb(pollutant = c("PM10","CO2","NOx")
                                ,veh_type = "BUS_URBAN_D"
                                ,model_year = 2011
                                ,as_list = FALSE)
my_ef_br_df <- ef_scaled_euro(ef_local = my_ef_br_df,
                              speed =  units::set_units(seq(10,100,10),"km/h")
                              ,veh_type = "Ubus Std 15 - 18 t"
                              ,euro = "V"
                              ,pollutant = c("PM10","CO2","NOx")
                              ,fuel = "D"
                              ,tech = "SCR")
my_ef_br_df <- my_ef_br_df$EF
my_ef_br_df[,source := "CETESB"]

# EMEP/EEA
my_ef_eu_df <- ef_europe_emep(speed =  units::set_units(seq(10,100,10),"km/h")
                              ,veh_type = "Ubus Std 15 - 18 t"
                              ,euro = "V"
                              ,pollutant = c("PM10","CO2","NOx")
                              ,fuel = "D"
                              ,tech = "SCR"
                              ,as_list = FALSE)
my_ef_eu_df[,source := "EEA"]

# EPA / MOVES
my_ef_moves_df <- ef_usa_moves(pollutant = c("PM10","CO2","NOx")
                               ,model_year = 2011
                               ,reference_year = 2019
                               ,speed = units::set_units(seq(10,100,10),"km/h")
                               ,fuel = "D"
                               ,as_list = FALSE)
my_ef_moves_df[,source := "MOVES"]

# EMFAC
my_ef_emfac_df <- ef_usa_emfac(pollutant =  c("PM10","CO2","NOx")
                               ,reference_year = 2019
                               ,fuel = "D"
                               ,model_year = 2011
                               ,speed = units::set_units(seq(10,100,10),"km/h")
                               ,as_list = FALSE)
my_ef_emfac_df[,source := "EMFAC"]

# rbind & process
my_ef_bind <- list(my_ef_br_df,my_ef_eu_df
                   ,my_ef_emfac_df,my_ef_moves_df) %>% 
  data.table::rbindlist(use.names = FALSE)

my_ef_plot <- data.table::melt.data.table(
  data = my_ef_bind
  ,id.vars = "source"
  ,measure.vars = c("PM10_Euro_V","CO2_Euro_V","NOx_Euro_V")
)
my_ef_plot[, variable := gsub("_Euro_V","",variable)]
my_ef_plot[, variable_f := dplyr::recode_factor(variable
                                                , `CO2` = "CO[2]"
                                                , `NOx` = "NO[X]"
                                                , `PM10` = "PM[10]")]
my_ef_plot[, value := as.numeric(value)]
my_ef_plot[, label := "data source"]
my_ef_plot[, speed := rep(seq(10,100,10),12)]

# plot
p <- ggplot(data = my_ef_plot) + 
  geom_line(aes(y= value,x = speed,color = source),lwd = 0.8)+
  geom_point(aes(y= value,x = speed,color = source))+
  facet_wrap(~variable_f
             ,scales = "free"
             , label = "label_parsed") +
  labs(color = NULL,x = "Speed (km/h)",y = "EF (g/km)")+
  viridis::scale_color_viridis(discrete = TRUE,option = "H",alpha = 1)+
  theme_light()+
  scale_y_continuous(expand = expansion(mult = 0.05)) +
  theme(axis.text.x = element_text(size = 9)
        ,text = element_text(family = "LM Roman 10")
        ,strip.text = element_text(colour = 'black')
        ,legend.position = c(0.9,0.675))

# save
ggsave(plot = p
       ,filename = "article/data/plots/ef_speed.png"
       ,scale = 1.0,width = 18,height = 7
       ,units = "cm",dpi = 300)

## h) EF by age ----

# CETESB
my_ef_br_df <- ef_brazil(pollutant = c("PM10","CO2","NOx","NMHC")
                         ,veh_type = "BUS_URBAN_D"
                         ,model_year = c(2000:2019)
                         ,as_list = TRUE)
my_ef_br_df <- emi_to_dt(emi_list = my_ef_br_df
                         ,emi_vars = 'EF'
                         ,veh_vars = c('veh_type','years')
                         ,pol_vars = 'pollutant')
my_ef_br_df$source <- "CETESB"
my_ef_br_df[,years_n := as.numeric(as.character(years))]
my_ef_br_df[pollutant == "NMHC",pollutant := "VOC"]

# EMEP/EEA
my_ef_eu_df <- ef_europe(speed =  units::set_units(34.12,"km/h")
                         ,veh_type = rep("Ubus Std 15 - 18 t",6)
                         ,euro = c("I", "II", "III", "IV", "V", "VI")
                         ,pollutant = c("PM10","CO2","NOx","VOC")
                         ,fuel = rep("D",6)
                         ,tech = c("-","-","-","SCR","SCR","SCR")
                         ,as_list = TRUE)
my_ef_eu_df <- emi_to_dt(emi_list = my_ef_eu_df
                         ,emi_vars = 'EF'
                         ,veh_vars = c("veh_type","euro","fuel","tech")
                         ,pol_vars = 'pollutant'
                         ,segment_vars = c('slope','load'))
my_ef_eu_df$source <- "EEA"

# EPA / MOVES
my_ef_moves_df <- ef_usa_moves(pollutant = c("PM10","CO2","NOx","VOC")
                               ,model_year = c(2000:2018)
                               ,calendar_year = 2019
                               ,speed = units::set_units(34.12,"km/h")
                               ,fuel_type = "D"
                               ,as_list = TRUE)
names(my_ef_moves_df)
my_ef_moves_df <- emi_to_dt(emi_list = my_ef_moves_df
                            ,emi_vars = 'EF'
                            ,veh_vars = c('model_year','fuel')
                            ,pol_vars = 'pollutant')
my_ef_moves_df$source <- "MOVES"
my_ef_moves_df[,model_year_n := as.numeric(as.character(model_year))]

# EMFAC
my_ef_emfac_df <- ef_usa_emfac(pollutant =  c("PM10","CO2","NOx","ROG")
                               ,calendar_year = 2019
                               ,fuel = "D"
                               ,model_year = c(2005:2019)
                               ,speed = units::set_units(34.12,"km/h")
                               ,as_list = TRUE)
my_ef_emfac_df <- emi_to_dt(emi_list = my_ef_emfac_df
                            ,emi_vars = 'EF'
                            ,veh_vars = c('model_year','fuel')
                            ,pol_vars = 'pollutant')
my_ef_emfac_df$source <- "EMFAC"
my_ef_emfac_df[,model_year_n := as.numeric(as.character(model_year))]
my_ef_emfac_df[pollutant == "ROG",pollutant := "VOC"]

# rbind & process

# rbind & process
my_ef_bind <- list(my_ef_br_df,my_ef_emfac_df,my_ef_moves_df) %>% 
  data.table::rbindlist(use.names = FALSE)


my_ef_bind[, pollutant_f := dplyr::recode_factor(pollutant
                                                 , `CO2` = "CO[2]"
                                                 , `PM10` = "PM[10]"
                                                 , `NOx` = "NO[x]"
                                                 ,`VOC` = "VOC")]
my_ef_bind[years_n > 2004] %>% 
  ggplot() + 
  geom_path(aes(y= as.numeric(EF),x = as.factor(years_n)
                ,color = source,group = source))+
  geom_point(aes(y= as.numeric(EF),x = as.factor(years_n)
                 ,color = source))+
  scale_color_manual(values = viridis::rocket(4))+
  scale_x_discrete(breaks = c(seq(2005,2019,3),2019),
                   labels = c(seq(2005,2019,3),2019))+
  facet_wrap(facets = vars(pollutant_f),nrow = 2
             ,scales = "free_y", label = "label_parsed")+
  labs(x = "Fleet age",y = "EF (g/km)",color = "Source")+
  theme_light()+
  theme(axis.text.x = element_text(size = 8)
        ,text = element_text(family = "LM Roman 10")
        ,strip.text = element_text(colour = 'black')
        ,legend.position = c(0.9,0.35))

# save
ggsave(filename = "article/data/plots/ef_age_plots.png"
       ,width = 36,height = 30,dpi = 300,units = "cm",scale = 0.5)

## i) Fleet of São Paulo ----
rm(list=ls())
gc(reset=TRUE)
devtools::load_all()
devtools::document()
devtools::load_all()
# Read fleet 
fleet_spo <- readr::read_rds("../../../data/fleet/bra_spo/bra_spo.rds")


# adjust technology
fleet_spo[fuel %in% "Elétrico",fuel := "Electric"]
fleet_spo[fuel %in% "D",fuel := "Diesel"]



# total fleet
sum(tp$V1)
# electric
sum(tp[,])
tp[]

sum(tp$V1) + 202
fleet_spo <- fleet_spo[,lapply(.SD,sum)
                       ,by = .(year,type_name_eu,fuel)
                       ,.SDcols = "N"]
fleet_spo[,type_name_eu_f := factor(x = type_name_eu
                                    ,levels = c("Ubus Midi <=15 t",
                                                "Ubus Std 15 - 18 t",
                                                "Ubus Artic >18 t")
                                    ,labels = c("Midi <=15 t",
                                                "Standard 15 - 18 t",
                                                "Articulated >18 t"))]
N_by_year <- fleet_spo[,sum(N), by = .(year)]
N_by_year
# pallete
library(ggsci) # devtools::install_github("awhstin/awtools")
# plot
ggplot(data = fleet_spo[fuel == "Diesel"]) + 
  # data
  geom_bar(aes(y= N,x = as.factor(year),fill = type_name_eu_f)
           ,stat = "identity"
           ,position = "dodge"
           #,position = "stack"
           ,width = 0.75)+
  # scale
  #scale_fill_manual(values = ggsci::pal_nejm()(8)[c(7,2,1)])+
  scale_fill_manual(values = ggsci::pal_jama()(7)[c(2,3,6)])+
  # labs
  labs(x = "Model year"
       ,fill = "Vehicle category"
       ,y = "Number of vehicles")+
  # theme
  theme_light()+
  theme(axis.text.x = element_text(size = 9)
        ,text = element_text(family = "LM Roman 10")
        ,strip.text = element_text(colour = 'black')
        #,legend.position = c(0.125,0.80)
        ,legend.position = "bottom"
  )+
  guides(fill = guide_legend(title.position = "top"))


# save
ggsave(filename = "article/data/plots/fleet_jama.png",scale = 0.9,
       width = 15,height = 10,units = "cm",dpi = 300)


# 6) Stats avg EF ----
# list-files
rm(list=ls())
gc(reset = TRUE)
files_gps_t <- list.files(path = 'article/data/emi_age/',full.names = TRUE)

tmp_stats_ef <- lapply(files_gps_t, readr::read_rds) %>% 
  data.table::rbindlist()

tmp_stats_ef

tmp_stats_ef[,fleet_composition := as.numeric(as.character(fleet_composition))]
tmp_stats_ef[,VTK := as.numeric(as.character(VTK))]
tmp_stats_ef[,single_VTK := VTK * fleet_composition]

tmp_stats_ef1 <- tmp_stats_ef[,lapply(.SD,sum,na.rm=TRUE)
                              ,.SDcols = c("emi","single_VTK")
                              , by = .(pollutant)]

tmp_stats_ef1[,EF := emi / units::set_units(single_VTK,"km")] 
tmp_stats_ef1[,EF := round(EF,3)] 
tmp_stats_ef1

# 7) text------

## a) type of transport model output ------
rm(list=ls())
gc(reset = TRUE)
files_tp <- list.files(path = 'article/data/gps/',full.names = TRUE)

colToRound <- c("speed","dist","cumdist")

tp <- readr::read_rds(file = files_tp[4]) %>% 
  gtfs2gps::adjust_speed(gps_data = .
                         ,min_speed = 5
                         ,max_speed = 80) %>% 
  gtfs2gps::gps_as_sflinestring()
data.table::setDT(tp)
tp[1]
tp <- tp[!is.na(stop_sequence ) & to_stop_id != "-"]
tp <- tp[trip_number == 1]
tp <- tp[c(1:3,(.N-2):.N)]
tp <- tp[,.SD,.SDcols = c(
  "stop_sequence","from_stop_id","to_stop_id",
  "from_timestamp", "to_timestamp",
  "speed","dist","cumdist")]
tp[,dist := units::set_units(dist,"m")]
tp[,(colToRound) := lapply(.SD,as.numeric) ,.SDcols = c(colToRound)]
tp[,(colToRound) := lapply(.SD,round,1) ,.SDcols = colToRound]
tp[,(colToRound) := lapply(.SD,as.character) ,.SDcols = colToRound]
tp
clipr::write_clip(tp)


## b)  emissions by time-----
rm(list=ls())
gc(reset = TRUE)
files_gps <- list.files(path = 'article/data/emi_time/',full.names = TRUE)
tmp_my_time <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist(fill = TRUE)

to_compartible_units <- function(i){  #i = my_time$CO2
  pol_digits <- floor(log10(as.numeric(i))) + 1
  if(median(pol_digits,na.rm = TRUE) <= 2){return(units::set_units(i,"g"))}
  if(median(pol_digits,na.rm = TRUE) <= 5){return(units::set_units(i,"kg"))}
  if(median(pol_digits,na.rm = TRUE) >= 6){return(units::set_units(i,"t"))}
}

# total
tmp1 <- copy(tmp_my_time)[,sum(emi),by = .(pollutant)]
tmp1 <- dcast(tmp1, .~ pollutant)
tmp1[,. := NULL]

tmp1[,lapply(.SD,to_compartible_units),.SDcols = names(tmp1)]

# pm10 by time intervals
tmp2 <- copy(tmp_my_time)[,sum(emi),by = .(timestamp_hour,pollutant)]
tmp2 <- tmp2[pollutant == "PM10"]
tmp2[,total_pol := sum(V1)]

# total pm10
tmp2[timestamp_hour %in% c(0,1,2),time_classe := "0:00 - 2:59"]
tmp2[timestamp_hour %in% c(6,7,8),time_classe := "6:00 - 8:59"]
tmp2[timestamp_hour %in% c(21,22,23),time_classe := "21:00 - 23:59"]

tmp2 <- tmp2[!is.na(time_classe)]
tmp2
tmp2[,{
  ratio =  100 * sum(V1) / total_pol[1]
  ratio = round(ratio,1)
  ratio = paste0(ratio,"%")
  
  total = units::set_units(sum(V1),"kg")
  total = round(total,1)
  
  total_day = units::set_units(total_pol[1],"kg")
  total_day = round(total_day,1) 
  
  list("total" = total
       ,"total_day" = total_day
       ,"ratio" = ratio)
},by = time_classe]


## c) emissions by veh -----

rm(list=ls())
gc(reset = TRUE)

# prep function
to_compartible_units <- function(i){  #i = my_time$CO2
  pol_digits <- floor(log10(as.numeric(i))) + 1
  if(median(pol_digits,na.rm = TRUE) <= 2){return(paste(round(i,1),"g"))}
  if(median(pol_digits,na.rm = TRUE) <= 5){return(paste(round(i / (10^3),1),"kg"))}
  if(median(pol_digits,na.rm = TRUE) >= 6){return(paste(round(i / (10^6),1),"t"))}
}

# list-files
files_gps <- list.files(path = 'article/data/emi_age/',full.names = TRUE)
files_gps_names <- list.files(path = 'article/data/emi_age/',full.names = FALSE)

# read files
tmp_my_age <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist()

# emissions by vehicle category
emi_cat <- copy(tmp_my_age)
emi_cat <- emi_cat[,list(total_emi = sum(emi)),by = .(veh_type,pollutant)]
emi_cat[,prop_emi := round(100 * total_emi / sum(total_emi),1)
        , by = pollutant]
emi_cat <- emi_cat[,{
  emi_adj <- to_compartible_units(total_emi)
  text_emi <- paste0(emi_adj," (",prop_emi,"%)")
  list(veh_type,text_emi)
},by = pollutant]

emi_cat[pollutant == "PM10"]
# emissions articulated
emi_artic <- copy(tmp_my_age)[veh_type == "Ubus Artic >18 t"]

emi_artic <- emi_artic[,list(total_emi = sum(emi)),by = .(euro,pollutant)]

emi_artic[,prop_emi := round(100 * total_emi / sum(total_emi),1), by = pollutant]

emi_artic
emi_artic[pollutant == "PM10"]
## d) fleet ----
fleet_spo <- readr::read_rds("../../../data/fleet/bra_spo/bra_spo.rds")

## e) euro V scenario -----

rm(list=ls())
gc(reset = TRUE)

# list-files
files_gps <- list.files(path = 'article/data/emi_age/',full.names = TRUE)
files_gps_names <- list.files(path = 'article/data/emi_age/',full.names = FALSE)

# read files
tmp_my_age <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist()

#tmp_my_age[,VTK := units::drop_units(VTK)]
tmp_my_age[,fleet_composition := as.numeric(as.character(fleet_composition))]

# emissions by euro
to_compartible_units <- function(i){  #i = my_time$CO2
  pol_digits <- floor(log10(as.numeric(i))) + 1
  if(median(pol_digits,na.rm = TRUE) <= 2){return(paste(round(i,1),"g"))}
  if(median(pol_digits,na.rm = TRUE) <= 6){return(paste(round(i / (10^3),1),"kg"))}
  if(median(pol_digits,na.rm = TRUE) > 6){return(paste(round(i / (10^6),1),"t"))}
}

tmp <- tmp_my_age[,list(sum(as.numeric(emi))
                       ,to_compartible_units(sum(as.numeric(emi))))
                       ,by = .(pollutant,euro)]
tmp[, prop := round(100 * V1/sum(V1),2),by = .(pollutant)]
tmp[, prop := round(100 * V1/sum(V1),2)]


tmp_my_age[,list(sum(as.numeric(emi))
                 ,to_compartible_units(sum(as.numeric(emi))))]

# aggregate by sum
tmp <- tmp_my_age[pollutant %in% c('NOx','PM10'),list(
  emi = sum(emi)
  ,fleet_composition = sum(fleet_composition)
),by = .(pollutant,veh_type,euro,fuel,tech)]

tmp[,fleet_composition := fleet_composition/sum(fleet_composition)
    ,by = .(pollutant)]
tmp <- tmp[,{
  euro3_id <- which(euro == "III")
  euro5_id <- which(euro == "V")
  
  emi_fleetcomp <- emi[euro5_id] / fleet_composition[euro5_id]
  new_emi <- fleet_composition * emi_fleetcomp
  list(euro,fuel,tech,emi,fleet_composition,emi_fleetcomp,new_emi)
},by = .(pollutant,veh_type)]

tmp[,{
  total_new_emi <-  sum(as.numeric(new_emi))
  total_emi <- sum(as.numeric(emi))
  rate <- (total_new_emi - total_emi) / (total_emi)
  rate <- round(100 * rate,2)
  list(total_new_emi,total_emi,rate)
} , by = .(pollutant)]

(tmp$fleet_composition/4) %>% sum()
# aggregate by sum
tmp_my_age <- tmp_my_age[,list(
  emi = sum(emi)
  ,single_VTK = sum(single_VTK)
  ,avg_ef = sum(emi)/sum(single_VTK)
),by = .(veh_type,euro ,pollutant)]
# total fleet
sum(fleet_spo$N)

# eletric
electric <- fleet_spo[fuel == "Elétrico",]$N %>% sum()
electric
# prop eletric
round(100 * electric / sum(fleet_spo$N),1)

# fuel
fuel <- fleet_spo[fuel != "Elétrico",]$N %>% sum()
fuel
round(100 * fuel / sum(fleet_spo$N),1)


# prop by vehicle type 
# total and proportion of fuel
tp <- copy(fleet_spo)[fuel != "Elétrico",]
tp <- tp[,{total = sum(N)},by = .(type_name_eu)]

tp[,prop := round(100 * V1 / sum(V1),1)]
tp[,text := paste0(V1," (",prop,"%)")]
tp[]

# pa - Proportion of articulated buses of Euro III
pa <- copy(fleet_spo)
pa <- pa[fuel != "Elétrico" & type_name_eu == "Ubus Artic >18 t",]
pa <- pa[,list(N = sum(N)),by = .(euro)]
pa[,prop := round(100 * N / sum(N),1)]
pa


#
# check IEMA------
# http://emissoes.energiaeambiente.org.br/graficos

#'      |    IEMA            |  gtfs2emis  |  diff    | IEMA EF               | 
#' PM10 |   449.04 kg        |   314.2 kg  |  -30%    | 0.1621083 g/km        | 
#' NOx  |   25 t             |   32.20  t  |   29%    |  10.41667  g/km      | 
#' CO2  |   2.356 kt         |   3.052 kt  |   29%    |
#' CH4  |   144 kg           |   70.5  kg  |  -49%    |
#' VKM  |   2.4 x 10^6 km    |    2.77 Mkm |   15%    |                      | 
#' VKM dia
#' 0.0574+0.0229+0.0127+0.0222+0.0661+0.1167+
#' 0.1353+0.1330+0.1331+0.1268+0.1171+0.1099+
#' 0.1064+0.1082+0.1104+0.1143+0.1213+0.1208+
#' 0.1157+0.1230+0.1231+0.1107+0.0997+0.0901

units::set_units(units::set_units(449.04,"kg"),"g")/units::set_units(2.4 * 10^6,"km")
units::set_units(units::set_units(137,"kg"),"g")/units::set_units(2.77 * 10^6,"km")
units::set_units(units::set_units(25,"t"),"g")/units::set_units(2.4 * 10^6,"km")


ef_europe_emep(speed = units::set_units(11,"km/h")
               ,veh_type = rep("Ubus Std 15 - 18 t",3)
               ,euro = c("III","IV","V")
               ,pollutant = "CH4"
               ,fuel = "D"
               #,tech = "-"
               ,slope = 0
               ,load = 0.5
               ,as_list = FALSE) %>% 
  as.numeric() %>% 
  paste0(.,collapse = " - ")

#' GTFS2emis
#' 137 kg/dia  
#' 11.95 t/dia
#' 2.77 x 10^6 km
#' 



rm(list=ls())
gc(reset = TRUE)

# list-files
files_gps <- list.files(path = 'article/data/emi_age_old_veh//',full.names = TRUE)
tmp_my_age <- lapply(files_gps, readr::read_rds) %>% 
  data.table::rbindlist()
tmp_my_age[,fleet_composition := as.numeric(as.character(fleet_composition))]
tmp_my_age[,list("emi"= sum(emi)
                 ,"vtk" = sum(fleet_composition  * VTK)
                 ,"fe" = round(sum(emi)/sum(fleet_composition  * VTK),4)
) 
,by = pollutant]

#' emi_age
#' pollutant              emi          vtk               fe
#' 1:       CO2 3.052492e+09 [g] 2776257 [km] 1099.4992 [g/km]
#' 2:       NOx 3.220615e+07 [g] 2776257 [km]   11.6006 [g/km]
#' 3:      PM10 3.142045e+05 [g] 2776257 [km]    0.1132 [g/km]
#' 4:       CH4 7.052168e+04 [g] 2776257 [km]    0.0254 [g/km]
#' 
#' emi_age_old
#' pollutant              emi          vtk               fe
#' 1:       CO2 3.160103e+09 [g] 2776257 [km] 1138.2603 [g/km]
#' 2:       NOx 3.307811e+07 [g] 2776257 [km]   11.9146 [g/km]
#' 3:      PM10 3.618146e+05 [g] 2776257 [km]    0.1303 [g/km]
#' 4:       CH4 9.530191e+04 [g] 2776257 [km]    0.0343 [g/km



# End ----