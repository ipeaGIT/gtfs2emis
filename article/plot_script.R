# 0) Load libraries ----

rm(list=ls())
gc(reset = TRUE)

library(devtools)

#devtools::load_all()
#devtools::document()
#devtools::load_all()
#devtools::install()
#library(gtfs2emis)

library(data.table)
library(magrittr)
library(ggplot2)
library(sfheaders)
library(furrr)
library(sf)
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
#extrafont::font_import()
#extrafont::loadfonts(device = "win")
#extrafont::fonts()
#extrafont::font_install('fontcm')


# 1)  Prep GTFS ----

# read gtfs
# spo_gtfs <- gtfstools::read_gtfs("../../Pessoal/IPEA/gtfs2gps/data-raw/gtfs/gtfs_spo_sptrans_2019-10.zip")
spo_gtfs <- gtfstools::read_gtfs("../../../data-raw/gtfs/bra_spo/gtfs_spo_sptrans_2019-10.zip")
spo_gtfs$`_transparencia_e-SIC_42374_email_05-09-19` <- NULL

# filter by bus route
spo_gtfs1 <- gtfstools::filter_by_route_id(spo_gtfs,route_id = "3")
temp_routeid <- spo_gtfs$routes[route_type == 3,route_id]
temp_shapeids <- spo_gtfs$trips[route_id %in% unique(temp_routeid),shape_id]
spo_gtfs <- gtfstools::filter_by_shape_id(spo_gtfs,
                                          shape_id = unique(temp_shapeids))

spo_gtfs <- gtfstools::filter_by_weekday(gtfs = spo_gtfs
                                         ,weekday = "wednesday"
                                         ,keep = TRUE)
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


# test on filter by day 
spo_gtfs$calendar
# service_id monday tuesday wednesday thursday friday saturday sunday start_date   end_date
# 1:        USD      1       1         0        0      1        1      1 2008-01-01 2020-05-01
# 2:        U__      1       1         0        0      1        0      0 2008-01-01 2020-05-01
# 3:        US_      1       1         0        0      1        1      0 2008-01-01 2020-05-01
# 4:        _SD      0       0         0        0      0        1      1 2008-01-01 2020-05-01
# 5:        __D      0       0         0        0      0        0      1 2008-01-01 2020-05-01
# 6:        _S_      0       0         0        0      0        1      0 2008-01-01 2020-05-01
spo_gtfs$trips$trip_id %>% uniqueN() # 2260
spo_gtfs$trips$shape_id %>% uniqueN() # 2260
spo_gtfs1$trips$trip_id  %>% uniqueN() # 2260
spo_gtfs1$shapes$shape_id  %>% uniqueN() # 2260
spo_gtfs1$stop_times$trip_id %>% uniqueN() # 2260
spo_gtfs1$trips$shape_id  %>% uniqueN() # 2260
spo_gtfs1$trips %>% nrow() # 193831

# generate gps

future::plan(session = "multisession",workers = 19)
devtools::load_all()
transport_model(gtfs_data = spo_gtfs
                ,min_speed = 2
                ,max_speed = 80
                ,new_speed = NULL
                ,parallel = TRUE
                ,spatial_resolution = 100
                ,output_path = "article/data"
                ,continue = TRUE)

## c) Adjust gps speed---------

## e) Generall statistics ----

files_gps <- list.files("article/data/gps_line/",full.names = TRUE)
files_gps_names <- list.files("article/data/gps_line/",full.names = FALSE)

future::plan(strategy =  "multisession",workers = 29)

gen_stats <- furrr::future_map(seq_along(files_gps)
                               ,function(i){ # i = 2
                                 
                                 #message(paste0("Stats of Linestring file '",files_gps_names[i],"'"))
                                 
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
                               }) %>% data.table::rbindlist(use.names = TRUE)

gen_stats
readr::write_rds(gen_stats,"article/data/general_stats.rds")
gen_stats[,total_time := as.numeric(total_time) ]
gen_stats[,lapply(.SD,weighted.mean,VTK)
          ,.SDcols = c("Q25","Q50","Q75","VTK_per_trip","VTK")]
gen_stats[,lapply(.SD,weighted.mean,as.numeric(VTK))
          ,.SDcols = c("number_trips","total_time","number_stop_id")]
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
gps_lines_sf$label <- "SPTRAN's shape_ids"

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
  scale_color_manual(values = "red",name = NULL
                     ,labels = "SPTRAN's shape_ids")+
  # add boundary
  ggnewscale::new_scale_color() +
  geom_sf(data = my_bound,aes(color = city_name)
          ,linetype = "dashed",alpha = 1, size = 0.35,fill = NA) +
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
        legend.box.margin = margin(3,3,3,3, "pt")) 
p
# save
ggplot2::ggsave(plot = p,
                filename = "article/data/plots/basic_sptrans.png",
                scale = 0.7,width = 18,
                bg = "white",
                height = 20,units = "cm",dpi = 300)



# 2) Emissions -----
# obs: this needs to read fleet first
#
rm(list=ls())
gc(reset=TRUE)
devtools::load_all()
devtools::document()
devtools::load_all()
# Read fleet 
fleet_spo <- readr::read_rds("../../../data/fleet/bra_spo/bra_spo.rds")
# adjust technology
fleet_spo <- fleet_spo[fuel %in% "D",] # remove electric
fleet_spo[euro %in% "V", Technology := "SCR"]
fleet_spo[euro %in% "III",Technology := "-"]
fleet_spo[,fleet_composition := N/sum(N)]
setnames(fleet_spo,"Technology","tech")
setnames(fleet_spo,"type_name_br","veh_type")
setnames(fleet_spo,"year","model_year")
dir.create("article/data/emissions/")

future::plan(session = "multisession",workers = 19)

emission_model(tp_model = "article/data/gps_line"
               ,ef_model = "ef_brazil_scaled_euro"
               ,pollutant = c("CO2","NOx","PM10","CH4")
               ,fleet_data = fleet_spo
               ,parallel = TRUE
               ,ncores = 19
               ,output_path = "article/data/emissions"
               ,continue = TRUE
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

oplan <- future::plan(strategy =  "multisession",workers = 29)
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


oplan <- future::plan(strategy =  "multisession",workers = 29)
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
                                            ,"model_year"))
  temp_emi_dt$VTK <- sum(units::set_units(temp_emi$tp_model$dist,"km"))
  # write
  readr::write_rds(x = temp_emi_dt
                   ,file = paste0("article/data/emi_age/",files_gps_names[i])
                   ,compress = 'gz')
  return(NULL)
}

future::plan(strategy =  "multisession",workers = 29)
requiredPackages = c('data.table', 'sf', 'units')
veh_age_processing <- furrr::future_map(seq_along(files_gps)
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


# 
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
                scale = 1.0,bg = "white",
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

tmp_my_age[,VTK := units::drop_units(VTK)]
tmp_my_age[,fleet_composition := as.numeric(as.character(fleet_composition))]
tmp_my_age[,single_VTK := VTK * fleet_composition]

# aggregate by sum
tmp_my_age[,list(
  emi = sum(emi)
  ,single_VTK = sum(single_VTK)
),by = .(pollutant)][,emi/single_VTK,by = .(pollutant)]

# aggregate by sum
tmp_my_age <- tmp_my_age[,list(
  emi = sum(emi)
  ,single_VTK = sum(single_VTK)
),by = .(veh_type,model_year,pollutant)]



# fix/check names
tmp_my_age[pollutant == "CO2",
           emi_total := units::set_units(emi,"t") %>% as.numeric()]
tmp_my_age[pollutant == "NOx"
           ,emi_total := units::set_units(emi,"kg") %>% as.numeric()]
tmp_my_age[pollutant == "CH4"
           ,emi_total := units::set_units(emi,"g") %>% as.numeric()]
tmp_my_age[pollutant == "PM10"
           ,emi_total := units::set_units(emi,"g") %>% as.numeric()]

# estimate MEF

tmp_my_age[,mef := emi / units::set_units(single_VTK,"km")]
tmp_my_age


# factors
tmp_my_age[,model_year_f := factor(x = model_year ,levels = 2008:2019)]
tmp_my_age[, pollutant_f := dplyr::recode_factor(pollutant
                                                 , `CO2` = "CO[2] (t)"
                                                 , `PM10` = "PM[10] (g)"
                                                 , `CH4` = "CH[4] (g)"
                                                 , `NOx` = "NO[X](kg)")]
tmp_my_age[, pollutant_meff := dplyr::recode_factor(pollutant
                                                    , `CO2` = "CO[2]"
                                                    , `PM10` = "PM[10]"
                                                    , `CH4` = "CH[4]"
                                                    , `NOx` = "NO[X]")]
## c) Total_emi ----
ggplot(data = tmp_my_age) + 
  geom_bar(aes(y= emi_total,x = model_year_f,fill = veh_type)
           ,stat = "identity",position = "dodge")+
  facet_wrap(~pollutant_f
             ,scales = "free"
             , label = "label_parsed") +
  labs(fill = NULL,x = "Year of fleet",y = "Emissions")+
  scale_fill_manual(values = viridis::rocket(4))+
  scale_x_discrete(breaks = c(seq(2008,2019,3),2019),
                   labels = c(seq(2008,2019,3),2019))+
  theme_light()+
  scale_y_continuous(expand = expansion(mult = 0.05)) +
  theme(axis.text.x = element_text(size = 8)
        ,text = element_text(family = "LM Roman 10")
        ,strip.text = element_text(colour = 'black')
        ,legend.position = c(0.875,0.9))

# save
ggsave(filename = "article/data/plots/emissions_age.png"
       ,width = 36,height = 30,dpi = 300,units = "cm",scale = 0.5)


## d) MEF ----

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

## e) Spatial emissions -----
rm(list=ls())
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

# PM10
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
                              ,direction = -1) +
  ## add boundary
  ggnewscale::new_scale_color() +
  geom_sf(data = my_bound,color = "black"
          ,linetype = "dashed",alpha = 0.5, size = 0.35,fill = NA) +
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
                 family = "LM Roman 10",
                 fontface = "plain",
                 dist_unit = "km",st.size = 3,
                 st.bottom = FALSE, st.color = "black",
                 transform = FALSE, model = "WGS84") +
  theme(legend.position = c(0.8,0.3),
        text = element_text(family = "LM Roman 10"),
        legend.box.background = element_rect(fill = "white",color = "white"),
        legend.box.margin = margin(3,3,3,3, "pt")) 


mapview::mapview(bind_grid[bind_grid$pollutant == "CO2",],zcol = "value")

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
                              ,direction = -1) +
  ## add boundary
  ggnewscale::new_scale_color() +
  geom_sf(data = my_bound,color = "black"
          ,linetype = "dashed",alpha = 0.5, size = 0.35,fill = NA) +
  # facet_wrap(~pollutant)+
  scale_color_manual(values = "black"
                     ,labels = "City \nboundary",)+
  coord_sf(xlim = xlim_coord, ylim = ylim_coord,expand = FALSE) +
  # labels and theme
  labs(title = "Total emissions"
       , subtitle = expression(CO[2][])
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
        legend.box.margin = margin(3,3,3,3, "pt")) 

co2_plot
library(patchwork)
both_plot <- co2_plot + pm10_plot
both_plot
# save
ggplot2::ggsave(plot = both_plot
                , filename = "article/data/plots/spatial_co2_pm10.png"
                , scale = 0.7
                , width = 32
                , bg = "white"
                , height = 20
                , units = "cm"
                , dpi = 300)


## e1) zoom emissions ----

get_bbox <- c(-46.63319,-23.54904) %>% 
  sf::st_point(.,dim = "XY") %>% 
  sf::st_sfc() %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_transform(3857) %>% 
  sf::st_buffer(5000) %>% 
  sf::st_coordinates() %>% 
  as.data.frame() %>% 
  data.table::setDT() %>% 
  .[,list(X =  c(min(X),max(X),max(X),min(X))
          ,Y = c(min(Y),min(Y),max(Y),max(Y)))] %>% 
  sfheaders::sf_polygon(.,x = "X",y = "Y") %>% 
  sf::st_set_crs(x = .,3857)

intersect_bound <- sf::st_intersection(x = bind_grid
                                       ,y = get_bbox)

lim_coord_zoom <- bind_grid %>%
  sf::st_coordinates() %>% 
  as.data.frame() %>% 
  data.table::setDT() %>% 
  .[,list(X =  c(min(X),max(X))
          ,Y = c(min(Y),min(Y)))]

lim_coord_zoom

pm10_zoom <-  ggplot(intersect_bound[intersect_bound$pollutant == "PM10",]) + 
  geom_sf(aes(fill = as.numeric(value))
          , colour = "transparent")  +
  viridis::scale_fill_viridis(option = "D"
                              ,direction = -1) +
  #coord_sf(xlim = lim_coord_zoom$X, ylim = lim_coord_zoom$Y,expand = FALSE) +
  # labels and theme
  labs(title = NULL
       #, subtitle =  expression(PM[10][] emissions)
       , subtitle =  expression(PM[10][])
       , fill =  expression(PM[10][] (g))
       , color = NULL
       , x = NULL
       , y = NULL) +
  theme(legend.position = c(0.9,0.1)) + 
  theme_minimal() + 
  theme_void() +
  theme(legend.position = c(0.8,0.3),
        text = element_text(family = "LM Roman 10"),
        legend.box.background = element_rect(fill = "white",color = "white"),
        legend.box.margin = margin(3,3,3,3, "pt")) 

pm10_zoom

## f) EF (@ speed = 34.12 kph) -----


# CETESB
my_ef_br_df <- ef_brazil(pollutant = c("PM10","CO2")
                         ,veh_type = "BUS_URBAN_D"
                         ,model_year = 2011
                         ,as_list = FALSE)
my_ef_br_df <- as.data.frame(my_ef_br_df)
data.table::setDT(my_ef_br_df)[,source := "CETESB"]

# EMEP/EEA
my_ef_eu_df <- ef_europe(speed =  units::set_units(34.12,"km/h")
                         ,veh_type = "Ubus Std 15 - 18 t"
                         ,euro = "V"
                         ,pollutant = c("PM10","CO2")
                         ,fuel = "D"
                         ,tech = "SCR"
                         ,as_list = FALSE)
my_ef_eu_df[,source := "EEA"]

# EPA / MOVES
my_ef_moves_df <- ef_usa_moves(pollutant = c("PM10","CO2")
                               ,model_year = 2011
                               ,calendar_year = 2019
                               ,speed = units::set_units(34.12,"km/h")
                               ,fuel_type = "D"
                               ,as_list = FALSE)
my_ef_moves_df[,source := "MOVES"]

# EMFAC
my_ef_emfac_df <- ef_usa_emfac(pollutant =  c("PM10","CO2")
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
my_ef_plot[, variable_f := dplyr::recode_factor(variable
                                                , `CO2` = "CO[2]"
                                                , `PM10` = "PM[10]")]
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
my_ef_br_df <- ef_brazil(pollutant = c("PM10","CO2")
                         ,veh_type = "BUS_URBAN_D"
                         ,model_year = 2011
                         ,as_list = FALSE)
my_ef_br_df <- ef_euro_scaled(ef_local = my_ef_br_df,
                              speed =  units::set_units(seq(10,100,10),"km/h")
                              ,veh_type = "Ubus Std 15 - 18 t"
                              ,euro = "V"
                              ,pollutant = c("PM10","CO2")
                              ,fuel = "D"
                              ,tech = "SCR")
my_ef_br_df <- my_ef_br_df$EF
my_ef_br_df[,source := "CETESB"]

# EMEP/EEA
my_ef_eu_df <- ef_europe(speed =  units::set_units(seq(10,100,10),"km/h")
                         ,veh_type = "Ubus Std 15 - 18 t"
                         ,euro = "V"
                         ,pollutant = c("PM10","CO2")
                         ,fuel = "D"
                         ,tech = "SCR"
                         ,as_list = FALSE)
my_ef_eu_df[,source := "EEA"]

# EPA / MOVES
my_ef_moves_df <- ef_usa_moves(pollutant = c("PM10","CO2")
                               ,model_year = 2011
                               ,calendar_year = 2019
                               ,speed = units::set_units(seq(10,100,10),"km/h")
                               ,fuel_type = "D"
                               ,as_list = FALSE)
my_ef_moves_df[,source := "MOVES"]

# EMFAC
my_ef_emfac_df <- ef_usa_emfac(pollutant =  c("PM10","CO2")
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
my_ef_plot[, variable_f := dplyr::recode_factor(variable
                                                , `CO2` = "CO[2]"
                                                , `PM10` = "PM[10]")]
my_ef_plot[, value := as.numeric(value)]
my_ef_plot[, label := "data source"]
my_ef_plot[, speed := rep(seq(10,100,10),8)]

# plot
ggplot(data = my_ef_plot) + 
  geom_line(aes(y= value,x = speed,color = source),lwd = 0.8)+
  geom_point(aes(y= value,x = speed,color = source))+
  facet_wrap(~variable_f
             ,scales = "free"
             , label = "label_parsed") +
  labs(color = "Source",x = "Speed (km/h)",y = "EF (g/km)")+
  viridis::scale_color_viridis(discrete = TRUE,option = "H",alpha = 1)+
  theme_light()+
  scale_y_continuous(expand = expansion(mult = 0.05)) +
  theme(axis.text.x = element_text(size = 9)
        ,text = element_text(family = "LM Roman 10")
        ,strip.text = element_text(colour = 'black')
        ,legend.position = c(0.9,0.75))

# save
ggsave(filename = "article/data/plots/ef_speed.png",scale = 1.3,
       width = 18,height = 8,units = "cm",dpi = 300)

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
# item 2) Need to read fleet first


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
  geom_bar(aes(y= N,x = as.factor(year),fill = type_name_eu_f),stat = "identity"
           ,position = "dodge",width = 0.75)+
  labs(x = "Year",fill = "Urban bus category",y = "Total number of vehicles")+
  scale_fill_manual(values = viridis::turbo(n = 9,alpha = 0.85)[c(1,3,9)])+
  theme_light()+
  theme(axis.text.x = element_text(size = 9)
        ,text = element_text(family = "LM Roman 10")
        ,strip.text = element_text(colour = 'black')
        ,legend.position = c(0.09,0.825))


# save
ggsave(filename = "article/data/plots/fleet_sp.png",scale = 1.3,
       width = 18,height = 8,units = "cm",dpi = 300)


# 6) Stats avg EF ----
# list-files
files_gps_t <- list.files(path = 'article/data/emi_age/',full.names = TRUE)
files_gps_names_t <- list.files(path = 'article/data/emi_age/',full.names = FALSE)

tmp_stats_ef <- lapply(files_gps_t, readr::read_rds) %>% 
  data.table::rbindlist()

tmp_stats_ef[,fleet_composition := as.numeric(as.character(fleet_composition))]
tmp_stats_ef[,total_VTK := as.numeric(as.character(total_VTK))]
tmp_stats_ef[,VTK := as.numeric(as.character(VTK))]
tmp_stats_ef[,single_VTK := VTK * fleet_composition]

tmp_stats_ef1 <- tmp_stats_ef[,lapply(.SD,sum,na.rm=TRUE)
                              ,.SDcols = c("emi","single_VTK")
                              , by = .(pollutant)]

tmp_stats_ef1[,EF := emi / units::set_units(single_VTK,"km")] 
tmp_stats_ef1[,EF := round(EF,3)] 
tmp_stats_ef1

# End ----