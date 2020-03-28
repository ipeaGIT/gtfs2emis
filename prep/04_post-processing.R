#
# pos-processing functions
#
muni <- readr::read_rds(paste0("../../data-raw/muni_border/",proj_cities$abrev_city,".rds")) %>% sf::st_transform(31982)
hex <- readr::read_rds(paste0("../../data-raw/hex/",proj_cities$abrev_city,"/full09.rds")) %>% 
  data.table::as.data.table() %>% sf::st_as_sf() %>% sf::st_transform(31982)
map_tile <- readr::read_rds(paste0("../../data-raw/tiles/map_tiles/map_tile_crop_",proj_cities$abrev_city,".rds"))
#
# read terminals from GTFS
#
stations <- gtfs2gps::read_gtfs("../../data-raw/gtfs/gtfs_BRA_cur_201910.zip")
term <- stations$stops[stop_name %like% "Terminal" & stop_desc %in% "",][,.SD[1],by = stop_name]
term <- term[,{
  geometry <- sf::st_point(x = matrix(c(stop_lon,stop_lat),ncol = 2)) %>% 
    sf::st_sfc() %>% sf::st_sf()
},by = .(stop_name)] %>% sf::st_as_sf() %>% sf::st_set_crs(4326)
#
# read emi_lines
#
input_folder <- paste0("../../data/gps_linestring_emis/",proj_cities$abrev_city)
emi_line <- list.files(input_folder,recursive = FALSE,full.names = TRUE)
emi_line_name <- list.files(input_folder,recursive = FALSE,full.names = FALSE) %>% 
  stringr::str_remove_all(".rds")
# loop
emi <- lapply(1:length(emi_line),function(i){ # i = 1
  # message
  message(paste0("shape_id #",emi_line_name[i],", ",i," out of ",length(emi_line)))
  # main
  temp_emi <- readr::read_rds(emi_line[i]) %>% sf::st_as_sf() %>% sf::st_transform(31982) 
  e_grid <- vein::emis_grid(spobj = temp_emi[c("EM_CO","EM_NOx")], g = hex)
  # add extra infor
  e_grid$id <- hex$id_hex
  e_grid$tipo_veiculo <- temp_emi$tipo_de_veiculo[1]
  e_grid$frota_ano <- temp_emi$frota_ano[1]
  e_grid$categoria <- temp_emi$categoria[1]
  # remove unuseful data
  e_grid <- e_grid[as.numeric(e_grid$EM_CO) > 0 & as.numeric(e_grid$EM_NOx) > 0,]
  return(e_grid)
}) %>% data.table::rbindlist()
readr::write_rds(x = emi,path = "../../data/hex_emi/cur/cur.rds")
emi <- readr::read_rds(path = "../../data/hex_emi/cur/cur.rds")
emi <- emi[,`:=`(EM_CO = sum(EM_CO),EM_NOx = sum(EM_NOx)),by = id][,.SD[1],by = id] %>% sf::st_as_sf()
emi$EM_CO <- emi$EM_CO * units::set_units(sf::st_area(emi),km^2) 
emi$EM_NOx <- emi$EM_NOx * units::set_units(sf::st_area(emi),km^2)
#
# line
#
line <- lapply(1:length(emi_line),function(i){
  temp_emi <- readr::read_rds(emi_line[i])
}) %>% data.table::rbindlist() %>% sf::st_as_sf() %>% sf::st_transform(31982) 
readr::write_rds(x = line,path = "../../data/linestring_emi/cur/cur.rds")
#
# plot  (1)
#
mapview(emi["EM_CO"],alphaColors = 0.5) + mapview(line$geometry)
ggplot() +
  # cur border
  geom_sf(data = muni$geom, colour = "black", fill = NA, size = 0.3) +  
  # emissions
  geom_sf(data = emi, aes(fill = as.numeric(EM_CO)), color = NA, alpha=.7)  +
  viridis::scale_fill_viridis(breaks = seq(min(as.numeric(emi$EM_CO),na.rm = T),
                                            max(as.numeric(emi$EM_CO),na.rm = T),
                                            length.out = 8),
                               label = seq(min(as.numeric(emi$EM_CO),na.rm = T),
                                           max(as.numeric(emi$EM_CO),na.rm = T),
                                           length.out = 8) %>% round(),
                               discrete = F,option = "A",
                               direction = +1) #+
  # line background
  #geom_sf(data = line$geometry, colour = "black", fill = NA, size = 0.1, alpha=1) + 
  # points terminals
  geom_sf(data = term$geometry, colour = "red",size =5,alpha = 1) 
#
# plot (2)
#
line$departure_time1 <- stringr::str_sub(line$departure_time,1,2)
df <- table(line$departure_time1)
df
ggplot(data = line,aes(x = departure_time1, y = as.numeric(EM_CO))) + 
  geom_bar(aes(fill = as.numeric(EM_CO)),stat="identity",colour="black", size=.3, alpha=.8)

labs(fill = "CO") +
  geom_sf(data = muni, colour = "black", fill = NA, size = 0.3) #+
facet_grid(cols = vars(mod))+
  scale_x_continuous(limits = sf::st_bbox(muni)[c(1,3)] %>% as.vector())+
  scale_y_continuous(limits = sf::st_bbox(muni)[c(2,4)] %>% as.vector())+
  ggsn::north(data = muni,location = "topright",symbol = 12) +
  ggsn::scalebar(x.min = sf::st_bbox(muni)[1] %>% as.numeric(),
                 x.max = sf::st_bbox(muni)[3] %>% as.numeric(),
                 y.min = sf::st_bbox(muni)[2] %>% as.numeric(),
                 y.max = sf::st_bbox(muni)[4] %>% as.numeric(),
                 dist = 2,
                 dist_unit = "km",st.size = 3,
                 st.bottom = FALSE, st.color = "black",
                 transform = FALSE, model = "WGS84") +
  theme(panel.background = element_rect(fill = NA),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5,size=10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())







table(sf::st_geometry_type(temp_emi))
summary(temp_emi$EM_CO)
plot(temp_emi["EM_CO"])
plot(temp_emi["speed"])

#
# emis_grid
#
e_grid <- vein::emis_grid(spobj = temp_emi["EM_CO"], g = hex$geometry)
saveRDS(e_grid,
        file=paste0(path1,"emi/",pol_det[p],"/e_grid09/",tipo,"_LCV_age_",i,".rds"))

ggplot() +
  geom_sf(data = dat, colour = "black", fill = NA, size = 0.3)
geom_sf(data = temp_emi,aes(color = "shape_id"))
