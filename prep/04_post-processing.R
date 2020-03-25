#
# pos-processing functions
#
muni <- readr::read_rds(paste0("../../data-raw/muni_border/",proj_cities$abrev_city,".rds")) %>% sf::st_transform(31982)
hex <- readr::read_rds(paste0("../../data-raw/hex/",proj_cities$abrev_city,"/full09.rds")) %>% 
  data.table::as.data.table() %>% sf::st_as_sf() %>% sf::st_transform(31982)

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
emi <- emi[,`:=`(EM_CO = sum(EM_CO),EM_NOx = sum(EM_NOx)),by = id][,.SD[1],by = id] %>% sf::st_as_sf()
emi$EM_CO <- emi$EM_CO * units::set_units(sf::st_area(emi),km^2)
emi$EM_NOx <- emi$EM_NOx * units::set_units(sf::st_area(emi),km^2)
#
# line
#
line <- lapply(1:length(emi_line),function(i){
  temp_emi <- readr::read_rds(emi_line[i])
}) %>% data.table::rbindlist() %>% sf::st_as_sf() %>% sf::st_transform(31982) 

#
# plot 
#
ggplot() +
  geom_sf(data = muni$geom, colour = "black", fill = NA, size = 0.3) + 
  geom_sf(data = emi, aes(fill = 1000 * as.numeric(EM_NOx)), color = NA, alpha=.7) + 
  geom_sf(data = line$geometry, colour = "grey", fill = NA, size = 0.01, alpha=.3)

ggplot() +
  geom_sf(data = muni$geom, colour = "black", fill = NA, size = 0.3) + 
  geom_sf(data = emi, aes(fill = as.numeric(EM_NOx)), color = NA, alpha=.7)

ggplot() +
  geom_sf(data = muni$geom, colour = "black", fill = NA, size = 0.3) + 
  geom_sf(data = emi, aes(fill = as.numeric(EM_CO)), color = NA, alpha=.7)  +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,"OrRd"),
                       limits = c(1.0*min(as.numeric(emi$EM_CO)),1.0*max(as.numeric(emi$EM_CO))),
                       breaks = seq(min(as.numeric(emi$EM_CO)),1.0*max(as.numeric(emi$EM_CO)),length.out = 6),
                       labels = seq(min(as.numeric(emi$EM_CO)),1.0*max(as.numeric(emi$EM_CO)),length.out = 6) %>% round(3),
                       guide = guide_colourbar(barheight = 10,
                                               frame.colour = "black")) +
  scale_x_continuous(limits = sf::st_bbox(muni)[c(1,3)] %>% as.vector())+
  scale_y_continuous(limits = sf::st_bbox(muni)[c(2,4)] %>% as.vector())


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
