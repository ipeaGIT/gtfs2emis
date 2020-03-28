#
# maps
#
#
rm(list=ls())
# read
emi <- readr::read_rds(path = "../../data/hex_emi/cur/cur.rds") 
map_tile <- readr::read_rds(paste0("../../data-raw/tiles/map_tiles/map_tile_crop_cur.rds"))
muni <- readr::read_rds(paste0("../../data-raw/muni_border/cur.rds")) %>% sf::st_transform(31982)  %>% sf::st_transform(3857)
# read terminals from GTFS
stations <- gtfs2gps::read_gtfs("../../data-raw/gtfs/gtfs_BRA_cur_201910.zip")
stations <- stations$stops[stop_name %like% "Terminal" & stop_desc %in% "",][,.SD[1],by = stop_name]
stations <- stations[,{
  geometry <- sf::st_point(x = matrix(c(stop_lon,stop_lat),ncol = 2)) %>% 
    sf::st_sfc() %>% sf::st_sf()
},by = .(stop_name)] %>% sf::st_as_sf() %>% sf::st_set_crs(4326)  %>% sf::st_transform(3857)
#
# main plot
#
emi1 <- emi 
emi1 <- data.table::setDT(emi1)[,`:=`(EM_CO = sum(EM_CO),EM_NOx = sum(EM_NOx)),by = id][,.SD[1],by = id] 
emi1 <- emi1 %>% sf::st_as_sf() %>% sf::st_transform(3857)
emi1$EM_CO <- emi1$EM_CO * units::set_units(sf::st_area(emi1),km^2) 
emi1$EM_NOx <- emi1$EM_NOx * units::set_units(sf::st_area(emi1),km^2)
emi1$title <- "Total emissions"

pf <- ggplot() +
  geom_raster(data = map_tile, aes(x, y,fill=hex), alpha = 1) + 
  coord_equal() +
  scale_fill_identity() +
  ggnewscale::new_scale_fill() +
  geom_sf(data = muni$geom, colour = "black", fill = NA, size = 0.3) +  
  ggnewscale::new_scale_fill() +
  geom_sf(data = emi1, aes(fill = as.numeric(EM_CO)), color = NA, alpha=.7)  +
  viridis::scale_fill_viridis(breaks = seq(min(as.numeric(emi1$EM_CO),na.rm = T),
                                           max(as.numeric(emi1$EM_CO),na.rm = T),
                                           length.out = 8),
                              limits = c(1.0*min(as.numeric(emi1$EM_CO)),1.0*max(as.numeric(emi1$EM_CO))),
                              label = seq(min(as.numeric(emi1$EM_CO),na.rm = T),
                                          max(as.numeric(emi1$EM_CO),na.rm = T),
                                          length.out = 8) %>% round(),
                              discrete = F,option = "A",
                              direction = +1,
                              guide = guide_colourbar(barheight = 10,
                                                      frame.colour = "black")) + 
  labs(fill = paste0("CO \n (g/day)")) + 
  facet_grid(cols = vars(title)) +
  ggsn::north(x.min = min(map_tile$x),
              x.max = max(map_tile$x),
              y.min = min(map_tile$y),
              y.max = max(map_tile$y),location = "topright",symbol = 12) +
  ggsn::scalebar(x.min = min(map_tile$x),
                 x.max = max(map_tile$x) - 1000,
                 y.min = min(map_tile$y) + 1000,
                 y.max = max(map_tile$y) ,
                 dist = 2,location = "bottomright",
                 dist_unit = "km",st.size = 3,
                 st.bottom = FALSE, st.color = "black",
                 transform = FALSE, model = "WGS84") +
  theme(#panel.background = element_rect(fill = NA),
    legend.position = "right",
    plot.title = element_text(hjust = 0.0,size=10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +  
  coord_sf(expand = F,xlim = c(min(map_tile$x),max(map_tile$x)),
           ylim = c(min(map_tile$y), max(map_tile$y)) )

ggsave(plot = pf,filename = paste0("figures/CO_total_cur.jpg"),
       width = 12.5,height = 16,units = "cm",scale = 1.3,dpi = 300)
